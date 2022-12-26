open FastNoiseLite
open SixLabors.ImageSharp
open SixLabors.ImageSharp.PixelFormats
open System.Threading.Tasks
open System

let radius =
    4000

let size =
    radius*2

type INoise =
    abstract member Get: x: float -> y: float -> float

type FastNoise(from: FastNoiseLite) =
    let range = sqrt (2f / 4f)
    interface INoise with
        member _.Get x y =
            let rawNoise = from.GetNoise(float32 x, float32 y)
            float ((rawNoise+range)/(range*2f))

type ScaleBias(scale: float, bias: float, inner: INoise) =
    interface INoise with
        member _.Get x y = ((inner.Get x y) * scale) + bias

let lerp (v0: float) (v1: float) (t: float): float =
    (1.0 - t) * v0 + t * v1

let destructure (c0: Rgba32) =
    (float c0.R / 255.0, float c0.G / 255.0, float c0.B / 255.0, float c0.A / 255.0)

let lerpc (c0: Rgba32) (c1: Rgba32) (t: float): Rgba32 =
    let (r0, g0, b0, a0) = destructure c0
    let (r1, g1, b1, a1) = destructure c1
    Rgba32(float32 <| lerp r0 r1 t, float32 <| lerp g0 g1 t, float32 <| lerp b0 b1 t, float32 <| lerp a0 a1 t)

type Blend(lhs: INoise, rhs: INoise, pos: INoise) =
    interface INoise with
        member _.Get x y =
            let lhsV = lhs.Get x y
            let rhsV = rhs.Get x y
            let pos = pos.Get x y
            lerp lhsV rhsV pos            

type Multiply(lhs: INoise, rhs: INoise) =
    interface INoise with
        member _.Get x y =
            let lhsV = lhs.Get x y
            let rhsV = rhs.Get x y
            lhsV * rhsV

type Power(lhs: INoise, rhs: INoise) =
    interface INoise with
        member _.Get x y =
            let lhsV = lhs.Get x y
            let rhsV = rhs.Get x y
            lhsV ** rhsV

type Add(lhs: INoise, rhs: INoise) =
    interface INoise with
        member _.Get x y =
            let lhsV = lhs.Get x y
            let rhsV = rhs.Get x y
            lhsV + rhsV

type Subtract(lhs: INoise, rhs: INoise) =
    interface INoise with
        member _.Get x y =
            let lhsV = lhs.Get x y
            let rhsV = rhs.Get x y
            lhsV - rhsV

type Divide(lhs: INoise, rhs: INoise) =
    interface INoise with
        member _.Get x y =
            let lhsV = lhs.Get x y
            let rhsV = rhs.Get x y
            lhsV / rhsV

type Constant(value: float) =
    interface INoise with
        member _.Get _ _ = value

type YBulge(width: float, exp: float) =
    interface INoise with
        member _.Get _ y =
            (((-cos( (Math.PI / (width / 2.0)) * y )) + 1.0) / 2.0) ** exp

type TwoYBulges(radius: float) =
    interface INoise with
        member _.Get _ y =
            let g x = (sin((Math.PI / radius) * x)) ** 18.0
            let f x = -g(abs(x - radius))
            f y

let twoYBulges: INoise = TwoYBulges radius

let Perlin (seed: int) (frequency: float) =
    let noise = FastNoiseLite seed
    noise.SetNoiseType FastNoiseLite.NoiseType.Perlin
    noise.SetFrequency (float32 frequency)
    FastNoise noise

let OSimplex (seed: int) (frequency: float) =
    let noise = FastNoiseLite seed
    noise.SetNoiseType FastNoiseLite.NoiseType.OpenSimplex2S
    noise.SetFrequency (float32 frequency)
    FastNoise noise

let Ridged (seed: int) (frequency: float) (lacunarity: float) octaves =
    let noise = FastNoiseLite seed
    noise.SetFractalType FastNoiseLite.FractalType.Ridged
    noise.SetFrequency (float32 frequency)
    noise.SetFractalLacunarity (float32 lacunarity)
    noise.SetFractalOctaves octaves
    FastNoise noise

module NoiseOperators =
    let inline (+) (lhs: INoise) (rhs: INoise) =
        Add(lhs, rhs)

    let inline (-) (lhs: INoise) (rhs: INoise) =
        Subtract(lhs, rhs)

    let inline (*) (lhs: INoise) (rhs: INoise) =
        Multiply(lhs, rhs)

    let inline (^) (lhs: INoise) (rhs: INoise) =
        Power(lhs, rhs)

    let inline (/) (lhs: INoise) (rhs: INoise) =
        Divide(lhs, rhs)

    let inline (@) (num: float) =
        Constant(num)

    let inline c num = Constant(num)

module Noises =
    open NoiseOperators

    let elevationNoise: INoise =
        let summedNoise =
            let broadNoise = OSimplex 50 0.000935
            let coarseNoise = Perlin 3 0.00435
            let coarserNoise = Perlin 51239213 0.00135
            let coarsererNoise = Perlin 3021 0.00735
            let ridgeyNoise = Ridged 30123 0.012 2.0 6
            let e =
                broadNoise +
                coarseNoise * c 0.5 +
                coarserNoise * c 0.25 +
                coarsererNoise * c 0.2 + 
                ridgeyNoise * c 0.1
            e / c 1.95

        let redistributed = summedNoise ^ c 4.0
        redistributed

    let temperatureNoise: INoise =
        let summedNoise =
            let broadNoise = YBulge (size, 0.4) // OSimplex 12312 0.00135
            let coarseNoise = Ridged 1234 0.00135 2.0 6
            let coarserNoise = Perlin 1245123 0.00135
            let ridgeyNoise = Ridged 12353214 0.020 1.0 6
            let e =
                broadNoise +
                coarseNoise * c 0.5 +
                coarserNoise * c 0.25 +
                ridgeyNoise * c 0.2
            e / c 1.75
        let redistributed = summedNoise ^ c 2.0
        redistributed

    let moistureNoise: INoise =
        let summedNoise =
            let broadNoise = Perlin 137 0.00135
            let coarseNoise = Ridged 1239 0.00135 2.0 6
            let coarserNoise = Perlin 1239 0.00135
            let ridgeyNoise = Ridged 12301 0.020 1.0 6
            let e =
                broadNoise +
                coarseNoise * c 0.5 +
                coarserNoise * c 0.25 +
                ridgeyNoise * c 0.2
            e / c 1.75

        let redistributed = summedNoise ^ c 2.0
        redistributed

open Noises

let WaterLevel = 0.15
let BeachLevel = 0.02

let biomeColor elevation moisture =
    let color r g b =
        Rgba32(byte r, byte g, byte b)

    if elevation < WaterLevel then
        color 0x44 0x44 0x7a // ocean
    else if elevation < WaterLevel+BeachLevel then
        color 0xA0 0x90 0x77 // beach
    else if elevation > 0.8 then
        if moisture < 0.1 then
            color 0x55 0x55 0x55 // scorched
        else if moisture < 0.2 then
            color 0x88 0x88 0x88 // bare
        else if moisture < 0.5 then
            color 0xbb 0xbb 0xaa // tundra
        else
            color 0xdd 0xdd 0xe4 // snow
    else if elevation > 0.6 then
        if moisture < 0.33 then
            color 0xc9 0xd2 0x9b // temperate desert
        else if moisture < 0.66 then
            color 0x88 0x99 0x77 // shrubland
        else
            color 0x99 0xaa 0x77 // taiga
    else if elevation > 0.3 then
        if moisture < 0.16 then
            color 0xc9 0xd2 0x9b // temperate desert
        else if moisture < 0.50 then
            color 0x88 0xaa 0x55 // grassland
        else if moisture < 0.83 then
            color 0x67 0x94 0x59 // temperate deciduous forest
        else
            color 0x44 0x88 0x55 // temperate rainforest
    else if moisture < 0.16 then
        color 0xd2 0xb9 0x8b // subtropical desert
    else if moisture < 0.33 then
        color 0x88 0xaa 0x55 // grassland
    else if moisture < 0.66 then
        color 0x55 0x99 0x44 // tropical seasonal forest
    else
        color 0x33 0x77 0x55 // tropical rainforest

// let elevationNoise: INoise =
//     let baseLandNoise =
//         let noise = FastNoiseLite 1
//         noise.SetNoiseType FastNoiseLite.NoiseType.Perlin
//         noise.SetFrequency 0.00135f
//         noise.SetFractalLacunarity 2.0f
//         noise.SetFractalOctaves 6
//         FastNoise noise
//     let mountainNoise =
//         let noise = FastNoiseLite 1
//         noise.SetFractalType FastNoiseLite.FractalType.Ridged
//         noise.SetFrequency 0.012f
//         noise.SetFractalLacunarity 2.0f
//         noise.SetFractalOctaves 6
//         FastNoise noise
//     let landMountainDeterminer =
//         let noise = FastNoiseLite 1
//         noise.SetNoiseType FastNoiseLite.NoiseType.Perlin
//         noise.SetFrequency 1.2f
//         noise.SetFractalLacunarity 2.0f
//         noise.SetFractalOctaves 5
//         FastNoise noise
//     let coastRoughener =
//         let noise = FastNoiseLite 1
//         noise.SetNoiseType FastNoiseLite.NoiseType.Perlin
//         noise.SetFrequency 0.12f
//         noise.SetFractalLacunarity 2.0f
//         noise.SetFractalOctaves 5
//         FastNoise noise

//     let elev = Blend(baseLandNoise, mountainNoise, Constant(0.2)) // Blend(baseLandNoise, Multiply(mountainNoise, ScaleBias(0.5, 0.5, landMountainDeterminer)), Constant(0.5))
//     let elev' = Blend(elev, coastRoughener, Constant(0.1))
//     let elev'' = ScaleBias(0.5, 0.5, elev')
//     let elev''' = Power(elev'', Constant(3.0))
//     let elev'''' = ScaleBias(5500.0, -5.00, elev''')

//     elev''''

let funkysin taper length theta =
    if theta > taper && theta < length-taper then
        1.0
    else
        sin (Math.PI * (theta / (taper*2.0)))

let doot = funkysin (float radius / 3.0) (float radius*2.0)

let colorAt x y =
    let e = elevationNoise.Get x y
    let m = moistureNoise.Get x y

    let e' =
        let mutable e' = e - WaterLevel - BeachLevel
        if e' >= 0 then
            let bulge = doot y // sin(Math.PI * (y / float size))
            e' <- lerp 1.0 e' bulge
            if e' < 0 then
                e' <- 0
        e' + WaterLevel + BeachLevel
        // if e-WaterLevel > 0.0 then
        //     let e0 = e - WaterLevel
        //     if e0 >= 0 then
        //         let adj = 
        //         if adj < 0 then
        //             WaterLevel
        //         else
        //             adj + WaterLevel
        //     else
        //         e
        // else
        //     e

    biomeColor e' m

let toColor elevation =
    if elevation < WaterLevel then
        Rgba32(0f, 0f, 1f)
    else
        Rgba32(float32 elevation, float32 elevation, float32 elevation)
    // if (elevation - 500.0) < 0.0 then
    //     let deepness = float32 <| abs ((elevation - 500.0) / 500.0)
    //     Rgba32(0f, 0f, 1f-deepness)
    // else
    //     let elevationAboveSeaLevel = float32 <| (elevation - 500.0)
    //     if elevationAboveSeaLevel < 1000f then
    //         Rgba32(0f, 1f, 0f)
    //     else
    //         Rgba32(elevationAboveSeaLevel / 5000f, 1f, elevationAboveSeaLevel / 5000f)

let dist x y =
    let dx = abs ((float x) - (float radius)) ** (float 2)
    let dy = abs ((float y) - (float radius)) ** (float 2)
    sqrt (dx+dy)

let inline compute (image: string) (pixelColor: float -> float -> Rgba32) =
    use img = new Image<Rgba32>(radius*2, radius*2)
    let ys = seq { 1..radius*2 }
    Parallel.ForEach(
        ys,
        fun (y: int) ->
            for x in 1..radius*2 do
                img[x-1, y-1] <- pixelColor x y
    ) |> ignore
    img.Save(image)

let inline greyscale (noise: INoise) (x: float) (y: float) =
    let e = float32 <| noise.Get x y
    Rgba32(e, e, e)

let inline cutoff (cutoff: float) (noise: INoise) (x: float) (y: float) =
    let e = if noise.Get x y < cutoff then 0f else 1f
    Rgba32(e, e, e)

let colorAtNew (x: float) (y: float): Rgba32 =
    let inline color r g b =
        Rgba32(byte r, byte g, byte b)

    let e = elevationNoise.Get x y
    if e <= WaterLevel then
        let t = temperatureNoise.Get x y
        if t < 0.1 then
            color 0x73 0x9c 0xda
        else if t < 0.2 then
            // cold ocean
            color 0x39 0x38 0xC9
        else if t < 0.3 then
            // chilly ocean
            color 0x3D 0x57 0xD6
        else if t < 0.6 then
            // normal ocean
            color 0x3F 0x76 0xE4
        else if t < 0.8 then
            // lukewarm ocean
            color 0x45 0xAD 0xF2
        else
            // warm ocean
            color 0x43 0xD5 0xEE
    else if e < WaterLevel+BeachLevel then
        // beach
        color 0xA0 0x90 0x77
    else
        let t = temperatureNoise.Get x y
        let m = moistureNoise.Get x y // max (moistureNoise.Get x y + twoYBulges.Get x y) 0.0
        if t < 0.2 then
            if m < 0.1 then
                color 0x55 0x55 0x55 // scorched
            else if m < 0.2 then
                color 0x88 0x88 0x88 // bare
            else if m < 0.5 then
                color 0xbb 0xbb 0xaa // tundra
            else
                color 0xdd 0xdd 0xe4 // snow
        else if t < 0.4 then
            if m < 0.3 then
                color 0x93 0xb7 0x68 // plains
            else
                color 0x3c 0x5e 0x53 // taiga
        else if t < 0.7 then
            if m < 0.25 then
                color 0x93 0xb7 0x68 // plains
            else if m < 0.5 then
                color 0xb1 0xc0 0x67 // shrubland
            else if m < 0.75 then
                color 0x3b 0x7b 0x4e // forest
            else
                color 0x13 0xf9 0xb6 // swamp
        else
            if m < 0.25 then
                color 0xfa 0x9a 0x24 // desert
            else if m < 0.5 then
                color 0xc0 0xb6 0x67 // savannah
            else if m < 0.75 then
                color 0x61 0xa1 0x74 // seasonal forest
            else
                color 0x5c 0x82 0x16 // jungle
                // let c = (e - WaterLevel) / (1.0 - WaterLevel)
                // Rgba32(0f, float32 c, 0f)
    // let m = moistureNoise.Get x y
    // let t = temperatureNoise.Get x y
    // Rgba32(float32 e, float32 m, float32 t)

[<EntryPoint>]
let main args =
    printfn "Running..."
    // compute "img.png" colorAt
    // compute "elv.png" (cutoff WaterLevel elevationNoise)
    // compute "tmp.png" (greyscale temperatureNoise)
    // compute "moi.png" (greyscale moistureNoise)
    // compute "bulge.png" (greyscale (YBulge (size, 7.0)))
    compute "neu.png" colorAtNew

    0

