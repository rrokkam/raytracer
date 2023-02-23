open Raytracer.Vector
open Raytracer.Ray
open Raytracer.Sphere
open Raytracer.Object
open Raytracer.Camera
open Raytracer.Material

let R = System.Random()

let rec ray_color r world max_bounces =
    match max_bounces, test_many r world 0.001 infinity with
    | bounces, _ when bounces <= 0 -> color.Zero
    | _, Some (ix, mat) ->
        match mat.scatter r ix R with
        | Some (attenuation, bounce) ->
            let c = ray_color bounce world (max_bounces - 1)

            { r = attenuation.r * c.r
              g = attenuation.g * c.g
              b = attenuation.b * c.b }
        | None -> color.Zero
    | _, None ->
        let direction = normalize r.direction
        let t = 0.5 * (direction.e1 + 1.0)
        (1.0 - t) * { r = 1.0; g = 1.0; b = 1.0 } + t * { r = 0.5; g = 0.7; b = 1.0 }

[<EntryPoint>]
let main _ =
    // Image
    let image_width = 400
    let image_height = int <| float image_width / aspect_ratio // 225
    let samples_per_pixel = 100
    let max_bounces = 50

    // World
    let ground =
        { center = { e0 = 0.0; e1 = -100.5; e2 = -1.0 }
          radius = 100
          material = { albedo = { r = 0.8; g = 0.8; b = 0.0 } } }

    let center =
        { center = { point3.zero with e2 = -1.0 }
          radius = 0.5
          material = { albedo = { r = 0.1; g = 0.2; b = 0.5 } } }

    let left =
        { center = { e0 = -1.0; e1 = 0; e2 = -1.0 }
          radius = 0.5
          material = { eta = 1.5 } }

    let left_hollow =
        { center = { e0 = -1.0; e1 = 0; e2 = -1.0 }
          radius = -0.45
          material = { eta = 1.5 } }

    let right =
        { center = { e0 = 1.0; e1 = 0; e2 = -1.0 }
          radius = 0.5
          material =
            { albedo = { r = 0.8; g = 0.6; b = 0.2 }
              fuzz = 0.0 } }

    let world: object list = [ ground; center; left; left_hollow; right ]

    // Render
    let pixels =
        seq {
            for j in [ image_height - 1 .. -1 .. 0 ] do
                eprintfn $"Lines remaining: {j}"

                for i in [ 0 .. image_width - 1 ] do
                    [ 1..samples_per_pixel ]
                    |> List.map (fun _ ->
                        let u = (float i + R.NextDouble()) / (float image_width - 1.0)
                        let v = (float j + R.NextDouble()) / (float image_height - 1.0)
                        let r = ray_from_camera u v R
                        ray_color r world max_bounces)
                    |> List.average
        }

    printfn "P3"
    printfn $"%i{image_width} %i{image_height}"
    printfn "255"
    let ppm_body = pixels |> Seq.map string |> String.concat "\n"
    printfn $"{ppm_body}"
    0
