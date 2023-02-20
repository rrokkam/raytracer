open Raytracer.Vector
open Raytracer.Ray
open Raytracer.Sphere
open Raytracer.Object
open Raytracer.Camera

let ray_color r world =
    match test_many r world 0 infinity with
    | Some ix ->
        0.5
        * { r = ix.N.e0 + 1.0
            g = ix.N.e1 + 1.0
            b = ix.N.e2 + 1.0 }
    | None ->
        let direction = normalize r.direction
        let t = 0.5 * (direction.e1 + 1.0)
        (1.0 - t) * { r = 1.0; g = 1.0; b = 1.0 } + t * { r = 0.5; g = 0.7; b = 1.0 }

[<EntryPoint>]
let main _ =
    // Image
    let aspect_ratio = 16.0 / 9.0
    let image_width = 400
    let image_height = int <| float image_width / aspect_ratio // 225
    let samples_per_pixel = 100

    // World
    let sphere =
        { center = { point3.zero with e2 = -1.0 }
          radius = 0.5 }

    let sphere2 =
        { center = { e0 = 0.0; e1 = -100.5; e2 = -1.0 }
          radius = 100 }

    let world: object list = [ sphere; sphere2 ]

    let R = System.Random()

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
                        let r = ray_from_camera u v
                        ray_color r world)
                    |> List.average
        }

    printfn "P3"
    printfn $"%i{image_width} %i{image_height}"
    printfn "255"
    let ppm_body = pixels |> Seq.map string |> String.concat "\n"
    printfn $"{ppm_body}"
    0
