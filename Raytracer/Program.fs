module PigLatin

open Raytracer.Vector
open Raytracer.Ray

[<EntryPoint>]
let main args =
    // Image
    let aspect_ratio = 16.0 / 9.0
    let image_width = 400
    let image_height = int <| float image_width / aspect_ratio // 225

    // Camera
    let viewport_height = 2.0
    let viewport_width = aspect_ratio * viewport_height
    let focal_length = 1.0

    let origin = point3.zero
    let horizontal = { vec3.zero with e0 = viewport_width }
    let vertical = { vec3.zero with e1 = viewport_height }

    let lower_left_corner =
        origin
        - horizontal / 2.0
        - vertical / 2.0
        - { vec3.zero with e2 = focal_length }

    // Render
    let pixels =
        seq {
            for j in [ image_height - 1 .. -1 .. 0 ] do
                for i in [ 0 .. image_width - 1 ] do
                    let u = float i / (float image_width - 1.0)
                    let v = float j / (float image_height - 1.0)
                    let dir = lower_left_corner + u * horizontal + v * vertical - origin
                    let r = { origin = origin; direction = dir }
                    yield ray_color r
        }

    printfn "P3"
    printfn $"%i{image_width} %i{image_height}"
    printfn "255"
    let ppm_body = pixels |> Seq.map string |> String.concat "\n"
    printfn $"{ppm_body}"
    0
