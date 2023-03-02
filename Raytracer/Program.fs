open Raytracer.Vector
open Raytracer.Ray
open Raytracer.Sphere
open Raytracer.Camera
open Raytracer.Material

let R = System.Random()

let random_scene (R: System.Random) : sphere list =
    let ground =
        { center = { vec3.zero with e1 = -1000.0 }
          radius = 1000
          material = { albedo = { r = 0.5; g = 0.5; b = 0.5 } } }

    let dielectric =
        { center = { e0 = 0; e1 = 1.0; e2 = 0 }
          radius = 1.0
          material = { eta = 1.5 } }

    let lambertian =
        { center = { e0 = -4.0; e1 = 1.0; e2 = 0 }
          radius = 1.0
          material = { albedo = { r = 0.4; g = 0.2; b = 0.1 } } }

    let metal =
        { center = { e0 = 4.0; e1 = 1.0; e2 = 0 }
          radius = 1.0
          material =
            { albedo = { r = 0.7; g = 0.6; b = 0.5 }
              fuzz = 0.0 } }

    let material (R: System.Random) : material =
        let r = R.NextDouble()

        match r with
        | r when r < 0.8 ->
            let c1, c2 = random_color R, random_color R

            { albedo =
                { r = c1.r * c2.r
                  g = c1.g * c2.g
                  b = c1.b * c2.b } }
        | r when r < 0.95 ->
            let c = random_color R

            { albedo =
                { r = (c.r + 1.0) / 2.0
                  g = (c.g + 1.0) / 2.0
                  b = (c.b + 1.0) / 2.0 }
              fuzz = (R.NextDouble() + 0.001) / 2.0 }
        | _ -> { eta = 1.5 }

    let randoms: sphere list =
        List.ofSeq
        <| seq {
            for a in [ -1 .. 1 ] do
                for b in [ -1 .. 1 ] do
                    let center =
                        { e0 = float a + 0.9 * R.NextDouble()
                          e1 = 0.2
                          e2 = float b + 0.9 * R.NextDouble() }

                    if len (center - { e0 = 4.0; e1 = 0.2; e2 = 0 }) > 0.9 then
                        yield
                            { center = center
                              radius = 0.2
                              material = material R }

        }

    List.append [ ground; dielectric; lambertian; metal ] randoms


let rec ray_color r world max_bounces =
    match max_bounces, test_many r world 0.001 with
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
    let image_width = 1200
    let image_height = int <| float image_width / aspect_ratio
    let samples_per_pixel = 100
    let max_bounces = 10

    let world = random_scene R

    // Render
    let pixels =
        seq {
            for j in image_height - 1 .. -1 .. 0 do
                eprintfn $"Lines remaining: {j}"

                for i in 0 .. image_width - 1 do
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
