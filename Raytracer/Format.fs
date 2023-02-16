module Raytracer.Format

let image_width = 256
let image_height = 256

let ppm_header = $"P3\n%i{image_width} %i{image_height}\n255\n"

let pixel row col =
    let r = float row / float (image_width - 1)
    let g = float col / float (image_height - 1)
    let b = 0.25
    
    let ir = int (255.999 * r)
    let ig = int (255.999 * g)
    let ib = int (255.999 * b)
    $"%i{ir} %i{ig} %i{ib}\n"

let ppm_body_elements =
    [ for j in [ image_height - 1 .. -1 .. 0] do
          eprintfn $"Scanlines remaining: {j}"
          yield
              [ for i in [ 0 .. image_width - 1 ] do
                    yield pixel i j ] ]

let ppm_body =
    ppm_body_elements
        |> List.map (String.concat "")
        |> String.concat ""