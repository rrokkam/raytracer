module Raytracer.Format

open Raytracer.Vector
let image_width = 256
let image_height = 256
let max_color = 255

let ppm_header = $"P3\n%i{image_width} %i{image_height}\n%i{max_color}\n"

let pixel row col =
    let c =
        { r = float row / float (image_width - 1)
          g = float col / float (image_height - 1)
          b = 0.25 }
    string c

let ppm_body_elements =
    [ for j in [ image_height - 1 .. -1 .. 0 ] do
          eprintfn $"Scanlines remaining: {j}"

          yield
              [ for i in [ 0 .. image_width - 1 ] do
                    yield pixel i j ] ]

let ppm_body = ppm_body_elements |> List.concat |> String.concat ""
