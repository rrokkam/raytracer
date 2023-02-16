module PigLatin

open Raytracer.Format

[<EntryPoint>]
let main args =
   printfn $"{ppm_header}{ppm_body}" 
   0