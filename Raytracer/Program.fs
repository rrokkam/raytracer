module PigLatin

open Raytracer.Format
open Raytracer.Vector

[<EntryPoint>]
let main args =
   printfn $"{ppm_header}{ppm_body}"
   let v = 2.0 * {e0=0; e1=2; e2=1} * {e0=3; e1=3; e2=3}
   //let length = len v
   //printfn $"%f{length}"
   //printfn $"%s{string v}"
   int (v+v).e0