module Raytracer.Ray

open Raytracer.Vector

type ray = { origin: point3; direction: vec3 }

let at r (t: float) : point3 = r.origin + t * r.direction

let ray_color r =
    let direction = normalize r.direction
    let t = 0.5 * (direction.e1 + 1.0)
    (1.0 - t) * { r = 1.0; g = 1.0; b = 1.0 } + t * { r = 0.5; g = 0.7; b = 1.0 }
