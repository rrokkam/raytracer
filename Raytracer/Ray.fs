module Raytracer.Ray

open Raytracer.Vector

type ray = { origin: point3; direction: vec3 }

let at r (t: float) : point3 = r.origin + t * r.direction

let hit_sphere center radius r =
    let oc = r.origin - center
    let a = r.direction * r.direction
    let b = 2.0 * oc * r.direction
    let c = oc * oc - radius * radius
    let discriminant = b * b - 4.0 * a * c
    discriminant > 0

let ray_color r =
    let sphere_center = { point3.zero with e2 = -1.0 }

    if hit_sphere sphere_center 0.5 r then
        { r = 1; g = 0; b = 0 }
    else
        let direction = normalize r.direction
        let t = 0.5 * (direction.e1 + 1.0)
        (1.0 - t) * { r = 1.0; g = 1.0; b = 1.0 } + t * { r = 0.5; g = 0.7; b = 1.0 }
