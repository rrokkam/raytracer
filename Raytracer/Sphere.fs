module Raytracer.Sphere

open Raytracer.Object
open Raytracer.Vector
open Raytracer.Ray
open Raytracer.Material

type sphere =
    { center: point3
      radius: float
      material: material }

    interface object with
        member s.test r t_min t_max =
            let oc = r.origin - s.center
            let a = len_squared r.direction
            let half_b = oc * r.direction
            let c = len_squared oc - s.radius * s.radius
            let fourth_discriminant = half_b ** 2 - a * c

            if fourth_discriminant < 0 then
                None
            else
                // the value of t where the ray intersects the sphere
                let t1 = (-half_b - sqrt fourth_discriminant) / a
                let t2 = (-half_b + sqrt fourth_discriminant) / a

                let hit_record t =
                    let n = (r.at t - s.center) / s.radius
                    let front_face = r.direction * n < 0
                    let sign = if front_face then 1.0 else -1.0

                    { N = sign * n
                      p = r.at t
                      t = t
                      front_face = front_face }

                if t1 > t_min && t1 < t_max then
                    Some(hit_record t1, s.material)
                else if t2 > t_min && t2 < t_max then
                    Some(hit_record t2, s.material)
                else
                    None
