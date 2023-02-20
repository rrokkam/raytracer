module Raytracer.Sphere

open Raytracer.Object
open Raytracer.Vector
open Raytracer.Ray

type sphere = {center:point3; radius:float} with
    interface object with
        member s.test r t_min t_max =
            let oc = r.origin - s.center
            let a = len_squared r.direction
            let half_b = oc * r.direction
            let c = len_squared oc - s.radius * s.radius
            let fourth_discriminant = half_b ** 2 - a * c

            if fourth_discriminant > 0 then
                // the value of t where the ray intersects the sphere
                Some({t=(-half_b - sqrt fourth_discriminant) / a})
            else
                None
