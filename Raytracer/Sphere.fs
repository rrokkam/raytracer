module Raytracer.Sphere

open Raytracer.Vector
open Raytracer.Ray
open Raytracer.Material

type sphere =
    { center: point3
      radius: float
      material: material }

let hit_record (r: ray) s t =
    let n = (r.at t - s.center) / s.radius
    let front_face = r.direction * n < 0

    { N = if front_face then n else -n
      p = r.at t
      t = t
      front_face = front_face }

let test r s =
    let oc = r.origin - s.center
    let a = len_squared r.direction
    let half_b = oc * r.direction
    let c = len_squared oc - s.radius * s.radius
    let fourth_discriminant = half_b ** 2 - a * c

    if fourth_discriminant < 0 then
        []
    else
        // the value of t where the ray intersects the sphere
        let t1 = (-half_b - sqrt fourth_discriminant) / a
        let t2 = (-half_b + sqrt fourth_discriminant) / a

        [ s, t1; s, t2 ]

let minBy f l =
    if List.isEmpty l then None else Some(l |> List.minBy f)

let test_many r (spheres: sphere list) t_min : (intersect * material) option =
    spheres
    |> List.map (test r)
    |> List.collect id
    |> List.filter (fun (_, t) -> t > t_min)
    |> minBy snd
    |> Option.map (fun (s, t) -> hit_record r s t, s.material)
