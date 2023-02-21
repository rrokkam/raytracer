module Raytracer.Material

open Raytracer.Ray
open Raytracer.Vector

type material =
    abstract member scatter: r: ray -> ix: intersect -> R: System.Random -> (color * ray) option

type lambertian =
    { albedo: color }

    interface material with
        member s.scatter _ ix R =
            let proposed_direction = ix.N + random_on_unit_sphere R

            let bounce =
                { origin = ix.p
                  direction =
                    if NearZero proposed_direction then
                        ix.N
                    else
                        proposed_direction }

            Some(s.albedo, bounce)

type metal =
    { albedo: color
      fuzz: float } // fuzz should be <= 1

    interface material with
        member s.scatter r ix R =
            let reflected = reflect (normalize r.direction) ix.N

            let scattered =
                { origin = ix.p
                  direction = reflected + s.fuzz * random_in_unit_sphere R }

            match reflected * ix.N > 0 with
            | true -> Some(s.albedo, scattered)
            | false -> None
