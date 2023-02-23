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

type dielectric =
    { eta: float }

    member s.reflectance cos_theta refraction_ratio =
        let r0 = ((1.0 - refraction_ratio) / (1.0 + refraction_ratio)) ** 2.0
        r0 + (1.0 - r0) * (1.0 - cos_theta) ** 5.0

    interface material with
        member s.scatter r ix R =
            let unit_direction = normalize r.direction
            let refraction_ratio = if ix.front_face then 1.0 / s.eta else s.eta
            let cos_theta = min (-unit_direction * ix.N) 1.0
            let sin_theta = sqrt <| 1.0 - cos_theta * cos_theta

            let cannot_refract_by_snell = refraction_ratio * sin_theta > 1.0
            let schlick_reflectance = s.reflectance cos_theta refraction_ratio

            let direction =
                if cannot_refract_by_snell || schlick_reflectance > R.NextDouble() then
                    reflect unit_direction ix.N
                else
                    refract unit_direction ix.N refraction_ratio

            Some({ r = 1.0; g = 1.0; b = 1.0 }, { origin = ix.p; direction = direction })
