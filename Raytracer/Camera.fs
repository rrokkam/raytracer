module Raytracer.Camera

open Raytracer.Vector
open Raytracer.Ray

let vertical_fov = 20.0 * 2.0 * System.Math.PI / 360.0
let h = tan <| vertical_fov / 2.0
let aspect_ratio = 16.0 / 9.0
let viewport_height = 2.0 * h
let viewport_width = aspect_ratio * viewport_height

let lookfrom = { e0 = 3.0; e1 = 3.0; e2 = 2.0 }
let lookat = { vec3.zero with e2 = -1.0 }
let vup = { vec3.zero with e1 = 1.0 }
let focus_dist = len <| lookfrom - lookat
let aperture = 2.0

let w = normalize <| lookfrom - lookat
let u = normalize <| vup ^^ w
let v = w ^^ u
let focal_length = 1.0

let origin = lookfrom
let horizontal = focus_dist * viewport_width * u
let vertical = focus_dist * viewport_height * v

let lower_left_corner = origin - horizontal / 2.0 - vertical / 2.0 - focus_dist * w
let lens_radius = aperture / 2.0

let ray_from_camera s t R =
    let rd = lens_radius * random_in_unit_disk R
    let offset = rd.e0 * u + rd.e1 * v
    let dir = lower_left_corner + s * horizontal + t * vertical - origin - offset

    { origin = origin + offset
      direction = dir }

// let inline clamp minimum maximum value = value |> max minimum |> min maximum
