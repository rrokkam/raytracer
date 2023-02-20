module Raytracer.Camera

open Raytracer.Vector
open Raytracer.Ray

let aspect_ratio = 16.0 / 9.0
let viewport_height = 2.0
let viewport_width = aspect_ratio * viewport_height
let focal_length = 1.0

let origin = point3.zero
let horizontal = { vec3.zero with e0 = viewport_width }
let vertical = { vec3.zero with e1 = viewport_height }

let lower_left_corner =
    origin
    - horizontal / 2.0
    - vertical / 2.0
    - { vec3.zero with e2 = focal_length }

let ray_from_camera u v =
    let dir = lower_left_corner + u * horizontal + v * vertical - origin
    { origin = origin; direction = dir }

// let inline clamp minimum maximum value = value |> max minimum |> min maximum
