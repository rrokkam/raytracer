module Raytracer.Ray

open Raytracer.Vector

type ray =
    { origin: point3
      direction: vec3 }

    member r.at(t) : point3 = r.origin + t * r.direction

type intersect =
    { p: point3
      N: vec3
      t: float
      front_face: bool }
