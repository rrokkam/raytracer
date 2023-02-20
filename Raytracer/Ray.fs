module Raytracer.Ray

open Raytracer.Vector

type ray =
    { origin: point3
      direction: vec3 }

    member r.at(t) : point3 = r.origin + t * r.direction

