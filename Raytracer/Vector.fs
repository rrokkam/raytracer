module Raytracer.Vector

type vec3 =
    { e0: float
      e1: float
      e2: float }

    static member (+)(u, v) =
        { e0 = u.e0 + v.e0
          e1 = u.e1 + v.e1
          e2 = u.e2 + v.e2 }

    static member (*)(t, v) =
        { e0 = t * v.e0
          e1 = t * v.e1
          e2 = t * v.e2 }

    static member (/)(v: vec3, t) = 1.0 / t * v
    static member (~-) v = -1.0 * v
    static member (-)(u, v) = u + -v

    static member (*)(u, v) =
        { e0 = u.e0 * v.e0
          e1 = u.e1 * v.e1
          e2 = u.e2 * v.e2 }

    static member (^)(u, v) =
        { e0 = u.e1 * v.e2 - u.e2 * v.e1
          e1 = u.e2 * v.e0 - u.e0 * v.e2
          e2 = u.e0 * v.e1 - u.e1 * v.e0 }

    override v.ToString() = $"{v.e0} {v.e1} {v.e2}\n"

// printing?
let zero = { e0 = 0; e1 = 0; e2 = 0 }
let len_squared v = v.e0 ** 2 + v.e1 ** 2 + v.e2 ** 2
let len = len_squared >> sqrt
let normalize v = 1.0 / (len v) * v
let norm2 v = v / (len v)

type point3 = vec3

type color =
    { r: float
      g: float
      b: float }

    override c.ToString() =
        let normalize v = int <| 255.999 * v
        $"%i{normalize c.r} %i{normalize c.g} %i{normalize c.b}\n"
