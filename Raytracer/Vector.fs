module Raytracer.Vector

type vec3 =
    { e0: float
      e1: float
      e2: float }

    static member zero = { e0 = 0; e1 = 0; e2 = 0 }

    static member (+)(u, v) =
        { e0 = u.e0 + v.e0
          e1 = u.e1 + v.e1
          e2 = u.e2 + v.e2 }

    static member (*)(t, v) =
        { e0 = t * v.e0
          e1 = t * v.e1
          e2 = t * v.e2 }

    static member (/)(v: vec3, t) = 1.0 / t * v
    static member (~-)(v: vec3) = { e0 = -v.e0; e1 = -v.e1; e2 = -v.e2 }
    static member (-)(u: vec3, v: vec3) = u + -v

    static member (*)(u, v) = u.e0 * v.e0 + u.e1 * v.e1 + u.e2 * v.e2

    // use double wedge because single wedge is taken by default
    static member (^^)(u, v) =
        { e0 = u.e1 * v.e2 - u.e2 * v.e1
          e1 = u.e2 * v.e0 - u.e0 * v.e2
          e2 = u.e0 * v.e1 - u.e1 * v.e0 }

    override v.ToString() = $"{v.e0} {v.e1} {v.e2}\n"

let len_squared v = v.e0 ** 2 + v.e1 ** 2 + v.e2 ** 2
let len = len_squared >> sqrt
let normalize v = 1.0 / (len v) * v
let norm2 v = v / (len v)

let reflect (v: vec3) n : vec3 = v - 2.0 * v * n * n

let refract v (n: vec3) refraction_ratio =
    let cos_theta = min (-v * n) 1.0
    let r_perpendicular = refraction_ratio * (v + cos_theta * n)
    let r_parallel = -(sqrt (abs 1.0 - len_squared r_perpendicular)) * n
    r_perpendicular + r_parallel

let NearZero v =
    let s = 1e-8
    v.e0 < s && v.e1 < s && v.e2 < s

// Passing in System.Random prevents calls to this function from being cached.
// Without this, this function would only be called once for the life of the program.
let rec random_in_unit_sphere (R: System.Random) =
    let v =
        { e0 = 2.0 * R.NextDouble() - 1.0
          e1 = 2.0 * R.NextDouble() - 1.0
          e2 = 0.0 }

    if len_squared v <= 1 then v else random_in_unit_sphere R

let random_on_unit_sphere R = normalize <| random_in_unit_sphere R

let rec random_in_unit_disk (R: System.Random) =
    let v =
        { e0 = 2.0 * R.NextDouble() - 1.0
          e1 = 2.0 * R.NextDouble() - 1.0
          e2 = 0.0 }

    if len_squared v <= 1 then v else random_in_unit_disk R

type point3 = vec3

type color =
    { r: float
      g: float
      b: float }

    static member (+)(u, v) =
        { r = u.r + v.r
          g = u.g + v.g
          b = u.b + v.b }

    static member (*)(t, c) =
        { r = t * c.r
          g = t * c.g
          b = t * c.b }

    static member DivideByInt(c: color, q: int) = 1.0 / float q * c
    static member Zero = { r = 0; g = 0; b = 0 }


    override c.ToString() =
        let clamp minimum maximum value = value |> max minimum |> min maximum

        let normalize v =
            int <| 255.999 * clamp 0.0 0.999 (sqrt v)

        $"%i{normalize c.r} %i{normalize c.g} %i{normalize c.b}"

let random_color (R: System.Random) =
    { r = R.NextDouble()
      g = R.NextDouble()
      b = R.NextDouble() }
