type face =
  | Tri of int * int * int
  | Quad of int * int * int * int

type point = (float * float * float)

type mesh = point array * face array

val catmull_clark : mesh:mesh -> mesh
