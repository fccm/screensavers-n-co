open GL
open Glu
open VertArray

open Subsurf

let li = ref 0
(*
let polyfactor = ref 1.0
let polyunits  = ref 0.1
*)
let polyfactor = ref 0.8
let polyunits  = ref 0.0


let calculate_normal points faces =
  let minus p1 p2 =
    let x1, y1, z1 = points.(p1)
    and x2, y2, z2 = points.(p2) in
    (x1 -. x2,
     y1 -. y2,
     z1 -. z2)
  in
  let aux face = match face with
  | Quad (p1, p2, p3, _)
  | Tri (p1, p2, p3) ->
      let u_x, u_y, u_z = minus p2 p1
      and v_x, v_y, v_z = minus p3 p1
      in
      let x = (u_y *. v_z) -. (u_z *. v_y)
      and y = (u_z *. v_x) -. (u_x *. v_z)
      and z = (u_x *. v_y) -. (u_y *. v_x)
      in
      let len = sqrt ( x *. x +. y *. y +. z *. z ) in
      let x, y, z = x /. len, y /. len, z /. len in
      (face, (x, y, z))
  in
  let r = Array.map aux faces in
  (r)


open Dynar

let extrude_faces len points faces_norm =
  let da_points = Dynar.of_array points in
  let new_faces = Dynar.of_array [| |] in
  let push_face face = Dynar.push new_faces face in
  let trans (xn,yn,zn) pi =
    let (px,py,pz) = points.(pi) in
    (xn +. px, yn +. py, zn +. pz)
  in
  let scale f (x,y,z) = (f *. x, f *. y, f *. z) in
  Array.iter (function
  | Tri(a,b,c), normal ->
      let n = scale len normal in
      let e_ = trans n a
      and f_ = trans n b
      and g_ = trans n c in
      let e = Dynar.pushi da_points e_
      and f = Dynar.pushi da_points f_
      and g = Dynar.pushi da_points g_ in
      push_face (Tri (e,f,g));
      push_face (Quad (b,c,g,f));
      push_face (Quad (a,b,f,e));
      push_face (Quad (c,a,e,g));
  | Quad(a,b,c,d), normal ->
      let n = scale len normal in
      let e_ = trans n a
      and f_ = trans n b
      and g_ = trans n c
      and h_ = trans n d in
      let e = Dynar.pushi da_points e_
      and f = Dynar.pushi da_points f_
      and g = Dynar.pushi da_points g_
      and h = Dynar.pushi da_points h_ in
      push_face (Quad (e,f,g,h));
      push_face (Quad (b,c,g,f));
      push_face (Quad (c,d,h,g));
      push_face (Quad (d,a,e,h));
      push_face (Quad (a,b,f,e));
  ) faces_norm;
  (Dynar.to_array da_points,
   Dynar.to_array new_faces)


let catmull_clark_iter n mesh =
  let rec aux i mesh =
    if i >= n then (mesh)
    else aux (succ i) (Subsurf.catmull_clark mesh)
  in
  aux 0 mesh


let mk_geom1 mult =
  (*
  let pointsC = [|
    (-1.0,  1.0,  1.0);
    (-1.0, -1.0,  1.0);
    ( 1.0, -1.0,  1.0);
    ( 1.0,  1.0,  1.0);
    ( 1.0, -1.0, -1.0);
    ( 1.0,  1.0, -1.0);
    (-1.0, -1.0, -1.0);
    (-1.0,  1.0, -1.0);
  |]
  and facesC = [|
    Quad (0, 1, 2, 3);
    Quad (3, 2, 4, 5);
    Quad (5, 4, 6, 7);
    Quad (7, 0, 3, 5);
    Quad (7, 6, 1, 0);
    Quad (4, 2, 1, 6);
  |] in
  *)
  let pointsC = [|
    ( 0.0, -1.2,  0.0);
    ( 1.0,  0.0,  1.0);
    (-1.0,  0.0,  1.0);
    (-1.0,  0.0, -1.0);
    ( 1.0,  0.0, -1.0);
    (-0.0,  1.2, -0.0);
  |]
  and facesC = [|
    Tri (0, 1, 2);
    Tri (0, 2, 3);
    Tri (0, 3, 4);
    Tri (1, 0, 4);
    Tri (4, 3, 5);
    Tri (3, 2, 5);
    Tri (1, 4, 5);
    Tri (2, 1, 5);
  |] in
  (*
  let points0 = [|
     1.0,  1.0, -1.0;     1.0, -1.0, -1.0;    -1.0, -1.0, -1.0;
    -1.0,  1.0, -1.0;     1.0,  1.0,  1.0;     1.0, -1.0,  1.0;
    -1.0, -1.0,  1.0;    -1.0,  1.0,  1.0;    -1.0,  3.0,  1.0;
     1.0,  3.0,  1.0;    -1.0,  3.0, -1.0;     1.0,  3.0, -1.0;
    -1.0,  1.0,  3.0;    -1.0, -1.0,  3.0;     1.0, -1.0,  3.0;
     1.0,  1.0,  3.0;    -1.0, -3.0,  1.0;     1.0, -3.0,  1.0;
    -1.0, -3.0, -1.0;     1.0, -3.0, -1.0;    -1.0,  1.0, -3.0;
    -1.0, -1.0, -3.0;     1.0, -1.0, -3.0;     1.0,  1.0, -3.0;
     3.0, -1.0,  1.0;     3.0,  1.0,  1.0;     3.0, -1.0, -1.0;
     3.0,  1.0, -1.0;    -3.0,  1.0,  1.0;    -3.0, -1.0,  1.0;
    -3.0,  1.0, -1.0;    -3.0, -1.0, -1.0;
  |]
  and faces0 = [|
    Quad (7, 4, 9, 8);      Quad (3, 7, 8, 10);     Quad (4, 0, 11, 9);
    Quad (0, 3, 10, 11);    Quad (9, 11, 10, 8);    Quad (7, 6, 13, 12);
    Quad (6, 5, 14, 13);    Quad (4, 7, 12, 15);    Quad (5, 4, 15, 14);
    Quad (15, 12, 13, 14);  Quad (5, 6, 16, 17);    Quad (6, 2, 18, 16);
    Quad (1, 5, 17, 19);    Quad (2, 1, 19, 18);    Quad (19, 17, 16, 18);
    Quad (2, 3, 20, 21);    Quad (1, 2, 21, 22);    Quad (3, 0, 23, 20);
    Quad (0, 1, 22, 23);    Quad (23, 22, 21, 20);  Quad (4, 5, 24, 25);
    Quad (5, 1, 26, 24);    Quad (0, 4, 25, 27);    Quad (1, 0, 27, 26);
    Quad (27, 25, 24, 26);  Quad (6, 7, 28, 29);    Quad (7, 3, 30, 28);
    Quad (2, 6, 29, 31);    Quad (3, 2, 31, 30);    Quad (31, 29, 28, 30);
  |] in
  *)
  let facesC_norm = calculate_normal pointsC facesC in
  let mesh = extrude_faces mult pointsC facesC_norm in
  let res = catmull_clark_iter 2 mesh in
  (res)
;;

let mk_geom2 _ =
  let pointsC = [|
    ( 0.687388,  0.705800,  0.599684);  (-0.687390,  0.705800,  0.599684);
    (-0.687390, -0.705800,  0.599684);  (-0.646252, -0.684296,  0.942610);
    (-0.646254,  0.684296,  0.942610);  ( 0.646250,  0.684296,  0.942610);
    ( 0.484810,  0.464598,  1.687004);  (-0.484812,  0.464598,  1.687004);
    (-0.484812, -0.464598,  1.687004);  ( 0.484810, -0.464598,  1.687004);
    (-0.306562,  1.703306,  1.799258);  ( 0.306558,  1.703308,  1.799258);
    ( 0.408642,  1.936078,  1.039236);  (-0.408646,  1.936078,  1.039236);
    (-0.442850,  2.215370,  0.599684);  ( 0.442846,  2.215372,  0.599684);
    (-0.442850,  2.215372, -0.599686);  ( 0.442846,  2.215372, -0.599686);
    ( 0.442846, -2.215370, -0.599686);  (-0.442850, -2.215370, -0.599686);
    ( 0.442846, -2.215370,  0.599684);  (-0.442850, -2.215370,  0.599684);
    (-0.408646, -1.936076,  1.039236);  ( 0.408642, -1.936078,  1.039236);
    ( 0.306558, -1.703306,  1.799258);  (-0.306562, -1.703306,  1.799258);
    ( 0.646250, -0.684296,  0.942610);  ( 0.687388, -0.705800,  0.599684);
    ( 0.687388, -0.705800, -0.599686);  ( 0.646250, -0.684296, -0.942612);
    (-0.306562, -1.703306, -1.799260);  ( 0.306558, -1.703306, -1.799260);
    ( 0.408642, -1.936078, -1.039238);  (-0.408646, -1.936076, -1.039238);
    (-0.408646,  1.936078, -1.039238);  ( 0.408642,  1.936078, -1.039238);
    ( 0.306558,  1.703308, -1.799260);  (-0.306562,  1.703306, -1.799260);
    ( 0.484810, -0.464598, -1.687004);  (-0.484812, -0.464598, -1.687004);
    (-0.484812,  0.464598, -1.687004);  ( 0.484810,  0.464598, -1.687004);
    ( 0.646250,  0.684296, -0.942612);  (-0.646254,  0.684296, -0.942612);
    (-0.646252, -0.684296, -0.942612);  (-0.687390, -0.705800, -0.599686);
    (-0.687390,  0.705800, -0.599686);  ( 0.687388,  0.705800, -0.599686);
  |]
  and facesC = [|
    Quad (46, 1, 2, 45);
    Quad (0, 47, 28, 27);
    Quad (2, 1, 4, 3);
    Quad (0, 27, 26, 5);
    Quad (1, 0, 5, 4);
    Quad (5, 26, 9, 6);
    Quad (3, 4, 7, 8);
    Quad (6, 9, 8, 7);
    Quad (7, 4, 13, 10);
    Quad (6, 7, 10, 11);
    Quad (5, 6, 11, 12);
    Quad (4, 5, 12, 13);
    Quad (13, 12, 11, 10);
    Quad (0, 1, 14, 15);
    Quad (1, 46, 16, 14);
    Quad (47, 0, 15, 17);
    Quad (46, 47, 17, 16);
    Quad (17, 15, 14, 16);
    Quad (18, 20, 21, 19);
    Quad (45, 28, 18, 19);
    Quad (28, 27, 20, 18);
    Quad (2, 45, 19, 21);
    Quad (27, 2, 21, 20);
    Quad (22, 23, 24, 25);
    Quad (3, 26, 23, 22);
    Quad (26, 9, 24, 23);
    Quad (9, 8, 25, 24);
    Quad (8, 3, 22, 25);
    Quad (2, 27, 26, 3);
    Quad (45, 28, 29, 44);
    Quad (39, 44, 33, 30);
    Quad (38, 39, 30, 31);
    Quad (29, 38, 31, 32);
    Quad (44, 29, 32, 33);
    Quad (33, 32, 31, 30);
    Quad (34, 35, 36, 37);
    Quad (43, 42, 35, 34);
    Quad (42, 41, 36, 35);
    Quad (41, 40, 37, 36);
    Quad (40, 43, 34, 37);
    Quad (41, 38, 39, 40);
    Quad (44, 43, 40, 39);
    Quad (42, 29, 38, 41);
    Quad (46, 47, 42, 43);
    Quad (47, 28, 29, 42);
    Quad (45, 46, 43, 44);
  |] in
  let res = catmull_clark_iter 2 (pointsC, facesC) in
  (res)
;;


let mk_geom3 _ =
  let pointsC = [|
0.707106769085, -0.5, -0.707106769085;
0.965925812721, -0.5, 0.258819162846;
0.258818924427, -0.5, 0.965925872326;
-0.707106888294, -0.5, 0.707106649876;
-0.965925693512, -0.5, -0.258819460869;
-0.25881883502, -0.5, -0.965925872326;
0.965925812721, 0.5, 0.258819162846;
0.707106769085, 0.5, -0.707106769085;
0.258818924427, 0.5, 0.965925872326;
-0.707106888294, 0.5, 0.707106649876;
-0.965925693512, 0.5, -0.258819460869;
-0.25881883502, 0.5, -0.965925872326;
3.27825546265e-07, 0.5, -1.93185162544;
0.965925931931, 0.5, -1.6730325222;
3.27825546265e-07, -0.5, -1.93185162544;
0.965925931931, -0.5, -1.6730325222;
1.6730325222, 0.5, -0.965925812721;
1.93185162544, 0.5, 1.49011611938e-07;
1.93185162544, -0.5, 1.49011611938e-07;
1.6730325222, -0.5, -0.965925812721;
0.965925633907, 0.5, 1.67303276062;
1.6730325222, 0.5, 0.96592605114;
0.965925633907, -0.5, 1.67303276062;
1.6730325222, -0.5, 0.96592605114;
-0.965926110744, 0.5, 1.6730325222;
-2.98023223877e-07, 0.5, 1.93185162544;
-0.965926110744, -0.5, 1.6730325222;
-2.98023223877e-07, -0.5, 1.93185162544;
-1.93185162544, 0.5, -7.15255737305e-07;
-1.67303276062, 0.5, 0.965925395489;
-1.93185162544, -0.5, -7.15255737305e-07;
-1.67303276062, -0.5, 0.965925395489;
-0.965925455093, 0.5, -1.67303287983;
-1.67303228378, 0.5, -0.965926468372;
-0.965925455093, -0.5, -1.67303287983;
-1.67303228378, -0.5, -0.965926468372;
5.96046447754e-08, -1.75351452827, 0.0;
5.96046447754e-08, 1.75351452827, 0.0;
  |]
  and facesC = [|
    Quad(11, 7, 13, 12);
    Quad(5, 11, 12, 14);
    Quad(7, 0, 15, 13);
    Quad(0, 5, 14, 15);
    Quad(15, 14, 12, 13);
    Quad(6, 1, 18, 17);
    Quad(7, 6, 17, 16);
    Quad(0, 7, 16, 19);
    Quad(1, 0, 19, 18);
    Quad(18, 19, 16, 17);
    Quad(8, 2, 22, 20);
    Quad(6, 8, 20, 21);
    Quad(1, 6, 21, 23);
    Quad(2, 1, 23, 22);
    Quad(22, 23, 21, 20);
    Quad(9, 3, 26, 24);
    Quad(8, 9, 24, 25);
    Quad(2, 8, 25, 27);
    Quad(3, 2, 27, 26);
    Quad(26, 27, 25, 24);
    Quad(10, 4, 30, 28);
    Quad(9, 10, 28, 29);
    Quad(3, 9, 29, 31);
    Quad(4, 3, 31, 30);
    Quad(30, 31, 29, 28);
    Quad(11, 5, 34, 32);
    Quad(10, 11, 32, 33);
    Quad(4, 10, 33, 35);
    Quad(5, 4, 35, 34);
    Quad(34, 35, 33, 32);
    Quad(2, 3, 4, 36);
    Quad(0, 36, 4, 5);
    Quad(0, 1, 2, 36);
    Quad(8, 37, 10, 9);
    Quad(6, 7, 37, 8);
    Quad(10, 37, 7, 11);
  |] in
  let res = catmull_clark_iter 2 (pointsC, facesC) in
  (res)
;;


let mk_geom4 _ =
  let pointsC = [|
 0.502549529076, -2.85817146301, -0.502549529076;
 0.502549529076, -2.85817146301,  0.50254958868;
-0.50254970789,  -2.85817146301,  0.502549409866;
-0.502549350262, -2.85817146301, -0.502549648285;
 0.50254970789,  -1.85307180882, -0.502549290657;
 0.502549231052, -1.85307180882,  0.502549886703;
-0.502549767494, -1.85307180882,  0.502549350262;
-0.502549529076, -1.85307180882, -0.502549529076;
-0.289404898882, -1.2831619978,  -0.28940486908;
-0.289405047894, -1.2831619978,   0.289404779673;
 0.289404690266, -1.2831619978,   0.289405107498;
 0.289404988289, -1.2831619978,  -0.289404720068;
 0.289404988289,  0.438185870647, -0.289404720068;
 0.289404690266,  0.438185870647,  0.289405107498;
-0.289405047894,  0.438185870647,  0.289404779673;
-0.289404898882,  0.438185870647, -0.28940486908;
 0.731100380421,  0.914681971073,  0.317542910576;
 0.731100738049,  0.914681971073, -0.317542493343;
-0.31754270196,   0.914681971073, -0.731100678444;
 0.317542821169,  0.914681971073, -0.731100440025;
-0.731100797653,  0.914681971073,  0.317542552948;
-0.731100618839,  0.914681971073, -0.317542672157;
-0.317542850971,  0.914681971073,  0.731100559235;
 0.317542493343,  0.914681971073,  0.731100916862;
 0.731100380421,  1.1903873682,   0.317542910576;
 0.731100738049,  1.1903873682,  -0.317542493343;
 0.317542821169,  1.1903873682,  -0.731100440025;
-0.31754270196,   1.1903873682,  -0.731100678444;
-0.731100618839,  1.1903873682,  -0.317542672157;
-0.731100797653,  1.1903873682,   0.317542552948;
-0.317542850971,  1.1903873682,   0.731100559235;
 0.317542493343,  1.1903873682,   0.731100916862;
 0.292440116405,  1.60394537449,  0.127017214894;
 0.292440235615,  1.60394537449, -0.127016976476;
 0.127017065883,  1.60394537449, -0.292440116405;
-0.127017140388,  1.60394537449, -0.29244017601;
-0.292440295219,  1.60394537449, -0.127017021179;
-0.292440354824,  1.60394537449,  0.127017065883;
-0.127017199993,  1.60394537449,  0.292440295219;
 0.127016931772,  1.60394537449,  0.292440444231;
-8.81092319105e-08,  2.29320859909,  9.22362204392e-08;
 0.724502801895, 2.61362838745,  1.02226448059;
 1.02226436138,  2.61362838745,  0.7245028615;
 1.02634608746,  2.19995331764,  1.26572012901;
 1.26571989059,  2.19995331764,  1.02634632587;
-1.02226448059,  2.61362838745, -0.724502623081;
-0.7245028615,   2.61362838745, -1.02226424217;
-1.2657200098,   2.19995331764, -1.02634608746;
-1.02634620667,  2.19995331764, -1.26571989059;
-0.724502980709, 2.61362838745,  1.0222645998;
-1.0222645998,   2.61362838745,  0.724502682686;
-1.02634632587,  2.19995331764,  1.2657200098;
-1.26572036743,  2.19995331764,  1.02634596825;
 0.228630021214, 2.60698795319,  1.238083601;
-0.228631407022, 2.60698795319,  1.23808336258;
 0.183798059821, 2.19354772568,  1.61754381657;
-0.183799505234, 2.19354772568,  1.61754345894;
 1.23808336258,  2.60698795319, -0.228630229831;
 1.23808312416,  2.60698795319,  0.228631347418;
 1.61754357815,  2.19354772568, -0.183798193932;
 1.61754333973,  2.19354772568,  0.183799415827;
-1.238083601,    2.60698795319,  0.228630498052;
-1.23808336258,  2.60698795319, -0.228630870581;
-1.61754357815,  2.19354772568,  0.183798491955;
-1.61754357815,  2.19354772568, -0.183798998594;
-0.228630587459, 2.60698795319, -1.23808336258;
 0.228631019592, 2.60698795319, -1.23808312416;
-0.183798566461, 2.19354772568, -1.61754357815;
 0.183799117804, 2.19354772568, -1.61754333973;
 0.724502801895, 2.61362838745, -1.02226424217;
 1.02226436138,  2.61362838745, -0.724502623081;
 1.02634620667,  2.19995331764, -1.26571977139;
 1.26572012901,  2.19995331764, -1.02634596825;
  |]
  and facesC = [|
    Quad(0, 1, 2, 3);
    Quad(0, 4, 5, 1);
    Quad(1, 5, 6, 2);
    Quad(2, 6, 7, 3);
    Quad(4, 0, 3, 7);
    Quad(7, 6, 9, 8);
    Quad(6, 5, 10, 9);
    Quad(4, 7, 8, 11);
    Quad(5, 4, 11, 10);
    Quad(10, 11, 12, 13);
    Quad(11, 8, 15, 12);
    Quad(9, 10, 13, 14);
    Quad(8, 9, 14, 15);
    Quad(13, 12, 17, 16);
    Quad(12, 15, 18, 19);
    Quad(15, 14, 20, 21);
    Quad(14, 13, 23, 22);
    Tri(13, 16, 23);
    Tri(12, 19, 17);
    Tri(15, 21, 18);
    Tri(14, 22, 20);
    Quad(16, 17, 25, 24);
    Quad(19, 18, 27, 26);
    Quad(21, 20, 29, 28);
    Quad(22, 23, 31, 30);
    Quad(23, 16, 24, 31);
    Quad(17, 19, 26, 25);
    Quad(18, 21, 28, 27);
    Quad(20, 22, 30, 29);
    Tri(32, 33, 40);
    Tri(34, 35, 40);
    Tri(36, 37, 40);
    Tri(38, 39, 40);
    Tri(39, 32, 40);
    Tri(33, 34, 40);
    Tri(35, 36, 40);
    Tri(37, 38, 40);
    Quad(32, 39, 41, 42);
    Quad(39, 31, 43, 41);
    Quad(24, 32, 42, 44);
    Quad(31, 24, 44, 43);
    Quad(43, 44, 42, 41);
    Quad(36, 35, 46, 45);
    Quad(28, 36, 45, 47);
    Quad(35, 27, 48, 46);
    Quad(27, 28, 47, 48);
    Quad(48, 47, 45, 46);
    Quad(38, 37, 50, 49);
    Quad(30, 38, 49, 51);
    Quad(37, 29, 52, 50);
    Quad(29, 30, 51, 52);
    Quad(52, 51, 49, 50);
    Quad(38, 30, 56, 54);
    Quad(39, 38, 54, 53);
    Quad(31, 39, 53, 55);
    Quad(30, 31, 55, 56);
    Quad(56, 55, 53, 54);
    Quad(32, 24, 60, 58);
    Quad(33, 32, 58, 57);
    Quad(25, 33, 57, 59);
    Quad(24, 25, 59, 60);
    Quad(60, 59, 57, 58);
    Quad(36, 28, 64, 62);
    Quad(37, 36, 62, 61);
    Quad(29, 37, 61, 63);
    Quad(28, 29, 63, 64);
    Quad(64, 63, 61, 62);
    Quad(34, 26, 68, 66);
    Quad(35, 34, 66, 65);
    Quad(27, 35, 65, 67);
    Quad(26, 27, 67, 68);
    Quad(68, 67, 65, 66);
    Quad(34, 33, 70, 69);
    Quad(26, 34, 69, 71);
    Quad(33, 25, 72, 70);
    Quad(25, 26, 71, 72);
    Quad(72, 71, 69, 70);
  |] in
  let res = catmull_clark_iter 2 (pointsC, facesC) in
  (res)
;;

let geom = ref(Random.int 4)
let mk_geom p =
  match !geom with
  | 0 -> print_endline "1"; mk_geom1 p
  | 1 -> print_endline "2"; mk_geom2 p
  | 2 -> print_endline "3"; mk_geom3 p
  | 3 -> print_endline "4"; mk_geom4 p
  | _ -> assert false
;;

let new_geom () =
  let new_val = ref(Random.int 4) in
  while !new_val = !geom do
    new_val := Random.int 4
  done;
  geom := !new_val;
;;

type background_gradient =
  { mutable top: float * float * float;
    mutable bot: float * float * float; }

let bg_color =
  { top = (0.8, 0.0, 0.2);
    bot = (1.0, 0.0, 0.5); }

let item_color = ref (0.0, 0.7, 0.9)

let new_colors () =
  let r = (Random.float 0.8) +. 0.2
  and g = (Random.float 0.8) +. 0.2
  and b = (Random.float 0.8) +. 0.2 in
  bg_color.bot <- (r,g,b);
  bg_color.top <- (r *. 0.7, g *. 0.7, b *. 0.7);
  let f v = min 1.0 ((1.0 -. v) *. 1.2) in
  item_color := (f r, f g, f b);
;;


let init_func ~num_screens:(n:int) ~screen:(s:int) ~width:(w:int) ~height:(h:int) =
  (* init opengl *)
  glClearColor 1.0 0.0 0.5 0.0;

  glEnable GL_DEPTH_TEST;
  glPolygonMode GL_FRONT_AND_BACK GL_LINE;

  (* init the saver *)
  Random.self_init();
  new_colors ();
  new_geom ();
;;


let draw (points, faces) =
  let put_point i = glVertex3v points.(i) in
  Array.iter (function
  | Tri (a,b,c) ->
      glBegin GL_TRIANGLES;
        put_point a;
        put_point b;
        put_point c;
      glEnd();
  | Quad (a,b,c,d) ->
      glBegin GL_QUADS;
        put_point a;
        put_point b;
        put_point c;
        put_point d;
      glEnd();
  ) faces


let draw_func ~screen:(s:int) =
  glClear [GL_COLOR_BUFFER_BIT; GL_DEPTH_BUFFER_BIT];
  if true then
  begin
    glMatrixMode GL_PROJECTION;
    let mat = glGetMatrixFlat Get.GL_PROJECTION_MATRIX in
     glLoadIdentity();
     glOrtho (-1.5) (1.5) (-1.0) (1.0) (-1.0) (1.0);
    glMatrixMode GL_MODELVIEW;

   glPushMatrix();
    glLoadIdentity();
    glDisable GL_DEPTH_TEST;
    (* draw background gradient *)
    glBegin GL_POLYGON;
    glColor3v bg_color.top;  glVertex2 ( 1.5) ( 1.0);
                             glVertex2 (-1.5) ( 1.0);
    glColor3v bg_color.bot;  glVertex2 (-1.5) (-1.0);
                             glVertex2 ( 1.5) (-1.0);
    glEnd();
    glEnable GL_DEPTH_TEST;
   glPopMatrix();

    glMatrixMode GL_PROJECTION;
     glLoadMatrixFlat mat;
    glMatrixMode GL_MODELVIEW;
  end;

  glPushMatrix();
  (**) glLoadIdentity();
  glTranslate (0.0) (0.0) (-8.0);

  let now = Unix.gettimeofday() in

  let ax = (cos (now /. 6.0)) *. 20.0 +. 8.0 in
  glRotate ax 1.0 0.0 0.0;

  let ay = mod_float (now *. 30.) 360. in
  glRotate ay 0.0 1.0 0.0;

  let mul = abs_float(cos (now /. 5.0)) *. 1.4 +. 0.8 in
  let draw_item() = draw (mk_geom mul) in

  glLineWidth 8.0;
  glDisable GL_DEPTH_TEST;
  glColor3v (0.0, 0.0, 0.0);
  glPolygonMode GL_FRONT_AND_BACK GL_LINE;
  draw_item();
  glEnable GL_DEPTH_TEST;
  glLineWidth 1.0;

  glColor3v (1.0, 1.0, 1.0);
  glPolygonMode GL_FRONT_AND_BACK GL_LINE;
  glPolygonOffset (-. !polyfactor) (-. !polyunits);
  glEnable GL_POLYGON_OFFSET_LINE;
  draw_item();
  glDisable GL_POLYGON_OFFSET_LINE;

  glColor3v !item_color;
  glPolygonMode GL_FRONT_AND_BACK GL_FILL;
  draw_item();

  glPopMatrix();
;;


let event_func ~screen:(s:int) ~key:(c:char) =
  if c = '\027' then (false) else
  begin
    (match c with
    | 'f' -> polyfactor := !polyfactor -. 0.1;  Printf.printf "polyfactor: %f\n%!" !polyfactor
    | 'F' -> polyfactor := !polyfactor +. 0.1;  Printf.printf "polyfactor: %f\n%!" !polyfactor
    | 'u' -> polyunits := !polyunits -. 1.0;  Printf.printf "polyunits: %f\n%!" !polyunits
    | 'U' -> polyunits := !polyunits +. 1.0;  Printf.printf "polyunits: %f\n%!" !polyunits
    | 'c' -> new_colors ()
    | 'g' -> new_geom ()
    | ' ' ->
        new_colors ();
        new_geom ()

    | _ -> Printf.printf " '%c'\n%!" c);
    (true)
  end
;;


let reshape_func ~screen:(s:int) ~width:(w:int) ~height:(h:int) =
  let h = if h = 0 then 1 else h in
  glViewport 0 0 w h;
  glMatrixMode GL_PROJECTION;
  glLoadIdentity();
  gluPerspective 45.0 (float w /. float h) 4.0 12.0;
  glMatrixMode GL_MODELVIEW;
;;


let () =
  Callback.register "init_callback" init_func;
  Callback.register "draw_callback" draw_func;
  Callback.register "event_callback" event_func;
  Callback.register "reshape_callback" reshape_func;
;;

