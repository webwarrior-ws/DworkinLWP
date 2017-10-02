module LibDworkin.SimplexNoise

open PharoCompatibility
open Geometry

// A SimplexNoise provides methods for generating n-D simplex noise.
// The code is based on http://staffwww.itn.liu.se/~stegu/aqsis/aqsis-//newnoise/noise1234.cpp


let private perm = 
    let p =[|
        151; 160; 137; 91; 90; 15; 131; 13; 201; 95; 96; 53; 194; 233; 7; 225; 140; 36; 103; 30; 69; 
        142; 8; 99; 37; 240; 21; 10; 23; 190; 6; 148; 247; 120; 234; 75; 0; 26; 197; 62; 94; 252; 219; 
        203; 117; 35; 11; 32; 57; 177; 33; 88; 237; 149; 56; 87; 174; 20; 125; 136; 171; 168; 68; 175; 
        74; 165; 71; 134; 139; 48; 27; 166; 77; 146; 158; 231; 83; 111; 229; 122; 60; 211; 133; 230; 220; 
        105; 92; 41; 55; 46; 245; 40; 244; 102; 143; 54; 65; 25; 63; 161; 1; 216; 80; 73; 209; 76; 132; 187; 
        208; 89; 18; 169; 200; 196; 135; 130; 116; 188; 159; 86; 164; 100; 109; 198; 173; 186; 3; 64; 52; 
        217; 226; 250; 124; 123; 5; 202; 38; 147; 118; 126; 255; 82; 85; 212; 207; 206; 59; 227; 47; 16; 
        58; 17; 182; 189; 28; 42; 223; 183; 170; 213; 119; 248; 152; 2; 44; 154; 163; 70; 221; 153; 101; 
        155; 167; 43; 172; 9; 129; 22; 39; 253; 19; 98; 108; 110; 79; 113; 224; 232; 178; 185; 112; 104; 
        218; 246; 97; 228; 251; 34; 242; 193; 238; 210; 144; 12; 191; 179; 162; 241; 81; 51; 145; 235; 249; 
        14; 239; 107; 49; 192; 214; 31; 181; 199; 106; 157; 184; 84; 204; 176; 115; 121; 50; 45; 127; 4; 
        150; 254; 138; 236; 205; 93; 222; 114; 67; 29; 24; 72; 243; 141; 128; 195; 78; 66; 215; 61; 156; 
        180; 151; 160; 137; 91; 90; 15; 131; 13; 201; 95; 96; 53; 194; 233; 7; 225; 140; 36; 103; 30; 69; 
        142; 8; 99; 37; 240; 21; 10; 23; 190; 6; 148; 247; 120; 234; 75; 0; 26; 197; 62; 94; 252; 219; 203; 
        117; 35; 11; 32; 57; 177; 33; 88; 237; 149; 56; 87; 174; 20; 125; 136; 171; 168; 68; 175; 74; 165; 71; 
        134; 139; 48; 27; 166; 77; 146; 158; 231; 83; 111; 229; 122; 60; 211; 133; 230; 220; 105; 92; 41; 55; 
        46; 245; 40; 244; 102; 143; 54; 65; 25; 63; 161; 1; 216; 80; 73; 209; 76; 132; 187; 208; 89; 18; 169; 
        200; 196; 135; 130; 116; 188; 159; 86; 164; 100; 109; 198; 173; 186; 3; 64; 52; 217; 226; 250; 124; 
        123; 5; 202; 38; 147; 118; 126; 255; 82; 85; 212; 207; 206; 59; 227; 47; 16; 58; 17; 182; 189; 28; 
        42; 223; 183; 170; 213; 119; 248; 152; 2; 44; 154; 163; 70; 221; 153; 101; 155; 167; 43; 172; 9; 
        129; 22; 39; 253; 19; 98; 108; 110; 79; 113; 224; 232; 178; 185; 112; 104; 218; 246; 97; 228; 251; 
        34; 242; 193; 238; 210; 144; 12; 191; 179; 162; 241; 81; 51; 145; 235; 249; 14; 239; 107; 49; 192; 
        214; 31; 181; 199; 106; 157; 184; 84; 204; 176; 115; 121; 50; 45; 127; 4; 150; 254; 138; 236; 205; 
        93; 222; 114; 67; 29; 24; 72; 243; 141; 128; 195; 78; 66; 215; 61|]
    Array.append p p

let defaultPersistence = 0.5f


// auxiliary functions
let private grad1 hash x =
    let h = hash &&& 15
    let mutable grad = 1.0f + float32 (h &&& 7)
    if h &&& 8 <> 0 then
        grad <- -grad
    grad * x

let private grad2 hash x y =
    let h = hash &&& 7
    let u, v = if h < 4 then x, y else y, x
    (if h &&& 1 <> 0 then -u else u) + (if h &&& 2 <> 0 then -2.0f * v else 2.0f * v)


// noise functions
let noise1d (x : float32) =
    let i0 = x |> floor |> int
    let i1 = i0 + 1
    let x0 = x - float32 i0
    let x1 = x0 - 1.0f
    let t0 = sqr (1.0f - (sqr x0))
    let n0 = (sqr t0) * (grad1 perm.[(i0 &&& 255) + 1] x0)
    let t1 = sqr (1.0f - (sqr x1))
    let n1 = (sqr t1) * (grad1 perm.[(i1 &&& 255) + 1] x1)
    0.395f * (n0 + n1)

let noise2d (x : float32) (y : float32) =
    let f2 = 0.366025403f
    let g2 = 0.211324865f
    let s = (x + y) * f2
    let mutable i = (x + s) |> floor |> int
    let mutable j = (y + s) |> floor |> int
    let t = float32 (i + j) * g2
    let p0 = Point((x - float32 i) + t, (y - float32 j) + t)
    let i1, j1 = if p0.X > p0.Y then 1, 0 else 0, 1
    let p1 = p0 + Point(g2 - float32 i1, g2 - float32 j1)
    let p2 = p0 + Point((g2 * 2.0f) - 1.0f)
    i <- (i % 256) + 1
    j <- (j % 256) + 1
    let mutable t0 = 0.5f - Point.Dot(p0, p0)
    let n0 = 
        if t0 < 0.0f then
            0.0f
        else
            t0 <- sqr (sqr t0)
            t0 * (grad2 perm.[i + perm.[j]] p0.X p0.Y)
    let mutable t1 = 0.5f - Point.Dot(p1, p1)
    let n1 = 
        if t1 < 0.0f then
            0.0f
        else
            t1 <- sqr (sqr t1)
            t1 * (grad2 perm.[(i + i1) + (perm.[j + j1])] p1.X p1.Y)
    let mutable t2 = 0.5f - Point.Dot(p2, p2)
    let n2 = 
        if t2 < 0.0f then
            0.0f
        else
            t2 <- sqr (sqr t2)
            t2 * (grad2 perm.[(i + 1) + (perm.[j + 1])] p2.X p2.Y)
    45.0f * ((n0 + n1) + n2)


// extension methods
type System.Single with
    member inline this.SNoise() : float32 = noise1d this

type OpenTK.Vector2 with
    member inline this.SNoise() : float32 = noise2d this.X this.Y


// generic function to rule them all
let inline sNoise (arg : ^U) =
    (^U : (member SNoise : unit -> float32) arg)
