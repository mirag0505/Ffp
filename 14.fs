// 40.1
let rec sum (p, xs) =
    match xs with
    | [] -> 0
    | head::tail when p head -> head + sum (p, tail)
    | _::tail -> sum (p, tail)

// 40.2.1
let rec count (xs, n) =
    match xs with
    | [] -> 0
    | h::t when h > n -> 0
    | h::t when h = n -> 1 + count (t, n)
    | _::t -> count (t, n)

// 40.2.2
let rec insert (xs, n) =
    match xs with
    | [] -> [n]
    | h::t when n <= h -> n :: xs
    | h::t -> h :: insert (t, n)

// 40.2.3
let rec intersect (xs1, xs2) =
    match xs1, xs2 with
    | [], _ | _, [] -> []
    | h1::t1, h2::t2 ->
        if h1 = h2 then h1 :: intersect (t1, t2)
        elif h1 < h2 then intersect (t1, xs2)
        else intersect (xs1, t2)

// 40.2.4
let rec plus (xs1, xs2) =
    match xs1, xs2 with
    | [], ys -> ys
    | xs, [] -> xs
    | h1::t1, h2::t2 ->
        if h1 <= h2 then h1 :: plus (t1, xs2)
        else h2 :: plus (xs1, t2)

// 40.2.5
let rec minus (xs1, xs2) =
    match xs1, xs2 with
    | [], _ -> []
    | xs, [] -> xs
    | h1::t1, h2::t2 ->
        if h1 = h2 then minus (t1, t2)
        elif h1 < h2 then h1 :: minus (t1, xs2)
        else minus (xs1, t2)

// 40.3.1
let rec smallest = function
    | [] -> None
    | [x] -> Some x
    | x::xs ->
        match smallest xs with
        | Some y when y < x -> Some y
        | _ -> Some x

// 40.3.2
let rec delete (n, xs) =
    match xs with
    | [] -> []
    | h::t when h = n -> t
    | h::t -> h :: delete (n, t)

// 40.3.3
let rec sort xs =
    match smallest xs with
    | None -> []
    | Some m -> m :: sort (delete (m, xs))

// 40.4
let revrev xss =
    let rec rev acc = function
        | [] -> acc
        | x::xs -> rev (x::acc) xs
    let rec maprev acc = function
        | [] -> acc
        | xs::xss -> maprev (rev [] xs :: acc) xss
    rev [] (maprev [] xss)