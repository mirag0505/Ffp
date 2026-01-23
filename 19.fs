// 48.4.1
let rec fibo1 n n1 n2 =
    match n with
    | 0 -> n2
    | 1 -> n1
    | _ -> fibo1 (n-1) (n1 + n2) n1

let rec fibo2 n c =
    match n with
    | 0 | 1 -> c n
    | _ -> fibo2 (n-1) (fun x -> fibo2 (n-2) (fun y -> c(x + y)))

// 48.4.2
let rec bigList n k =
    let rec loop m acc =
        if m = 0 then k acc
        else loop (m-1) (1::acc)
    loop n []