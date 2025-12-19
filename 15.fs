// 41.4.1 - list_filter с использованием List.foldBack
let list_filter f xs = 
    List.foldBack (fun x acc -> if f x then x::acc else acc) xs []

// 41.4.2 - sum с использованием List.fold
let sum (p, xs) = 
    List.fold (fun acc x -> if p x then acc + x else acc) 0 xs

// 41.4.3 - revrev с использованием List.fold
let revrev = 
    let reverse xs = List.fold (fun acc x -> x::acc) [] xs
    List.fold (fun acc xs -> (reverse xs)::acc) []