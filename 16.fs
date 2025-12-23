// 42.3
let rec allSubsets n k =
    if k = 0 then
        set [Set.empty]
    elif n = 0 || k > n then
        Set.empty
    else
        let subsetsWithoutN = allSubsets (n-1) k
        let subsetsWithN = allSubsets (n-1) (k-1) |> Set.map (fun subset -> Set.add n subset)
        Set.union subsetsWithoutN subsetsWithN