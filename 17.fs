// 43.3
let try_find key m = 
    Map.fold (fun acc k v -> 
        match acc with
        | Some _ -> acc  // уже нашли, возвращаем как есть
        | None -> if k = key then Some v else None) None m