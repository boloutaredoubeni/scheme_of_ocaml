open Core.Std

module List = struct

  let traverse l ~f = 
    let open Result.Let_syntax in
    let folder head tail = 
      let%bind h = f head in
      let%bind t = tail in
      Result.return @@ h :: t
    in
    List.fold_right l
      ~f:folder
      ~init:(Result.return [])

  let fold_1 ~f = function
    | (x::xs) -> List.fold ~f:f ~init:x
    | err -> failwith "Failed to fold func"

  include List
end
