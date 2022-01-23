(* ex23_mode/main.ml *)
(* AtCoder for beginners *)
(* Author: nyarkits *)
(* associate arrays *)


module AssocMap = Map.Make(Int);;

let () =
  let n = Scanf.scanf "%d" @@ fun x -> x in
  let a = Array.init n @@ fun _ -> Scanf.scanf " %d" @@ fun x -> x in
  let assoc = AssocMap.empty in
  let assoc = Array.fold_left (fun m k ->
                  match AssocMap.mem k m with
                    true -> AssocMap.update k (Option.map (fun v -> v + 1)) m
                  | false -> AssocMap.add k 1 m) assoc a in
  let assoc_2 = AssocMap.bindings assoc in
  let ans = List.sort (fun (k1, v1) (k1, v2) -> ~- (compare v1 v2)) assoc_2 in
  let print_pair (x, y) = Printf.printf "%d %d\n" x y in
  print_pair (List.nth ans 0)
;;
