(* lucas_number/main.ml *)
(* Author: RockwallNest *)
(* Creation Time: Sunday, /14/01/2021 JST *)
(* 
  ~~ Dynamic Programming: dp ~~
  Bottom-up method: Recurrence formula loop  
*)

let () =
  let main () =
    let n = read_int () in
    (* Make n pieces of dp-Array with initial value 0 *)
    let dp = Array.init (n+2) (fun i -> 0) in
    (* Substitute two initial integer numbers for {each value of dp-Array: 0}. *)
    dp.(0) <- 2;
    dp.(1) <- 1;
    let result =
    (* Create Recursive function: `loop j () dp` *)
      let rec loop j () dp =
      (* 
         Becase of `j` is up to `n` in Recursive function `loop` , if-clause is set in `j = (n+1)`
         ~~~ Basecase Step ~~~
       *)
        if j = (n+1) then dp.(n)
        else 
        (* 
          It is necessary for the Recursive function `loop` to assign its argument to {the `Unit-type` value: `()`}
          if you substitute new value for dp-Array's value recursively. 
          ~~~ Rcurrence case Step ~~~~
        *) 
          loop (j+1) (dp.(j) <- (dp.(j-1) + dp.(j-2))) dp
          (* Calculate {third dp-Array's value: dp.(2) = 3} wtih {initial dp-Array's value: 2 and 1} *)
    in loop 2 (dp.(2) <- 3) dp
      in Printf.printf "%d\n" result
    in main ();;
    
