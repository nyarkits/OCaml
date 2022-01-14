(* lucas_number/main.ml *)
(* 
  ~~ Dynamic Programming ~~
  Bottom-up method: Recurrence formula loop  
*)

let () =
  let main () =
    let n = read_int () in
    let dp = Array.init (n+2) (fun i -> 0) in
    dp.(0) <- 2;
    dp.(1) <- 1;
    let result =
      let rec loop j () dp =
        if j = (n+1) then dp.(n)
        else 
          loop (j+1) (dp.(j) <- (dp.(j-1) + dp.(j-2))) dp
    in loop 2 (dp.(2) <- 3) dp
      in Printf.printf "%d\n" result
    in main ();;
    
