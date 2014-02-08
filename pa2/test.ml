(* CSE 130 PA 2. Autotester *)

#use "misc.ml"
#use "expr.ml" 
#use "art.ml"
#use "tester.ml" 

let sampleTests =
  [
  (fun () -> mkTest
     assoc
     (-1, "william", [("ranjit",85);("william",23);("moose",44)])
     23
     "sample: assoc 1"
  );
  (fun () -> mkTest 
    assoc
    (-1, "bob", [("ranjit",85);("william",23);("moose",44)])
    (-1)
    "sample: assoc 2"
  ); 
  (fun () -> mkTest 
    removeDuplicates
    [1;6;2;4;12;2;13;6;9]
    [1;6;2;4;12;13;9]
    "sample: removeDuplicates 2"
  );
  (fun () -> mkTest 
    removeDuplicates
    [1;1;1]
    [1]
    "sample: removeDuplicates 2"
  );

  (fun () -> mkTest 
    wwhile 
    ((fun x -> let xx = x*x*x in (xx, xx < 100)), 2) 
    512 
    "sample: wwhile 1"
  ); 
  (fun () -> mkTest 
	fixpoint
    ((fun x -> truncate (1e6 *. cos (1e-6 *. float x))), 0)
    739085
    "sample: fixpoint 1"
  ); 
 
 (fun () -> mkTest 
   emitGrayscale
   (eval_fn sampleExpr, 150,"sample")
   ()
   "sample: eval_fn 1: manual"
 ); 
 (fun () -> mkTest 
   emitGrayscale
   (eval_fn sampleExpr2, 150,"sample2")
   ()
   "sample: eval_fn 2: manual"
 );
 
 (fun () -> mkTest 
   (fun () -> doRandomGray (g1 ()))
   ()
   ()
   "sample: gray 1 : manual"
 );
 (fun () -> mkTest 
   (fun () -> doRandomGray (g2 ()))
   ()
   ()
   "sample: gray 2 : manual"
 );
 (fun () -> mkTest 
   (fun () -> doRandomGray (g3 ()))
   ()
   ()
   "sample: gray 3 : manual"
 );

 (fun () -> mkTest 
   (fun () -> doRandomColor (c1 ()))
   ()
   ()
   "sample: color 1 : manual"
 );
 (fun () -> mkTest 
   (fun () -> doRandomColor (c2 ()))
   ()
   ()
   "sample: color 2 : manual"
 );
 (fun () -> mkTest 
   (fun () -> doRandomColor (c3 ()))
   ()
   ()
   "sample: color 3 : manual"
 )] 

(*130*************************************************************)
(*130**************** BEGIN MODIFY *******************************)
(*130*************************************************************)

let yourTests = 
  [ 
  (fun () -> mkTest
     assoc
     (-1, "", [("hello", 10);("louis", 20)])
     (-1)
     "assoc 1"
  );
  (fun () -> mkTest
     assoc
     (-1, "", [("hello", 10);("louis", 20);("", 50)])
     50
     "assoc 2"
  );
  (fun () -> mkTest
     assoc
     (-1, "louis", [("hello", 10);("louis", 20);("", 50)]) 
     20 
     "assoc 3"
  );

  (fun () -> mkTest 
    removeDuplicates
    []
    []
     "removeDuplicates 1"
  );
  (fun () -> mkTest 
    removeDuplicates
    [1;1]
    [1] 
     "removeDuplicates 2"
  );
  (fun () -> mkTest 
    removeDuplicates
    [1;2;3;4;5;6;7;6;5;4;3;2;1] 
    [1;2;3;4;5;6;7]
     "removeDuplicates 3"
  );

  (fun () -> mkTest 
    wwhile 
    (let f x = (x+1, x < 10) in f, 11) 
    11 
    "wwhile 1"
  );
  (fun () -> mkTest 
    wwhile 
    (let f x = (x+1, x < 10) in f, 10) 
    (failwith "TBD:output") 
    "wwhile 2"
  );
  (fun () -> mkTest 
    wwhile 
    (failwith "TBD:input") 
    (failwith "TBD:output") 
    "wwhile 3"
  );
  
  (fun () -> mkTest 
    fixpoint 
    (failwith "TBD:input") 
    (failwith "TBD:output") 
    "fixpoint 1"
  );
  (fun () -> mkTest 
    fixpoint 
    (failwith "TBD:input") 
    (failwith "TBD:output") 
    "fixpoint 2"
  );
  (fun () -> mkTest 
    fixpoint 
    (failwith "TBD:input") 
    (failwith "TBD:output") 
    "fixpoint 3"
  );
  ]

(*130*************************************************************)
(*130**************** END MODIFY *********************************)
(*130*************************************************************)

let doTest f = 
  try f () with ex -> 
    Printf.sprintf "WARNING: INVALID TEST THROWS EXCEPTION!!: %s \n\n"
    (Printexc.to_string ex)

let _ =
  let report = List.map doTest (sampleTests @ yourTests) in
  let _ = List.iter print130 (report@([scoreMsg()])) in
  let _ = print130 ("Compiled\n") in
  (!score, !max)

