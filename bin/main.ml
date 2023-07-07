open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type taste = int [@@deriving yojson];;

type coord = (int * int) [@@deriving yojson];;

type instrument = int [@@deriving yojson];;

type attendee = { x: int;
                  y: int;
                  tastes: taste list;
                } [@@deriving yojson];;

type problem = { room_width : int;
                 room_height : int;
                 stage_width: int;
                 stage_height: int;
                 stage_bottom_left: coord;
                 musicians: instrument list;
                 attendees: attendee list;
                 pillars: bool list;
               } [@@deriving yojson];;


type placement = { x: int; y: int } [@@deriving yojson];;
type solution = { placements: placement list } [@@deriving yojson]

(* type solver = problem -> solution;; *)


(* Randomly place the musicians on the stage *)
let spread_solver p =
  Random.init 42;
  let (x_bl, y_bl) = p.stage_bottom_left in
  let placements = List.mapi (fun _n _player ->
      ({ x = x_bl + 1 + (Random.int (p.stage_width - 2 ));
         y = y_bl + 1 + (Random.int (p.stage_height - 2));
       } : placement) ) p.musicians in
  { placements = placements };;


let () =
  let _ = taste_of_yojson (Yojson.Safe.from_string "5") in
  let _ = coord_of_yojson (Yojson.Safe.from_string "[0, 0]") in
  let _ = attendee_of_yojson (Yojson.Safe.from_string "{\"x\": 0, \"y\": 0, \"tastes\": [-5, 6, 3]}")
  in ();
;;


let () =
  let prob = problem_of_yojson (Yojson.Safe.from_channel Stdlib.stdin) in
  let soln = spread_solver prob in
  Yojson.Safe.to_channel Stdlib.stdout (yojson_of_solution soln);;




(*     List.iter print_endline ["Hello, World!"; "So long"; "whatevs"; "yo yo yo"]; *)
