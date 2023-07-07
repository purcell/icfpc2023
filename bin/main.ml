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
  let (x_bl, y_bl) = p.stage_bottom_left in
  let inner_width = p.stage_width - 20 in
  let inner_height = p.stage_height - 20 in
  let to_placement p = { x = p.x + x_bl + 10; y = p.y + y_bl + 10; } in
  let spots = ref [] in
  let pos = ref { x = 0; y = 0; } in
  while !pos.x <= inner_width && !pos.y <= inner_height do
    spots := to_placement !pos :: !spots;
    pos := if !pos.x + 10 > inner_width then
        { x = if !pos.x mod 10 = 0 then 5 else 0; y = !pos.y + 10; }
      else
        { x = !pos.x + 10; y = !pos.y; }
  done;
  Random.init 42;
  let placements = List.to_seq !spots |> Seq.take (List.length p.musicians) |> List.of_seq in
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
