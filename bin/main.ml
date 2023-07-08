open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type taste = int [@@deriving yojson];;

type coord = (int * int) [@@deriving yojson];;

type instrument = int [@@deriving yojson];;

type attendee = { x: int;
                  y: int;
                  tastes: taste array;
                } [@@deriving yojson];;

type problem = { room_width : int;
                 room_height : int;
                 stage_width: int;
                 stage_height: int;
                 stage_bottom_left: coord;
                 musicians: instrument array;
                 attendees: attendee array;
                 pillars: bool list;
               } [@@deriving yojson];;


type placement = { x: int; y: int } [@@deriving yojson_of];;
type solution = { placements: placement list } [@@deriving yojson_of]

(* let shuffle d = *)
(*   let nd = List.map (fun c -> (Random.bits (), c)) d in *)
(*   let sond = List.sort compare nd in *)
(*   List.map snd sond;; *)

(* Make a set of potential positions by using staggered rows *)
(* of points within the stage space inset from the edge *)
let grid_positions p =
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
  !spots;;

let distancesq (ax, ay) (bx, by) =
  let dx = ax - bx in
  let dy = ay - by in
  dx * dx + dy * dy;;

let raw_score_at instrument attendees placement =
  let total = ref 0.0 in
  Array.iter (fun attendee ->
      let affinity = Array.get attendee.tastes instrument in
      let d2 = distancesq (placement.x, placement.y) (attendee.x, attendee.y) in
      let score = 1.0e6 *. (float_of_int affinity) /. (float_of_int d2) in
      total := !total +. score;
    ) attendees;
  !total;;

(* let choose_random p placements = *)
(*   shuffle placements *)
(*   |> List.to_seq *)
(*   |> Seq.take (Array.length p.musicians) *)
(*   |> List.of_seq;; *)

module IntSet = Set.Make(Int);;

let choose_best p placements =
  let placements = Array.of_list placements in
  let scored_placements = ref [] in
  Array.iteri
    (fun player_num instrument ->
       Array.iteri
         (fun placement_num placement ->
            let score = raw_score_at instrument p.attendees placement in
            scored_placements := (player_num, placement_num, score) :: !scored_placements
         )
         placements)
    p.musicians;
  (* Sort placements to get highest-scoring first *)
  let best_placements = List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) !scored_placements in
  let (_, final_placements) = Array.fold_left_map
      (fun used this_player ->
         let (_, best_placement_num, _) = List.find
             (fun (player_num, placement_num, _) ->
                this_player = player_num && not (IntSet.mem placement_num used))
             best_placements in
         (IntSet.add best_placement_num used, Array.get placements best_placement_num)
      )
      IntSet.empty
      (Array.mapi (fun i _ -> i) p.musicians) in
  final_placements |> Array.to_list
;;

let spread_solver p =
  { placements = grid_positions p |> choose_best p };;


let () =
  Random.init 42;
  let prob = problem_of_yojson (Yojson.Safe.from_channel Stdlib.stdin) in
  let soln = spread_solver prob in
  Yojson.Safe.to_channel Stdlib.stdout (yojson_of_solution soln);;
