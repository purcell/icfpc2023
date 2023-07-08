open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type taste = int [@@deriving yojson];;

type coord = (int * int) [@@deriving yojson];;

type instrument = int [@@deriving yojson];;

type attendee = { x: int;
                  y: int;
                  tastes: taste array;
                } [@@deriving yojson];;

type pillar = { center: coord;
                radius: int
              } [@@deriving yojson];;

type problem = { room_width : int;
                 room_height : int;
                 stage_width: int;
                 stage_height: int;
                 stage_bottom_left: coord;
                 musicians: instrument array;
                 attendees: attendee array;
                 pillars: pillar list;
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

module IntMap = Map.Make(Int);;
module IntSet = Set.Make(Int);;

let choose_best p positions =
  let positions = Array.of_list positions in
  let scored_positions = ref [] in
  Seq.iter
    (fun instrument ->
       Array.iteri
         (fun placement_num placement ->
            let score = raw_score_at instrument p.attendees placement in
            scored_positions := (instrument, placement_num, score) :: !scored_positions
         )
         positions)
    (p.musicians |> Array.to_seq |> IntSet.of_seq |> IntSet.to_seq);
  (* Sort positions to get highest-scoring first *)
  let best_positions = List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) !scored_positions in
  let choose_place already_placed this_instrument =
    List.to_seq best_positions
    |> Seq.filter (fun (instrument, placement_num, _) ->
        this_instrument = instrument && not (IntMap.mem placement_num already_placed))
    |> Seq.take 1
    |> List.of_seq
    |> function | [(_, best_placement_num, _)] -> best_placement_num;
                | _ -> failwith "No valid placement found"
  in
  let placed = Array.fold_left
      (fun placed (this_player, this_instrument) ->
         IntMap.add (choose_place placed this_instrument) this_player placed)
      IntMap.empty
      (Array.mapi (fun i instr -> (i, instr)) p.musicians) in
  placed
  |> IntMap.to_list
  |> List.sort (fun a b -> Int.compare (snd a) (snd b))
  |> List.map (fun (placement_id, _) -> Array.get positions placement_id)
;;

let spread_solver p =
  { placements = grid_positions p |> choose_best p };;


let () =
  Random.init 42;
  let prob = problem_of_yojson (Yojson.Safe.from_channel Stdlib.stdin) in
  let soln = spread_solver prob in
  Yojson.Safe.to_channel Stdlib.stdout (yojson_of_solution soln);;
