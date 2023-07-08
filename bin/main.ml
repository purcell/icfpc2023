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

module IntPair =
struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match Int.compare x0 x1 with
      0 -> Int.compare y0 y1
    | c -> c
end;;

module IntPairMap = Map.Make(IntPair);;
module IntPairSet = Set.Make(IntPair);;

module IntMap = Map.Make(Int);;
module IntSet = Set.Make(Int);;

let players_by_instrument p =
  Array.to_seq p.musicians
  |> Seq.fold_lefti
    (fun by_instrument n i ->
       IntMap.add_to_list i n by_instrument
    ) IntMap.empty;;


let choose_best p positions =
  let positions = Array.of_list positions in
  let known_instruments = (p.musicians |> Array.to_seq |> IntSet.of_seq |> IntSet.to_seq) in
  let scored_positions = ref [] in
  Seq.iter
    (fun instrument ->
       Array.iteri
         (fun placement_num placement ->
            let score = raw_score_at instrument p.attendees placement in
            scored_positions := (instrument, placement_num, score) :: !scored_positions
         )
         positions)
    known_instruments;

  (* Sort positions to get highest-scoring first *)
  let best_positions = List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) !scored_positions in

  let rec go placed placed_count best_positions players_by_instrument =
    if placed_count = Array.length p.musicians then
      IntMap.to_seq placed
      |> Seq.map (fun (placement_num, player_num) -> (player_num, placement_num))
      |> IntPairSet.of_seq
      |> IntPairSet.to_seq
      |> Seq.map (fun (_player_num, placement_num) -> Array.get positions placement_num)
      |> List.of_seq
    else
      match best_positions with
      | [] -> failwith "ran out of positions!";
      | ((instrument, placement_num, _score) :: other_pos) ->
        if IntMap.mem placement_num placed then
          go placed placed_count other_pos players_by_instrument
        else
          match IntMap.find instrument players_by_instrument with
          | [] -> go placed placed_count other_pos players_by_instrument;
          | player_num :: other_players ->
            let new_placed = IntMap.add placement_num player_num placed in
            let new_players_by_instrument = IntMap.add instrument other_players players_by_instrument in
            go new_placed (placed_count + 1) other_pos new_players_by_instrument in

  go IntMap.empty 0 best_positions (players_by_instrument p);;

let spread_solver p =
  { placements = grid_positions p |> choose_best p };;

let () =
  Random.init 42;
  let prob = problem_of_yojson (Yojson.Safe.from_channel Stdlib.stdin) in
  let soln = spread_solver prob in
  Yojson.Safe.to_channel Stdlib.stdout (yojson_of_solution soln);;
