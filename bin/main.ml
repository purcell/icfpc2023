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

let choose_best p positions =
  let positions = Array.of_list positions in
  let num_instrs = 1 + Array.fold_left max 0 p.musicians in
  let instrument_counts = Array.make num_instrs 0 in
  Array.iter (fun i -> Array.set instrument_counts i (1 + Array.get instrument_counts i)) p.musicians;

  let scored_positions =
    positions |> Array.mapi
      (fun position_idx position ->
         Array.mapi
           (fun instrument _ ->
              (instrument, position_idx, (raw_score_at instrument p.attendees position)))
           instrument_counts)
    |> Array.to_list |> Array.concat |> Array.to_list in

  (* Mutable state while assigning places *)
  let best_positions = ref (List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) scored_positions) in
  let placements_by_position_idx = Array.map (fun _ -> None) positions in
  let placements_by_instrument = Array.init num_instrs (fun _ -> Queue.create ()) in

  let placed_count = ref 0 in
  while !placed_count < Array.length p.musicians do
    match !best_positions with
    | [] -> failwith "ran out of positions!";
    | ((instrument, position_idx, _score) :: rest) ->
      best_positions := rest;
      match Array.get placements_by_position_idx position_idx with
      | None -> begin
          match Array.get instrument_counts instrument with
          | 0 -> ();  (* No more of these instruments *)
          | remaining_players ->
            Array.set placements_by_position_idx position_idx (Some instrument);
            Array.set instrument_counts instrument (remaining_players - 1);
            Queue.push (Array.get positions position_idx) (Array.get placements_by_instrument instrument);
            placed_count := !placed_count + 1
        end
      | _ -> (); (* Spot already filled *)
  done;

  Array.map (fun i -> Array.get placements_by_instrument i |> Queue.pop) p.musicians |> Array.to_list;;

let spread_solver p =
  { placements = grid_positions p |> choose_best p };;

let () =
  Random.init 42;
  let prob = problem_of_yojson (Yojson.Safe.from_channel Stdlib.stdin) in
  let soln = spread_solver prob in
  Yojson.Safe.to_channel Stdlib.stdout (yojson_of_solution soln);;
