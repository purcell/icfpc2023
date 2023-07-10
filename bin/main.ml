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


type solution = { placements: placement list;
                  volumes: int list;
                } [@@deriving yojson_of]


type gridconfig = { xincr: int; yincr: int; offset: int };;

(* Make a set of potential positions by using potentially staggered
   rows of points within the stage space inset from the edge *)
let grid_positions p {xincr; yincr; offset} =
  let (x_bl, y_bl) = p.stage_bottom_left in
  let inner_width = p.stage_width - 20 in
  let inner_height = p.stage_height - 20 in
  let spots = ref [] in
  for row = 0 to inner_height / yincr - 1 do
    for col = 0 to inner_width / xincr - 1 do
      let y = yincr * row in
      let x = xincr * col + if row mod 2 = 1 then offset else 0 in
      if x <= inner_width then
        let grid_coord = (row, col) in
        let placement = { x = x + x_bl + 10; y = y + y_bl + 10; } in
        spots := (grid_coord, placement) :: !spots
    done
  done;
  if List.length !spots < Array.length p.musicians then
    failwith "grid too sparse";
  List.map snd !spots;;

let distancesq (ax, ay) (bx, by) =
  let dx = ax - bx in
  let dy = ay - by in
  dx * dx + dy * dy;;

let angle_to (ax, ay) (bx, by) =
  let dx = bx - ax in
  let dy = by - ay in
  atan (float_of_int dx /. float_of_int dy);;

let blocked_angles_from p1 p2 radius =
  let dist = sqrt (float_of_int (distancesq p1 p2)) in
  let radius_subtend = asin (dist /. float_of_int radius) in
  let to_center = angle_to p1 p2 in
  (to_center -. radius_subtend, to_center +. radius_subtend);;

let raw_score_at pillars instrument attendees {x; y} =
  let blocked_by (attendee : attendee) { radius; center } =
    let pos = (x, y) in
    let attendee_pos = (attendee.x, attendee.y) in
    let pillar_dist_sq = distancesq pos center in
    let attendee_dist_sq = distancesq pos attendee_pos in
    pillar_dist_sq < attendee_dist_sq &&
    let (angle1, angle2) = blocked_angles_from pos center radius in
    let angle_to_attendee = angle_to pos attendee_pos in
    angle_to_attendee >= angle1 && angle_to_attendee <= angle2
  in
  Array.fold_left
    (fun total (attendee : attendee) ->
       total +.
       match List.find_opt (fun p -> blocked_by attendee p) pillars with
       | Some pillar -> 0.0;
       | None ->
         let affinity = Array.get attendee.tastes instrument in
         let d2 = distancesq (x, y) (attendee.x, attendee.y) in
         1.0e6 *. (float_of_int affinity) /. (float_of_int d2)
    ) 0.0 attendees;;

let num_instrs p = 1 + Array.fold_left max 0 p.musicians;;

let solve p =
  let grid_config = { xincr = 10; yincr = 9; offset = 5; } in
  let positions = Array.of_list (grid_positions p grid_config) in
  let instrument_counts = Array.make (num_instrs p) 0 in
  Array.iter (fun i -> Array.set instrument_counts i (1 + Array.get instrument_counts i)) p.musicians;

  let scored_positions =
    positions |> Array.mapi
      (fun position_idx position ->
         Array.mapi
           (fun instrument _ ->
              (instrument, position_idx, (raw_score_at p.pillars instrument p.attendees position)))
           instrument_counts)
    |> Array.to_list |> Array.concat |> Array.to_list in

  (* Mutable state while assigning places *)
  let best_positions = ref (List.sort (fun (_, _, s1) (_, _, s2) -> Float.compare s2 s1) scored_positions) in
  let placements_by_position_idx = Array.map (fun _ -> None) positions in
  let placements_by_instrument = Array.init (num_instrs p) (fun _ -> Queue.create ()) in
  let volumes_by_position_idx = Array.map (fun _ -> 10) positions in

  let placed_count = ref 0 in
  while !placed_count < Array.length p.musicians do
    match !best_positions with
    | [] -> failwith "ran out of positions!";
    | ((instrument, position_idx, score) :: rest) ->
      best_positions := rest;
      match Array.get placements_by_position_idx position_idx with
      | None -> begin
          match Array.get instrument_counts instrument with
          | 0 -> ();  (* No more of these instruments *)
          | remaining_players ->
            Array.set placements_by_position_idx position_idx (Some instrument);
            Array.set instrument_counts instrument (remaining_players - 1);
            Queue.push position_idx (Array.get placements_by_instrument instrument);
            Array.set volumes_by_position_idx position_idx (if score > 0.0 then 10 else 0);
            placed_count := !placed_count + 1
        end
      | _ -> (); (* Spot already filled *)
  done;

  (* list of placement, volume *)
  Array.map
    (fun i ->
       let position_idx = Array.get placements_by_instrument i |> Queue.pop in
       let volume = Array.get volumes_by_position_idx position_idx in
       let position = Array.get positions position_idx in
       (position, volume))
    p.musicians |> Array.to_list;;

let spread_solver p =
  let placements_and_volumes = solve p in
  { placements = List.map fst placements_and_volumes;
    volumes = List.map snd placements_and_volumes;
  };;

let () =
  Random.init 42;
  let prob = problem_of_yojson (Yojson.Safe.from_channel Stdlib.stdin) in
  let soln = spread_solver prob in
  Yojson.Safe.to_channel Stdlib.stdout (yojson_of_solution soln);;
