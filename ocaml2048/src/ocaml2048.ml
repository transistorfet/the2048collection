

type world = {
  rows: int list list
}

let generate_world () =
  { rows =
    [ [ 0; 0; 2; 0];
      [ 0; 0; 0; 0];
      [ 0; 2; 0; 0];
      [ 0; 0; 8; 0] ] }


let show_matrix rows =
  List.iter begin fun row ->
    List.iter (fun num -> Printf.printf " %d" num) row;
    Printf.printf "\n"
  end rows;
  Printf.printf "\n";
  flush stdout


let to_columns rows =
  List.map begin fun i ->
    List.map (fun row -> List.nth row (i - 1)) rows
  end (List.init 4 succ)


let rec reverse row result =
  match row with
  | [] -> result
  | x :: remain -> reverse remain (x :: result)


let rec collapse_row row =
  match row with
  | [] -> []
  | 0 :: remain -> collapse_row remain 
  | x :: 0 :: remain -> collapse_row (x :: remain)
  | x :: y :: remain when x == y -> (x + y) :: collapse_row remain
  | x :: remain -> x :: collapse_row remain


let rec fill_zeros num =
  if num <= 0 then
    []
  else if num == 1 then
    if Random.float 1.0 < 0.2 then
      [2]
    else
      [0]
  else
    0 :: (fill_zeros (num - 1))


let collapse_and_fill_rows f rows =
  List.map begin fun row ->
    let new_row = collapse_row row in
    f new_row (fill_zeros (4 - List.length new_row))
  end rows


let collapse_and_fill_columns f rows =
  let columns = to_columns rows in
  to_columns (collapse_and_fill_rows f columns)


let shift_world_up world =
  let new_rows = collapse_and_fill_columns (fun data zeros -> List.concat [data; zeros]) world.rows in
  { rows = new_rows }


let shift_world_down world =
  let new_rows = collapse_and_fill_columns (fun data zeros -> List.concat [reverse zeros []; data]) world.rows in
  { rows = new_rows }


let shift_world_left world =
  let new_rows = collapse_and_fill_rows (fun data zeros -> List.concat [data; zeros]) world.rows in
  { rows = new_rows }


let shift_world_right world =
  let new_rows = collapse_and_fill_rows (fun data zeros -> List.concat [reverse zeros []; data]) world.rows in
  { rows = new_rows }



let draw_start () =
  Graphics.open_graph "";
  Graphics.auto_synchronize false;
  Graphics.set_font "-bitstream-*-medium-r-normal--80-*-*-*-*-*-iso8859-1"


let draw_sync () =
  Graphics.synchronize ()


let clear_screen () =
  let width = Graphics.size_x () in
  let height = Graphics.size_y () in
  Graphics.set_color Graphics.white;
  Graphics.fill_rect 0 0 width height


let draw_piece x y num =
  Graphics.set_color (Graphics.rgb 0x80 0x80 0x80);
  Graphics.fill_rect x y 180 180;
  Graphics.set_color Graphics.black;
  Graphics.draw_rect x y 180 180;
  Graphics.moveto (x + 70) (y + 50);
  Graphics.draw_string (string_of_int num)


let draw_board world =
  let height = List.length world.rows in
  List.iteri begin fun y row ->
    List.iteri begin fun x num ->
      draw_piece (100 + (x * 200)) (100 + ((height - y - 1) * 200)) num
    end row;
  end world.rows

let draw_world world =
  clear_screen ();
  draw_board world;
  draw_sync ()


let rec event_loop () =
  let _ = Graphics.wait_next_event [Graphics.Poll] in
  (*Unix.sleep 1;*)
  event_loop ()


let pause () =
  try event_loop ()
  with Graphics.Graphic_failure _ -> Printf.printf "Exiting...\n"


let get_key_press () =
  let status = Graphics.wait_next_event [Graphics.Key_pressed] in
  if status.keypressed then
    status.key
  else 
    Char.chr 0


let rec run world =
  draw_world world;

  let next_world =
    match get_key_press () with
    | 'a' -> shift_world_left world
    | 'e' -> shift_world_right world
    | ',' -> shift_world_up world
    | 'o' -> shift_world_down world
    | _ -> world
  in

  flush stdout; 
  run next_world


let () =
  Random.self_init ();

  draw_start ();
  clear_screen ();

  run (generate_world ())

