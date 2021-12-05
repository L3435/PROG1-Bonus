let load vsebina_datoteke =
    let s = vsebina_datoteke |> String.split_on_char '\n'  |> List.map String.trim in
	let s' = s |> List.map (String.split_on_char ' ') |> List.map (List.filter (function x -> x <> "->")) in
	s' |> List.map (List.map (String.split_on_char ',')) |> List.map (List.map (List.map int_of_string))

let sgn x = if x = 0 then 0 else if x > 0 then 1 else -1

let inc grid n m =
	grid.(n).(m) <- (grid.(n).(m) + 1)

let addline line grid =
	let [[a;b];[c;d]] = line in
	let rec update a b c d x y grid =
		if x = c && y = d then inc grid x y else
		(inc grid x y;
		update a b c d (x + sgn (c - a)) (y + sgn (d - b)) grid)
	in
	update a b c d a b grid

let count grid =
	let rec count' acc x y grid =
		match y with
		| 1000 -> acc
		| y -> match x with
			| 1000 -> count' acc 0 (y+1) grid
			| x -> count' (acc + if grid.(x).(y) > 1 then 1 else 0) (x+1) y grid
	in
	count' 0 0 0 grid

let naloga1 vsebina_datoteke =
	let sez = load vsebina_datoteke in
	let grid = Array.make_matrix 1000 1000 0 in
	let rec addlines grid =
		function
		| [] -> ()
		| [[a;b];[c;d]] :: p -> if a = c || b = d then
			addline [[a;b];[c;d]] grid;
			addlines grid p
	in
	addlines grid sez;
	string_of_int (count grid)

let naloga2 vsebina_datoteke =
	let sez = load vsebina_datoteke in
	let grid = Array.make_matrix 1000 1000 0 in
	let rec addlines grid =
		function
		| [] -> ()
		| a :: b ->
			addline a grid;
			addlines grid b
	in
	addlines grid sez;
	string_of_int (count grid)

let _ =
    let preberi_datoteko ime_datoteke =
        let chan = open_in_bin ime_datoteke in
        let vsebina = really_input_string chan (in_channel_length chan) in
        close_in chan;
        vsebina
    and izpisi_datoteko ime_datoteke vsebina =
        let chan = open_out_bin ime_datoteke in
        output_string chan vsebina;
        close_out chan
    in
    let vsebina_datoteke = preberi_datoteko ("input/day_5.in") in

    let p1_start = Sys.time () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let t1_time = Sys.time () -. p1_start in
	
    let p2_start = Sys.time () in
    let odgovor2 = naloga2 vsebina_datoteke in
    let t2_time = Sys.time () -. p2_start in

    print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
    print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
    print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

    izpisi_datoteko ("output/day_5_1.out") odgovor1;
    izpisi_datoteko ("output/day_5_2.out") odgovor2