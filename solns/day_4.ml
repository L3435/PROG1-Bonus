let rec convert bingo part =
	function
	| [] -> bingo
	| "" :: x -> convert (part :: bingo) [] x
	| a :: b -> convert bingo (a :: part) b

let load vsebina_datoteke =
    let stevila :: _ :: tail = String.split_on_char '\n' (vsebina_datoteke ^"\n") in
    let partial = tail |> List.map String.trim |> convert [] [] |> List.map (List.map String.trim) in
	let partial2 = partial |> List.map (List.map (String.split_on_char ' ')) |> List.map (List.map (List.filter (function x -> x <> ""))) in
	(stevila |> String.trim |> String.split_on_char ',' |> List.map int_of_string, partial2 |> List.map (List.map (List.map int_of_string)))

let delnum num sez =
	List.map (List.map (List.map (function x -> if x = num then -1 else x))) sez

let checkboard board =
	let rec checkrow =
		function
		| [] -> false
		| a :: b -> if a = [-1;-1;-1;-1;-1] then true else checkrow b
	in
	let rec checkcol =
		let rec splitter col rest =
			function
			| [] :: _ -> ([], [])
			| (a::b) :: c -> splitter (a :: col) (b :: rest) c
			| [] -> (col, rest)
		in
		function
		| [] -> false
		| x -> let (x, y) = splitter [] [] x in
			if x = [-1;-1;-1;-1;-1] then true else checkcol y
	in
	checkrow board || checkcol board

let value board =
	let filtered = List.map (List.filter (function x -> x <> -1)) board
	in
	List.fold_left (+) 0 ((List.map (List.fold_left (+) 0)) filtered)

let rec final =
	function
	| [] -> 0
	| a :: b -> if checkboard a then value a else final b

let rec iterate nums boards =
	match nums with
	| [] -> 0
	| a :: b -> let x = final (delnum a boards) in
		if x <> 0 then a * x else iterate b (delnum a boards)

let rec iterate2 nums boards =
	match nums with
	| [] -> 0
	| a :: b -> let x = delnum a boards in
		match x with
		| [] -> failwith "lol bad"
		| p :: [] -> if checkboard p then a * (value p) else iterate2 b [p]
		| x -> iterate2 b (List.filter (function x -> not (checkboard x)) x)

let naloga1 vsebina_datoteke =
	let (nums, boards) = load vsebina_datoteke in
	string_of_int (iterate nums boards)

let naloga2 vsebina_datoteke =
	let (nums, boards) = load vsebina_datoteke in
	string_of_int (iterate2 nums boards)

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
    let vsebina_datoteke = preberi_datoteko ("input/day_4.in") in

    let p1_start = Sys.time () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let t1_time = Sys.time () -. p1_start in
	
    let p2_start = Sys.time () in
    let odgovor2 = naloga2 vsebina_datoteke in
    let t2_time = Sys.time () -. p2_start in

    print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
    print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
    print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

    izpisi_datoteko ("output/day_4_1.out") odgovor1;
    izpisi_datoteko ("output/day_4_2.out") odgovor2