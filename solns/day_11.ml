let load_line line =
	let rec aux acc =
		function
		| "" -> acc
		| s -> aux ((int_of_char (String.get s 0) - int_of_char '0') :: acc) (String.sub s 1 (String.length s - 1))
	in
	aux [] line

let load vsebina_datoteke =
    let s = vsebina_datoteke |> String.split_on_char '\n' |> List.map String.trim in
	s |> List.map load_line |> List.map Array.of_list |> Array.of_list

let sosedi i j =
	List.init 9 (fun x -> (i + x / 3 - 1, j + (x mod 3) - 1)) |> List.filter (fun (x, y) -> x >= 0 && x < 10 && y >= 0 && y < 10)

let step a =
	let b = Array.init 10 (fun _ -> Array.make 10 false) in
	let rec queue acc i j a =
		if i = 10 then acc else
		if j = 10 then queue acc (i + 1) 0 a else (
			a.(i).(j) <- a.(i).(j) + 1;
			queue (if a.(i).(j) > 9 then (i,j) :: acc else acc) i (j + 1) a
		)
	in
	let rec flash i j a b =
		if i < 0 || i > 9 then false else
		if j < 0 || j > 9 then flash (i + 1) 0 a b else
		if b.(i).(j) then false else (
			a.(i).(j) <- a.(i).(j) + 1;
			if a.(i).(j) > 9 then (
				b.(i).(j) <- true;
				true
			) else false
		)
	in
	let rec flashq q a b =
		match q with
		| [] -> ()
		| (x, y) :: t -> flashq (if flash x y a b then (sosedi x y) @ t else t) a b
	in
	let rec reset acc i j a =
		if i = 10 then acc else
		if j = 10 then reset acc (i + 1) 0 a else (
			if a.(i).(j) > 9 then (
				a.(i).(j) <- 0;
				reset (acc + 1) i (j + 1) a
			) else reset acc i (j + 1) a
		)
	in
	flashq (queue [] 0 0 a) a b;
	reset 0 0 0 a

let naloga1 vsebina_datoteke =
    let s = load vsebina_datoteke in
	let rec steps acc a =
		function
		| 0 -> acc
		| x -> steps (acc + step a) a (x - 1)
	in
	string_of_int (steps 0 s 100)

let naloga2 vsebina_datoteke =
    let s = load vsebina_datoteke in
	let rec steps acc a =
		match step a with
		| 100 -> acc
		| _ -> steps (acc + 1) a
	in
	string_of_int (steps 1 s)
		


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
    let vsebina_datoteke = preberi_datoteko ("input/day_11.in") in

    let p1_start = Sys.time () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let t1_time = Sys.time () -. p1_start in
	
    let p2_start = Sys.time () in
    let odgovor2 = naloga2 vsebina_datoteke in
    let t2_time = Sys.time () -. p2_start in

    print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
    print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
    print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

    izpisi_datoteko ("output/day_11_1.out") odgovor1;
    izpisi_datoteko ("output/day_11_2.out") odgovor2