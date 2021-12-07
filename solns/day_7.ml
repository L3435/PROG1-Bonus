let load vsebina_datoteke =
    vsebina_datoteke |> String.split_on_char ','  |> List.map int_of_string

let fuel1 goal x = abs(goal - x)

let fuel2 goal x = 
	let d = abs(goal - x) in
	(d * (d + 1)) / 2

let total f sez =
	let rec total' acc f =
		function
		| [] -> acc
		| a :: b -> total' (f a + acc) f b
	in
	total' 0 f sez

let optimum f sez =
	let rec bisect m m' f sez =
		if m' = m + 1 then total (f m') sez else
		let k = (m + m') / 2 in
		if total (f k) sez > total (f (k + 1)) sez then bisect k m' f sez else bisect m k f sez
	in
	bisect 0 2000 f sez

let naloga1 vsebina_datoteke =
	let s = load vsebina_datoteke in
	string_of_int (optimum fuel1 s)

let naloga2 vsebina_datoteke =
	let s = load vsebina_datoteke in
	string_of_int (optimum fuel2 s)

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
    let vsebina_datoteke = preberi_datoteko ("input/day_7.in") in

    let p1_start = Sys.time () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let t1_time = Sys.time () -. p1_start in
	
    let p2_start = Sys.time () in
    let odgovor2 = naloga2 vsebina_datoteke in
    let t2_time = Sys.time () -. p2_start in

    print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
    print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
    print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

    izpisi_datoteko ("output/day_7_1.out") odgovor1;
    izpisi_datoteko ("output/day_7_2.out") odgovor2