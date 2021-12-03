let rec bin acc =
	function
	| "" -> acc
	| x -> bin (2 * acc + int_of_char (String.get x 0) - 48) (String.sub x 1 (String.length x - 1))

let load vsebina_datoteke =
    let s = String.split_on_char '\n' vsebina_datoteke in
    s |> List.map String.trim

let naloga1 vsebina_datoteke =
	let list = load vsebina_datoteke in
    let rec solve n1 n2 curr nl sez =
		match sez with
		| [] -> (match nl with
			| [] -> failwith "To ne bi smelo biti mogoče"
			| "" :: nlx -> ((2 * n1 + if curr >= 0 then 1 else 0) * (2 * n2 + if curr <= 0 then 1 else 0))
			| _ -> solve (2 * n1 + if curr >= 0 then 1 else 0) (2 * n2 + if curr >= 0 then 0 else 1) 0 [] nl
			)
		| a :: b -> solve n1 n2 (curr - 97 + 2 * int_of_char (String.get a 0)) ((String.sub a 1 (String.length a - 1)) :: nl) b
	in
	string_of_int (solve 0 0 0 [] list)

let naloga2 vsebina_datoteke =
	let list = load vsebina_datoteke in
    let rec solve c curr nl idx sez =
		match sez with
		| [] -> (let newlist = (nl |> List.filter (function x -> if c = 0 then
				String.get x idx = if 2 * curr >= 0 then '1' else '0'
				else
				String.get x idx = if 2 * curr >= 0 then '0' else '1'))
			in
			match newlist with
			| [] -> failwith "Teach me your ways"
			| [a] -> a
			| _ -> solve c 0 [] (idx + 1) newlist
			)
		| a :: b -> solve c (curr - 97 + 2 * int_of_char (String.get a idx)) (a :: nl) idx b
	in
	string_of_int (bin 0 (solve 0 0 [] 0 list) * bin 0 (solve 1 0 [] 0 list))

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
    let vsebina_datoteke = preberi_datoteko ("input/day_3.in") in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko ("output/day_3_1.out") odgovor1;
    izpisi_datoteko ("output/day_3_2.out") odgovor2