let dir string =
	match String.get string 0 with
	| 'f' -> 0
	| 'u' -> -1
	| 'd' -> 1
	| _ -> failwith "Napačen format vnosa!"

let rec convert =
	function
	| [a;b] -> [dir a; int_of_string b]
	| _ -> failwith "Seznam mora vsebovati 2 elementa!"

let load vsebina_datoteke =
    let s = String.split_on_char '\n' vsebina_datoteke in
    s |> List.map String.trim |> List.map (String.split_on_char ' ') |> List.map convert

let naloga1 vsebina_datoteke =
	let list = load vsebina_datoteke in
    let rec solve h d =
		function
		| [] -> (h, d)
		| [p;q] :: t -> if p = 0 then solve h (d + q) t else solve (h + p * q) d t
		| _ -> failwith "Vsak element seznama mora vsebovati dva elementa!"
	in
	let (h, d) = solve 0 0 list in string_of_int (h * d)

let naloga2 vsebina_datoteke =
	let list = load vsebina_datoteke in
    let rec solve h d aim =
		function
		| [] -> (h, d)
		| [p;q] :: t -> if p = 0 then solve (h + q * aim) (d + q) aim t else solve h d (aim + p * q) t
		| _ -> failwith "Vsak element seznama mora vsebovati dva elementa!"
	in
	let (h, d) = solve 0 0 0 list in string_of_int (h * d)

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
    let vsebina_datoteke = preberi_datoteko ("input/day_2.in") in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko ("output/day_2_1.out") odgovor1;
    izpisi_datoteko ("output/day_2_2.out") odgovor2