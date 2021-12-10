let load vsebina_datoteke =
    vsebina_datoteke |> String.split_on_char '\n' |> List.map String.trim

let value =
    function
    | ')' -> 3
    | ']' -> 57
    | '}' -> 1197
    | '>' -> 25137
    | _ -> failwith "lol bad"

let value2 =
    function
    | ')' -> 1
    | ']' -> 2
    | '}' -> 3
    | '>' -> 4
    | _ -> failwith "lol bad"

let matching =
    function
    | '(' -> ')'
    | '[' -> ']'
    | '{' -> '}'
    | '<' -> '>'
    | _ -> failwith "lol bad"

let stck_val stck =
	let rec aux acc =
		function
		| [] -> acc
		| a :: b -> aux (value2 a + 5 * acc) b
	in
	aux 0 stck

let checkline line part =
    let rec aux stck string =
		match string with
		| "" -> if part = 1 then 0 else stck_val stck
		| _ -> let c = String.get string 0 in
			if c = '(' || c = '[' || c = '{' || c = '<' then aux ((matching c) :: stck) (String.sub string 1 (String.length string - 1))
			else
			match stck with
			| [] -> 0
			| a :: b -> if a = c then aux b (String.sub string 1 (String.length string - 1)) else if part = 1 then value c else 0
    in
    aux [] line

let naloga1 vsebina_datoteke =
    let s = load vsebina_datoteke in
	let rec aux acc =
        function
        | [] -> acc
        | a :: b -> aux (checkline a 1 + acc) b
    in
    string_of_int (aux 0 s)

let naloga2 vsebina_datoteke =
    let s = load vsebina_datoteke in
	let rec aux acc =
        function
        | [] -> acc
        | a :: b -> aux (checkline a 2 :: acc) b
    in
	let a = Array.of_list (List.filter (fun x -> x <> 0) (aux [] s)) in
	Array.sort (fun x y -> x - y) a;
	string_of_int a.(Array.length a / 2)


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
    let vsebina_datoteke = preberi_datoteko ("input/day_10.in") in

    let p1_start = Sys.time () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let t1_time = Sys.time () -. p1_start in
	
    let p2_start = Sys.time () in
    let odgovor2 = naloga2 vsebina_datoteke in
    let t2_time = Sys.time () -. p2_start in

    print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
    print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
    print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

    izpisi_datoteko ("output/day_10_1.out") odgovor1;
    izpisi_datoteko ("output/day_10_2.out") odgovor2