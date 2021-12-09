let right_side string =
	let (_ :: [s]) = String.split_on_char '|' string in
	let (_ :: out) = String.split_on_char ' ' s in
	out

let load1 vsebina_datoteke =
	let s = String.split_on_char '\n' vsebina_datoteke in
	s |> List.map String.trim |> List.map right_side
	

let naloga1 vsebina_datoteke =
	let s = load1 vsebina_datoteke in
	let s' = List.map ((List.fold_left (fun x y -> x + if List.exists (fun z -> z = String.length y) [2;3;4;7] then 1 else 0) 0)) s in
	string_of_int (List.fold_left (+) 0 s')

let naloga2 vsebina_datoteke = failwith "Lol nope"

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
    let vsebina_datoteke = preberi_datoteko ("input/day_8.in") in

    let p1_start = Sys.time () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let t1_time = Sys.time () -. p1_start in

    print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");

    izpisi_datoteko ("output/day_8_1.out") odgovor1