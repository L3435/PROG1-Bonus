let load vsebina_datoteke =
    vsebina_datoteke |> String.split_on_char ','  |> List.map int_of_string

let count x list =
  let rec count' acc x =
    function
    | [] -> acc
    | a :: b -> count'(acc + if x = a then 1 else 0) x b
  in
  count' 0 x list

let convert list =
  let rec convert' acc =
    function
    | -1 -> acc
    | x -> convert' ((count x list) :: acc) (x-1)
  in
  convert' [] 8

let rec fiiiish list =
  let fish [a;b;c;d;e;f;g;h;i] =
    [b;c;d;e;f;g;h+a;i;a]
  in
  function
  | 0 -> list
  | x -> fiiiish (fish list) (x-1)

let naloga1 vsebina_datoteke =
	let s = convert (load vsebina_datoteke) in
  string_of_int (List.fold_left (+) 0 (fiiiish s 80))

let naloga2 vsebina_datoteke =
	let s = convert (load vsebina_datoteke) in
  string_of_int (List.fold_left (+) 0 (fiiiish s 256))

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
    let vsebina_datoteke = preberi_datoteko ("input/day_6.in") in

    let p1_start = Sys.time () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let t1_time = Sys.time () -. p1_start in
	
    let p2_start = Sys.time () in
    let odgovor2 = naloga2 vsebina_datoteke in
    let t2_time = Sys.time () -. p2_start in

    print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
    print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
    print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

    izpisi_datoteko ("output/day_6_1.out") odgovor1;
    izpisi_datoteko ("output/day_6_2.out") odgovor2