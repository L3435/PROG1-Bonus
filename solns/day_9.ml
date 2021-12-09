let string_to_list s =
    let rec aux acc =
        function
        | "" -> acc
        | s -> aux ((String.get s 0) :: acc) (String.sub s 1 (String.length s - 1))
    in
    aux [] s

let int_from_char c =
    int_of_char c - int_of_char '0'

let load vsebina_datoteke =
	let s = vsebina_datoteke |> String.split_on_char '\n' in
	let s' = s |> List.map String.trim |> List.map string_to_list in
    s' |> List.map (List.map int_from_char) |> List.map Array.of_list |> Array.of_list

let is_lower i j x y grid =
    if x < 0 || x >= Array.length grid || y < 0 || y >= Array.length grid.(x) then 1 else
    if grid.(i).(j) < grid.(x).(y) then 1 else 0

let is_lowest i j grid =
    if is_lower i j (i+1) j grid + is_lower i j (i-1) j grid + is_lower i j i (j+1) grid + is_lower i j i (j-1) grid = 4 then
    grid.(i).(j) + 1 else 0

let component_size i j grid bgrid =
    let rec dfs acc list grid bgrid =
        match list with
        | [] -> acc
        | (x,y) :: t -> if x < 0 || x >= Array.length grid || y < 0 || y >= Array.length grid.(x) || bgrid.(x).(y) || grid.(x).(y) = 9 then
            dfs acc t grid bgrid else
            (
                bgrid.(x).(y) <- true;
                dfs (acc + if grid.(x).(y) < 9 then 1 else 0) ([(x+1,y);(x-1,y);(x,y+1);(x,y-1)] @ t) grid bgrid
            )
    in
    dfs 0 [(i,j)] grid bgrid

let naloga1 vsebina_datoteke =
	let grid = load vsebina_datoteke in
    let rec aux acc i j grid =
        if i = Array.length grid then acc else
        if j =  Array.length grid.(0) then aux acc (i+1) 0 grid else aux (acc + is_lowest i j grid) i (j+1) grid
    in
    string_of_int (aux 0 0 0 grid)

let naloga2 vsebina_datoteke =
	let grid = load vsebina_datoteke in
    let bgrid = Array.init (Array.length grid) (fun _ -> Array.make (Array.length grid.(0)) false) in
    let rec aux acc i j grid bgrid =
        if i = Array.length grid then acc else
        if j = Array.length grid.(0) then aux acc (i+1) 0 grid bgrid else
        aux (if bgrid.(i).(j) then acc else (component_size i j grid bgrid) :: acc) i (j+1) grid bgrid
    in
    let (a :: b :: c :: t) = List.sort (fun x y -> y - x) (aux [] 0 0 grid bgrid) in
    string_of_int (a * b * c)


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
    let vsebina_datoteke = preberi_datoteko ("input/day_9.in") in

    let p1_start = Sys.time () in
    let odgovor1 = naloga1 vsebina_datoteke in
    let t1_time = Sys.time () -. p1_start in
	
    let p2_start = Sys.time () in
    let odgovor2 = naloga2 vsebina_datoteke in
    let t2_time = Sys.time () -. p2_start in

    print_endline ("P1 taken: " ^ string_of_float t1_time ^ "s");
    print_endline ("P2 taken: " ^ string_of_float t2_time ^ "s");
    print_endline ("Total: " ^ string_of_float (t1_time +. t2_time) ^ "s");

    izpisi_datoteko ("output/day_9_1.out") odgovor1;
    izpisi_datoteko ("output/day_9_2.out") odgovor2