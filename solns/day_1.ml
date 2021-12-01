let load vsebina_datoteke =
    let s = String.split_on_char '\n' vsebina_datoteke in
    s |> List.map String.trim |> List.map int_of_string

let rec find_mono sez acc =
        match sez with
        | [] -> acc
        | a :: [] -> acc
        | a :: (b :: c) -> if a < b then find_mono (b :: c) (acc + 1) else find_mono (b :: c) acc

let reverse sez =
    let rec rev acc =
        function
        | [] -> acc
        | a :: b -> rev (a :: acc) b
    in
    rev [] sez

let sums sez =
    let rec sums' acc =
        function
        | [] -> acc
        | a :: a' -> match a' with
            | [] -> acc
            | b :: b' -> match b' with
                | [] -> acc
                | c :: c' -> sums' ((a + b + c) :: acc) a'
    in
    reverse (sums' [] sez)

let naloga1 vsebina_datoteke =
    let sez = load vsebina_datoteke in
    string_of_int (find_mono sez 0)

let naloga2 vsebina_datoteke =
    let sez = load vsebina_datoteke in
    string_of_int (find_mono (sums sez) 0)

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
    let vsebina_datoteke = preberi_datoteko ("input/day_1.in") in
    let odgovor1 = naloga1 vsebina_datoteke
    and odgovor2 = naloga2 vsebina_datoteke
    in
    izpisi_datoteko ("output/day_1_1.out") odgovor1;
    izpisi_datoteko ("output/day_1_2.out") odgovor2