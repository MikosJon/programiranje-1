let odstej_trojici (x1, y1, z1) (x2, y2, z2) = (x1 - x2, y1 - y2, z1 - z2)

let max_rezultat_do_n f n =
  match List.init (n + 1) f with
  | x :: xs -> List.fold_left max x xs
  | [] -> failwith "argument n ne sme biti manj od 0" (* Tu naj bi že List.init moral dati napako*)

let pocisti_seznam li =
  let rec aux acc = function
    | [] -> List.rev acc
    | x :: xs -> match x with
        | None -> aux acc xs
        | Some v -> aux (v :: acc) xs
  in
  aux [] li

let preveri_urejenost li =
  let rec aux max_sodo min_liho = function
    | [] -> true
    | x :: xs ->
        if x mod 2 = 0 then
          if x < max_sodo then false
          else aux x min_liho xs
        else
          if x > min_liho then false
          else aux max_sodo x xs
  in
  aux Int.min_int Int.max_int li (* Rabimo zacetno najvecje in najmanjse videno stevilo *)

(*----------------------------------------------------------------------------------------*)

type 'a gnezdenje =
  | Element of 'a
  | Podseznam of 'a gnezdenje list

let gnezdenje_primer = [Element 1; Element 2; Podseznam [Element 3; Podseznam [Element 4]; Podseznam []]; Podseznam [Element 5]]

let rec najvecja_globina = function
  | [] -> 1
  | Element _ :: xs -> najvecja_globina xs
  | Podseznam ys :: xs -> max (1 + (najvecja_globina ys)) (najvecja_globina xs)

let rec preslikaj f = function
  | [] -> []
  | Element x :: xs -> Element (f x) :: preslikaj f xs
  | Podseznam ys :: xs -> Podseznam (preslikaj f ys) :: preslikaj f xs

let rec splosci = function
  | [] -> []
  | Element x :: xs -> Element x :: splosci xs
  | Podseznam ys :: xs -> splosci ys @ splosci xs

let alternirajoci_konstruktorji li =
  (* expected: 0 -- Podseznam, 1 -- Element*)
  let rec aux expected = function
    | [] -> true
    | Element _ :: xs -> if expected = 0 then aux 1 xs else false
    | Podseznam _ :: xs -> if expected = 1 then aux 0 xs else false
  in
  match li with
    | [] -> true
    | Element _ :: ys -> aux 1 ys
    | Podseznam _ :: ys -> aux 0 ys

let zlozi_preko_gnezdenja f n li =
  let rec aux acc = function
    | [] -> acc
    | Element x :: xs -> aux (f acc x) xs
    | Podseznam ys :: xs -> aux (aux acc ys) xs
  in
  aux n li

(*-----------------------------------------------------------------------*)