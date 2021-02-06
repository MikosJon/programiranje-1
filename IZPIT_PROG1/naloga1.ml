(* a *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki vrne razliko med produktom in vsoto dveh celih števil.

    razlika_produkta_in_vsote : int -> int -> int

[*----------------------------------------------------------------------------*)

let razlika_produkta_in_vsote a b = (a * b) - (a + b)

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo, ki združi dva para v četverico.

    zlimaj_para : 'a * 'b -> 'c * 'd -> 'a * 'b * 'c * 'd

[*----------------------------------------------------------------------------*)

let zlimaj_para (a, b) (c, d) = (a, b, c, d)

(* c *)
(*----------------------------------------------------------------------------*]
  Imamo podatke tipa [int option * int option * int option], ki jih želimo
  grafično predstaviti. Napišite funkcijo [trojica_graficno], ki sprejme takšno
  trojico in vrne niz, kjer so ``manjkajoči'' elementi nadomeščeni z [-].
  Primer vrnjenega niza je ["(1, 2, -)"]

    trojica_graficno : int option * int option * int option -> string

[*----------------------------------------------------------------------------*)

let trojica_graficno (a, b, c) =
  let new_a = if Option.is_none a then "-" else string_of_int (Option.get a)
  and new_b = if Option.is_none b then "-" else string_of_int (Option.get b)
  and new_c = if Option.is_none c then "-" else string_of_int (Option.get c)
  in
  "(" ^ new_a ^ ", " ^ new_b ^ ", " ^ new_c ^ ")"

(* d *)
(*----------------------------------------------------------------------------*]
  Klic funkcije [nedeljivo_do x n] preveri, da število [x] ni deljivo z nobenim
  naravnim številom od 2 do vključno [n]. Število 73859 je praštevilo, torej
  mora [nedeljivo_do 73859 73858] vrniti [true].

    nedeljivo_do : int -> int -> bool

[*----------------------------------------------------------------------------*)

let nedeljivo_do x n =
  let nedeljivo = ref true
  and koren = n |> float_of_int |> sqrt |> int_of_float
  in
  for i = 2 to koren + 1 do  (* koren + 1 je zato, če slučajno pride do npr. sqrt 16.0 -> 3.999 *)
    if x mod i = 0 then nedeljivo := false
  done;
  !nedeljivo

(* e *)
(*----------------------------------------------------------------------------*]
  Seznam elementov tipa ['a option] želimo razdeliti na podsezname glede na
  pojavitve vrednosti [None].

    razcepi_pri_None : 'a option list -> 'a list list.

  Kot primer, funkcija seznam

    [Some 1; None; Some 2; Some 3; None; None; Some 4; None]

  razcepi v [[1]; [2;3]; []; [4]; []]. Funkcija naj bo repno rekurzivna.
[*----------------------------------------------------------------------------*)

let razcepi_pri_None li =
  let rec aux acc current = function
    | [] -> List.rev (List.rev current :: acc)
    | None :: xs -> aux (List.rev current :: acc) [] xs
    | Some x :: xs -> aux acc (x :: current) xs
  in
  aux [] [] li