(*============================================================================*]
  Filip potrebuje pomoč pri organiziranju kuhinje. Posode in omare mu je uspelo
  popisati in ustrezno označiti, sedaj pa mora nad tem izvajati kopico
  arhivskih nalog, kjer nastopite vi.
[*============================================================================*)

type 'a kuhinjski_element =  (* popravljeno ime, da se ujema z navodili*)
  | Ponev of 'a
  | Lonec of 'a * 'a
  | Omara of 'a list

(* a *)
(*----------------------------------------------------------------------------*]
  Definirajte primer seznama kuhinjskih elementov [kuhinja], kjer ponev vsebuje
  niz "tuna", lonec vsebuje "brokoli" in "mango", omara pa vsebuje "sir",
  "toast", "sok" in "ragu".
[*----------------------------------------------------------------------------*)

let kuhinja = [Ponev "tuna"; Lonec ("brokoli", "mango"); Omara ["sir"; "toast"; "sok"; "ragu"]]

(* b *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [prestej], ki za podani seznam kuhinjskih elementov vrne
  skupno število vsebinskih elementov. Za zgornji primer [kuhinja] bi tako
  vrnila 7.
[*----------------------------------------------------------------------------*)

let rec prestej = function
  | [] -> 0
  | Ponev _ :: xs -> 1 + prestej xs
  | Lonec _ :: xs -> 2 + prestej xs
  | Omara li :: xs -> List.length li + prestej xs

(* c *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo, ki sprejme funkcijo [f] in kuhinjski element ter
  funkcijo [f] uporabi na celotni vsebini elementa.

    pretvori : (’a -> ’b) -> ’a kuhinjski_element -> ‘b kuhinjski_element

[*----------------------------------------------------------------------------*)

let rec pretvori f = function
  | Ponev x -> Ponev (f x)
  | Lonec (a, b) -> Lonec (f a, f b)
  | Omara li -> Omara (List.map f li)

(* d *)
(*----------------------------------------------------------------------------*]
  Definirajte funkcijo [pospravi], ki sprejme seznam kuhinjskih elementov in
  vsebino vseh elementov pospravi v eno samo [Omaro]. Vrstni red elementov v
  končni omari je nepomemben. Za vse točke naj bo funkcija repno rekurzivna.

    pospravi : ’a kuhinjski_element list -> ‘a kuhinjski_element

[*----------------------------------------------------------------------------*)

let pospravi li =
  let rec aux acc = function
    | [] -> acc
    | Ponev x :: xs -> aux (x :: acc) xs
    | Lonec (a, b) :: xs -> aux (a :: b :: acc) xs
    | Omara li :: xs -> aux (List.rev_append li acc) xs
  in
  Omara (aux [] li)

(* e *)
(*----------------------------------------------------------------------------*]
  Napišite funkcijo [oceni], ki sprejme seznam tipa ['a kuhinjski_element list]
  in cenilko vsebine tipa [‘a -> int]. Funkcija izračuna skupno ceno celotnega
  seznama, kjer je cena vsebine v loncih množena s 3, v omarah pa s 5.

  Ocena testne kuhinje za cenilko [String.length] je 115.
[*----------------------------------------------------------------------------*)

let rec oceni li f = match li with
  | [] -> 0
  | Ponev x :: xs -> f x + oceni xs f
  | Lonec (a, b) :: xs -> 3 * (f a + f b) + oceni xs f
  | Omara li :: xs ->
      match li with
        | [] -> oceni xs f
        | y :: ys -> 5 * List.fold_left (fun acc v -> acc + (f v)) (f y) ys + oceni xs f

