###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end] (vključujoč oba robova).
#
# Primer: za [start = 1] in [end = 7] tabelo
#
#     [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 4 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 0, 2, 4, 11, 5, 17, 15, 18]
###############################################################################

def pivot(li, start, end):
    pivot = li[start]
    boundary = start

    for i in range(start + 1, end + 1):
        if li[i] < pivot:
            li[i], li[boundary + 1] = li[boundary + 1], li[i]
            boundary += 1
    li[start], li[boundary] = li[boundary], li[start]
    return boundary

###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti element
# po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da jo
# rešite brez da v celoti uredite tabelo [a].
###############################################################################

def kth_element(li, k):
    def aux(start, end):
        pos = pivot(li, start, end)
        if pos == k:
            return li[pos]
        elif pos > k:
            return aux(start, pos - 1)
        else:
            return aux(pos + 1, end)
    return aux(0, len(li) - 1)

###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def quicksort(li):
    def quicksort_part(start, end):
        if start >= end: return None
        pos = pivot(li, start, end)
        quicksort_part(start, pos - 1)
        quicksort_part(pos + 1, end)
    return quicksort_part(0, len(li) - 1)

###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
#
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
#
# Sestavite funkcijo [merge(target, list_1, list_2)], ki v tabelo [target]
# zlije tabeli [list_1] in [list_2]. V primeru, da sta elementa v obeh tabelah
# enaka, naj bo prvi element iz prve tabele.
#
# Primer:
#
#     >>> list_1 = [1, 3, 5, 7, 10]
#     >>> list_2 = [1, 2, 3, 4, 5, 6, 7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> merge(target, list_1, list_2)
#     >>> target
#     [1, 1, 2, 3, 3, 4, 5, 5, 6, 7, 7, 10]
#
###############################################################################

def zlij(target, begin, end, list1, list2):
    len_1 = len(list1)
    len_2 = len(list2)
    i1, i2 = 0, 0

    while i1 < len_1 and i2 < len_2:
        elem1, elem2 = list1[i1], list2[i2]
        if elem1 <= elem2:
            target[begin + i1 + i2] = elem1
            i1 += 1
        else:
            target[begin + i1 + i2] = elem2
            i2 += 1

    while i1 < len_1:
        target[begin + i1 + i2] = list1[i1]
        i1 += 1
    while i2 < len_2:
        target[begin + i1 + i2] = list2[i2]
        i2 += 1

###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort).
# Tabelo razdelimo na polovici, ju rekurzivno uredimo in nato zlijemo z uporabo
# funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja.
# Za razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je
# potrebno narediti na mestu.
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> mergesort(a)
#     >>> a
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def mergesort(li):
    n = len(li)
    if n in {0, 1}:
        return None

    divider = n // 2
    left = li[:divider]
    right = li[divider:]

    mergesort(left)
    mergesort(right)

    zlij(li, 0, n, left, right)