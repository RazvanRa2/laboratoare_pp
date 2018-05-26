% Povestea (inspirată de Știrile de la Ora 5)
%
% În liniștitul nostru oraș s-a produs o crimă. Un individ a pătruns
% în casa unui bătrân și l-a ucis. Cadavrul a fost ascuns de către
% criminal și nu este de găsit. Este un caz complicat, dar doi
% detectivi, D1 și D2, fac cercetări și au deja o listă de
% suspecți. Știu despre fiecare dintre aceștia ce mașină are și care
% este arma lui preferată.
%
% Pentru a rezolva cazul trebuie să afle cu ce armă a fost ucisă
% victima și cu ce mașină a fugit criminalul. Din fericire, dacă se
% poate vorbi despre așa ceva în cazul unei crime îngrozitoare, nu
% există doi suspecți care să aibă aceeași mașină și aceeași armă.
%
% Cei doi detectivi se întâlnesc la secție. D1 s-a întâlnit cu un
% martor și a aflat cu ce mașină a fugit criminalul. D2 a găsit arma
% crimei. Cei doi se întâlnesc la secție, unde au următorul dialog pe
% care tu îl asculți indiscret.
%
% D1: Știu că nu știi cine-i criminalul. Nici eu nu știu.
% D2: Încă nu știu cine este.
% D1: Nici eu nu știu încă cine este criminalul.
% D1: Acum mi-am dat seama.
% D2: Mi-am dat seama și eu.
%
% Cine este criminalul?

% ----------------------------------------------------------------------------
% Mașini

conduce(aurel, ford).
conduce(bogdan, bmw).
conduce(cosmin, bmw).
conduce(daniel, ford).
conduce(eugen, bmw).
conduce(florin, dacia).
conduce(george, fiat).
conduce(horia, audi).
conduce(irina, dacia).
conduce(jean, fiat).
conduce(kiki, audi).
conduce(laura, seat).
conduce(marian, mercedes).
conduce(nicodim, opel).
conduce(ovidiu, honda).
conduce(paul, honda).

% Arme

inarmat(aurel, sabie).
inarmat(bogdan, pistol).
inarmat(cosmin, arbaleta).
inarmat(daniel, grenada).
inarmat(eugen, grenada).
inarmat(florin, sabie).
inarmat(george, pistol).
inarmat(horia, arbaleta).
inarmat(irina, pusca).
inarmat(jean, cutit).
inarmat(kiki, prastie).
inarmat(laura, pusca).
inarmat(marian, cutit).
inarmat(nicodim, prastie).
inarmat(ovidiu, maceta).
inarmat(paul, mitraliera).

setMinus(From, ToRemove, Result) :-
        findall(E, (member(E, From), \+ member(E, ToRemove)), Result).
checkSet(P, SetC) :- call(P, Set), msort(Set, SetSorted),
    (   SetSorted == SetC, ! ;
        format("~nEsec: predicatul a intors ~w, dar se astepta ~w.~n", [Set, SetC]), fail).
checkSetOf(P, SetC) :-
    ( setof(S, call(P, S), Set), ! ;
        format("~nEsec: nicio solutie intoarsa de predicatul ~w.~n", [P]), fail),
    (   Set == SetC, ! ;
        setMinus(Set, SetC, TooMuch),
        setMinus(SetC, Set, TooLittle),
        format("~nEsec: predicatul a intors~n ~w,~n dar se astepta~n ~w~n (~w sunt in plus, ~w lipsesc~n", [Set, SetC, TooMuch, TooLittle]), fail).

% ----------------------------------------------------------------------------
% 1. (1p) Scrieți un predicat suspect(Nume:Marca:Arma) care să fie
% adevărat pentru fiecare suspect al problemei noastre.

suspect(_Nume:_Marca:_Arma) :- conduce(_Nume, _Marca), inarmat(_Nume, _Arma).

check1:-test(1, 1, checkSetOf(suspect,
            [aurel:ford:sabie, bogdan:bmw:pistol,
             cosmin:bmw:arbaleta, daniel:ford:grenada, eugen:bmw:grenada,
             florin:dacia:sabie, george:fiat:pistol, horia:audi:arbaleta,
             irina:dacia:pusca, jean:fiat:cutit, kiki:audi:prastie,
             laura:seat:pusca, marian:mercedes:cutit, nicodim:opel:prastie,
             ovidiu:honda:maceta, paul:honda:mitraliera])).

% ----------------------------------------------------------------------------
% 2. (1p) Scrieți un predicat au_pusca(-ListaNume) care să fie
% adevărat atunci când ListaNume este lista cu numele tuturor celor care
% au pușcă.

au_pusca(_ListaNume) :- findall(X, inarmat(X, pusca), _ListaNume).

check2 :- test(2, 1, checkSet(au_pusca, [irina, laura])).

% ----------------------------------------------------------------------------
% 3. (1p) Scrieți predicatele au_arma(+Arma, -ListaNume)
% și au_marca(+Arma, -ListaNume), care să fie adevărate atunci când
% ListaNume este lista cu numele tuturor celor care au arma de tipul
% Arma, respectiv mașina de tipul Marca.

au_arma(_Arma, _ListaNume) :- findall(X, inarmat(X, _Arma), _ListaNume).

au_marca(_Marca, _ListaNume) :- findall(X, conduce(X, _Marca), _ListaNume).

check3 :- tests(3, 1, [
               checkSet(au_arma(pistol), [bogdan, george]),
               checkSet(au_arma(cutit), [jean, marian]),
               checkSet(au_arma(mitraliera), [paul]),
               checkSet(au_marca(bmw), [bogdan, cosmin, eugen]),
               checkSet(au_marca(dacia), [florin, irina]),
               checkSet(au_marca(seat), [laura])]).

% ----------------------------------------------------------------------------
% 4. (1.5p) Scrieți un predicat arme_bmw(ListaArme) care să fie adevărat
% atunci când ListaArme reprezintă mulțimea tuturor armelor deținute
% de conducători de bmw.

arme_bmw(_ListaArme) :- findall(Y, suspect(X : bmw: Y), _ListaArme).

check4 :- test(4, 1.5, checkSet(arme_bmw, [arbaleta, grenada, pistol])).

% ----------------------------------------------------------------------------
% 5. (1.5p) Scrieți un predicat arme_marca(Marca, ListaArme) care să
% fie adevărat atunci când ListaArme reprezintă mulțimea tuturor
% armelor deținute de conducători de mașini de tipul Marca.

arme_marca(_Marca, _ListaArme) :- findall(Y, suspect(X : _Marca : Y), _ListaArme).

check5 :- tests(5, 1.5, [
                       checkSet(arme_marca(bmw), [arbaleta, grenada, pistol]),
                       checkSet(arme_marca(dacia), [pusca, sabie]),
                       checkSet(arme_marca(seat), [pusca])]).

% ----------------------------------------------------------------------------
% 6. (2p) Scrie un predicat marci_arma_unica(ListaMarci) care să afișeze
% lista mașinilor pentru care lista armelor pe care le dețin
% conducătorii unei mărci conține un singur element. Hint: folosiți-vă
% de rezolvarea exercițiului 5. Nu folosiți length/2.

marci_arma_unica(_ListaMarci) :- findall(X, (conduce(_, X), arme_marca(X, [_])), _ListaMarci).

check6:- test(6, 1, checkSet(marci_arma_unica, [mercedes, opel, seat])).

% ----------------------------------------------------------------------------
% ----------------------------------------------------------------------------

% Să revenim la secție de poliție unde tu tragi cu urechea la dialogul
% dintre cei doi detectivi:
%
% Prima replică:
% Detectiv A : Știam că nu știi cine-i criminalul.
%
% Ce înseamnă asta? Detectivul A știe mașina cu care a fugit
% suspectul. Această marcă de mașină este condusă de suspecți care
% mânuiesc diferite arme. Dacă măcar una dintre aceste arme ar fi
% aparținut doar unui singur suspect, atunci Detectivul B ar fi putut
% ști care este soluția acestui caz.
%
% Ce soluții eliminăm?

% Dacă Detectivul A ar fi aflat că adevăratul criminal a condus o
% Honda, atunci ar fi existat două soluții posibile:
%
% ovidiu - honda - maceta
% ovidiu - paul - mitraliera
%
% Dar cum nu există decât un singur individ care are macetă (Ovidiu),
% Detectivul A nu ar fi putut afirma că Detectivul B nu poate ști.
%
% honda nu este, deci, o soluție
%
% Trebuie eliminate toate mașinile care sunt "în pereche" cu arme
% pentru care nu există mai multe posibilități.

% ----------------------------------------------------------------------------
% 7. (2.5p) Scrie un predicat suspect1/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului A: niciuna dintre armele asociate cu mașina suspectului
% nu indică în mod unic un anumit individ.

suspect1(Nume:Marca:Arma) :- suspect(Nume:Marca:Arma),
                             arme_marca(Marca, ListaArme),
                             member(Arma, ListaArme),
                             au_arma(Arma, [_,_|_]).

check7 :- test(7, 2.5, checkSetOf(suspect1,
            [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta,
             daniel:ford:grenada,eugen:bmw:grenada,florin:dacia:sabie,
             george:fiat:pistol,horia:audi:arbaleta,irina:dacia:pusca,
             jean:fiat:cutit,kiki:audi:prastie,laura:seat:pusca,
             marian:mercedes:cutit,nicodim:opel:prastie])).

% ----------------------------------------------------------------------------
% A doua replică:
%
% Detectivul A: Nici eu nu știu!
%
% 8. (1.5p) Scrie un predicat suspect2/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului A: marca nu indică unic un individ.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primei replici.

suspect2(Nume:Marca:Arma) :- suspect1(Nume:Marca:Arma),
                            findall(X, conduce(X, Marca), List), length(List, Len), Len > 1.
check8 :- test(8, 1.5, checkSetOf(suspect2,
            [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta,
              daniel:ford:grenada,eugen:bmw:grenada,florin:dacia:sabie,
              george:fiat:pistol,horia:audi:arbaleta,irina:dacia:pusca,
              jean:fiat:cutit,kiki:audi:prastie])).

% ----------------------------------------------------------------------------
% A treia replică:
%
% Detectivul B: Nici eu nu știu!
%
% 9. (1p) Scrie un predicat suspect3/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului B: arma nu identifică unic un individ.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primelor două replici.

suspect3(Nume:Marca:Arma) :- suspect2(Nume:Marca:Arma),
                             findall(X, suspect2(X:Y:Arma), ListaSuspecti),
                             length(ListaSuspecti, Len),
                             Len > 1.

check9 :- test(9, 1, checkSetOf(suspect3,
            [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta,
              daniel:ford:grenada,eugen:bmw:grenada,florin:dacia:sabie,
              george:fiat:pistol,horia:audi:arbaleta])).

% ----------------------------------------------------------------------------
% A patra replică:
%
% Detectivul A: Eu tot nu știu!
%
% 10. (1p) Scrie un predicat suspect4/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului A.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primelor trei replici.

suspect4(Nume:Marca:Arma) :- suspect3(Nume:Marca:Arma),
                             findall(X, suspect3(X:Marca:Y), ListaSuspecti),
                             length(ListaSuspecti, Len),
                             Len > 1.

check10 :- test(10, 1, checkSetOf(suspect4,
             [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta,
              daniel:ford:grenada,eugen:bmw:grenada])).

% ----------------------------------------------------------------------------
% A cincea replică:
%
% Detectivul B: Eu am aflat!
%
% 11. (1p) Scrie un predicat suspect5/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului B.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primelor patru replici.

suspect5(Nume:Marca:Arma) :- suspect4(Nume:Marca:Arma),
                             findall(X, suspect4(X:Y:Arma), List),
                             length(List, Len),
                             Len == 1.

check11 :- test(11, 1, checkSetOf(suspect5,
             [aurel:ford:sabie,bogdan:bmw:pistol,cosmin:bmw:arbaleta])).

% ----------------------------------------------------------------------------
% A șasea replică:
%
% Detectivul A: Și eu am aflat!
%
% 12. (1p) Scrie un predicat suspect6/1 care este adevărat doar pentru
% numele suspecților care respectă condiția impusă de replica
% Detectivului A.
%
% Atenție: informația ce trebuie filtrată acum este cea care
% corespunde primelor cinci replici.

suspect6(Nume:Marca:Arma) :- suspect5(Nume:Marca:Arma),
                             findall(X, suspect5(X:Marca:Y), [_]).


check12 :- test(12, 1, checkSetOf(suspect6, [aurel:ford:sabie])).



%% -------------------------------------------------------------
%% -------------------------------------------------------------

:- dynamic punct/2.

test(Ex, Pts, T) :-
    (   call(T), !, assert(punct(Ex, Pts)),
        format('[OK] +~2f Exercitiul ~w a fost rezolvat corect.~n', [Pts, Ex])
    ;
        format('[--] Esec la exercitiul ~w: ~n    ~w~n', [Ex, T]),
        !, fail).

tests(Ex, Pts, [], _) :- !,
    assert(punct(Ex, Pts)),
    format('[OK] +~2f Exercitiul ~w a fost rezolvat corect.~n', [Pts, Ex]).
tests(Ex, Pts, [T | R], Idx) :-
        Idx1 is Idx + 1,
        (   call(T), !, write('.'), tests(Ex, Pts, R, Idx1);
        format('[--] Esec la exercitiul ~w testul ~w: ~n    ~w~n',
               [Ex, Idx1, T]),
            !, fail).
tests(Ex, Pts, L) :- tests(Ex, Pts, L, 0).

checks([check1, check2, check3, check4,
        check5, check6, check7, check8,
        check9, check10, check11, check12]).

check:- retractall(punct(_, _)),
        checks(Checks),
        forall(member(C, Checks), C),
        fail.
check:-
        findall(P, punct(_, P), L),
        sum_list(L, S),
        format('Punctaj total: ~f~n',[S]).
