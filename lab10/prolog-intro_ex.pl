%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% Atenție la semnăturile predicatelor și la specificarea parametrilor
%% p(+Arg1, -Arg2, ?Arg3)
%%  * Arg1 va fi instanțiat atunci când se va încerca satisfacerea p/3
%%  * Arg2 se va instanția odată cu satisfacerea p/3
%%  * Arg3 va putea fi instanțiat sau nu atunci când se va satisface p/3:
%%
%% Interogați Prolog cu "checkXX." pentru a verifica rezolvarea exercițiului XX.
%% Pentru a vedea progresul general, trimiteți scopul "check.".
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------


%% -----------------------------------------------------------------------------
%% 1. (1p)
%% myConcat/3 myConcat(?List1, ?List2, ?List) 'List' este lista
%% formată prin concatenarea listelor 'List1' și 'List2'.

myConcat([H | T], L2, [H | L]) :- myConcat(T, L2, L).
myConcat([], L, L).

check1:-
    tests(1, [
        (myConcat([], [], La), La == []),
        (myConcat(L1b, [1,2], [1,2]), L1b == []),
        (myConcat([1,2,3], L2c, [1,2,3,4,5]), L2c == [4, 5]),
        (myConcat([a, b, c], [d], [a, b, c, d])),
        (myConcat([X1, X2], [X1, X2], [1, 2, 1, 2])),
        (\+ myConcat([X1, X2], [X1, X2], [1, 2, 3, 4])),
        (findall(L3, myConcat([_,_,_], [_], L3), [_])),
        (findall(L4, myConcat([_,_,_], L4, [_,_,_,_]), [_])),
        (myConcat([X51],[X52],[X53,X54]), X51 == X53, var(X51), X52 == X54, var(X54))]).

%% -----------------------------------------------------------------------------
%% 2. (1p)
%% myReverse/2
%% myReverse(?List, +RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă.
%% Regulile pot conține și predicatul myConcat/3.

myReverse([], []).
myReverse([H | T], Revlist):- myReverse(T, Revtail),
                              myConcat(Revtail, [H], Revlist).


check2:-
    tests(2, [
        (myReverse([], []), !),
        (myReverse([1,2,3], [3,2,1])),
        (myReverse([1,2,3], Rev3), Rev3 == [3,2,1]),
        (myReverse(List4, [3,2,1]), List4 == [1,2,3]),
        (myReverse([1,X2,X3], [3,2,X1]), X1 == 1, X2 == 2, X3 == 3),
        (myReverse([Y1,Y2], L5), L5 == [Y2, Y1], var(Y1), var(Y2)),
        (myReverse(L6, [Z6]), L6 == [Z6], var(Z6)),
        (findall(L7, myReverse([_,_], L7), [_]))]).

%% -----------------------------------------------------------------------------
%% 3. (1p)
%% myReverseAcc/3
%% myReverseAcc(?List, ?Acc, ?RevList)
%% 'RevList' este o listă ce conține elementele listei 'List' în ordine inversă
%% și elementele listei 'Acc'.
%% (Indicație: 'Acc' se va comporta precum un acumulator)
%% Regulile nu trebuie sa conține alte predicate (în afară de "cut" și ",").

myReverseAcc([],[],[]).
myReverseAcc([], RevList, RevList).
myReverseAcc([H | T], Acc, RevList) :- myReverseAcc(T, [H | Acc], RevList).

check3:-
    tests(3, [
        (myReverseAcc([], [], [])),
        (myReverseAcc([1,2,3], [0], [3,2,1,0])),
        (myReverseAcc([1,2,3], [0], Rev3), Rev3 == [3,2,1,0]),
        (myReverseAcc(List4, [0], [3,2,1,0]), List4 == [1,2,3]),
        (myReverseAcc([X2,1], [3], [X1,2,3]), X1 == 1, X2 == 2)]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 4. (0.5p)
%% palindrom/1
%% palindrom(?List)
%% 'List' este un palindrom.

palindrom([]).
palindrom([_]).
palindrom(List) :- myConcat([H | T], [H], List), palindrom(T).
check4 :-
    tests(4, [
        (palindrom([1,2,3,2,1])),
        (palindrom([1,2,3,3,2,1])),
        (\+ palindrom([1,2,3,0,2,1])),
        (palindrom([1,2,3,X3,X2,X1]), X3 == 3, X2 == 2, X1 == 1),
        (\+ palindrom([1,2,3,X,_,X]))]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 5. (1.5p)
%% myFlatten/2
%% myFlatten(+List1,-List2)
%% 'List1' este o listă cu elemente și posibil alte liste în interior. Se cere ca
%% orice altă listă din interior sa fie înlocuita cu elementele sale.
%% Hint: folosire predicat: is_list(+List).

my_flatten([],[]).
my_flatten(X, [X]) :- \+ is_list(X).
my_flatten([Head|Tail], R) :- my_flatten(Head, L1), my_flatten(Tail, L2), myConcat(L1, L2, R).

check5 :-
	tests(5, [
		(my_flatten([1,2,3], [1,2,3])),
		(my_flatten([[1],2],[1,2])),
		(my_flatten([[[1],2],3],[1,2,3]))]).
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 6. (1.5p)
%% removeDuplicates/2
%% removeDuplicates(+List1,-List2)
%% 'List1' este o listă cu elemente duplicate. Se cere sa se obtina un rezultat
%% fara duplicate consecutive 'List2'.
%% ex: [1,2,2,3,2,2] => [1,2,3,2].
remove_duplicates([],[]).
remove_duplicates([Head|Tail], X) :- remove_duplicates(Tail, L), myConcat([Head], L, X).
remove_duplicates([X1, X2|Tail], X) :- X1 == X2, remove_duplicates([X1 | Tail], X).
remove_duplicates([X1, X2|Tail], X) :- X1 \= X2, remove_duplicates(Tail, L), myConcat([X1], L, X).

check6 :-
	tests(6, [
		(remove_duplicates([1,2,2,3],[1,2,3])),
		(remove_duplicates([1,2,2,3,1,3],[1,2,3,1,3])),
		(remove_duplicates([3,3,2,1,1],[3,2,1]))]).
%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 7. (1.5p)
%% sublista/2
%% sublista(+List, ?SubList)
%% 'SubList' este o sublistă a lui 'List' ('SubList' poate fi obținută prin
%% eliminarea a zero sau mai multe elemente din 'List'

sublista(_,[]).
sublista([H | T1], [H | T2]) :- sublista(T1, T2).
sublista([_ | T1], [H | T2]) :- sublista(T1, [H | T2]).
check7 :-
    tests(7, [
        (sublista([1,2,3,4,5],[2,3,4])),
        (sublista([1,2,3,4,5],[1,2,3,4,5])),
        (\+ sublista([1,2,3,4,5],[2,1,4])),
        (findall(S4, sublista([1,2,3], S4), All4)),
        (sort(All4, [[],[1],[1,2],[1,2,3],[1,3],[2],[2,3],[3]])),
        (sublista([X1,X2,X3],[Y1,Y2,Y3]), X1==Y1, X2==Y2, X3==Y3)]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% Se dau următoarele fapte ce descriu arcele unei păduri de arbori binari.
%% Fiecare nod poate avea maxim doi fii.

nod(a). nod(b). nod(c). nod(d). nod(e). nod(f). nod(g).
nod(h). nod(i). nod(j). nod(k). nod(l).
nod(m).
nod(n). nod(p). nod(o).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% arc/2
%%
%%    a           h         n    m
%%   / \         / \        |
%%  b   c       i   j       o
%%  |  / \      |   |       |
%%  d g   e     k   l       p
%%        |
%%        f

arc(a,b). arc(a,c). arc(b,d). arc(c,e). arc(c,g). arc(e,f).
arc(h,i). arc(h,j). arc(i,k). arc(j,l).
arc(n,o). arc(o,p).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 8. (1p)
%% isLeaf/1
%% isLeaf(?Nod)

isLeaf(X):- nod(X), \+ arc(X, _).

check8 :-
    tests(8, [
        (isLeaf(d), isLeaf(f), isLeaf(g), isLeaf(k), isLeaf(l)),
        (isLeaf(m), isLeaf(p)),
        (\+ isLeaf(a), \+ isLeaf(b), \+ isLeaf(c), \+ isLeaf(e)),
        (findall(X, isLeaf(X), L), length(L, 7))]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 9. (1p)
%% isRoot/1

isRoot(X):- nod(X), \+ arc(_, X).

check9 :-
    tests(9, [
        (isRoot(a), isRoot(h), isRoot(m), isRoot(n)),
        (\+ isRoot(b), \+ isRoot(d), \+ isRoot(e), \+ isRoot(l)),
        (findall(X, isRoot(X), L), length(L, 4))]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 10. (bonus: 1p)
%% descendantOf/2
%% descendantOf(?X,?Y)
%% Nodul X este un urmaș a lui Y.

descendantOf(X,Y):- nod(X), nod(Y), arc(Y,X).
descendantOf(X, Y) :- nod(X), nod(Y), arc(Y, C), descendantOf(X, C).

check10:-
    tests(10, [
        (descendantOf(b,a), descendantOf(c,a), descendantOf(d,b)),
        (descendantOf(j,h), descendantOf(l,j)),
        (descendantOf(j,h), descendantOf(f,a)),
        (\+ descendantOf(a,_), \+ descendantOf(h,_), \+ descendantOf(m,_)),
        (findall(X1, descendantOf(X1,a), L1), length(L1, 6)),
        (findall(X2, descendantOf(X2,h), L2), length(L2, 4)),
        (findall(X3, descendantOf(l,X3), L3), length(L3, 2)),
        (findall(X4, descendantOf(f,X4), L4), length(L4, 3))]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 11. (bonus: 1p)
%% descendants/2
%% descendants(?Nod, ?N)
%% Nodul Nod are N urmași.

descendants(X, 0) :- isLeaf(X).
descendants(Nod, N) :- nod(Nod), findall(X, descendantOf(X, Nod), L), length(L, N).



check11:-
    tests(11, [
        (descendants(a, 6), descendants(b, 1)),
        (descendants(a, N1), N1 == 6),
        (descendants(c, 3), descendants(h, 4)),
        (descendants(g, 0), descendants(l, 0)),
        (findall(X1, descendants(X1, 1), L), sort(L, [b,e,i,j,o])),
        (findall(X2, descendants(X2, 2), L2), sort(L2, [n]))]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 12. (bonus: 1p)
%% height/2
%% height(?Nod, ?H).
%% O frunză are înălțime 0.


height(Nod, 0):- isLeaf(Nod).
height(Nod, N):- arc(Nod, Y), height(Y, N1), \+ (arc(Nod, Z), height(Z, N2), N2 > N1),N is N1 + 1.

check12:-
    tests(12, [
        (height(a,N1), N1 == 3),
        (height(a,3), height(b,1), height(m,0)),
        (height(X3,3), X3 == a),
        (findall(X4, height(X4, 1), L4), sort(L4, [b,e,i,j,o])),
        (findall(X5, height(X5, 2), L5), sort(L5, [c,h,n]))]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% Se dă graful orientat și faptele ce desciru arcele acestuia:
%%         a
%%        /\
%%       /  \
%%     b/____\c
%%      \    /
%%       \  /
%%        \/
%%        d
arcG(a,b).
arcG(a,c).
arcG(b,c).
arcG(b,d).
arcG(c,d).
arcG(d,e).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 13. (bonus: 1p)
%% path/3
%% path(+Nod,+Nod,-Path).
%% Se cere să se obțină calea de la nodul X la nodul Y.
%% Observație: Există cicluri în graf.

path(N1, N2, [N1, N2]):- nod(N1),nod(N2), arcG(N1, N2).
path(N1, N2, [N1 | R]):- arcG(N1, X), path(X, N2, R).

check13:-
    tests(13, [
    	(path(a,d,[a,b,d])),
    	(path(c,e,[c,d,e])),
    	(findall(X3, path(a,e,X3), R), sort(R, RS), sort([[a,b,d,e],[a,c,d,e],[a,b,c,d,e]], SolS), R == SolS)]).

%% -----------------------------------------------------------------------------
%% -----------------------------------------------------------------------------
%% 14. (bonus: 1p)
%% packItems/2
%% packItems(+List1,-List2)
%% 'List1' este o listă cu elemente duplicate. Se cere să se obțină un rezultat
%% în care elementele sunt grupate în liste cu aceeași valoare, asemănător
%% funcției group.
%% ex: [1,2,2,3,2,2] => [[1],[2,2],[3],[2,2]].


packItems(_,_):- fail.

check14 :-
    tests(14, [
		(packItems([1,2,2,3],[[1],[2,2],[3]])),
		(packItems([1,2,2,3,1,3],[[1],[2,2],[3],[1],[3]])),
		(packItems([3,3,3,3,2,1,1],[[3,3,3,3],[2],[1,1]]))]).

%% ----------------------------------------
%% ----------------------------------------

:-dynamic(punct/2).

tests(Ex, [], _) :- !, format('Exercitiul ~w a fost rezolvat corect.~n', [Ex]).
tests(Ex, [T | R], Idx) :-
        Idx1 is Idx + 1,
        (   call(T), !, tests(Ex, R, Idx1);
        format('Esec la exercitiul ~w testul ~w: ~n    ~w~n',
               [Ex, Idx1, T]),
            !, fail).
tests(Ex, L) :- tests(Ex, L, 0).

check:-
        retractall(punct(_, _)),
        once((check1, assert(punct(1, 1)) ; assert(punct(1, 0)))),
        once((check2, assert(punct(2, 1)) ; assert(punct(2, 0)))),
        once((check3, assert(punct(3, 1)) ; assert(punct(3, 0)))),
        once((check4, assert(punct(4, 0.5)) ; assert(punct(4, 0)))),
        once((check5, assert(punct(5, 1.5)) ; assert(punct(5, 0)))),
        once((check6, assert(punct(6, 1.5)) ; assert(punct(6, 0)))),
        once((check7, assert(punct(7, 1.5)) ; assert(punct(7, 0)))),
        once((check8, assert(punct(8, 1)) ; assert(punct(8, 0)))),
        once((check9, assert(punct(9, 1)) ; assert(punct(9, 0)))),
        once((check10, assert(punct(10, 1)) ; assert(punct(10, 0)))),
        once((check11, assert(punct(11, 1)) ; assert(punct(11, 0)))),
        once((check12, assert(punct(12, 1)) ; assert(punct(12, 0)))),
        once((check13, assert(punct(13, 1)) ; assert(punct(13, 0)))),
        once((check14, assert(punct(14, 1)) ; assert(punct(14, 0)))),
        fail.
check:-
        findall(P, punct(_, P), L),
        sum_list(L, S),
        format('Punctaj total: ~f~n',[S]).
