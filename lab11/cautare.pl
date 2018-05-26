%% -------------------------------------------------------------
%% -------------------------------------------------------------

%% -- BACKTRACKING ATUNCI CÂND CUNOAȘTEM LUNGIMEA SOLUȚIEI --

%%% 1. (2p)
%% Înțelegeți predicatele solve_maps/1, template/1 și correct/1.
%% Observați ca lipsește definiția predicatului safe/2.

neighbours(1, [2, 3]). neighbours(2, [1, 3, 4]). neighbours(3, [1, 2, 4, 5, 6]).
neighbours(4, [2, 3, 5]). neighbours(5, [3, 4, 6]). neighbours(6, [3, 5]). neighbours(7, []).

%% template/1
%% template(?List)
%% List are forma unei soluții pentru problema colorarii harților pentru 7 țari.
%% Lungimea soluției este cunoscuta și fixa.
template([1/_, 2/_, 3/_, 4/_, 5/_, 6/_, 7/_]).

%% correct/1
%% correct(?Solution)
%% Solution reprezinta o soluție valida pentru problema colorarii harților.
correct([]):-!.
correct([X/Y|Others]):-
        correct(Others),
        member(Y, ["r", "g", "b"]),
        safe(X/Y, Others).

%% solve_maps/1
%% solve_maps(-Solution)
%% Solution este o soluție a problemei colorarii harților.
solve_maps(S):-template(S), correct(S).

%% Scrieți predicatul safe/2 utilizat în rezolvarea problemei colorarii harților.
%% Predicatul va avea antetul safe(+X/Y, +Others) cu semnificația ca se verifica
%% daca alegerea culorii Y pentru țara X este în concordanța cu culorile alese în
%% lista Others (nicio țara nu are aceeași culoare cu niciun vecin). Lista Others
%% are forma [X1/Y1, X2/ Y2/ ...].

%% safe/2
%% safe(+X/Y, +Others)

safe(X/Y, []).
safe(X/Y, [X1/Y1 | Others]):- (neighbours(X, L),
                              member(X1, L), Y=Y1 -> fail; safe(X/Y, Others)).


check1:-
        \+ safe(5/"r", [6/"r"]), write('.'),
        \+ safe(4/"g", [5/"g"]), write('.'),
        safe(2/"b", [5/"b"]), write('.'),
        \+ safe(1/"b", [2/"b", 3/"r"]), write('.'),
        safe(7/"b", [3/"b"]), write('.'),
        safe(4/"g", [2/"b", 3/"r", 5/"b", 6/"g"]), write('.'),
        \+ safe(3/"r", [1/"b", 2/"b", 4/"g", 5/"r", 6/"b"]), write('.'),
        safe(3/"r", [1/"b", 2/"g", 3/"r", 4/"b", 5/"g", 6/"g", 7/"r"]), write('.'),
        findall(Sol, solve_maps(Sol), All), length(All, 18), write('.'),
        writeln('Exercițiul 1 rezolvat corect!'),
        !.

%% Întrebați-l pe Prolog "solve_maps(Sol)" pentru a vizualiza soluțiile.

%% -------------------------------------------------------------
%% -------------------------------------------------------------

%% -- BACKTRACKING ATUNCI CÂND NU CUNOAȘTEM LUNGIMEA SOLUȚIEI --

%%% 2. (6p)
%% Înțelegeți cum funcționeza predicatele solve și search pentru rezolvarea
%% unei probleme de cautare în spațiul starilor. Observați utilizarea
%% predicatelor initial_state/1, final_state/1 și next_state/2.

search([CurrentState|Other], Solution):-
        final_state(CurrentState),
        !,
        reverse([CurrentState|Other], Solution).

search([CurrentState|Other], Solution):-
        next_state(CurrentState, NextState),
        \+ member(NextState, Other),
        search([NextState,CurrentState|Other], Solution).

solve(Solution):-
        initial_state(State),
        search([State], Solution).

%% Exemplu: problema țaranului, a lupului, a caprei și a verzei.
%% Vom reprezenta o stare astfel:
%% state(MalBarca, MalȚaran, MalLup, MalCapra, MalVarza)

opus(est, vest).
opus(vest, est).

initial_state(state(est, [est, est, est], [est, est, est])).

final_state(state(_, [vest, vest, vest], [vest, vest, vest])).

% 1 calatoreste singur
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
        state(RBoat, [R1, I2, I3], [Ia, Ib, Ic])):-
        opus(I1, R1),
        I1 = IBoat,
        opus(IBoat, RBoat),
        safe_state(state([R1, I2, I3], [Ia, Ib, Ic])).
% 1 calatoreste cu a
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
        state(RBoat, [R1, I2, I3], [Ra, Ib, Ic])):-
        opus(I1, R1),
        opus(Ia, Ra),
        I1 = Ia,
        I1 = IBoat,
        opus(IBoat, RBoat),
        safe_state(state([R1, I2, I3], [Ra, Ib, Ic])).
% 1 calatoreste cu b
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
           state(RBoat, [R1, I2, I3], [Ia, Rb, Ic]):-
           opus(I1, R1),
           opus(Ib, Rb),
           I1 = Ib,
           I1 = IBoat,
           opus(IBoat, RBoat),
           safe_state(state([R1, I2, I3], [Ia, Rb, Ic]))).
% 2 calatoreste cu c
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
           state(RBoat, [I1, R2, I3], [Ia, Ib, Rc]):-
           opus(I2, R2),
           opus(Ic, Rc),
           I2 = Ic,
           I2 = IBoat,
           opus(IBoat, RBoat),
           safe_state(state([I1, R2, I3], [Ia, Ib, Rc]))).

% 1 calatoreste cu 2
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
           state(RBoat, [R1, R2, I3], [Ia, Ib, Ic]):-
           opus(I1, R1),
           opus(I2, R2),
           I1 = I2,
           I1 = IBoat,
           opus(IBoat, RBoat),
           safe_state(state([R1, R2, I3], [Ia, Ib, Ic]))).

% 2 calatoreste singur
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
        state(RBoat, [I1, R2, I3], [Ia, Ib, Ic])):-
        opus(I2, R2),
        I2 = IBoat,
        opus(IBoat, RBoat),
        safe_state(state([I1, R2, I3], [Ia, Ib, Ic])).

% 3 calatoreste singur
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
        state(RBoat, [I1, I2, R3], [Ia, Ib, Ic])):-
        opus(I3, R3),
        I3 = IBoat,
        opus(IBoat, RBoat),
        safe_state(state([I1, I2, R3], [Ia, Ib, Ic])).

% 1 calatoreste cu 3
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
           state(RBoat, [R1, I2, R3], [Ia, Ib, Ic]):-
           opus(I1, R1),
           opus(I3, R3),
           I1 = I3,
           I1 = IBoat,
           opus(IBoat, RBoat),
           safe_state(state([R1, I2, R3], [Ia, Rb, Ic]))).

% 2 calatoreste cu 3
next_state(state(IBoat, [I1, I2, I3], [Ia, Ib, Ic]),
           state(RBoat, [I1, R2, R3], [Ia, Ib, Ic]):-
           opus(I2, R2),
           opus(I3, R3),
           I2 = I3,
           I2 = IBoat,
           opus(IBoat, RBoat),
           safe_state(state([I1, R2, R3], [Ia, Ib, Ic]))).






           persons_est([est, S2, S3], [1 | R]):- persons_est([S2, S3], R).
            persons_est([vest, S2, S3], R):- persons_est([S2, S3], R).

            persons_est([est, S3], [2 | R]):- persons_est([S3], R).
            persons_est([vest, S3], R):- persons_est([S3], R).

            persons_est([est], [3]).
            persons_est([vest], []).

            persons_vest(L, R):-
            allPeople(AP), persons_est(L, Pe),
            setMinus(AP, Pe, Temp), sort(Temp, R).


            bags_est([est, S2, S3], [a | R]):- bags_est([S2, S3], R).
            bags_est([vest, S2, S3], R):- bags_est([S2, S3], R).

            bags_est([est, S3], [b | R]):- bags_est([S3], R).
            bags_est([vest, S3], R):- bags_est([S3], R).

            bags_est([est], [c]).
            bags_est([vest], []).

            bags_vest(L, R):-
            allBags(AB), bags_est(L, Be),
            setMinus(AB, Be, Temp), sort(Temp, R).

            safe_state(state([S1, S2, S3], [Sa, Sb, Sc])):-
                    bags_est([Sa, Sb, Sc], BE),
                    persons_est([S1, S2, S3], PE),
                    safe_side(PE, BE),
                    bags_vest([Sa, Sb, Sc], BV),
                    persons_vest([S1, S2, S3], PV),
                    safe_side(PV, BV).

%% Rescrieți predicatele initial_state/1, final_state/1, și next_state/2 pentru
%% a rezolva problema celor trei calatori.

%% Trei calatori
%% Trei calatori se alfa pe același mal al unui râu și vor sa ajunga pe celalalt.
%% Primul calator are doua valize, al doilea are una singura, iar cel de-al treilea
%% nu are nicio valiza. Aceștia pot sa foloseasca o barca, care la o traversare a
%% râului poate duce un calator, doi calatori sau un calator și o valiza. Niciun calator
%% nu poate sa ramâna singur cu valizele altuia. Gasiți cum pot aceștia sa ajunga pe
%% malul opus, cu tot cu valizele lor.

%% Pentru o mai buna structura, implementați întâi predicatele boat/1
%% și safe/1 detaliate mai jos.

%% Predicate utile: sort/2, @</2 (vedeți help)

%% Atenție, nu lasați în spațiul de lucru predicatele pentru ambele
%% probleme!

% own(?Person, ?Bag)
own(1, [a, b]).
own(2, [c]).
own(3, []).

% people(-AllPeople)
% Obține lista cu toate persoanele.
allPeople(AllPeople) :- findall(Person, own(Person, _), AllPeople).

% bags(-AllBags)
% Obține lista cu toate pungile.
allBags(AllBags) :- findall(Bag, own(_, Bag), AllBagsUnflatten), flatten(AllBagsUnflatten, AllBags).

% person(?Person)
person(Person) :- allPeople(AllPeople), member(Person, AllPeople).

% bag(?Bag)
bag(Bag) :- allBags(AllBags), member(Bag, AllBags).

% setMinus(+From, +ToRemove, -Result)
% calculeaza diferența de mulțimi From \ ToRemove
setMinus(From, ToRemove, Result) :-
        findall(E, (member(E, From), \+ member(E, ToRemove)), Result).

% setPlus(+A, +B, -Result)
% concateneaza A și B în Result (Atenție! nu elimina duplicate)
setPlus(A, B, Result) :- append(A, B, Result).

% subSet(+Smaller, +Bigger)
% subSet(+Smaller, +Bigger)
% Verifica daca setul Smaller este inclus în sau egal cu setul Bigger
subSet([], _).
subSet([E|Smaller], Bigger) :- member(E, Bigger), !, subSet(Smaller, Bigger).

% boat/1
% boat(?People)
% o lista de una sau doua persoane, sau o persoana și o valiza care pot merge împreuna în barca.
% persoanele trebuie sa fie luate într-o singura ordine (daca sa luat [1, 2] nu se va lua [2, 1])
boat([]).
boat([X]) :- person(X).
boat([X , Y]) :- person(X), person(Y), X @< Y.
boat([X , Y]) :- person(X), bag(Y), own(X, BagsOfX), member(Y, BagsOfX).
% safe_side/1
% safe_side(+PeopleList, +BagsList)
% verifica daca persoanele și valizele din cele doua liste pot sta pe același mal.
safe_side([], []).
safe_side(PeopleList, []).
safe_side([], BagsList).
safe_side([Person], BagsList) :- \+ own(X, BagsOfX), \+ member(Y, BagsOfX),
                                    member(Y, BagsList).
safe_side(PeopleList, BagsList) :- length(PeopleList, L), L > 1.


check2:-tests(2, [
                  boat([1]),
                  boat([1, b]),
                  boat([1, 2]),
                  boat([2, c]),
                  boat([2, 3]),
                  \+ boat([3, c]),
                  \+ boat([a, b]),
                  \+ boat([2, a]),
                  safe_side([1, 2, 3], [a, b, c]),
                  safe_side([1, 2], [a, c]),
                  safe_side([1, 3], [a, b]),
                  safe_side([2, 3], [c]),
                  \+ safe_side([1], [c]),
                  \+ safe_side([3], [a]),
                  ;(   solve(X), length(X, L), L == 10),
                  ;(   findall(Sol, (solve(Sol), length(Sol, 10)), MinSols),
                      length(MinSols, Len), Len == 4)
                  ]).

%% -------------------------------------------------------------
%% -------------------------------------------------------------

%%% 3 (2p)
%% Implementați un predicat bfs/3 care sa descrie un mecanism de cautare în
%% lațime într-un graf. Se dau predicatele initial_node/1, final_node/1 și
%% edge/2. Observați similaritatea cu initial_state/1, final_state/1 și
%% next_state/2.

do_bfs(Solution):-
        initial_node(StartNode),
        bfs([(StartNode,nil)], [], Discovered),
        extract_path(Discovered, Solution).

%% bfs/3
%% bfs(+Frontier, +Closed, -Solution)
%% Frontier reprezinta coada nodurilor ce vor fi explorate, Closed reprezinta
%% lista nodurilor vizitate deja, iar Solution va reprezenta lista finala a
%% nodurilor vizitate pâna la gasirea soluției.

bfs([(FinalNode,Parent)|_], Closed, [(FinalNode, Parent)|Closed]):-
        final_node(FinalNode), !.
bfs([(CurrentNode,_)|Rest], Closed, Solution):-
        member((CurrentNode,_), Closed),
        !,
        bfs(Rest, Closed, Solution).
bfs([(CurrentNode,Parent)|Rest], Closed, Solution):-
        findall((Node, CurrentNode), edge(CurrentNode, Node), Children),
        append(Rest, Children, NewFrontier),
        bfs(NewFrontier, [(CurrentNode,Parent)|Closed], Solution).

%%% Testare

edge(a,b). edge(a,c). edge(a,d).
edge(c,e). edge(c,f).
edge(d,h).
edge(e,a). edge(e,g).
edge(f,a). edge(f,g).
edge(g,h).

%% extract_path/2
%% extract_path(Discovered, Solution)
%% Solution reprezinta calea de la nodul inițial la cel final extrasa din
%% lista nodurilor vizitate (data suba forma de perechi (Nod, Parinte).

extract_path(Discovered, Solution):-
        final_node(Node0),
        extract_path(Discovered, [Node0], Solution).

extract_path(Discovered, [Node | Other], [Node | Other]):-
        member((Node,nil), Discovered), !.

extract_path(Discovered, [Node | Other], Solution):-
        member((Node,Next), Discovered),
        extract_path(Discovered, [Next, Node | Other], Solution).

initial_node(a).
final_node(h).

check3:-
		bfs([(a,nil)], [], R),
        R == [(h, d), (f, c), (e, c), (d, a), (c, a), (b, a), (a, nil)],
        writeln('.Exercițiul 3 rezolvat corect!'),
        !.

%% -------------------------------------------------------------
%% -------------------------------------------------------------

%%% 4. BONUS. (5p)
%% Implementați algoritmul A*, folosit pentru a afla drumul de lungime minima
%% între doua puncte de pe o harta bidimensionala. Harta poate conține obstacole,
%% acestea fiind celule în care nu ne putem poziționa.

%% heappush/3
%% heappush(+Element, +List, -NewList)
%% NewList este o coada de prioritați rezultata adaugând Element la coada inițiala, List.
%% Elementele sunt de forma Prioritate:A, iar cel cu cea mai mica prioritate este primul.
%% Exemplu:
%% ?- heappush(3:"A", [], X).
%% X = [3:"A"].
%% ?- heappush(4:"B", [3:"A"], X).
%% X = [3:"A", 4:"B"].
%% ?- heappush(2:"B", [3:"A", 4:"B"], X).
%% X = [2:"B", 3:"A", 4:"B"].
heappush(X, [], [X]) :- !.
heappush(X:A, [Y:B|Rest], [X:A,Y:B|Rest]) :-
    X @< Y, !.
heappush(X:A, [Y:B|Rest0], [Y:B|Rest]) :-
    heappush(X:A, Rest0, Rest).

%% dictionary_insert/3
%% dictionary_insert(+Element, List, NewList)
%% NewList este un dicționar obținut prin inserarea unui Element de forma Key:Value într-un
%% dicționar List. Daca cheia Key este deja existenta, valoarea acesteia va fi înlocuita cu Value.
%% Exemplu:
%% ?- dictionary_insert("A":3, [], X).
%% X = ["A":3].
%% ?- dictionary_insert("B":4, ["A":3], X).
%% X = ["B":4, "A":3].
%% ?- dictionary_insert("A":10, ["B":4, "A":3], X).
%% X = ["B":4, "A":10].
dictionary_insert(Key:Value, Dict, [Key:Value|Dict]) :- \+ member(Key:_, Dict), !.
dictionary_insert(Key:Value, Dict, R) :-
  findall(K:V, (member(K:V, Dict), \+ K == Key), Rest),
  append(Rest, [Key:Value], R).

%% get_grid/3
%% get_grid(+Pos, +Grid, -Value)
%% Value este valoarea dintr-o celula a harții.
get_grid((X, Y), Grid, R) :- nth0(X, Grid, Row), nth0(Y, Row, R).

%% neighbours2/3
%% neighbours2(+Cell, +Grid, -Neighbours)
%% Neighbours este lista cu vecinii valizei ai unei celule (fara obstacole)
neighbours2(Current, Grid, Neigh) :-
  (X, Y) = Current,
  length(Grid, L1),
  nth0(0, Grid, Row), length(Row, L2),
  XS is X + 1, XN is X - 1, YW is Y - 1, YE is Y + 1,
  N1 = [(XS, Y), (XN, Y)],
  N2 = [(X, YW), (X, YE)],
  findall((A, B), (member((A, B), N1), A >= 0, A < L1), N1f),
  findall((A, B), (member((A, B), N2), B >= 0, B < L2), N2f),
  append(N1f, N2f, N),
  findall((X1, Y1), (member((X1, Y1), N), get_grid((X1, Y1), Grid, 0)), Neigh).

%% manhattan/3
%% manhattan(+P1, +P2, -Distance)
%% Distance este distanța Manhattan între doua puncte.
manhattan((X1, Y1), (X2, Y2), D) :- D is abs(X1-X2)+abs(Y1-Y2).

%% get_path/5
%% get_path(+Start, +End, +Discovered, +Aux, -Path)
%% Path este drumul între Start și End
get_path(Start, Start, _, Path, Path).
get_path(Start, End, Discovered, Aux, Path) :-
  member(End:(Parent, _), Discovered),
  get_path(Start, Parent, Discovered, [Parent|Aux], Path).

%% astar_search/4
%% astar_search(+Start, +End, +Grid, -Path)
%% Path este calea dintre Start și End
astar_search(Start, End, Grid, Path) :-
  manhattan(Start, End, H),
  astar(End, [H:Start], [Start:("None", 0)], Grid, Discovered),
  get_path(Start, End, Discovered, [End], Path).

%% TODO
%% astar/5
%% astar(+End, +Frontier, +Discovered, +Grid, -Result)
%% End: punctul de final de pe harta.
%% Frontier: coada de prioritați, care va conține inițial elementul de start cu
%% valoarea sa. (ex: [6:(5, 5)], 6 = distanța Manhattan de la Start: (5, 5) la End: (8, 8)))
%% Discovered: dicționar în care se vor reține parintele nodului și costul real cu care a fost
%% descoperit. (ex: [(5, 5):("None", 0)])
%% Grid: matrice care reprezinta harta.
%% Result: va fi Discovered în momentul în care primul element din Frontier este End.
%% Algoritm:
%% 1. Cât timp Frontier nu este goala, se ia primul element din aceasta
%% 2. Daca elementul este cel final, cautarea se încheie
%% 3. Se iau toți vecinii elementului și li se calculeaza costul(new_cost), adaugând 1
%% la costul parintelui din Discovered. Daca vecinul nu este în Discovered, sau costul
%% acestuia este mai mic decât cel deja existent, atunci noua valoare va fi adaugata în
%% Discovered, apoi se va fi introdus în Frontier cu prioritatea calculata astfel:
%% new_cost + manhattan(vecin, end)

astar(_, _, _, _, _) :- fail.

%% Testare

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]) :-
    I > 0,
    I1 is I - 1,
    replace(T, I1, X, R).

build(_,0,[]).

build(X, N, List)  :-
    findall(X, between(1, N, _), List).

grid(X, N, M, Grid) :- build(X, M, Row), build(Row, N, Grid).

replace_grid(X, N, M, Grid, R) :- nth0(N, Grid, Row), replace(Row, M, X, NewRow), replace(Grid, N, NewRow, R), !.

replace_more(R1, R1, _, Grid, Grid) :- !.
replace_more(R1, R2, X, Grid, LastGrid) :-
  R1 =< R2, R11 is R1 + 1,
  replace_grid(1, R1, X, Grid, NewGrid), replace_grid(1, X, R1, NewGrid, NewGrid1), replace_more(R11, R2, X, NewGrid1, LastGrid).

check4 :-
      grid(0, 10, 20, Grid),
      replace_more(2, 7, 6, Grid, FinalGrid),
      astar_search((5, 5), (8, 8), FinalGrid, Path),
      length(Path, L), L == 15,
        writeln('.Exercițiul 4 (BONUS) rezolvat corect!'),
        !.


%% -------------------------------------------------------------
%% -------------------------------------------------------------

:- dynamic punct/2.

tests(Ex, [], _) :- !, format('Exercitiul ~w a fost rezolvat corect.~n', [Ex]).
tests(Ex, [T | R], Idx) :-
        Idx1 is Idx + 1,
        (   call(T), !, write('.'), tests(Ex, R, Idx1);
        format('Esec la exercitiul ~w testul ~w: ~n    ~w~n',
               [Ex, Idx1, T]),
            !, fail).
tests(Ex, L) :- tests(Ex, L, 0).

check:-
        retractall(punct(_, _)),
        once((check1, assert(punct(1, 2)) ; assert(punct(1, 0)))),
        once((check2, assert(punct(2, 6)) ; assert(punct(2, 0)))),
        once((check3, assert(punct(3, 2)) ; assert(punct(3, 0)))),
        once((check4, assert(punct(4, 5)) ; assert(punct(4, 0)))),
        fail.

check:-
        findall(P, punct(_, P), L),
        sum_list(L, S),
        format('Punctaj total: ~f~n',[S]).
