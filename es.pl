% es: 1.1
% test: search(a, cons(a,cons(b,cons(c,nil)))). YES
% test: search(a, cons(c,cons(d,cons(e,nil)))). NO
search(X, cons(X, _)).
search(X, cons(_, Xs)) :- search(X, Xs).

% es: 1.2
search2(X, cons(X, cons(X, _))).
search2(X, cons(_, Xs)) :- search2(X, Xs).

% es: 1.3
search_two(X, cons(X, cons(_, cons(X, _)))).
search_two(X, cons(_, Xs)) :- search_two(X, Xs).

% es: 1.4
search_anytwo(X, cons(X, T)) :- search(X, T).
search_anytwo(X, cons(_, T)) :- search_anytwo(X, T).

% es: 2.1
size(nil, zero).
size(cons(H, T), s(X)) :- size(T, X).

% es 2.2
sum_list(nil, zero).
sum_list(cons(zero, T), X) :- sum_list(T, X).
sum_list(cons(s(H), T), s(X)) :- sum_list(cons(H, T), X).

% es: 2.3
count(List, E, N) :- count(List, E, zero, N).
count(nil, E, N, N).
count(cons(E, L), E, N, M) :- count(L, E, s(N), M).
count(cons(E, L), E2, N, M) :- E \= E2, count(L, E2, N, M).

% isNum
isNum(zero).
isNum(s(A)) :- isNum(A).

% es: 2.4
% ge: A >= B
% test: ge(s(s(s(zero))), X).
% test: ge(X, s(s(s(zero)))).
ge(A, B) :- geT(s(A), s(B)).
geT(A, zero).
geT(s(A), s(B)) :- isNum(A), geT(A, B).
% le: A <= B
% test: le(s(s(s(zero))), X).
% test: le(X, s(s(s(zero)))).
le(A, B) :- ge(B, A).
% g: A > B
g(A, B) :- gT(s(A), s(B)).
gT(A, zero).
gT(s(A), s(B)) :- isNum(A), A\=zero, gT(A, B).
% l: A < B
l(A, B) :- g(B, A).
% e: A = B
e(zero, zero).
e(s(A), s(B)) :- isNum(A), isNum(B), e(A, B).

% test:	max(cons(zero, cons(s(zero), nil)), X).
% test:	max(cons(s(zero), cons(zero, nil)), X).
% test: maxTest(cons(zero, cons(s(s(zero)), nil)), X).
maxTest(nil, M).
maxTest(cons(H, T), M) :- ge(M, H), search(M, cons(H, T)), maxTest(T, M).

max(List, Max) :- max(List, zero, Max).
max(nil, Max, Max).
max(cons(s(Temp), T), Temp, Max) :- max(T, s(Temp), Max).
max(cons(_, T), Temp, Max) :- max(T, Temp, Max).

% es: 2.5
% test:	min(cons(zero, cons(s(zero), nil)), X).	
% test:	min(cons(s(zero), cons(zero, nil)), X).
min(List, Min) :- min(List, zero, Min).
min(nil, Min, Min).
min(cons(s(Temp), T), Temp, Min) :- min(T, Temp, Min).
min(cons(_, T), Temp, Min) :- min(T, Temp, Min).

maxMin(List, Min, Max) :- max(List, Max), min(List, Min).

% es: 3.1
% test: same(cons(a, cons(b, nil)), cons(a, cons(b, nil))).	      YES
% test: same(cons(a, cons(b, cons(c, nil))), cons(a, cons(b, nil))).  NO
%same(nil, nil).
same(cons(H, T), cons(H, T)).

% es: 3.2
% test: all_bigger(cons(s(s(zero)), cons(s(zero), nil)),cons(s(zero),cons(zero, nil))).	YES
% test: all_bigger(cons(s(s(zero)), cons(zero, nil)),cons(s(zero),cons(zero, nil))).	NO
all_bigger(nil, nil).
all_bigger(cons(s(H), T), cons(H, T1)) :- all_bigger(T, T1).

% es: 3.3
% test: sublist(cons(a,cons(b,nil)), cons(c,cons(b,cons(a,nil)))). YES
% test: sublist(cons(e,cons(b,nil)), cons(c,cons(b,cons(a,nil)))). NO
sublist(nil, L).
sublist(cons(H, T), L) :- search(H, L), sublist(T, L).

% es: 4.1
% test: seq(s(s(s(zero))), a, cons(a, cons(a, cons(a, nil)))).	YES
% test: seq(s(s(zero)), a, cons(a, cons(a, cons(a, nil)))).	NO
seq(zero, _, nil).
seq(s(N), E, cons(E, T)) :- seq(N, E, T).

% es: 4.2
%test: seqR(s(s(s(zero))), cons(s(s(zero)),cons(s(zero),cons(zero,nil)))).  YES	
%test: seqR(s(s(zero)), cons(s(s(zero)),cons(s(zero),cons(zero,nil)))).	    NO
seqR(zero, nil).
seqR(s(N), cons(N, T)) :- seqR(N, T).

% es: 4.3
% test: last(cons(a,cons(b,nil)),c,cons(a,cons(b,cons(c,nil)))). YES
% test: last(cons(a,cons(b,nil)),c,cons(a,cons(b,cons(e,nil)))). NO
last(nil, E, cons(E, nil)).
last(cons(_, T), E, cons(_, T1)) :- last(T, E, T1).
% test: seqR2(s(s(s(zero))), cons(zero, cons(s(zero), cons(s(s(zero)), nil)))).	YES
% test: seqR2(s(s(zero)), cons(zero, cons(s(zero), cons(s(s(zero)), nil)))).	NO
seqR2(Max, List) :- seqR2(Max, zero, List).
seqR2(Max, Max, nil).
seqR2(Max, Init, cons(Init, T)) :- seqR2(Max, s(Init), T).

% es: 5
% LAST: element in list
% test: last(cons(s(zero), cons(s(s(zero)), nil)), X).
% test: last(nil, X).
last(cons(H, nil), H).
last(cons(H, T), E) :- last(T, E).
% MAP: map(_+1)
% test: map(cons(zero, cons(zero, nil)), X).
% test: map(cons(zero, cons(s(zero), nil)), X).
% test: map(cons(s(s(zero)), cons(s(zero), nil)), X).
% test: map(X, cons(s(s(zero)), cons(s(zero), nil))).
inc(A, s(A)).
dec(s(A), A).
map(nil, nil).
map(cons(H, T), cons(H1, T1)) :- inc(H, H1), map(T, T1).
% FILTER: filter(_>0)
% test: filter(cons(s(s(s(zero))), cons(s(zero), cons(zero, nil))), X).
% test: filter(cons(s(zero), cons(zero, nil)), X).
filter(L, FL) :- filter(L, nil, FL).
filter(nil, FL, FL).
filter(cons(H, T), Temp, FL) :- g(H, zero), filter(T, cons(H, Temp), FL).
filter(cons(H, T), Temp, FL) :- filter(T, Temp, FL).
% COUNT: count(_>0)
% test: count(cons(zero, cons(s(zero), cons(s(s(zero)), nil))), X).
count(L, C) :- filter(L, FL), size(FL, C).
%count(L, C) :- countA(L, zero, C).
countA(nil, C, C).
countA(cons(H, T), Temp, C) :- g(H, zero), countA(T, s(Temp), C).
countA(cons(H, T), Temp, C) :- countA(T, Temp, C).
% FIND
% test: find(cons(1, cons(2, cons(3, nil))), X).
%find(L, E) :- search(E, L).
find(cons(H, T), H).
find(cons(H, T), E) :- find(T, E).
% DROP_RIGHT

% DROP_WHILE: dropWhile(_>0)
% test: dropWhile(cons(s(zero), cons(s(s(zero)), cons(zero, nil))), X).
% test: dropWhile(cons(s(zero), cons(zero, cons(s(zero), nil))), X).
% test: dropWhile(cons(zero, cons(zero, cons(s(zero), nil))), X).
dropWhile(L, DL) :- dropWhile(L, nil, nil, DL).
dropWhile(Remain, Remain, DL, DL).
dropWhile(cons(H, T), TempR, TempL, DL) :- g(H, zero), dropWhile(T, TempR, cons(H, TempL), DL).
dropWhile(cons(H, T), TemoR, TempL, DL) :- dropWhile(T, T, TempL, DL).
% PARTITION: parition(_>0)
%test: partition(cons(s(zero), cons(s(s(zero)), cons(zero, cons(zero, nil)))), X, Y).
%test: partition(X, cons(s(zero), cons(s(s(zero)), nil)), cons(zero, cons(zero, nil))). non va
partition(L, PL1, PL2) :- partition(L, nil, PL1, nil, PL2).
partition(nil, PL1, PL1, PL2, PL2).
partition(cons(H, T), T1, PL1, T2, PL2) :- g(H, zero), partition(T, cons(H, T1), PL1, T2, PL2).
partition(cons(H, T), T1, PL1, T2, PL2) :- partition(T, T1, PL1, cons(H, T2), PL2).
% REVERSED
% test: reversed(cons(1, cons(2, cons(3, nil))), X).
% test: reversed(X, cons(1, cons(2, cons(3, nil)))).
reversed(L, RL) :- reversed(L, nil, RL).
reversed(nil, RL, RL).
reversed(cons(H, T), M, RL) :- reversed(T, cons(H, M), RL).
% DROP: drop(n) 
% test: drop(cons(1, cons(2, cons(3, cons(4, nil)))), s(s(zero)), X).
% test: drop(cons(1, cons(2, cons(3, cons(4, nil)))), zero, X).
% test: drop(cons(1, cons(2, cons(3, cons(4, nil)))), X, cons(4, nil)).
% test: drop(X, s(zero), cons(4, nil)).
% test: drop(X, Y, Z).
drop(L, N, DL) :- drop(L, N, nil, DL).
drop(nil, N, DL, DL).
drop(cons(H, T), N, TempL, DL) :- size(T, N), drop(T, N, T, DL).
drop(cons(H, T), N, TempL, DL) :- drop(T, N, TempL, DL).
% TAKE: take(n)
% test: take(cons(1, cons(2, cons(3, nil))), s(s(zero)), X).
% test: take(cons(1, cons(2, cons(3, nil))), X, cons(2, cons(1, nil))).
% ricodarti di fare reverse
take(L, N, TL) :- take(L, zero, N, nil, TL).
take(L1, N, N, TL, TL).
take(cons(H, T), TempN, N, TempL, TL) :- take(T, s(TempN), N, cons(H, TempL), TL).
% ZIP
% cons(1, cons(2, cons(3, nil)))
% test: zip(cons(1, cons(2, cons(3, nil))), cons(a, cons(b, cons(c, nil))), X).
% ricodarti di fare reverse
zip(L1, L2, ZL) :- size(L1, N), size(L2, N), zip(L1, L2, nil, ZL).
zip(nil, nil, ZL, ZL).
zip(cons(H, T), cons(H1, T1), Temp, ZL) :- zip(T, T1, cons((H, H1), Temp), ZL).






