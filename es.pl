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
count(nil, E, N, N).
count(List, E, N) :- count(List, E, zero, N).
count(cons(E, L), E, N, M) :- count(L, E, s(N), M).
count(cons(E, L), E2, N, M) :- E \= E2, count(L, E2, N, M).

% isNum
isNum(zero).
isNum(s(A)) :- isNum(A).

% es: 2.4
% ge: A >= B
% test: ge(s(s(s(zero))), X).
% test: ge(X, s(s(s(zero)))).
ge(A, zero).
ge(s(A), s(B)) :- isNum(A), ge(A, B).
% le: A <= B
% test: le(s(s(s(zero))), X).
% test: le(X, s(s(s(zero)))).
le(A, B) :- ge(B, A).
% g: A > B
g(A, zero).
g(s(A), s(B)) :- isNum(A), A\=zero, g(A, B).
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
% filter
filter(L, FL) :- filter(nil, nil, FL).
filter(nil, FL, FL).
filter(L).
% count
% find
% dropRight
% dropWhile
% partition
% REVERSED
% test: reversed(cons(1, cons(2, cons(3, nil))), X).
% test: reversed(X, cons(1, cons(2, cons(3, nil)))).
reversed(L, RL) :- reversed(L, nil, RL).
reversed(nil, RL, RL).
reversed(cons(H, T), M, RL) :- reversed(T, cons(H, M), RL).
% drop
% take
% zip
