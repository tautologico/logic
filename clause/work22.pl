/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 22: Ordered Search Trees
 *
 * Andrei de A. Formiga, 2000-02-09
 *
 */


insert(N, [], n(N, [], [])).
insert(N, n(X, L, R), n(X, L1, R)) :- N < X, insert(N, L, L1).
insert(N, n(X, L, R), n(X, L, R1)) :- N > X, insert(N, R, R1).
insert(N, n(N, L, R), n(N, L, R)).


lookup(I, n(I, _, _)).
lookup(I, n(N, L, _)) :- I < N, !, lookup(I, L).
lookup(I, n(N, _, R)) :- I > N, lookup(I, R).
