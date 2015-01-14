/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 19: Scattered Maps with State
 *
 * Andrei de A. Formiga, 2008-11-18
 *
 */

coll([], []).
coll([q(N, X)|R], [q(T, X)|R2]) :- collz(X, N, R, Q, T), coll(Q, R2).

collz(_, N, [], [], N).
collz(X, N, [q(Num, X)|R], Q, T) :- M is N + Num, collz(X, M, R, Q, T).
collz(X, N, [Q|R], [Q|Qs], T) :- collz(X, N, R, Qs, T).

