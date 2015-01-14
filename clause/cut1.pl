/*
 * Clause and Effect, William F. Clocksin
 *
 * Some examples from Chapter 4: Choice and Commitment
 *
 * Andrei de A. Formiga, 2009-02-09
 *
 */

max(X, Y, X) :- X >= Y, !.
max(X, Y, Y).


drink(milk).
drink(beer) :- !.
drink(gin).

class(N, pos) :- N > 0, !.
class(0, zero) :- !.
class(N, neg) :- N < 0.


/* This example is from Worksheet 20 */
membercheck(X, [X|_]) :- !.
membercheck(X, [_|L]) :- membercheck(X, L).

sd([], _, []).
sd([E|S1], S2, S3) :- membercheck(E, S2), !, sd(S1, S2, S3).
sd([E|S1], S2, [E|S3]) :- sd(S1, S2, S3).

/*
   Note that sd(A, B, C) will only work if C is uninstatiated, or if
   the elements in A and C appear in the same order. So,
   sd([x, y, z, w, o], [y, z], [x, w, o]).

   works, but

   sd([x, y, z, w, o], [y, z], [x, o, w]).

   doesn't.
*/

