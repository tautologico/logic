/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 6: Length of a list
 *
 * Andrei de A. Formiga, 2008-11-10
 *
 */

length([], 0).
length([_|T], N) :- length(T, N1), N is N1 + 1.

accumulate([], A, A).
accumulate([_|T], A, N) :- A1 is A + 1, accumulate(T, A1, N).

length2(L, N) :- accumulate(L, 0, N).


/* practice */
sum([], 0).
sum([X|T], S) :- sum(T, St), S is St + X.

