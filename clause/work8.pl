/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 8: Maximum of a list
 *
 * Andrei de A. Formiga, 2008-11-10
 *
 */

max([], A, A).
max([H|T], A, M) :- H > A, max(T, H, M).
max([H|T], A, M) :- H =< A, max(T, A, M).



/* practice */


/* 1 */
maximum([H|T], M) :- max(T, H, M).


/* 3 */
min([], A, A).
min([H|T], A, M) :- H < A, min(T, H, M).
min([H|T], A, M) :- H >= A, min(T, A, M).

minimum([H|T], M) :- min(T, H, M).


minmax(L, Min, Max) :- maximum(L, Max), minimum(L, Min).

