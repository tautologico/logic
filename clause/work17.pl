/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 17: Full Maps with State
 *
 * Andrei de A. Formiga, 2008-11-18
 *
 */

ms([], _, []).
ms([H|T], X, [C|L]) :- C is H + X, ms(T, C, L).

mapsum(A, S) :- ms(A, 0, S).


/* indexing elements of a list */
en([], _, []).
en([H|T], X, [n(H, X)|L]) :- Y is X + 1, en(T, Y, L).

enum(A, B) :- en(A, 1, B).
