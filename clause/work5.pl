/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 5: Member
 *
 * Andrei de A. Formiga, 2008-11-10
 *
 */

member(X, [X|_]).
member(X, [_|T]) :- member(X, T).

mystery(X, A, B) :- member(X, A), member(X, B).
