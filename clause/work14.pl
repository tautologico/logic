/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 14: Partial Maps with a Parameter
 *
 * Andrei de A. Formiga, 2008-11-18
 *
 */

reduce([X|T], X, T).
reduce([H|T], X, [H|L]) :- reduce(T, X, L).

