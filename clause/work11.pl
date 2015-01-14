/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 11: Multiple Choices
 *
 * Andrei de A. Formiga, 2008-11-18
 *
 */

squint([], []).
squint([X|T], [Y|L]) :- integer(X), Y is X * X, squint(T, L).
squint([X|T], [X|L]) :- squint(T, L).
