/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 16: Multiple Disjoint Partial Maps
 *
 * Andrei de A. Formiga, 2008-11-18
 *
 */

chopcol([], [], []).
chopcol([[H|T]|R], [H|Hs], [T|Ts]) :- chopcol(R, Hs, Ts).

transpose([[]|_], []).
transpose(R, [H|C]) :- chopcol(R, H, M), transpose(M, C).
