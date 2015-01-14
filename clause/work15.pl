/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 15: Multiple Disjoint Partial Maps
 *
 * Andrei de A. Formiga, 2008-11-18
 *
 */

herd([], [], []).
herd([sheep|L], [sheep|S], G) :- herd(L, S, G).
herd([goat|L], S, [goat|G]) :- herd(L, S, G).


/* separating goats, sheeps, and the rest */
herd2([], [], [], []).
herd2([sheep|L], [sheep|S], G, Z) :- herd2(L, S, G, Z).
herd2([goat|L], S, [goat|G], Z) :- herd2(L, S, G, Z).
herd2([X|L], S, G, [X|Z]) :- X \== goat, X \== sheep, herd2(L, S, G, Z).

/* alternating elements of a list */
alternate([], [], []).
alternate([X, Y | T], [X|O], [Y|E]) :- alternate(T, O, E).


