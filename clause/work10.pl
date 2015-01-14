/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 10: Full Maps
 *
 * Andrei de A. Formiga, 2008-11-18
 *
 */

envelope([], []).
envelope([X|T], [container(X)|L]) :- envelope(T, L).


/* practice */

/* the word encoding */
encode(the,  17).
encode(cat,  23).
encode(sits, 46).
encode(on,    9).
encode(mat,   2).


fullmap([], []).
fullmap([X|T], [Y|L]) :- encode(X, Y), fullmap(T, L).

