/*
 * Clause and Effect, William F. Clocksin
 *
 * Worksheet 12: Partial Maps
 *
 * Andrei de A. Formiga, 2008-11-18
 *
 */

prohibit(bother).
prohibit(blast).
prohibit(drat).
prohibit(fiddlestick).

censor([], []).
censor([X|T], L) :- prohibit(X), censor(T, L).
censor([X|T], [X|L]) :- censor(T, L).

/*
  censor as it is generates the correct answer first, but then generates
  spurious lists later. This is because the third clause for censor matches
  everything, including censored words. We could define a predicate permit
  that lists permitted words to use as a condition in the third clause; this
  would cause a fail if any words not recognized as prohibited or permitted
  appear.

  The other way around this is using cut. 
 */
