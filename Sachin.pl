:- use_module(library(lists)).

reflexive(_, []).
reflexive(R, [X|Rest]) :- member((X, X), R), reflexive(R, Rest).

% Transitive closure
transitive(_, []).
transitive(R, [(X, Y)|Rest]) :- member((X, Y), R), reflexive(R, [X, Y]), transitive(R, Rest).

% Reflexive-transitive closure
rtc(R, X, Y) :- reflexive(R, [X, Y]), transitive(R, [(X, Y)]).


%R = [(a,b),(b,c),(c,c),(d,d)]
% Example usage
%rtc(R, a, c).
%rtc(R, a, b).
%rtc(R,b,d).


% Base cases for reflexivity, symmetry, and transitivity
rtclosure(_, []).
rtclosure(S, [(X, X) | Rest]) :- rtclosure(S, Rest).
rtclosure(S, [(X, Y) | Rest]) :- member((Y, X), S), rtclosure(S, Rest).
rtclosure(S, [(X, Y) | Rest]) :- member((Y, Z), S), rtclosure(S, [(X, Z) | Rest]).

% Main predicate to compute the reflexive-symmetric-transitive closure
reflexive_symmetric_transitive_closure(S, R, Closure) :- findall((X, Y), (rtclosure(S, R), member((X, Y), R)), Closure).

% Example usage



% S = [(a, a), (a, b)],R = [(a, b)],reflexive_symmetric_transitive_closure(S, R, Closure),write('Reflexive-Symmetric-Transitive Closure: '), write(Closure).



/* del(X,L1,L2) -- delete element X from a list L1 to obtain L2 */

del(X, [], []) :- !.

del(X, [X|R], Z) :- del(X, R, Z), !.

del(X, [Y|R], [Y|Z]) :- del(X, R, Z), !.


/* remdups(L, L1) remove duplicates from a list L to get L1 */

remdups([], []) :- !.

remdups([X|R], [X|Z]) :- del(X, R, L), remdups(L, Z).


/* unionI(S1, S2, S3) -- union of S1 and S2 is stored in S3 */

unionI([], S2, S2) :- !.

unionI(S1, [], S1) :- !.

unionI([X|R], S2, [X|Z]) :- \+ member(X, S2), unionI(R, S2, Z), !.

unionI([X|R], S2, Z) :- member(X, S2), unionI(R, S2, Z).


/* interI(S1, S2, S3) -- intersection of S1 and S2 is stored in S3 */

interI([], _, []) :- !.

interI(_, [], []) :- !.

interI([X|R], S2, [X|Z]) :- member(X, S2), interI(R, S2, Z), !.

interI([_|R], S2, Z) :- interI(R, S2, Z).


/* diffI(S1, S2, S3) -- set difference S1 - S2 is stored in S3 */

diffI([], _, []) :- !.

diffI(S1, [], S1) :- !.

diffI([X|R], S2, Z) :- member(X, S2), diffI(R, S2, Z), !.

diffI([X|R], S2, [X|Z]) :- diffI(R, S2, Z).


/* cartesianI(S1, S2, S3) -- cartesian product of S1 and S2 is stored in S3 */

cartesianI(_, [], []) :- !.

cartesianI([X|R], S2, Z) :- cartesian_product(X, S2, P), cartesianI(R, S2, T), append(P, T, Z).

cartesian_product(_, [], []) :- !.

cartesian_product(X, [Y|R], [[X,Y]|Z]) :- cartesian_product(X, R, Z).


/* append(L1, L2, L3) -- append lists L1 to list L2 to get list L3 */

append([], L, L).

append([X|R], L, [X|Z]) :- append(R, L, Z).


/* mapcons(X, L1, L2) -- cons the element X to each list in L1 to get L2 */

mapcons(_, [], []) :- !.

mapcons(X, [Y|R], [[X|Y]|Z]) :- mapcons(X, R, Z).


/* powerI(S, P1): Here is an implementation of powerset of S */

powerI([], [[]]) :- !.

powerI([X|R], P) :- powerI(R, P1), mapcons(X, P1, P2), append(P2, P1, P).


/* Helper predicate to check if an element is a member of a list */

member(X, [X|_]) :- !.

member(X, [_|R]) :- member(X, R).


/* Helper predicate to check if two lists are equal */

lists_equal([], []) :- !.

lists_equal([X|R1], [X|R2]) :- lists_equal(R1, R2).


/* Helper predicate to check if the powersets obtained from two different valid representations of a set are equal */

equal_powersets(S1, S2) :- powerI(S1, P1), powerI(S2, P2), lists_equal(P1, P2).


/* Test Cases */

%test_union :- unionI([1,2,3], [3,4,5], Result), write(Result), nl.

%test_intersection :- interI([1,2,3], [3,4,5], Result), write(Result), nl.

%test_difference :- diffI([1,2,3], [3,4,5], Result), write(Result), nl.

%test_cartesian :- cartesianI([1,2], [a,b], Result), write(Result), nl.

%test_powersets_equality :- equal_powersets([1,2,3], [3,2,1]).
