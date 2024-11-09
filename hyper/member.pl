
% Problem definition for learning about member(X,L)

backliteral(member(X,L),[L:list],[X:item]).   % Background literal


% Refinement of terms

term(list,[X|L],[X:item,L:list]).
term(list,[],[]).

prolog_predicate(fail).     % No background predicate in Prolog 

start_clause([member(X,L)] / [X:item, L:list]).


% Positive and negative examples

ex( member(a,[a])).
ex( member(a,[a,b])).
ex( member(a,[a,b,c,d,e])).

nex( member(b,[a])).
nex( member(d,[a,b])).
nex( member(f,[a,b,c,d,e])).

