%———————————————————————————————————————————————
% Simultaneously learning two predicates odd(L) and even(L)
% Inducing odd and even length for lists
  
%———————————————————————————————————————————————
% Background Knowledge (BK)

% backliteral(even(L),[L:list],[]).
% backliteral(odd(L),[L:list],[]).
backliteral(even(L),[L:list]).
backliteral(odd(L),[L:list]).


term(list,[X|L],[X:item,L:list]).
term(list,[],[]).

prolog_predicate(fail).

start_hyp([[odd(L)]/[L:list],[even(L1)]/[L1:list]]).

% start_clause([odd(L)]/[L:list]).
% start_clause([even(L)]/[L:list]).

%———————————————————————————————————————————————
% Positive examples
% ex(+Example): +Example is a positive example
ex( even([])).
ex( even([a,b])).
ex( odd([a])).
ex( odd([b,c,d])).
ex( odd([a,b,c,d,e])).
ex( even([a,b,c,d])).

%———————————————————————————————————————————————
% Negative examples
% nex(+Example): +Example is a 
nex( even([a])).
nex( even([a,b,c])).
nex( odd([])).
nex( odd([a,b])).
nex( odd([a,b,c,d])).
