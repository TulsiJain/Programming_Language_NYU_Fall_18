% 1. remove every occurence of item X from a list
remove_item(_, [], []).
remove_item(X, [X|L1], L2):- remove_item(X, L1, L2). 
remove_item(X, [H|L1], [H|L2]):- X\==H, remove_item(X,L1,L2).


% 2. remove every occurence of item in list one from list two
remove_items([], L1, L1).
remove_items(_, [], []).
remove_items(L1, [X|T], Res):- member(X, L1), !, remove_items(L1, T, Res). 
remove_items(L1, [X|T], [X|Res]):- remove_items(L1, T, Res).


% 3. returns unique intersection of two lists 
intersection2([L|L1],L2,[L|I]):- member(L,L2),remove_item(L,L1,L3),intersection2(L3,L2,I),!.
intersection2([L|L1],L2,I):- \+member(L,L2), intersection2(L1,L2,I).
intersection2([],_,[]).


% 4. check if a list is set or not
is_set([]).
is_set([H|T]) :- \+member(H, T), is_set(T).


% 5. returns a disjunct union 
disjunct_union(L1,L2,U) :- remove_dups(L1, L11), remove_dups(L2, L12), append(L11, L12, L4), intersection2(L11, L12, L5), remove_items(L5, L4, U).


% 6. removes duplicate from a list
remove_dups([], []).
remove_dups([H | T], List2) :- member(H, T), remove_dups(T, List2).
remove_dups([H | T], [H | T2]) :- not(member(H, T)), remove_dups(T, T2).


% 7. find unique union
union(L1, L2, L3) :- remove_dups(L1, L11), remove_dups(L2, L12), append(L11, L12, L4), remove_dups(L4, L3), !.
