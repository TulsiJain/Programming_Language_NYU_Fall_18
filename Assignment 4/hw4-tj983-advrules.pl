% NANI SEARCH - A sample adventure game

% Nani Search is designed to illustrate Prolog programming.  It
% is an implementation of the principle example used in
% this tutorial.

main:- nani_search.       % main entry point

nani_search:-
  %init_dynamic_facts,     % predicates which are not compiled

  write('NANI SEARCH - A Sample Adventure Game'),nl,
  write('Copyright (C) Amzi! inc. 1990-2010'),nl,
  write('No rights reserved, use it as you wish'),nl,
  nl,
  write('Nani Search is designed to illustrate Prolog programming.'),nl,
  write('As such, it might be the simplest adventure game.  The game'),nl,
  write('is the primary example used in this tutorial.'),nl,
  write('Full source is included as well.'),nl,
  nl,
  write('Your persona as the adventurer is that of a three year'),nl,
  write('old.  The Nani is your security blanket.  It is getting'),nl,
  write('late and you''re tired, but you can''t go to sleep'),nl,
  write('without your Nani.  Your mission is to find the Nani.'),nl,
  nl,
  write('You control the game by using simple English commands'),nl,
  write('expressing the action you wish to take.  You can go to'),nl,
  write('other rooms, look at your surroundings, look in things'),nl,
  write('take things, drop things, eat things, inventory the'),nl,
  write('things you have, and turn things on and off.'),nl,
  nl,
  write('Hit any key to continue.'),get0(_),
  write('Type "help" if you need more help on mechanics.'),nl,
  write('Type "hint" if you want a big hint.'),nl,
  write('Type "quit" if you give up.'),nl,
  nl,
  write('Enjoy the hunt.'),nl,

  look,                   % give a look before starting the game
  command_loop.

% command_loop - repeats until either the nani is found or the
%     player types quit

command_loop:-
  repeat,
  get_command(X),
  do(X),
  (nanifound; X == quit).

% do - matches the input command with the predicate which carries out
%     the command.  More general approaches which might work in the
%     listener are not supported in the compiler.  This approach
%     also gives tighter control over the allowable commands.

%     The cuts prevent the forced failure at the end of "command_loop"
%     from backtracking into the command predicates.

do(goto(X)):-goto(X),!.
do(nshelp):-nshelp,!.
do(hint):-hint,!.
do(inventory):-inventory,!.
do(take(X)):-take(X),!.
% 8. Toss
do(toss(X)):-toss(X),!.
do(teleport(X)):-teleport(X),!.
do(drop(X)):-drop(X),!.
do(eat(X)):-eat(X),!.
do(look):-look,!.
do(turn_on(X)):-turn_on(X),!.
do(turn_off(X)):-turn_off(X),!.
do(look_in(X)):-look_in(X),!.
do(study(X)):-study(X),!.
do(quit):-quit,!.

% These are the predicates which control exit from the game.  If
% the player has taken the nani, then the call to "have(nani)" will
% succeed and the command_loop will complete.  Otherwise it fails
% and command_loop will repeat.

nanifound:-
  have(nani),
  write('Congratulations, you saved the Nani.'),nl,
  write('Now you can rest secure.'),nl,nl.

quit:-
  write('Giving up?  It''s going to be a scary night'),nl,
  write('and when you get the Nani it''s not going'),nl,
  write('to smell right.'),nl,nl.

% The help command

nshelp:-
  write('Use simple English sentences to enter commands.'),nl,
  write('The commands can cause you to:'),nl,
  nl,
  write('   go to a room          (ex. go to the office)'),nl,
  write('   look around           (ex. look)'),nl,
  write('   look in something     (ex. look in the desk)'),nl,
  write('   study something       (ex. study the book)'),nl,
  write('   take something        (ex. take the apple)'),nl,
  write('   drop something        (ex. drop the apple)'),nl,
  write('   eat something         (ex. eat the apple)'),nl,
  write('   turn something on     (ex. turn on the light)'),nl,
  write('   inventory your things (ex. inventory)'),nl,
  nl,
  write('The examples are verbose, terser commands and synonyms'),nl,
  write('are usually accepted.'),nl,nl,
  write('Hit any key to continue.'),nl,
  get0(_),
  look.

hint:-
  write('You need to get to the cellar, and you can''t unless'),nl,
  write('you get some light.  You can''t turn on the cellar'),nl,
  write('light, but there is a flash light in the desk in the'),nl,
  write('office you might use.'),nl,nl,
  look.


room(kitchen).
room(office).
room(hall).
room('dining room').
room(cellar).

% 1. Changes Created a room card attic......................................................................
room(attic).

door(office, hall).
door(kitchen, office).
door(hall, 'dining room').
door(kitchen, cellar).
door('dining room', kitchen).

:- dynamic location/2.
location(desk, office).
location(apple, kitchen).
location(flashlight, desk).
location('washing machine', cellar).
location(nani, 'washing machine').
location('PL textbook', office).
location(broccoli, kitchen).
location(crackers, kitchen).
location(computer, office).
location(envelope, desk).
location(stamp, envelope).
location(key, envelope).


% 2. placed two item in attic named doll and plate..............................................................
location(doll, attic).
location(plate, attic).

% 3. placed teleporter desk.....................................................................................
location(teleporter, desk).

% 4. Assigned heavy objects......................................................................................
is_heavy(desk).
is_heavy('washing machine').
is_heavy(computer).

% 8. Tossable Objects............................................................................................
is_tossable(apple).
is_tossable(flashlight).
is_tossable('PL textbook').
is_tossable(broccoli).
is_tossable(crackers).
is_tossable(envelope).
is_tossable(stamp).
is_tossable(key).
is_tossable(doll).
is_tossable(plate).
is_tossable(teleporter).


toss(X):- is_tossable(X), have(X),
  retract(have(X)), write("You threw "),
   write(X), write(" out of window"), nl, !.

toss(X):- is_tossable(X),
  write("You do not have it"), nl,
  !.

toss(X):- write("Item is not tossable "),
write(X), nl,
  fail.


:-dynamic have/1.

edible(apple).
edible(crackers).

tastes_yucky(broccoli).


:- dynamic turned_on/1.
turned_on(flashlight).


:- dynamic here/1.
here(kitchen).

connect(X,Y) :- door(X,Y).
connect(X,Y) :- door(Y,X).

% respond simplifies writing a mixture of literals and variables

respond([]):-
  write('.'),nl,nl.
respond([H|T]):-
  write(H),
  respond(T).

inventory:-
  have(_),                         % make sure you have at least one thing
  write('You have: '),nl,
  list_possessions.
inventory:-
  write('You have nothing'),nl.

list_possessions:-
  have(X),
  tab(2),write(X),nl,
  fail.
list_possessions.


list_things(Place) :-
  location(X, Place),
  tab(2),
  write(X),
  nl,
  fail.
list_things(_).

list_connections(Place) :-
  connect(Place, X),
  tab(2),
  write(X),
  nl,
  fail.
list_connections(_).

look :-
  here(Place),
  write('You are in the '), write(Place), nl,
  write('You can see:'), nl,
  list_things(Place),
  write('You can go to:'), nl,
  list_connections(Place).

study(Thing):-
  have(Thing),
  respond(['You will surely earn an A in the course.']).
study(Thing):-
  respond(['You don''t have the ',Thing]).


% 5. Teleport Objects............................................................................................

teleport(Place):- have(teleporter), move(Place), look, !.
teleport(Place):-  
write('You can not go to '), write(Place),
write(' because, You do not have a  teleport'), nl.


% 9. Implemented new feature ....................................................................................
goto(Place):-
  \+here(cellar), Place = cellar, have(key), !, can_go(Place), move(Place), look.

goto(Place):-
  \+here(cellar), Place = cellar, have(teleporter), move(Place), look, !.

goto(Place):- \+here(cellar), Place==cellar, \+have(key),
  write("You do not have a key. Without a key you can not go to cellar"), nl, !.

goto(Place):-
  can_go(Place),
  move(Place),
  look, !.

can_go(Place) :- can_go(Place,_).

can_go(Place,X):-
  here(X),
  connect(X, Place).

can_go(Place,X):-
  write("You can't get to the "),
  write(Place),
  write(" from here, the "),
  here(X),
  write(X),
  write("."),
  nl,
  fail.

move(Place) :- move(Place,_).

move(Place,X):-
  retract(here(X)),
  asserta(here(Place)).

take(X):-
  can_take(X),
  take_object(X),
  !.

% drop - allows the player to transfer a possession to a room

drop('PL textbook'):-
  have('PL textbook'),		   % you must have the thing to drop it
  here(Here),                      % where are we
  retract(have('PL textbook')),
  asserta(location('PL textbook',Here)),
  respond(['Man, I have a headache.']).
drop(Thing):-
  have(Thing),                     % you must have the thing to drop it
  here(Here),                      % where are we
  retract(have(Thing)),
  asserta(location(Thing,Here)),
  respond(['You put down the ',Thing]).
drop(Thing):-
  respond(['You don''t have the ',Thing]).


% turn_on recognizes two cases.  If the player tries to simply turn
% on the light, it is assumed this is the room light, and the
% appropriate error message is issued.  Otherwise turn_on has to
% refer to an object which is turned_off.

turn_on(light):-
  respond(['You can''t reach the switch and there''s nothing to stand on']).
turn_on(Thing):-
  have(Thing),
  turn_on2(Thing).
turn_on(Thing):-
  respond(['You don''t have the ',Thing]).

turn_on2(Thing):-
  turned_on(Thing),
  respond([Thing,' is already on']).
turn_on2(Thing):-
  turned_off(Thing),
  retract(turned_off(Thing)),
  asserta(turned_on(Thing)),
  respond([Thing,' turned on']).
turn_on2(Thing):-
  respond(['You can''t turn a ',Thing,' on']).


% 6. Implement flashlight turn off ............................................................................................
turn_off(light):-
  respond(['You can''t reach the switch and there''s nothing to stand on']).
turn_off(Thing):-
  have(Thing),
  turn_off2(Thing).
turn_off(Thing):-
  respond(['You don''t have the ',Thing]).

turn_off2(Thing):-
  turned_off(Thing),
  respond([Thing,' is already off']).
turn_off2(Thing):-
  turned_on(Thing),
  retract(turned_on(Thing)),
  asserta(turned_off(Thing)),
  respond([Thing,' turned off']).
turn_off2(Thing):-
  respond(['You can''t turn a ',Thing,' off']).


% look_in allows the player to look inside a thing which might
% contain other things

look_in(Thing):-
  location(_,Thing),               % make sure there's at least one
  write('The '),write(Thing),write(' contains:'),nl,
  list_things(Thing).
look_in(Thing):-
  respond(['There is nothing in the ',Thing]).


% 4. and 7. Added rule that item should not be heavy to pick up and it should be present.............................
can_take(Thing) :-
  have(Thing),
  write('You have already taken '),
  write(Thing),
  write('. You can not take this again.'),
  nl, !, fail.

can_take(Thing) :-
  have(Thing),
  write('You have already taken '),
  write(Thing),
  write('. You can not take this again.'),
  nl, !, fail.

can_take(Thing) :-
  here(Place),
  \+is_heavy(Thing),
  location(Thing, Place).

can_take(Thing) :-
  here(Place),
  \+is_heavy(Thing),
  location(Thing,X),
  location(X,Place).

can_take(Thing) :-
  \+is_heavy(Thing),
  write("There is no "),
  write(Thing),
  write(" here."),
  nl,
  fail.

can_take(Thing) :-
  is_heavy(Thing),
  write('You can not take '),
  write(Thing),
  write('. It is too heavy for you.'),
  nl,
  fail.

take_object(X):-
  retract(location(X,_)),
  asserta(have(X)),
  write('You took the '),
  write(X),
  write('.'),
  nl.


eat(Thing):-
  have(Thing),
  eat2(Thing).
eat(Thing):-
  respond(['You don''t have the ',Thing]).

eat2(Thing):-
  edible(Thing),
  retract(have(Thing)),
  respond(['That ',Thing,' was good']).
eat2(Thing):-
  tastes_yucky(Thing),
  respond(['Three year olds don''t eat ',Thing]).
eat2(Thing):-
  respond(['You can''t eat a ',Thing]).


put(X):- put(X,_).
put(X,Y):-
  retract(have(X)),
  here(Y),
  asserta(location(X,Y)),
  write('You placed the '),
  write(X),
  write(' in the '),
  write(Y),
  nl.


% Simple English command listener.  It does some semantic checking
% and allows for various synonyms.  Within a restricted subset of
% English, a command can be phrased many ways.  Also non grammatical
% constructs are understood, for example just giving a room name
% is interpreted as the command to goto that room.

% Some interpretation is based on the situation.  Notice that when
% the player says turn on the light it is ambiguous.  It could mean
% the room light (which can't be turned on in the game) or the
% flash light.  If the player has the flash light it is interpreted
% as flash light, otherwise it is interpreted as room light.

get_command(C) :-
  readlist(L),        % reads a sentence and puts [it,in,list,form]
  command(X,L,[]),    % call the grammar for command
  C =.. X,!.          % make the command list a structure
get_command(_) :-
  respond(['I don''t understand, try again or type help']),fail.

% The grammar doesn't have to be real English.  There are two
% types of commands in Nani Search, those with and without a
% single argument.  A special case is also made for the command
% goto which can be activated by simply giving a room name.

command([Pred,Arg]) --> verb(Type,Pred),nounphrase(Type,Arg).
command([Pred]) --> verb(intran,Pred).
command([goto,Arg]) --> noun(go_place,Arg).
% 5. Teleport.......................................................................................................
command([teleport,Arg]) --> noun(go_place,Arg).

% Recognize three types of verbs.  Each verb corresponds to a command,
% but there are many synonyms allowed.  For example the command
% turn_on will be triggered by either "turn on" or "switch on".

verb(go_place,goto) --> go_verb.
% 5. Teleport.......................................................................................................
verb(go_place,teleport) --> teleport_verb.
verb(thing,V) --> tran_verb(V).
verb(intran,V) --> intran_verb(V).

go_verb --> [go].
go_verb --> [go,to].
go_verb --> [g].
% 5. Teleport.......................................................................................................
teleport_verb --> [teleport].

tran_verb(take) --> [take].
% 8. Toss.......................................................................................................
tran_verb(toss) --> [toss].
tran_verb(take) --> [pick,up].
tran_verb(study) --> [study].
tran_verb(study) --> [read].
tran_verb(study) --> [memorize].
tran_verb(drop) --> [drop].
tran_verb(drop) --> [put].
tran_verb(drop) --> [put,down].
tran_verb(eat) --> [eat].
tran_verb(turn_on) --> [turn,on].
tran_verb(turn_on) --> [switch,on].
tran_verb(turn_off) --> [turn,off].
tran_verb(look_in) --> [look,in].
tran_verb(look_in) --> [look].
tran_verb(look_in) --> [open].

intran_verb(inventory) --> [inventory].
intran_verb(inventory) --> [i].
intran_verb(look) --> [look].
intran_verb(look) --> [look,around].
intran_verb(look) --> [l].
intran_verb(quit) --> [quit].
intran_verb(quit) --> [exit].
intran_verb(quit) --> [end].
intran_verb(quit) --> [bye].
intran_verb(nshelp) --> [help].
intran_verb(hint) --> [hint].

% a noun phrase is just a noun with an optional determiner in front.

nounphrase(Type,Noun) --> det,noun(Type,Noun).
nounphrase(Type,Noun) --> noun(Type,Noun).

det --> [the].
det --> [a].

% Nouns are defined as rooms, or things located somewhere.  We define
% special cases for those things represented in Nani Search by two
% words.  We can't expect the user to type the name in quotes.

noun(go_place,R) --> [R], {room(R)}.
noun(go_place,'dining room') --> [dining,room].

noun(thing,T) --> [T], {location(T,_)}.
noun(thing,T) --> [T], {have(T)}.
noun(thing,flashlight) --> [flash,light].
noun(thing,'washing machine') --> [washing,machine].
noun(thing,'dirty clothes') --> [dirty,clothes].
noun(thing,'PL textbook') --> [pl,textbook].

% If the player has just typed light, it can be interpreted three ways.
% If a room name is before it, it must be a room light.  If the
% player has the flash light, assume it means the flash light.  Otherwise
% assume it is the room light.

noun(thing,light) --> [X,light], {room(X)}.
noun(thing,flashlight) --> [light], {have(flashlight)}.
noun(thing,light) --> [light].

% readlist - read a list of words, based on a Clocksin & Mellish
% example.

readlist(L):-
  write('> '),
  read_word_list(L).

read_word_list([W|Ws]) :-
  get0(C),
  readword(C, W, C1),       % Read word starting with C, C1 is first new
  restsent(C1, Ws), !.      % character - use it to get rest of sentence

restsent(C,[]) :- lastword(C), !. % Nothing left if hit last-word marker
restsent(C,[W1|Ws]) :-
  readword(C,W1,C1),        % Else read next word and rest of sentence
  restsent(C1,Ws).

readword(C,W,C1) :-         % Some words are single characters
  single_char(C),           % i.e. punctuation
  !,
  name(W, [C]),             % get as an atom
  get0(C1).
readword(C, W, C1) :-
  is_num(C),                % if we have a number --
  !,
  number_word(C, W, C1, _). % convert it to a genuine number
readword(C,W,C2) :-         % otherwise if character does not
  in_word(C, NewC),         % delineate end of word - keep
  get0(C1),                 % accumulating them until
  restword(C1,Cs,C2),       % we have all the word
  name(W, [NewC|Cs]).       % then make it an atom
readword(_,W,C2) :-         % otherwise
  get0(C1),
  readword(C1,W,C2).        % start a new word

restword(C, [NewC|Cs], C2) :-
  in_word(C, NewC),
  get0(C1),
  restword(C1, Cs, C2).
restword(C, [], C).


single_char(0',).
single_char(0';).
single_char(0':).
single_char(0'?).
single_char(0'!).
single_char(0'.).


in_word(C, C) :- C >= 0'a, C =< 0'z.
in_word(C, L) :- C >= 0'A, C =< 0'Z, L is C + 32.
in_word(0'',0'').
in_word(0'-,0'-).

% Have character C (known integer) - keep reading integers and build
% up the number until we hit a non-integer. Return this in C1,
% and return the computed number in W.

number_word(C, W, C1, Pow10) :-
  is_num(C),
  !,
  get0(C2),
  number_word(C2, W1, C1, P10),
  Pow10 is P10 * 10,
  W is integer(((C - 0'0) * Pow10) + W1).
number_word(C, 0, C, 0.1).


is_num(C) :-
  C =< 0'9,
  C >= 0'0.

% These symbols delineate end of sentence

lastword(10).   % end if new line entered
lastword(0'.).
lastword(0'!).
lastword(0'?).
