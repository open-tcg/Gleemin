% First saved: 17/02/2011
% Last saved: 28/02/2011
%
% Doings:
%	changed atom_to_list; go back to previous stable predicates.pl
%	  if there's trouble. 
%	fixed make_unique (was creating copies of cards)
%	added list_difference
%	added flatten_list 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Next                 %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%next/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* next(?Current, ?Next, ?List) */
	% Find the next or previous element in a list
	%   or generate a list from elements.

	next(Current, Next, [Current, Next|_]).
	next(Current, Next, [H|T]):-
		next(Current, Next, T).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Compare Lists           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%compare_lists/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* compare_lists(+List_1, +List_2) */
	% True if each element in List_1 is in List_2
	%  exactly as many times.

	compare_lists(List_1, List_2):-
		compare_lists(List_1, List_1, List_2, List_2).

	compare_lists([],_, [], _).
	compare_lists([H1 | T1], List_1, [H2 | T2], List_2):- 
		member(H1, List_2),
		member(H2, List_1), 
		compare_lists(T1, List_1, T2, List_2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          List Difference          %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%list_difference/3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* list_difference(+List_1, +List_2, -Difference) */
	% Returns the difference of two lists (as in sets)

	list_difference(List, List, []).
	list_difference(L1, L2, Diff):-
		list_difference(L1, L2, [], Diff_1),
		list_difference(L2, L1, [], Diff_2),
		append(Diff_1, Diff_2, Diff).

	list_difference(List, List, Diff, Diff).
	list_difference([], _List, Diff, Diff).
	list_difference([H | T], L2, Temp, Diff):- 
		\+ member(H, L2),
		append(Temp, [H], New),
		list_difference(T, L2, New, Diff).
	list_difference([_ | T], L2, Temp, Diff):- 
		list_difference(T, L2, Temp, Diff).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Flatten List            %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%flatten_list/2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* flatten_list(+List, -Flat) */
	% from https://sites.google.com/site/prologsite/prolog-problems/1
	
	flatten_list(X,[X]) :- 
		\+ list(X).
	flatten_list([],[]).
	flatten_list([X|Xs],Zs) :- 
		flatten_list(X,Y), flatten_list(Xs,Ys), append(Y,Ys,Zs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Atom - list           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%atom_list/2 28/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  atom_list(?Atom, ?List) */
	% Converts between an atom and a list of atoms

%%%%%%%%%%%%%%atom_list/2 (1) 28/02/11

	% convert from an atom to a List 
	%  of atomic characters (not charcodes!)
	atom_to_list(Atom, List):-
		type(Atom, 3), type(List, 0),	
		%   	atom ^	variable ^
		atom_chars(Atom, Chars), 
		to_list(Chars, _Temp_list, List), !.

%%%%%%%%%%%%%%atom_list/2 (2) 28/02/11

	% Convert from a list of characters
	%   to an Atom
	atom_to_list(Atom, List):-
		type(Atom, 0), type(List, 6),
		% variable ^	   list ^
		to_chars(List, [], Chars),
		atom_chars(Atom, Chars), !.


%%%%%%%%%%%%%%to_list/3 28/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% Turn the List of character codes 
	%  to a list of atomic characters
	to_list([], List, List). 
	to_list([Head | Tail], Temp, List):-
		number_code(Head), 
		numbers([Head | Tail], [], Numbers, Rest),
		atom_chars(Number, Numbers),  
		append(Temp, [Number], New_temp), 
		to_list(Rest, New_temp, List).
	to_list([Head | Tail], Temp, List):-
		atom_chars(Atomic, [Head]), 
		append(Temp, [Atomic], New_temp), 
		to_list(Tail, New_temp, List).


%%%%%%%%%%%%%%numbers/4 28/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% A sequence of character codes of numeric digits 
	%  is turned into a single number (eg, 50,51 to 23)
	numbers([], Number, Number, []). 
	numbers([Head | Tail], Temp_num, Number, Rest):- 
		number_code(Head), 
		append(Temp_num, [Head], New_num),
		numbers(Tail, New_num, Number, Rest).
	numbers(Rest, Number, Number, Rest).

	% Number char codes.  
	number_code(Character) :-
		Character =< 0'9,
		Character >= 0'0.


%%%%%%%%%%%%%%to_chars/4 28/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% Turn the list of atomic characters 
	%  to a list of character codes.
	to_chars([], Chars, Chars).
	to_chars([Head | Tail], Temp, Chars):-
		atom_chars(Head, Char), 
		append(Temp, Char, New_temp),
		to_chars(Tail, New_temp, Chars).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%            Switches               %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%prompt_switches/2 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* prompt_switches(+Atoms, -Switches) */
	% Takes in a list of Atoms and returns a map of Atom - switch pairs
	%  and a list of Atoms with their switches enclosed in square brackets
	%  ie a list of Switches.

	prompt_switches(Atoms, [Map, Switches]):-
		map_switches(Atoms, Map), 
		prompt_switches(Map, [], Switches), !.

%%%%%%%%%%%%%%prompt_switches/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* prompt_switches(+Atoms, [], -Switches) */
	% Business goals of prompt_switches/2

%%%%%%%%%%%%%%prompt_switches/3 (0) 03/02/11

	prompt_switches([], Switches, Switches).

%%%%%%%%%%%%%%prompt_switches/3 (1) 03/02/11

	prompt_switches(Atoms, Temp, Switches):- 
		Atoms= [Atom - Switch | Rest_atoms], 
		atom_chars(Atom, Atom_codes),
		atom_chars(Switch, [Switch_codes]), 
		bracket(Switch_codes, Atom_codes, Bracketed), 
		append(Temp, [Bracketed], New_temp), 
		prompt_switches(Rest_atoms, New_temp, Switches).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%bracket/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  bracket(+Switch, +Atom, -Bracketed) */
	% Encloses the given Switch in square brackets.

%%%%%%%%%%%%%%bracket/3 (1) 03/02/11

	bracket(Switch, Atom, Bracketed):- 
		member(Switch, Atom, Position),
		insert_at(91, Atom, Position, Result), 
		P1 is Position + 2, 
		insert_at(93, Result, P1, Result_2),
		atom_chars(Bracketed, Result_2).

%%%%%%%%%%%%%%bracket/3 (2) 03/02/11

	bracket(Switch, Atom, Bracketed):- 
		append(Atom, [91, Switch, 93], Codes), 
		atom_chars(Bracketed, Codes).


%%%%%%%%%%%%%%insert_at/4 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* insert_at(?Element, ?List, +Position, ?Result).*/

	insert_at(X,L,K,R):- 
		remove_at(X,R,K,L).

%%%%%%%%%%%%%%remove_at/4 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* remove_at(?Element, ?List, +Position, ?Result) */

	remove_at(X,[X|Xs],1,Xs).
	remove_at(X,[Y|Xs],K,[Y|Ys]) :- K > 1, 
	   K1 is K - 1, remove_at(X,Xs,K1,Ys).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	insert_at and remove_at are
	  taken from "99 Prolog problems, 
	  at http://sites.google.com/site/prologsite/home
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%map_switches/2 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* map_switches(+Atoms, -Map) */
	% Takes in a list of Atoms and returns a map of 
	%  'Atom - Switches' pairs. 

	map_switches(Atoms, Map):-
		switches(Atoms, Switches), 
		map_switches(Atoms, Switches, [], Map).


%%%%%%%%%%%%%%map_switches/4 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* map_switches(+Atoms, Switches, [], -Map) */
	% Business goal for top-level goal map_switches/2. 

%%%%%%%%%%%%%%map_switches/4 (0) 03/02/11

	map_switches([], [], Map, Map). 

%%%%%%%%%%%%%%map_switches/4 (1) 03/02/11

	map_switches(Atoms, Switches, Temp, Map):-
		Atoms = [Atom | Rest_atoms], 
		Switches = [Switch | Rest_switches], 
		append(Temp, [Atom - Switch], New_temp),
		map_switches(Rest_atoms, Rest_switches, New_temp, Map). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%switches/2 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* switches(+Atoms, -Switches) */
	% Takes in a list of Atoms and returns a list of unique Switches
	% as character codes

	switches(Atoms, Switches):-
		switches(Atoms, [], Codes), 
		atom_chars(Chars, Codes), 
		atom_to_list(Chars, Switches), !. 

%%%%%%%%%%%%%%switches/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* switches(+Atoms, [], -Switches) */
	% Generates the list of Switches' character codes.
	
%%%%%%%%%%%%%%switches/3 (0) 03/02/11
	
	switches([], Switches, Switches).

%%%%%%%%%%%%%%switches/3 (1) 03/02/11

	switches([Atom | Rest], [], Temp):- 
		atom_chars(Atom, Chars), 
		member(Char, Chars), 
		append([], [Char], New_switches), 
		switches(Rest, New_switches, Temp). 

%%%%%%%%%%%%%%switches/3 (2) 03/02/11

	switches([Atom | Rest], Switches, Temp):-
		atom_chars(Atom, Chars),
		member(Char, Chars), 
		not member(Char, Switches), 
		append(Switches, [Char], New_switches), 
		switches(Rest, New_switches, Temp). 

%%%%%%%%%%%%%%switches/3 (3) 03/02/11

	switches([Atom | Rest], Switches, Temp):- 
		atom_chars(Atom, Chars),
		member(Char, Chars), 
		Char >= 0'a, Char =< 0'z, 
		Upper_char is Char - 32, 
		not member(Upper_char, Switches),
		append(Switches, [Upper_char], New_switches),
		switches(Rest, New_switches, Temp).

%%%%%%%%%%%%%%switches/3 (4) 03/02/11

	switches([_Atom | Rest], Switches, Temp):- 
		(setof(Char, integer_bound(0'a, Char, 0'z), Codes); 
		setof(Char, integer_bound(0'A, Char, 0'Z), Codes)), 
		member(Char, Codes), 
		not member(Char, Switches),
		append(Switches, [Char], New_switches),
		switches(Rest, New_switches, Temp).

%%%%%%%%%%%%%%switches/3 (5) 03/02/11

	switches(_Atoms, _Switches, _Temp):-
		write(`It seems I'm out of letters to assign as switches :(`), nl, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Make Unique           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
	:- dynamic(card_ids/2).
	:- dynamic(copy/3).

	card_ids([], [0]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%make_unique/2 04/02/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* make_unique(+Card, -Copy) */
	% Generates copies of cards with unique ids.


%%%%%%%%%%%%%%make_unique/2 (1) 04/02/2011

	make_unique(Card, copy(Card, Id, State)):-
		\+ card_ids(Card, _),	
		Id = 1, State = [],
		assert(card_ids(Card, [Id])).

%%%%%%%%%%%%%%make_unique/2 (2) 04/02/2011 

	make_unique(Card, copy(Card, Id, State)):-
		card_ids(Card, Ids),
		member(Id1, Ids), 		
		Id is Id1 + 1, State = [],	
		append([Id], Ids, New_ids),	
		retractall(card_ids(Card, _)),
		assert(card_ids(Card, New_ids)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Tabulate             %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%tabulate/4 17/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* tabulate(+Title, +Header, +Formatting, +Spacing, +Data, ) */
	% Output data in tabular format, 

	% Top goal; output table Title and Header
	tabulate(Title, Header, Formatting, Data):- 
		title(Title, Formatting), 
		header(Header, Formatting),
		tabulate(Data, Formatting).


%%%%%%%%%%%%%%title/2 17/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* header(+Title, +Formatting) */
	% Print out the title 

%%%%%%%%%%%%%%title/2 (1) 17/02/11

	title(Title, [L, S, R]):- 
		tab(L), 
		(member([Line], Title);	member(Line, Title)), 
		write(Line), 
		fail. 

%%%%%%%%%%%%%%title/2 (1) 17/02/11

	title(_, [_L, _S, R]):- write(`: `), tab(R), nl.


%%%%%%%%%%%%%%header/2 17/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* header(+Header, +Formatting) */
	% Print out the header

%%%%%%%%%%%%%%header/3 (1) 17/02/11

	% Print everything. 
	header(Header, [L, _S, R]):-
		member([Line], Header),
		tab(L), write(Line), tab(R), nl,
		fail.
	
%%%%%%%%%%%%%%header/3 (0) 17/02/11

	% Done. 
	header(_, _).
	

%%%%%%%%%%%%%%tabulate/2 17/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* tabulate(+Data, +Spacing) */
	% Business clauses. 

%%%%%%%%%%%%%%tabulate/2 (1) 17/02/11

	% Data is a list of tuples: functor(Arg1, Arg2...)
	tabulate([Datum | Rest], [L, S, R]):- 
		type(Datum, 7), Datum =.. [_ | Record], % Datum is a tuple
		tab(L),
		findall(Entry, 
				(member(Entry, Record), 
				len(Entry, Length),
				Tab is S - Length,
				write(Entry), tab(Tab), fail),
			_Entries), 
		tab(R), nl,
		tabulate(Rest, [L, S, R]). 

%%%%%%%%%%%%%%tabulate/2 (2) 17/02/11

	% Data is a list of lists: [El1, El2...]
	tabulate([Datum | Rest], [L, S, R]):- 
		type(Datum, 6), Datum = Record, % Datum is a List
		tab(L),
		findall(Entry, 
				(member([Entry], Record), %; member(Entry, Record),
				% ^ Separate brackets from Entries
				len(Entry, Length),
				Tab is S - Length,
				write(Entry), tab(Tab), fail),
			_Entries), 
		tab(R), nl,
		tabulate(Rest, [L, S, R]). 

%%%%%%%%%%%%%%tabulate/2 (3) 17/02/11

	% Data is a list of atoms or strings: [El1, El2...]
	tabulate([Datum | Rest], [L, S, R]):- 
		(type(Datum, 3); type(Datum, 4)), 
		% Datum is an atom	or a string 
		tab(L),
		findall(Datum, 
				(len(Datum, Length),
				Tab is S - Length,
				write(Datum), tab(Tab), fail),
			_Entries), 
		tab(R), nl,
		tabulate(Rest, [L, S, R]). 

%%%%%%%%%%%%%%tabulate/2 (0) 17/02/11	

	% When all else fails, we're done.
	tabulate(_, _Formatting).















		