% First saved: 22/01/2011
% Last saved:  20/02/2011
% 
%	Incremental save
% 
% 	Status: 
%	Should be OK
% 
% Doing:
%	Done adding player states.
%	Done fixing take_from_pool
%		added match_cost as draw_mana.
% Todo: 
%	string_to_list: check notes.
% NOTES: 
%	In multiplayer games, a team is needed.	
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          Initialisation           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic 
	mana_pool/2, 
	life_total/2,
	poison_counters/2, 
	hand_size/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Player Facts          %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%player/1 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* player(+Name) */

	% Who is a player?
	player('Player 1').
	player('Player 2').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%order_of_play/1 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* order_of_play(+Players) */

	% All the players, in turn from first to last
	order_of_play(['Player 1', 'Player 2']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%match_score/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* match_score(+Player, +Matches_won) */

	% Index numbers of matches won by player
	match_score('Player 1', []).
	match_score('Player 2', []).
	% The game should check how many matches have been won
	% And also assign index numbers to matches. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%life_total/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* life_total(+Player, +Life). */

	% Life totals
	life_total('Player 1', 20).
	life_total('Player 2', 20).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% poison_counters/2 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* poison_counters(+Player, +Poison). */

	% Poison counters
	poison_counters('Player 1', 0).
	poison_counters('Player 2', 0).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%hand_size/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* hand_size(+Player, +Hand) */

	% Hand sizes
	hand_size('Player 1', 7).
	hand_size('Player 2', 7).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%mana_pool/2 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_pool(Player, Mana) */
	% Where Mana == [Colourless, White, Blue, Black, Red, Green]

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Mana quantities should be of the type: 
	 <number> (colourless) or
	 w (white) or
	 u (blue) or
	 b (black) or
	 r (red) or
	 g (green)
	So for example, the cost two, red, black, black
	should look like: 
	  2rbb
	This should then be turned into a list: 
	  [2, r, b, b]
	and finally a mana_quantity: 
	  [c-2, w-0, u-0, b-2, r-1, g-0]
	Yep, those are key-value pairs.
*/	

	% Mana Pools
	mana_pool('Player 1', [c-0, w-0, u-0, b-0, r-0, g-0]).
	mana_pool('Player 2', [c-0, w-0, u-0, b-0, r-0, g-0]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%player_state/2 27/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* player_state(+Player, +State) */

	player_state('Player 1', []).
	player_state('Player 2', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Player Rules            %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%empty_mana_pool/ 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* empty_mana_pool(+Player) */
	% Empties a player's mana pool.

	empty_mana_pool(Player):-
		mana_pool(Player, Mana_pool),
		retractall(mana_pool(Player, Mana_pool)),
		assert(mana_pool(Player, [c-0, w-0, u-0, b-0, r-0, g-0])).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% add_to_pool/4 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* add_to_pool(+Player, +Mana, -New_pool)  */
	% Adds a quantity of mana to a player's mana pool.

	% eg, enter: add_to_pool('Player 1', Mana_pool, '2rrbg', New_pool).
	add_to_pool(Player, Mana, New_pool):-
		player(Player),
		mana_pool(Player, Mana_pool),
		string_to_list(Mana, List),
		add_mana(List, Mana_pool, New_pool), 
		retractall(mana_pool(Player, Mana_pool)),
		assert(mana_pool(Player, New_pool)).


%%%%%%%%%%%%%% add_mana/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* add_mana(Mana_list, Mana_pool, New_pool) */
	% Adds the mana to the player's pool. 

%%%%%%%%%%%%%%add_mana/3 (1) 21/01/11

	% Boundary condition...
	add_mana([], New_pool, New_pool).

%%%%%%%%%%%%%%add_mana/3 (2) 21/01/11

	% For each symbol in the mana list, add 1 to the appropriate value
	%  in the mana pool. Numbers (representing colourless mana) are 
	%  added wholesale. 
	add_mana([Symbol | Rest], [c-C, w-W, u-U, b-B, r-R, g-G], New_pool):-
		(number(Symbol) -> C1 is C + Symbol; C1 is C),
		(Symbol == w -> W1 is W + 1; W1 is W),
		(Symbol == u -> U1 is U + 1; U1 is U),
		(Symbol == b -> B1 is B + 1; B1 is B),
		(Symbol == r -> R1 is R + 1; R1 is R),
		(Symbol == g -> G1 is G + 1; G1 is G),  
		remove(Symbol, [Symbol | Rest], Rest), 
		add_mana(Rest, [c-C1, w-W1, u-U1, b-B1, r-R1, g-G1], New_pool),!.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% take_from_pool/4 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* take_from_pool(+Player, +Mana, -New_pool)  */
	% Draws a quantity of mana from a player's mana pool.
	% Fails disgracefully if more Mana is attempted to be drawn than 
	%  available.

	% eg, enter: take_from_pool('Player 1', '2rrbg', New_pool).
	take_from_pool(Player, Mana, New_pool):-
		player(Player),
		mana_pool(Player, Mana_pool),
		string_to_list(Mana, List),
		(
			(
				draw_mana(Mana_pool, List, New_pool) -> 
				retractall(mana_pool(Player, Mana_pool)),
				assert(mana_pool(Player, New_pool))
			); 
			New_pool = Mana_pool, 
			output(draw_mana, [Player, Mana_pool]), 
			fail, !
		).


%%%%%%%%%%%%%%draw_mana/3 21/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* draw_mana(Mana_pool, +Mana_list, -Remaining) */
	% draws mana from a mana pool and returns the new pool.

%%%%%%%%%%%%%%draw_mana/3 (0) 21/02/11

	% Whole amount drawn. 
	draw_mana(Mana_pool, Mana_list, Mana_pool):- 
		(Mana_list = [0] ; Mana_list = []). 

%%%%%%%%%%%%%%draw_mana/3 (1) 21/02/11

	% Match coloured mana in the Mana_list to coloured mana
	%  in the Mana_pool 
	draw_mana(Mana_pool, Mana_list, Remaining):- 
		Mana_pool = [c - C, w - W, u - U, b - B, r - R, g - G],
		member(Symbol, Mana_list),
		\+ number(Symbol), % A coloured mana symbol
		member(Symbol - Amount, Mana_pool),		
		Amount >= 1, % not sure if needed explicitly
		(
			((Symbol == w -> W1 is W - 1, W1 >= 0) ; W1 is W),
			((Symbol == u -> U1 is U - 1, U1 >= 0) ; U1 is U),
			((Symbol == b -> B1 is B - 1, B1 >= 0) ; B1 is B),
			((Symbol == r -> R1 is R - 1, R1 >= 0) ; R1 is R),
			((Symbol == g -> G1 is G - 1, G1 >= 0) ; G1 is G)	
		),
		New_mana_pool = [c - C, w - W1, u - U1, b - B1, r - R1, g - G1], 
		remove(Symbol, Mana_list, New_mana_list), !, %red cut. Trace it!
		draw_mana(New_mana_pool, New_mana_list, Remaining).

%%%%%%%%%%%%%%draw_mana/3 (2) 21/02/11

	% Match colourless mana symbols in Mana_list to coloured mana symbols
	%  in Mana_pool 
	draw_mana(Mana_pool, Mana_list, Remaining):- 
		Mana_pool = [c - C, w - W, u - U, b - B, r - R, g - G],
		member(Symbol_2, Mana_list), 
		number(Symbol_2),  % There's only colourless mana left in Mana_list
		member(Symbol - Amount, Mana_pool),
		Symbol \= c, % Don't use colourless mana yet
		Amount >= 1, 
		(
			((Symbol == w -> W1 is W - 1, W1 >= 0) ; W1 is W),
			((Symbol == u -> U1 is U - 1, U1 >= 0) ; U1 is U),
			((Symbol == b -> B1 is B - 1, B1 >= 0) ; B1 is B),
			((Symbol == r -> R1 is R - 1, R1 >= 0) ; R1 is R),
			((Symbol == g -> G1 is G - 1, G1 >= 0) ; G1 is G)	
		),
		New_mana_pool = [c - C, w - W1, u - U1, b - B1, r - R1, g - G1], 
		New_mana_list is Symbol_2 - 1, 
		draw_mana(New_mana_pool, [New_mana_list], Remaining).	

%%%%%%%%%%%%%%draw_mana/3 (3) 21/02/11

	% Match colourless mana symbols in Mana_list to colourless mana 
	%  symbols in Mana_pool 
	draw_mana(Mana_pool, Mana_list, Remaining):- 
		Mana_pool = [c - C, w - W, u - U, b - B, r - R, g - G],	
		findall(Symbol_2, 
			(member(Symbol_2, Mana_list), 
			\+ number(Symbol_2)), Symbols), 
		Symbols = [], % no more coloured mana symbols in the Mana_list
		member(Symbol - Amount, Mana_pool),
		Symbol = c,
		Amount >= 1, 
		C1 is C - 1,
		New_mana_pool = [c - C1, w - W, u - U, b - B, r - R, g - G], 		
		member(Symbol_3, Mana_list), 
		New_mana_list is Symbol_3 - 1, New_mana_list >= 0,
		draw_mana(New_mana_pool, [New_mana_list], Remaining). 	


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% string_to_list/2 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  string_to_list(+Atom, -List) */
	% Takes in a string of mana symbols and returns it in list form
	%  separating numbers from non-numbers.

	% If there are no numbers in the string. 
	string_to_list(Atom, List):-
		name(Atom, Full_list), 
		separate_numbers(Full_list, Numbers, Characters),
		Numbers == [],	% check here so as not to bind prematurely.
		to_list(Characters, _Temp_list, To_list),  
		reverse(To_list, List), !.
	
	% If there are numbers in the string. 
	string_to_list(Atom, List):-
		name(Atom, Full_list), 
		separate_numbers(Full_list, Numbers, Characters),
		to_list(Characters, _Temp_list, To_list), 
		reverse(To_list, Ordered_chars), 
		append([Numbers], Ordered_chars, List), !.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	a) to_list clause removed: is also in predicates.pl
	b) string_to_list (and others) should use atom_chars etc, 
	  rather than name/2, apparently. 
*/


%%%%%%%%%%%%%% separate_numbers/3 19/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* separate_numbers(+List, -Numbers, -Characters) */
	% Takes in a list and returns a number and a list of char codes

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
 	This is rather sloppy and lazy- it returns all digits in the atom
	  as one number, regardless of their position in the atom! Obviously 
	  this opens the door for amusing errors. It should be fixed,
	  but currently I can't be bovvered. Mana should be always coming in as 
	  strings of exactly one number at the front and any number of symbols. 
	  So if everything else does its job as it should, this should cause no
	  trouble. 
*/


	% Top-level goal (leave out don't-care vars.
	separate_numbers(List, Numbers, Characters):-
		separate_numbers(List, _, Numbers, _, Characters).

	% If there are no numbers in the string. 
	separate_numbers([], [], [], Other_chars, Chars):-
		reverse(Other_chars, Chars).

	% Some reversing needed to trim out undef'ed vars and reorder
	%  things properly.
	separate_numbers([], Numbers, Atomic_numbers, Other_chars, Chars):-
		reverse(Numbers, Reversed),
		name(Atomic_numbers, Reversed), 
		reverse(Other_chars, Chars). 

	% While the next char code is that of a number, add it to the list
	%  of digits. 
	separate_numbers([Char | Rest], List, Numbers, Other, Chars):-
		is_num(Char), 
		separate_numbers(Rest, [Char | List], Numbers, Other, Chars), !.

	% While the next char code is not that of a number, keep it separate. 
	separate_numbers([Char | Rest], List, Numbers, Other, Chars):-
		not is_num(Char),
		separate_numbers(Rest, List, Numbers, [Char | Other], Chars), !.

	% Number char codes.  
	is_num(Character) :-
		Character =< 0'9,
		Character >= 0'0.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%gain_life/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* gain_life(+Player, +Life, -New_life_total) */
	% adds to a player's life total.

	gain_life(Player, Life, New_total):-
		life_total(Player, Life_total),
		New_total is Life_total + Life,
		retractall(life_total(Player, Life_total)),
		assert(life_total(Player, New_total)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%lose_life/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* lose_life(+Player, +Life, -New_life_total) */
	% reduces a player's life total.

	lose_life(Player, Life, New_total):-
		life_total(Player, Life_total),
		New_total is Life_total - Life,
		retractall(life_total(Player, Life_total)),
		assert(life_total(Player, New_total)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%gain_poison/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* gain_poison(+Player, +Poison, -Poison_counters) */
	% Gives poison counters to a player

	gain_poison(Player, Poison, New_counters):-
		poison_counters(Player, Poison_counters),
		New_counters is Poison_counters + Poison,
		retractall(poison_counters(Player, Poison_counters)),
		assert(poison_counters(Player, New_counters)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%increase_hand_size/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* increase_hand_size(+Player, +Increase_by, -Hand_size) */
	% Increases a player's hand size.

	increase_hand_size(Player, Increase, New_size):-
		hand_size(Player, Size),
		New_size is Size + Increase, 
		retractall(hand_size(Player, Size)),
		assert(hand_size(Player, New_size)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%decrease_hand_size/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* increase_hand_size(+Player, +Increase_by, -Hand_size) */
	% Decreases a player's hand size.

	decrease_hand_size(Player, Increase, New_size):-
		hand_size(Player, Size),
		New_size is Size - Increase, 
		retractall(hand_size(Player, Size)),
		assert(hand_size(Player, New_size)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%save_player 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  save_player(+Player, -Saved) */
	% Returns the player's current state, ie cards in all zones
	%  and all Attribute values (life, mana, poison and max hand size)

	save_player(Player, Saved):- 
		findall(Zone, zone(Player, Zone, _), Zones), 
		Attributes = [life_total, mana_pool, hand_size, poison_counters],
		save_zones(Player, Zones, [], Saved_zones), 
		save_attributes(Player, Attributes, [], Saved_attributes), 
		append([Saved_zones], [Saved_attributes], Saved), !.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Note that "save" is a bit of a misnomer. This predicate keeps 
	  the appropriate values safe, while other predicates modify 
	  the player's state, then if the player needs to be reverted 
	  to a previous state, it feeds the Saved information to 
	  restore_player/2. 
	However, nothing is actually written in the database and so
	  nothing "saved" in the traditional sense of assigning a value
	  to a variable (and possibly saving it to a file). But then, this 
	  is Prolog! Maybe this should be named "get", but this I fear will
	  mix up the semantics something horrible with getter/setter methods. 
	Another predicate can later use this (and others) to save a copy 
	  of a game in a separate file. 
*/


%%%%%%%%%%%%%%save_zone/4 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* save_zones(+Player, Zones, [], Saved) */
	% Collects information about the Player's cards in all zones

%%%%%%%%%%%%%%save_zone/4 (0) 10/02/11
	save_zones(Player, [], Zones, Zones).

%%%%%%%%%%%%%%save_zone/4 (1) 10/02/11
	save_zones(Player, [Zone | Rest], Temp, Zones):- 
		player(Player),
		zone(Player, Zone, Cards),
		append(Temp, [zone(Player, Zone, Cards)], New_temp),
		save_zones(Player, Rest, New_temp, Zones).

	% This needs to save the shared zones also! 	  

%%%%%%%%%%%%%%save_attributes/4 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* save_zones(+Player, Attributes, [], Saved) */
	% Collects information about the Player's Attributes 
	%  ie: life_total, mana_pool, hand_size and poison_counters

%%%%%%%%%%%%%%save_attributes/4 (0) 10/02/11

	save_attributes(Player, [], Attrs, Attrs).

%%%%%%%%%%%%%%save_zone/4 (1) 10/02/11

	save_attributes(Player, [Attr | Rest], Temp, Attrs):- 
		player(Player),
		Attr(Player, Value),
		 append(Temp, [Attr(Player, Value)], New_temp),
		save_attributes(Player, Rest, New_temp, Attrs).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%restore_player/2 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_player(+Player, +Save) */
	% Restores the player's state, ie cards in all zones and all 
	%  Attribute values to a previous State (returned by save_player/2).

	restore_player(Player, Saved):- 
		Saved = [Saved_zones, Saved_attributes], 
		restore_zones(Player, Saved_zones), 
		restore_attributes(Player, Saved_attributes).


%%%%%%%%%%%%%%restore_zones/4 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_zones(+Player, +Zones) */
	% Restores the player's zones to a previous state.

%%%%%%%%%%%%%%restore_zones/2 (0) 10/02/11

	restore_zones(Player, []).

%%%%%%%%%%%%%%restore_zones/2 (1) 10/02/11

	restore_zones(Player, [Zone | Rest]):-
		Zone = zone(Player, Name, Cards),
		retractall(zone(Player, Name, _)), 
		asserta(Zone), 
		restore_zones(Player, Rest).


%%%%%%%%%%%%%%restore_attributes/4 10/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* restore_attributes(+Player, +Attributes) */
	% Restores the player's Attributes to a previous state.

%%%%%%%%%%%%%%restore_attributes/2 (0) 10/02/11

	restore_attributes(Player, []).

%%%%%%%%%%%%%%restore_attributes/2 (1) 10/02/11

	restore_attributes(Player, [Attr | Rest]):-
		Attr =.. [Name, Player, Value], 
		retractall(Name(Player, _)), 
		asserta(Attr), 
		restore_attributes(Player, Rest).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Multiplayer predicates: 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%team/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* team(Number, Players) */

	% Multiplayer teams
%	team(1, ['Player 1', 'Player 2']).
%	team(2, ['Player 3', 'Player 4']).
	% No use currently. 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%order_of_play/1 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% Multiplayer order of play lists teams rather than players:
%	order_of_play([1, 2]).
	% Uncomment multi/two-player version accordingly. 


/* WORKING:

	name_string(String, Chars, List):-
		name(String, Chars),
		mana_string_to_list(Chars, _, Mana_list), 
		reverse(Mana_list, List), !.

	mana_string_to_list([], Mana_list, Mana_list).
	mana_string_to_list([Head|Tail], Temp_list, Actual_list):-
		name(Back_to_symbol, [Head]),
		mana_string_to_list(Tail, [Back_to_symbol | Temp_list], Actual_list).


*/




/*
%%%%%%%%%%%%%% draw_mana/3 21/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	 draw_mana(Mana_list, Mana_pool, New_pool) 
	% Takes the mana from the player's pool. 

% OK, this is wrong- it only draws colourless from the c-C pair. 
% It should draw from any available. 

%%%%%%%%%%%%%%add_mana/3 (1) 21/01/11

	% Boundary condition...
	draw_mana([], New_pool, New_pool).

%%%%%%%%%%%%%%add_mana/3 (2) 21/01/11

	% For each symbol in the mana list, reduce by 1 the appropriate value
	%  in the mana pool. Numbers (representing colourless mana) are 
	%  subtracted wholesale. 
	draw_mana([Symbol | Rest], [c-C, w-W, u-U, b-B, r-R, g-G], New_pool):-
		((number(Symbol) -> C1 is C - Symbol, C1 >= 0) ; C1 is C),
		((Symbol == w -> W1 is W - 1, W1 >= 0) ; W1 is W),
		((Symbol == u -> U1 is U - 1, U1 >= 0) ; U1 is U),
		((Symbol == b -> B1 is B - 1, B1 >= 0) ; B1 is B),
		((Symbol == r -> R1 is R - 1, R1 >= 0) ; R1 is R),
		((Symbol == g -> G1 is R - 1, G1 >= 0) ; G1 is G),
		draw_mana(Rest, [c-C1, w-W1, u-U1, b-B1, r-R1, g-G1], New_pool),!.
	% Fails if more mana is drawn than available.

*/




