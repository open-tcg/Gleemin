% First saved: 08/02/11
% Last saved:  26/02/11
%
% 	Status: 
%	cut panic in cast_spell
%
%	play_land is: 100%
%	cast_spell: 80% 
%	activate_ability: 50%
%
% Doing:
%	redefine mana_cost and mana_string in terms of the MGL interpreter.
%	activate_abilities
% 	Done cleanups
% Todo
%	redefining choose_spell
%	add warning in tap_for_mana that a permanent is already tapped
%	input/output for cast_spells & activate_abilities.
%		Done for cast_spells (except missing stuff of course)
%	Complete pay_cost (to cover all possible costs) (use DCG?)
% NOTES:
%	Once finished redefining all: 
% 	bring cast_spell in line with mgl_interpreter.pl
%		changing order of clauses 
%		adding choose_targets
%		change determine_abilities 
%		add make_choices


		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      Player actions facts         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- dynamic played_land/2.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%played_land/2 29/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* played_land(+Player, +Yes/No) */
	% Remembers whether a player has played a land this turn

	played_land('Player 1', 'no').
	played_land('Player 2', 'no').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%      Player actions rules         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%play_land/4 11/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  play_land(+Player, +Step, +Phase, -Play) */
	% Checks that it's legal to put a land into play, does so if it is
	%  and returns an appropriate Play character
	

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Some of the tests are redundant, since the option to play land is
	  only presented in legal situations. However, this may catch illegal
	  "[l]and" input. Anyway, it's extra safety. 
*/

%%%%%%%%%%%%%%play_land/3 (1) 29/01/11

	% Is this the player's turn?
	play_land(Active_player, Priority_player, _Step, _Phase, 99):-
		Active_player \== Priority_player,
		output(play_land, ['wrong_turn']).

%%%%%%%%%%%%%%play_land/3 (2) 29/01/11

	% Is this a main phase?
	play_land(_Active_player, _Priority_player, _Step, Phase, 99):-
		( 
			not Phase == 'First Main',
			not Phase == 'Second Main'
		),
		output(play_land, ['wrong_phase']).

%%%%%%%%%%%%%%play_land/3 (3) 29/01/11

	% Has the player already played a land this turn?
	play_land(_Active_player, Priority_player, _Step, _Phase, 99):-
		played_land(Priority_player, 'yes'),
		output(play_land, ['land_limit']).

%%%%%%%%%%%%%%play_land/3 (4) 29/01/11

	% Is the stack empty? 
	play_land(_Active_player, _Priority_player, _Step, Phase, 99):-
		not zone('Stack', []),
		output(play_land, ['stack_not_empty']).

%%%%%%%%%%%%%%play_land/3 (5) 29/01/11

	% List the land in the player's hand
	play_land(_Active_player, Priority_player, _Step, _Phase, Play):-
		zone(Priority_player, 'Hand', Cards),
		findall(Card, (member(Card, Cards), 
		card([card_name Card, _, type_line Type, _, _, _, _, _]),
			% Type == [['Basic'], ['Land'], [Card]]; 
			Type = [_, ['Land'], _]
		),					
			Land),  
		sort(Land, Sorted), 
		append(['cancel'], Sorted, Full), 
		prompt_switches(Full, Switches), 
		play_land(Priority_player, Switches, Play).


%%%%%%%%%%%%%%play_land/2 04/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* play_land(+Player, +Land, -Play) */
	% Puts a land in play, or cancels; returns the Play character

%%%%%%%%%%%%%%play_land/2 (1) 04/02/11

	% The player has no lands in hand. 
%	play_land(Player, [], Play):-
%	play_land(Player, [_, _|[]], 99):-	
	% ^ Contents of Land are: [[cancel - c],['[c]ancel']]
	play_land(Player, [[cancel - c],['[c]ancel']], 99):-	
		output(play_land, ['no_lands_in_hand']).
	
%%%%%%%%%%%%%%play_land/2 (2) 04/02/11

	% Move the land card from the player's hand to the player's
	%  side of the battlefield. 
	play_land(Player, [Map, Switches], Input):-
		Context = [Player, Map, Switches],
		input(play_land, Context, Input) -> 
		(
			Input == 99, true; 	% Cancel
			move_land(Map, Input) 
		); 
		play_land(Player, [Map, Switches], Input).
		% ^ Go back if the input was wrong.

%%%%%%%%%%%%%%move_land/2 (2) 04/02/11

	% Choose the right land card to move
	move_land(Map, Input):-
		atom_chars(Switch, [Input]), 
		member(Land_card - Switch , Map),
		move_to_zone(Player, Land_card, 'Hand', 'Battlefield'), 
		retractall(played_land(Player, _)),
		asserta(played_land(Player, 'yes')),
		output(play_land, ['played_land', Player, Land_card]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%cast_spell/3 15/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* cast_spell(+Player, -Action) */
	% Cast a spell from the Player's Hand or another Zone
	% Will need to check for timing restrictions (Step/Phase). Send it
	%  as 2nd Arg: Context = [Active, Priority, Step, Phase]!


	cast_spell(Player, Action):-
		save_player(Player, Saved_state),
		choose_spell(Player, 'Hand', Spell, Text_box), 
		% ^ Leave Zone undefined to backtrack into more zones
		(
			Spell = [], Action = 99;
			move_to_zone(Player, Spell, 'Hand', 'Stack'),
%			determine_abilities(Rules, [], Abilities), 
%			make_choices(Player, Name, Ability, Choices),
			zone('Stack', [Object | _rest]), 
			choose_targets(Player, Object, Text_box, _Targeted), 
			determine_cost(Spell, _Abilities, Cost),
			(
				pay_cost(Cost, Player, Spell) ->
				% removed ^^ brackets from here, after changing
				%  determine_cost. Keep an eye out. 
				Action = 115, 
				output(cast_spell, ['spell_cast', Player, Spell, 'Hand']);
				restore_player(Player, Saved_state), 
				Action = 99, 
				output(cast_spell, ['spell_fails', Player, Spell, 'Hand'])
			)
		),!. % Green cut but revise (changing pay_cost/3)

		% The player has chosen to cancel, 
		% _or_ the casting has failed. 
		cast_spell(Player, 99).
		% Kinda fugdy- you get here in many different ways. 
		%  Specifically if there are no spells in hand?


%%%%%%%%%%%%%%choose_spell/4 06/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_spell(+Controller, ?Zone -Spell, -Rules_text) */
	% Determines the Spell to be cast, and returns its Rules text
	%  and possibly the Zone it is in 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	If the Zone is not instantiated, the spell is in a Zone other than 
	  the currently topmost zone in the database, the 
	  'Input == 111, fail, !' clause in choose_spell/3 will cause 
	  choose_spell/3 to backtrack into a new zone/3 and find a new 
	  set of cards. That is a feature- the player may indeed be looking 
	  for a spell  in that Zone. 
	However, most spells will only be castable from the Hand
	  so I should be checking that a spell is in the right zone
	  to be cast from. 
	I'll implement this when I get to a card that requires it. Currently
	  it just fails (when called from cast_spell).
*/

	% Ask the Player which Spell to play
	choose_spell(Player, Zone, Spell, Rules):- 
		zone(Player, Zone, Cards),
		findall(Spell, (member(Spell, Cards), 
		card([card_name Spell, _, type_line Type, _, _, _, _, _]),
			Type \= [_, ['Land'], _]
		), Hand),  
		sort(Hand, Sorted), 
		(
			Sorted \= [], 
			append(['other', 'cancel'], Sorted, Full),
			prompt_switches(Full, Switches), 
			%^ Switches is really: [Map, Switches]!
			choose_spell([Switches, Player], Spell, Rules);
			choose_spell([no_spells, Player, Zone], Spell, Rules)
			% ^ No spells in hand...
		).
		% append(['other', 'cancel'], Hand, Full), 
		% ^ When I sort and then append, I don't see my whole hand, obviously.
		% I can add a number at the end of each card name as in: [C]ard(3)
		%  to inform the player of how many copies of the card are in Hand.
		% But not now. 


%%%%%%%%%%%%%%choose_spell/3 06/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_spell(Context, Input, Abilities) */
	% Handles input and output for parent, choose_spell/4

	% There are no spells in the player's hand. 
	choose_spell([no_spells, Player, Zone], [], []):-
		output(cast_spell, [no_spells, Player, Zone]).

	% Ask the Player which Spell to play
	choose_spell([[Map, Switches], Player], Spell, Rules):-
		Context = [Map, Switches], %also Player? For "P1 plays Sp1"?
		% Switches = [Cancel | [Other | Rest] ], %check rigorously
		input('cast_spell', Context, Input) ->  
		(
			Input == 99, Spell = [], Rules = []; %true; % cancel
			Input == 111, fail, !; % other
			atom_chars(Switch, [Input]), 
			member(Spell - Switch , Map),
			card([card_name Spell, _, _, text_box Rules, _, _, _, _])
		);
		choose_spell([[Map, Switches], Player], Spell, Rules).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%activate_ability/3 07/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  activate_ability(+Controller, +Object, +Zone) */	
	% Activates one ability of an Object in Zone
	% Check stack_object for details on what needs to change...

%%%%%%%%%%%%%%activate_ability/3 (1) 15/02/11

	% Deals with one ability (of one permanent) at a time. 
	activate_ability(Player, Permanent, Action):-
		save_player(Player, Saved_state),
%		choose_ability(Player, 'Hand', Spell, Text_box), 
		% ^ Leave Zone undefined to backtrack into more zones
		choose_ability(Player, Permanent, Ability),
		(
			(% No Ability chosen
			Ability = [], Action = 99;

			% A mana ability
			Permanent = copy(Name, _Id, _State),
			mana_ability(Name, Ability, Mana),
			determine_cost([], Ability, Cost),
			%tap_untap(Permanent, Player, 'tap'),
			% ^ fails disgracefully if the Permanent is tapped already
			add_to_pool(Player, Mana, _New_pool);
			% ^ If payment fails, this should be retracted by 
			%  restore_player

			% Not a mana ability
			create_in_zone(Player, Ability, 'Stack'),
%			determine_abilities(Rules, [], Abilities), 
%			make_choices(Player, Name, Ability, Choices),
			zone('Stack', [Object | _rest]), 
			choose_targets(Player, Object, Ability, Targeted), 
			determine_cost([], Ability, Cost)
		),
			(
				pay_cost(Cost, Player, Permanent) ->
				Action = 115; 
				%output/2 call
				restore_player(Player, Saved_state), 
				Action = 99 
				%output/2 call
			)
		),!. % Green cut but revise (changing pay_cost/3)

%%%%%%%%%%%%%%activate_ability/3 (1) 15/02/11

	% The player has chosen to cancel, _or_ the ability activation has failed. 
	activate_ability(_Player, _Permanent, 99).


%%%%%%%%%%%%%%choose_ability/2 06/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_ability(+Controller, +Permanent, -Ability) */
	% Takes in a Permanent's name, Id and Controller and 
	%  returns the player's chosen ability to activate 

	% Which ability to activate?
	choose_ability(Player, Object, Ability):-
		Object = copy(Name, Id, State), 
		zone(Player, 'Battlefield', Permanents), % Player's permanents
		member(copy(Name, Id, State), Permanents), % card copy
		card([card_name Name, _, _, text_box Abilities, _, _, _, _]), 
		% ^ the card's abilities
		ask_player(Abilities, Ability), %which ability?
		member(Ability, Abilities). % verify it's on the card!



%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*	choose_ability(Player, [Name, Id], Ability):-
		write(`That does not seem to be an ability of `), 
		write(Name), write(`!`), nl, fail, !.
*/	


%%%%%%%%%%%%%%ask_player/2 06/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% TEMPORARY!. Use input/3 to prompt the player
	ask_player(Abilities, Ability):- 
		write(`Please choose from: `), nl,  
		write(Abilities), nl,
		read(Ability), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%pay_cost/3 06/02/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* pay_cost(+Cost, +Controller, +Permanent) */
	% Pay the Cost for an activated ability of a Permanent or to cast 
	%  a spell. If paying a cost fails, all payments are undone

%%%%%%%%%%%%%%pay_cost/3 (0) 20/02/2011

	pay_cost([], Player, Permanent):- !.

%%%%%%%%%%%%%%pay_cost/3 (1) 20/02/2011

	% The cost is to tap the permanent
	pay_cost(['tap' | Rest], Player, Permanent):-
		tap_untap(Permanent, Player, 'tap'),
		pay_cost(Rest, Player, Permanent).

	pay_cost(['tap' | _Rest], _Player, Permanent):-
		output(pay_cost, [Permanent, 'tap_failed']), 
		fail, !.

%%%%%%%%%%%%%%pay_cost/3 (2) 20/02/2011

	% The cost is to untap the permanent
	pay_cost(['untap' | Rest], Player, Permanent):-
		tap_untap(Permanent, Player, 'untap'),
		pay_cost(Rest, Player, Permanent).

	pay_cost(['untap' | _Rest], _Player, Permanent):-
		output(pay_cost, [Permanent, 'untap_failed']), 
		fail, !.

%%%%%%%%%%%%%%pay_cost/3 (2) 20/02/2011

	% The cost is a mana cost
	pay_cost([Cost | Rest], Player, Object):-
		mana_cost(Cost),
		pay_mana_cost(Cost, Rest, Player, Object), 
		pay_cost(Rest, Player, Object). 


	pay_mana_cost(Cost, Rest, Player, Object):-
		mana_sources(Player, Switches, Identified), %!,
		tap_for_mana(Player, Switches, Identified), !,
		spend_mana(Player, Cost, Mana), !,
		% ^ Green cuts; stop from backtracking uselessly on a 
		% [c]ancel or [s]kip input. 
		take_from_pool(Player, Mana, _New_pool), !,
		pay_cost(Rest, Player, Object). 

	pay_mana_cost(_Cost, _Rest, _Player, Object):-
		output(pay_cost, [Object, 'mana_failed']), 
		fail, !.


%%%%%%%%%%%%%%mana_cost/3 07/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_cost(+Cost) */
	% Checks that a cost is a mana cost.

	% A mana cost is a (non-negative) number, 
	%   optionally followed by a string of mana symbols. 
	mana_cost(Cost):-
		string_to_list(Cost, [First | Rest]),
		((number(First), First >= 0, 
		mana_string(Rest));
		mana_string([First | Rest])).


%%%%%%%%%%%%%%mana_string/1 07/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_string(+String) */
	% Checks whether an atom is a string of mana symbols.

	% A mana string is a string of mana symbols. 
	mana_string(Rest) :-
		\+ (
			member(Symbol, Rest),
			Symbol \== w,
			Symbol \== u,
			Symbol \== b,
			Symbol \== r,
			Symbol \== g
		).


%%%%%%%%%%%%%%mana_sources/2 22/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_sources(+Player, -Switches, -Identified) */
	% Returns the mana sources on the Player's side of the battlefield
	%  Identified is a list of [Permanent - Switch, Id]'s

	mana_sources(Player, Switches, Identified):-
		findall([Permanent, Id], (zone(Player, 'Battlefield', Cards), 
			member(copy(Permanent, Id, _State), Cards), 
			mana_ability(Permanent, _Ability, _Mana)),
			Sources), %write(Sources), nl,
		findall(Permanent, member([Permanent , Id], Sources), Permanents), 
		append(['cancel', 'skip'], Permanents, Full), 
		prompt_switches(Full, Switches), %write(Switched), nl,
		Switches = [Map, _SwitcheS],
		identify_copy(Map, Sources, 2, [], Identified).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	In the call to identify_copy/4
	Maybe I can remove non-cancel/skip key-value pairs from Map
	  append Identified [key - value, Id] lists in their place and
	  send it all on... but it may cause problems further down?
	 Currently I'm working with two lists, Map and Identified, in 
	  subsequent calls and it's a bit confusing. 
*/


%%%%%%%%%%%%%%identify_copy/4 22/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* identify_copy(+Map, +Copies, +Padding, [], -Identified) */
	% Matches card copies and their Ids to a map of Card - Switch 's.
	% Returns a list of [Name - Switch, Id]'s used to identify card 
	%  copies for user input/output. 
	% Padding is the number of additional options added to the Map of 
	%  Card - Switch, eg. [c]ancel, etc. 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	There doesn't seem to be a problem with non-land cards on the battlefield.
	  Those don't get reported by mana_sources anyway. 
*/

%%%%%%%%%%%%%%tap_for_mana/3 (0) 21/02/2011
	
	identify_copy(Map, [], Padding, Identified, Identified).

%%%%%%%%%%%%%%tap_for_mana/3 (0) 21/02/2011

	identify_copy(Map, Copies, Padding, Temp, Identified):- 
		member([Copy, Id], Copies, Copies_position),
		Map_position is Copies_position + Padding, 
		member(Copy - Switch, Map, Map_position),
		append(Temp, [[Copy - Switch, Id]], New_temp), 
		remove(Copy - Switch, Map, New_map),
		remove([Copy, Id], Copies, New_copies),
		identify_copy(New_map, New_copies, Padding, New_temp, Identified).

%%%%%%%%%%%%%%tap_for_mana/3 21/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* tap_for_mana(+Player, [+Map, +Switches]) */
	%  Prompts the player to tap any number of permanents for mana 
	%  to pay a cost

	tap_for_mana(Player, [Map, Switches], Identified):-
		Context = [Player, Map, Switches],
		string_input(tap_for_mana, Context, Input) ->
		atom_to_list(Input, List), 
		tap_for_mana(Player, Map, Identified, List); 
		tap_for_mana(Player, [Map, Switches], Identified).
	% This fails for Permanents that call for a mana payment or an other
	%  additional cost for producing mana (not just tapping). The problem
	%  is in output/2 for this. Check it out. 


%%%%%%%%%%%%%%tap_for_mana/3 20/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* tap_for_mana(+Player, +Map, -Input) */
	% Fails on [c]ancel (because there is no "[c]ancel" on the battlefield.
	% [s]kip succeeds and lets you draw mana from your pool. 

%%%%%%%%%%%%%%tap_for_mana/3 (0) 21/02/2011

	% All chosen permanents have been tapped 
	tap_for_mana(Player, Map, Identified, []).

%%%%%%%%%%%%%%tap_for_mana/3 (1) 21/02/2011

	% Input = s. Skip tapping any permanents for mana and go 
	%  straight to drawing  mana from your pool. 
	tap_for_mana(_Player, Map, _Identified, Input):-
		Input = [Switch], % remove brackets...
		member('skip' - Switch, Map).
	%^ This will allow to enter a string containing 'skip'; a feature.
	% But test it. 

%%%%%%%%%%%%%%tap_for_mana/3 (2) 21/02/2011

	% The permanent the Player is attempting to tap is already tapped
	%  Warn, and continue. 
	tap_for_mana(Player, Map, Identified, [Switch | Rest]):-
		member(Permanent - Switch , Map), 
		% ^ "Permanent" may be cancel or skip; those are not 
		%  in Identified, so this will fail? >
		member([Permanent - Switch, Id], Identified), 
		zone(Player, 'Battlefield', Cards), 
		member(copy(Permanent, Id, State), Cards), 
		member('tapped', State), % the Permanent is already tapped
		% output/2 call
		remove(Permanent - Switch, Map, New_map), 
		remove([Permanent - Switch, Id], Identified, New_identified), 
		% ^ But, this removes the wrong permanent because there is no
		%  correspondence between Switch and Permanent. 
		tap_for_mana(Player, New_map, New_identified, Rest).

%%%%%%%%%%%%%%tap_for_mana/3 (3) 21/02/2011

	% The Permanent is untapped. Draw the mana its mana ability can
	%  produce and tap it. Well, the other way around. 
	tap_for_mana(Player, Map, Identified, [Switch | Rest]):-
		member(Permanent - Switch , Map),
		member([Permanent - Switch, Id], Identified), 
		zone(Player, 'Battlefield', Cards), 
		member(copy(Permanent, Id, State), Cards), 
		(member('untapped', State) ; State = [] ),
		% ^ Else tap_untap/3 fails and the whole casting fails. 
		mana_ability(Permanent, _Ability, Mana), 
		% ^ I'll have to choose abilities for permanents with multiple
		%  mana abilities
		tap_untap(copy(Permanent, Id, State), Player, 'tap'),
		add_to_pool(Player, Mana, _New_pool),
		remove(Permanent - Switch, Map, New_map),
		remove([Permanent - Switch, Id], Identified, New_identified), 
		tap_for_mana(Player, New_map, New_identified, Rest).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Issues: 
		a) When I try to tap a tapped land the whole thing fails?
		c) I am not reporting the state of permanents before tapping them.
*/


%%%%%%%%%%%%%%spend_mana/3 20/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* spend_mana(+Player, +Cost, -Mana) */
	% Lets the Player spend Mana to pay a Cost; verifies that the correct
	%  cost has been payed (well, that actually happens in output/2)

	spend_mana(Player, Cost, Mana):- 
		mana_pool(Player, Mana_pool),
		Context = [Mana_pool, Cost],
		(string_input(spend_mana, Context, Mana), !,
		match_cost(Cost, Mana);
		spend_mana(Player, Cost, Mana)). 


%%%%%%%%%%%%%%match_cost/2 22/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* match_cost(+Cost, +Payment) */
	% Checks whether Cost can be fullfilled by Payment. Both are 
	%  Mana strings. 

%%%%%%%%%%%%%%match_cost/2 (0) 22/02/2011

	% The strings are identical- no more testing needed. 
	match_cost(Cost, Cost).

%%%%%%%%%%%%%%match_cost/2 (1) 22/02/2011

	% Attempts to match Cost by Payment. 
	match_cost(Cost, Payment):- 
		string_to_list(Payment, Payment_list), 
		string_to_list(Cost, Cost_list),
		Pool = [c - 0,w - 0,u - 0,b - 0,r - 0,g - 0],
		add_mana(Payment_list, Pool, New_pool), 
		draw_mana(New_pool, Cost_list, Empty_pool), 
		Empty_pool = [c - 0,w - 0,u - 0,b - 0,r - 0,g - 0].

%%%%%%%%%%%%%%match_cost/2 (2) 22/02/2011

	% Payment cannot satisfy Cost. 
	match_cost(_, _):- 
		write(`Wrong payment`), nl, fail, !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%determine_cost/3 16/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* determine_cost(+Spell, +Abilities, -Cost) */
	% Returns the Spell's cost(s); just mana costs for now.

%	determine_cost(_Object, Ability, 'tap'):- 
%		member(['tap'], Ability).
/*
	determine_cost(Spell, _Abilities, Cost):-
		card([card_name Spell, mana_cost Cost, _, _, _, _, _, state State]).

% Needs to deal with the variable (X) and hybrid (g/r) costs. /*


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%               Notes               %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%actions_taken/ 29/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* actions_taken(+Player, +Actions) */
	% Lists the actions taken by a player in this turn. 
	% Each action is a list of [+Action_type, Further_details]
	% Where further details can be any number of relevant terms. 
	% OK, need to define this. 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Action_types: 		Further_details: 		Example:
	'play_land'			(list of) Land_name	[Plains, Island]

It may have been convenient here to be keeping a turn count. 
*/


%	actions_taken('Player 1', []).
%	actions_taken('Player 2', []).

	% This means I'll have to cleanup the facts at the end of turns?

%*/


/*
	determine_abilities(Card, Abilities):-
		card([card_name Card, _, _, text_box Text_box, _, _, _, _]),		
		determine_abilities(Text_box, [], Abilities).

	determine_abilities([], Abilities, Abilities).
	determine_abilities(Text_box, Temp, Abilities):- 
		member(Ability, Text_box), 
		phrase(ability, Ability), 
		remove(Ability, Text_box, New_text_box), 
		append(Temp, [Ability], New_temp),
		determine_abilities(New_text_box, New_temp, Abilities).
*/

/*
	determine_ability(Object, Ability):- 
		card([card_name Object, _, _, text_box Abilities, _, _, _, _]),
		sublist(Ability, Abilities), 
		phrase(ability, Ability).
*/

%(Text_box \= [] -> [Ability] = Text_box ; Ability = []),
% ^ hack; check "casting spells and activating abilities.txt"
% ^ don't work. Not even my hacks work... 

