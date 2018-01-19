% First saved: 17/02/11
% Last saved: 22/02/11
%
% 	Status: 
%	OK
%	
%
% Doing:
%	Done redefining how mana abilities are identified 
%		(using the new MGL-aware mana_ability/3)
%	fix inspect (stack- shows copy(Name,Id,State)) 
%	add Id and State output tap_for_mana/ spend_mana?
%	add [h]elp options to player actions sub-menus!
%	add break option to player actions menu (needs player_actions change)
%
% Todo
%
% NOTES:


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Input                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%input/3 01/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* input(+Caller, +Context, -Input) */
	% Handles user input depending on the Caller predicate. 
	% Context is a list of arguments used to establish the conditions
	%  of the call, eg, information about the current step or phase etc.

	% Check the user input, output a warning if it's wrong
	%  otherwise pass control back to the caller predicate. 
	input(Caller, Context, Input):-
		prompt([Caller, Context]), 
		get(Input),
		flush, 
		(	wrong_input(Caller, Context, Input)
		-> 
			warning(Caller, Context), 
			fail % !?
			;
			true
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%string_input/3 22/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* string_input(+Caller, +Context, -Input) */
	% Same as input/3 but takes input as a string of characters 
	%  (not character codes!) and returns it as an atom. 

	% Check the user input, output a warning if it's wrong
	%  otherwise pass control back to the caller predicate. 
	string_input(Caller, Context, Input):-
		prompt([Caller, Context]), 
		fread(a, 0, -1, Input),
		%read(Input),
		flush, 
		(	wrong_input(Caller, Context, Input)
		-> 
			warning(Caller, Context), 
			fail % !?
			;
			true
		).


%%%%%%%%%%%%%%prompt/1 31/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* prompt([+Caller, +Context]) */
	% Prompt user for input, depending on the Caller predicate. 
	% Context is a list used to establish the conditions of the call.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Args are in a list because prompt/2 is built in. 
*/

%%%%%%%%%%%%%%prompt/1  (1) 31/01/2011

	prompt([main, _]):- 
		output(main, ['instructions']).

%%%%%%%%%%%%%%prompt/1  (2.1) 31/01/2011
% prompt/1 2.X clauses: player_actions prompt. 
% Context == [Active_player, Priority_player, Step, Phase, New_play]

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Note that Step information comes as State(Step) tuples
	  but the player is prompted for (this) input only when he 
	  or she gets priority, so only while continues(Step). 
	Specifically, Combat actions choices like declaring attackers 
	  or blockers etc are not taken while a player has priority
	  but at the beginning of a Combat step, as turn-based actions 
	  and therefore input/ output is handled by game_actions, 
	  _not_ player_actions. 
	  
*/

	% The Active_player has priority, it's a Main phase
	%  and he or she has not played a land this turn.
	prompt([player_actions, [Active, Active, _, Phase, _]]):-
		(Phase == 'First Main'; 
		Phase == 'Second Main'),
		played_land(Active, 'no'),
		output(player_actions, ['prompt', 'land']), !.
	% Cut or on a wrong input, we fall in the clause below 
	%  which causes strange behaviour. 

%%%%%%%%%%%%%%prompt/1  (2.2) 31/01/2011

	prompt([player_actions, _]):-
		output(player_actions, ['prompt', 'no_land']).

%%%%%%%%%%%%%%prompt/1  (3) 31/01/2011
% prompt/1 3.X clauses: inspect_game prompt. 

	prompt([inspect_game, _]):-
		output(inspect_game, ['prompt']).

%%%%%%%%%%%%%%prompt/1  (4) 31/01/2011
% prompt/1 4.X clauses: play_land prompt. 
% Switches: list of user input choices with bracketed switches, 
%  eg "[P]lains, [S]wamp, [I]sland"

	prompt([play_land, [_Player, Map, Switches]]):- 
		output(play_land, ['choose', Map, Switches]).


%%%%%%%%%%%%%%prompt/1  (5) 16/02/2011
% prompt/1 5.X clauses: cast_spell prompt. 

		prompt([cast_spell, [Map, Switches]]):- 
			output(cast_spell, ['choose', [Map, Switches]]).


%%%%%%%%%%%%%%prompt/1  (6) 16/02/2011
% prompt/1 6.X clauses: spell_abilities prompt. 

	prompt([spell_abilities, []]):- 
			output(spell_abilities, ['prompt']).

%%%%%%%%%%%%%%prompt/1  (7) 20/02/2011
% prompt/1 7.X clauses: pay_cost prompt. 

	prompt([tap_for_mana, [Player, Map, Switches]]):- 
		output(tap_for_mana, ['choose_source', Player, Map, Switches]).

%%%%%%%%%%%%%%prompt/1  (8) 20/02/2011
% prompt/1 8.X clauses: spend_mana prompt. 

	prompt([spend_mana, [Mana_pool, Cost]]):- 
		%output(spend_mana, [Mana_pool, Cost]).
		output(spend_mana, ['prompt', Mana_pool, Cost]).


%%%%%%%%%%%%%%wrong_input/2 31/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/*  wrong_input(+Caller, +Context, +Input) */
	% Check for wrong input, in the given Contxt of the Caller predicate.

%%%%%%%%%%%%%%wrong_input/2 (1) 31/01/2011

	wrong_input(main, _, Input):-
		not
		(
			Input == 115;	% Start
			Input == 104;	% Help
			Input == 113	% Quit
		).

%%%%%%%%%%%%%%wrong_input/2 (2) 31/01/2011

	wrong_input(player_actions, _, Input):-
		not
		(
			Input == 108;	% Land
			Input == 115;	% Spells
			Input == 97;	% Abilities
			Input == 116;	% Take
			Input == 112;	% Pass
			Input == 104;	% Help
			Input == 67;	% Concede (uppercase C)
			Input == 105;	% Inspect 
			Input == 100	% Debug
		). 

%%%%%%%%%%%%%%wrong_input/2 (3) 31/01/2011

	wrong_input(inspect_game, _, Input):-
		not
		(
			Input == 108;	% Library
			Input == 104;	% Hand
			Input == 115;	% Stack
			Input == 98;	% Battlefield
			Input == 103;	% Graveyard
			Input == 101;	% Exile
			Input == 99;	% Cancel
			Input == 109	% Mana
		). 

%%%%%%%%%%%%%%wrong_input/2 (4) 31/01/2011

	wrong_input(play_land, Context, Input):- 
		Context = [_Player, Map, _Switches],
		atom_chars(Switch, [Input]),
		\+  member(_Land_card - Switch , Map).

%%%%%%%%%%%%%%wrong_input/2 (5) 16/02/2011

	wrong_input(cast_spell, Context, Input):- 
		Context = [Map, _Switches],
		atom_chars(Switch, [Input]),
		\+  member(_Spell - Switch , Map).

%%%%%%%%%%%%%%wrong_input/2 (6) 16/02/2011

	wrong_input(tap_for_mana, Context, Input):-
		Context = [_Player, Map, _Switches],
		atom_to_list(Input, List), 
		findall(Symbol,  
			(member(Symbol, List), 
			\+ member(_Spell - Symbol, Map)),
		Symbols), 
		Symbols \= []. 

%%%%%%%%%%%%%%wrong_input/2 (7) 20/02/2011

	 wrong_input(spend_mana, _Context, Input):-
		string_to_list(Input, List),
		findall(Symbol, 
			(member(Symbol, List), 
			\+ (
				Symbol == w;	% w
				Symbol == u;	% u
				Symbol == b;	% b
				Symbol == r;	% r
				Symbol == g;	% g
				Symbol == c		% Cancel
			)),Symbols),
		Symbols \= [].


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	A problem is I don't know what switch is chosen for cancel, 
	  except that it's first in the picking so it will probably end up
	  with "c". That's not 100% certain though, so it's conceivable that
	  this may fail- plan ahead. 
	
*/


%%%%%%%%%%%%%%warning/1 31/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* warning(+Caller) */
	% Warn user for wrong input, depending on the Caller predicate. 

%%%%%%%%%%%%%%warning/1 (1) 31/01/2011

	warning(main, _):-
		output(main, ['warning']).

%%%%%%%%%%%%%%warning/1 (2) 31/01/2011

	warning(player_actions, _):-
		output(player_actions, [warning]).

%%%%%%%%%%%%%%warning/1 (3) 31/01/2011

	warning(inspect_game, _):-
		output(inspect_game, ['warning']).


%%%%%%%%%%%%%%warning/1 (4) 31/01/2011

	warning(play_land, _):- 
		output(play_land, ['warning']).


%%%%%%%%%%%%%%warning/1 (5) 31/01/2011

	warning(cast_spell, _):- 
		output(cast_spell, ['warning']).


%%%%%%%%%%%%%%warning/1 (6) 31/01/2011

	warning(tap_for_mana, _):- 
		output(tap_for_mana, ['warning']).

%%%%%%%%%%%%%%warning/1 (6) 31/01/2011

	warning(spend_mana, _):- 
		output(spend_mana, ['warning']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Output                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output(+Predicate_name, +[Arguments]) */
	% General output predicate for the parts of a turn
	% Output_clause is instantiated by the parent predicate
	%  depending on the desired/ necessary output. 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Having checks about outputting or not here could be confusing. 
	Currently utputs are generally stand-ins for functionality, 
	so when they should happen should be decided by the calling 
	predicates?  
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%output/2 03/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* output(+Caller, +Context) */
	% Handles output for the Caller predicate.
	% Context is a list; the first element determines the specific
	%  output case for Caller and the remaining are arguments to output

%%%%%%%%%%%%%%output/2 (0.1) 31/01/11
% 0.* clauses: main output.

	output(main, ['instructions']):-
		nl,
		write(`Enter: "[s]tart", "[h]elp", or "[q]uit" to proceed.`), nl.


%%%%%%%%%%%%%%output/2 (0.2) 31/01/11

	output(main, ['warning']):-
		write(`Please enter a valid option`), nl.


%%%%%%%%%%%%%%output/2 (0.3) 31/01/11

	output(main, ['help']):-
		nl,
		write(`Type "[s]tart" to start a game of Magic: the Gathering against Gleemin.`),
		nl,
		write(`Type "[h]elp" to see this help text.`),
		nl,
		write(`Type "[q]uit" to exit this program.`),
		nl.


%%%%%%%%%%%%%%output/2 (1.1) 03/01/11
% 1.* clauses: turn_begins output.

		% There is a new active Player
		output(turn_begins, ['new', Player]):-
			tab(5),
			write(Player), 
			write(`'s turn begins (1)`), nl.

%%%%%%%%%%%%%%output/3 (1.2) 03/01/11

		% The active Player remains the same. 
		output(turn_begins, ['current', Player]):-
			tab(5),
			write(`Active player (2): `),
			write(Player), nl.


%%%%%%%%%%%%%%output/3 (2.1) 03/01/11
% 2.* clauses: phase_begins output.

	% A phase begins
	output(phase_begins, [Phase]):- 
		tab(10),
		write(Phase),
		write(` phase begins`), nl.

%%%%%%%%%%%%%%output/3 (2.2) 03/01/11

	% The Untap or Cleanup Step is ongoing or ends. No output.
	output(phase_begins, [State, Step, _]):- 
		(
			Step == 'Untap'; 
			Step == 'Cleanup'
		), 
		(
			State == ongoing; 
			State == ends
		).
	% This will have to be refined for Cleanup steps where 
	%  players receive priority. 

%%%%%%%%%%%%%%output/3 (2.3) 03/01/11

	% Any other Step of a Phase ends. No output. 
	output(phase_begins, [ends, _, _]):- 
		true.

%%%%%%%%%%%%%%output/3 (2.4) 03/01/11

	% Any other Step of a Phase is ongoing. 
	output(phase_begins, [_, _, Phase]):- 
		tab(10),
		write(`Current phase: `),
		write(Phase), nl.


%%%%%%%%%%%%%%output/3 (3.1) 03/01/11
% 3.* clauses: step_begins output.

	% Some phases have no steps.
	output(step_begins, []):- 
		tab(15),
		write(`This phase has no steps.`), nl.
	% This will fire at the end of an [] Step
	%  because of the slight fudge, which I should correct
	%  anyway. So I leave it here until I have. 

%%%%%%%%%%%%%%output/3 (3.2) 03/01/11

	% A step begins
	output(step_begins, [Step]):-
		tab(15),		
		write(Step),
		write(` step begins.`), nl.

%%%%%%%%%%%%%%output/3 (3.3) 03/01/11

	% The Untap step is ongoing
	output(step_begins, [ongoing, 'Untap']):-
		true.

	% The Cleanup step may allow priority.
	% Will implement eventually. 

%%%%%%%%%%%%%%output/3 (3.4) 03/01/11

	% Any other Step is ongoing. 
	output(step_begins, [ongoing, Step]):-
		tab(15),	
		write(`Current step: `), 	
		write(Step), nl.

%%%%%%%%%%%%%%output/3 (4.1) 03/01/11
% 4.* clauses: phase_actions output.

	output(phase_actions, ['Beginning']):-
		tab(20),
 		write(`Beginning phase actions`), nl.

%%%%%%%%%%%%%%output/3 (4.2) 03/01/11

	output(phase_actions, ['First Main']):-
		tab(20),
		write(`Playing spells and abilities (1)`), nl.

%%%%%%%%%%%%%%output/3 (4.3) 03/01/11

	output(phase_actions, ['Second Main']):-
		tab(20),
		write(`Playing spells and abilities (2)`), nl.

%%%%%%%%%%%%%%output/3 (5.1) 03/01/11
% 5.* clauses: step_actions output.

	output(step_actions, ['Untap']):-
		tab(20),
		write(`Untapping`), nl.

%%%%%%%%%%%%%%output/3 (5.2) 03/01/11

	output(step_actions, ['Upkeep']):-
		tab(20),
		write(`Managing Upkeep`), nl.

%%%%%%%%%%%%%%output/3 (5.3) 03/01/11

	output(step_actions, ['Draw']):-
		tab(20),
		write(`Drawing a card`), nl.

%%%%%%%%%%%%%%output/3 (5.4) 03/01/11

	output(step_actions, ['Beginning of Combat']):-
		tab(20),
		write(`Beginning combat`), nl.

%%%%%%%%%%%%%%output/3 (5.5) 03/01/11

	output(step_actions, ['Declare Attackers']):-
		tab(20),
		write(`Declaring attackers`), nl.

%%%%%%%%%%%%%%output/3 (5.6) 03/01/11

	output(step_actions, ['Declare Blockers']):-
		tab(20),
		write(`Declarking blockers`), nl.

%%%%%%%%%%%%%%output/3 (5.7) 03/01/11

	output(step_actions, ['Combat Damage']):-
		tab(20),
		write(`Assigning combat damage`), nl.

%%%%%%%%%%%%%%output/3 (5.8) 03/01/11

	output(step_actions, ['End of Combat']):-
		tab(20),
		write(`Ending combat`), nl.

%%%%%%%%%%%%%%output/3 (5.9) 03/01/11

	output(step_actions, ['End']):-
		tab(20),
		write(`Ending turn`), nl.

%%%%%%%%%%%%%%output/3 (5.10) 03/01/11

	output(step_actions, ['Cleanup']):-
		tab(20),
		write(`Doing cleanup`), nl.


%%%%%%%%%%%%%%output/3 (6) 03/01/11
% 6.* clauses: end_actions output.
 
		output(end_actions, []):-
		tab(20),
 		write(`Emptying mana pools`), nl.


%%%%%%%%%%%%%%output/3 (7.1) 03/01/11
% 7.* clauses: receives_priority output.

	% No player receives priority this step. 
	output(receives_priority, []):-
		tab(25),
		write(`No Priority`), nl.

%%%%%%%%%%%%%%output/3 (7.2) 03/01/11
		
	% The priority Player has changed
	output(receives_priority, ['new', Player]):-
		tab(25),
		write(`New priority player : `),
		write(Player), nl.

%%%%%%%%%%%%%%output/3 (7.4) 03/01/11
		
	% The priority player keeps priority 
	output(receives_priority, [Player]):-
		tab(25),
		write(`Priority player : `),
		write(Player), nl.


%%%%%%%%%%%%%%output/2 (8.1) 01/02/11
% 8.* clauses: player_actions output.

	output(player_actions, []):-
		tab(25),
		write(`No Prompt`), nl.



%%%%%%%%%%%%%%output/2 (8.X) 01/02/11

	% Active_player's Main phase actions. 
	output(player_actions, ['prompt', Context]):-
		nl,
		tab(25),
		write(`Enter:`), nl,
			player_actions_context(Context), 
%	(Context == 'land' -> tab(30), write(`"[l]and"`), nl, !),
% 	^ can also do this, but I think the disambiguation thing is more
% 	clear, especially since theer's no cut... :)
			tab(30),
			write(`"[s]pells"`), nl,
			tab(30),
			write(`"[a]bilities"`), nl,
			tab(30),
			write(`"[t]ake"`), nl,
			tab(30),
			write(`"[p]ass"`), nl,
			tab(30),
			write(`"[h]elp"`), nl,
			tab(30),
			write(`"[i]nspect"`), nl,
		tab(25),
		write(`to play.`), nl,
		tab(25),
		write(`Enter: "[C]oncede" to quit.`), nl,
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%player_actions_context/1 
%%%%%%%%%%%%%%%%%%%%%%%

	player_actions_context(Context):-		
		(Context == 'land' -> tab(30), write(`"[l]and"`), nl);
		(Context == 'no_land' -> true). 

%%%%%%%%%%%%%%output/3 (8.3) 03/01/11

	output(player_actions, [warning]):-
		nl,
		tab(25),
		write(`Please enter a valid option`),
		nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/3 (9.1) 03/01/11
% 9.* clauses: play output.

	output(play, [Priority_player, 'land']):-
		nl,
		tab(25),
		write(Priority_player),
		write(` is playing land`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.2) 03/01/11

	output(play, [Priority_player, 'spells']):-
		nl,
		tab(25),
		write(Priority_player),
		write(` is casting spells`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.3) 03/01/11

	output(play, [Priority_player, 'abilities']):-
		nl,
		tab(25),
		write(Priority_player),
		write(` is activating abilities`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.4) 03/01/11

	output(play, [Priority_player, 'special']):-
		nl,
		tab(25),
		write(Priority_player),
		write(` is taking special actions`),
		nl,
			tab(35),
			write(`* * *`), nl. 

%%%%%%%%%%%%%%output/3 (9.5) 03/01/11

	output(play, [Priority_player, 'help']):-
		nl,
		tab(25),
		write(Priority_player),
		write(` requested help`), 
		nl,
		tab(25),
		write(`Enter: `), nl,
			tab(30),
			write(`"[l]and": to play land`), nl,
			tab(30),
			write(`"[s]pells": to cast spells`), nl,
			tab(30),
			write(`"[a]bilities": to activate abilities of permanents`), nl,
			tab(30),
			write(`"[t]ake": to take special actions (other than playing a land)`), nl,
			tab(30),
			write(`"[p]ass": to do nothing and pass priority`), nl,
			tab(30),
			write(`"[h]elp": to show this help text`), nl,
			tab(30),
			write(`"[i]nspect": to see your cards in each zone`), nl,
			tab(30),
			write(`* * *`), nl. 

%%%%%%%%%%%%%%output/3 (9.6) 03/01/11

	output(play, [Priority_player, 'pass']):-
		nl,
		tab(25),
		write(Priority_player),
		write(` passes priority`),
		nl,
			tab(35),
			write(`* * *`), nl. 

%%%%%%%%%%%%%%output/3 (9.7) 03/01/11

	output(play, [Priority_player, 'concede']):-
		nl,
		tab(25),
		write(Priority_player),
		write(` concedes`),
		nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (9.8) 03/01/11

	output(play, [Priority_player, 'debug']):-
		tab(25),
		write(Priority_player),
		write(` debug`), nl.


%%%%%%%%%%%%%%output/3 (10.1) 28/01/11
% 10.* clauses: inspect_game output.

%%%%%%%%%%%%%%output/3 (10.1) 28/01/11

	% Inspect prompt
	output(inspect_game, ['prompt']):-
		nl,
		tab(25),
		write(`Enter:`), nl,
			tab(30),
			write(`"[l]ibraries"`),nl,
			tab(30),
			write(`"[h]ands"`), nl,
			tab(30),
			write(`"[s]tack"`), nl,
			tab(30),
			write(`"[b]attlefield"`), nl,
			tab(30),
			write(`"[g]raveyards"`), nl,
			tab(30),
			write(`"[e]xile"`), nl,
		tab(25),
		write(`to see cards in that zone.`), nl,
		tab(25),
		write(`Enter: [m]ana to see mana in players' pools.`), nl,
		tab(25),
		write(`Enter: [c]ancel to return to the previous menu.`), nl,
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (10.2) 28/01/11

	% inspect warning
	output(inspect_game, ['warning']):-
		nl,
		tab(25),
		write(`Please enter a valid option`),
		nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/3 (11.1) 28/01/11
% 11.* clauses: inspect_output output.

	output(inspect_output, [_Priority_player, 'library']):-
		tab(25),
		write(`Cards in libraries: `), nl, fail, !.

	output(inspect_output, [_Priority_player, 'library']):-
		zone(Player, 'Library', Cards),
		length(Cards, Length),
		tab(30),
		write(Player),
		write(`: `), write(Length), write(` cards`), nl, fail.

	output(inspect_output, [_Priority_player, 'library']):-
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (11.2) 28/01/11

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Only the player should be able to see his or her hand!
	I should be checking that the player is human (the computer
	  player doesn't need output for its inspect actions). 
*/

	output(inspect_output, [_Priority_player, 'hand']):-
		tab(25),
		write(`Cards in hand: `), nl, fail, !.

	output(inspect_output, [_Priority_player, 'hand']):-
		zone(Player, 'Hand', Cards),
		length(Cards, Length),
		tab(30),
		write(Player),
		write(`: `), 
		write(Length), write(` cards`), nl, fail.

	output(inspect_output, [Priority_player, 'hand']):-
		zone(Priority_player, 'Hand', Cards),
		findall(Info,(member(Card, Cards),
				card([card_name Card, mana_cost Cost, type_line [_, [Type], _], _, _, _, _, _]),
				Info = [ [Card], [Type], [Cost] ]), Infos),
		Title = [Priority_player,`'s hand`],
		Header = [` Name                Type                 Mana Cost`],
		Separator = [`-------------------+-------------------+--------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Infos), nl,
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (11.3) 28/01/11

	output(inspect_output, [_Priority_player, 'stack']):-
		findall(Card, (zone('Stack', Stack), member(Card, Stack)), Cards),
		findall([[Player], [Card]], ( zone(Player, 'Stack', Stack),member(Card, Stack),
				member(Card, Cards)),  Players_stack),
		length(Stack, Length), % Need to fix stack \= Players' Stacks. Then >
		%stack_by_player(Cards, [], Players_stack), 
		Title = [Length, ` Cards on the Stack`],
		Header = [` Name                Controller`],
		Separator = [`-------------------+-------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Players_stack), nl,
			tab(30),
			write(`* * *`), nl.

	try_this(Info):- 
		findall(Card, (zone('Stack', Stack), member(Card, Stack)), Cards),
		findall([[Player], [Card]], ( zone(Player, 'Stack', Stack),member(Card, Stack),
				member(Card, Cards)),  Info).


%%%%%%%%%%%%%%stack_by_player/3 18/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* stack_by_player(+Cards, [], +On_stack) */
	% Returns the cards on the stack and their controllers.

%%%%%%%%%%%%%%stack_by_player/3 (0) 18/02/11

	stack_by_player([], Players_stack, Players_stack).

%%%%%%%%%%%%%%stack_by_player/3 (1) 18/02/11

	stack_by_player([Card | Rest], Temp, Players_stack):-
		zone(Player, 'Stack', Stack), 
		member(Card, Stack),
		append(Temp, [[[Card], [Player]]], New_temp), 
		stack_by_player(Rest, New_temp, Players_stack).


%%%%%%%%%%%%%%output/3 (11.4) 28/01/11

	output(inspect_output, [_Priority_player, 'battlefield']):-
		tab(25),
		write(`Cards on the battlefield: `), nl, fail, !.

	output(inspect_output, [_Priority_player, 'battlefield']):-
		(zone(Player, 'Battlefield', Cards),
		length(Cards, Length),
		Title = [Player,`'s side (`, Length, `)`],
		Header = [` Name                Id                   State    `], nl,
		Separator = [`-------------------+-------------------+--------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Cards)),
			fail,
			tab(30),
			write(`* * *`), nl.

	output(inspect_output, [_Priority_player, 'battlefield']):-
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (11.5) 28/01/11

	output(inspect_output, [_Priority_player, 'graveyard']):-
		tab(25),
		write(`Cards in graveyards: `), nl, fail, !.

	output(inspect_output, [_Priority_player, 'graveyard']):-
		(zone(Player, 'Graveyard', Cards),
		length(Cards, Length),
%		findall(Info,(member(Card, Cards),
		findall(Info,(member(copy(Card, _Id, _State), Cards),
				card([card_name Card, _, type_line [_, [Type], _], _, _, _, _, _]),
				Info = [ [Card], [Type] ]), Infos),
				% ^ Could add some check for abilities playable from the Graveyard.
		Title = [Player,`'s Graveyard (`, Length, `)`],
		Header = [` Name                Type                 `],
		Separator = [`-------------------+-------------------+`], nl,
		tabulate(Title, [Header, Separator], [5, 20, 0], Infos)), %nl, 
			fail,
			tab(30),
			write(`* * *`), nl.

	output(inspect_output, [_Priority_player, 'graveyard']):-
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (11.6) 28/01/11

	output(inspect_output, [_Priority_player, 'exile']):-
		tab(25),
		write(`Cards in Exile: `), nl, fail, !.

	output(inspect_output, [_Priority_player, 'exile']):-
		(zone(Player, 'Exile', Cards),
		length(Cards, Length),
		Title = [Player,`'s  Side (`, Length, `)`],
		Header = [` Name                Id                   State    `], nl,
		Separator = [`-------------------+-------------------+--------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Cards)),
			fail,
			tab(30),
			write(`* * *`), nl.
	% Some cards in Exile need a state, eg face down, phased out etc.
	% In fact, exiled cards are copy(Name, Id, State) tuples. 

	output(inspect_output, [_Priority_player, 'exile']):-
			tab(30),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/3 (11.6) 28/01/11

	output(inspect_output, [_Priority_player, 'mana_pools']):-
		(mana_pool(Player, Mana_pool), 
		Mana_pool = [c - C,w - W,u - U,b - B,r - R,g - G],
		Mana = [
				[[`Colourless`], 	[C]],
				[[`White`], 	[W]],
				[[`Blue`],		[U]],
				[[`Black`], 	[B]],
				[[`Red`],		[R]],
				[[`Green`], 	[G]]
			], 
		Title = [Player, `'s Mana pool`],
		Header = [` Mana Colour         Amount               `], nl,
		Separator = [`-------------------+-------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Mana)), %, nl),
		fail.

	output(inspect_output, [_Priority_player, 'mana_pools']):-
		nl, tab(35), 
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/3 (12.1) 03/01/11
% 12.* clauses: step_ends output.

	output(step_ends, [ends(Step)]):-
				tab(15),
		write(`Ending step: `),
		write(Step), nl,
			tab(25),
			write(`#* * * *#`), nl, nl. 


%%%%%%%%%%%%%%output/3 (13.1) 03/01/11
% 13.* clauses: phase_ends output.

	output(phase_ends, [Phase]):-
		tab(10),
		write(`Ending phase: `),
		write(Phase), nl,
			tab(20),
			write(`#* * * * *#`), nl, nl. 


%%%%%%%%%%%%%%output/3 (14.1) 03/01/11
% 14.* clauses: turn_ends output.

	output(turn_ends, [Current_player]):-
		tab(5),
		write(Current_player), 
		write(`'s turn ends `), nl,
		tab(15),
		write(`##* * * * * * * * * * * *##`), nl, nl.


%%%%%%%%%%%%%%output/2 (15.1) 31/01/11
% 15.* clauses: play_land output.

	output(play_land, ['wrong_turn']):-
		tab(25),
		write(`You can only play land during your turn`),  nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.2) 31/01/11

	output(play_land, ['wrong_phase']):-
		tab(25),
			write(`Not a main phase`), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (15.3) 31/01/11

	output(play_land, ['land_limit']):-
		tab(25),
			write(`Already played land this turn`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.4) 31/01/11

	output(play_land, ['stack_not_empty']):-
		tab(25),
			write(`Stack not empty`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.5) 31/01/11

	output(play_land, ['no_lands_in_hand']):-
		tab(25),
			write(`No lands in hand`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.6) 20/02/11

	% Chaos, because lands can produce several types of mana. 
	% Nit to pick:  this outputs additional colours in []'s.
	output(play_land, ['choose', [_Cancel | Map], [Cancel | Land]]):-
		findall( Switch - Colour , 
				(member(Land_card - _Switch, Map, Position),
				member(Switch, Land, Position),  
%				card([card_name Land_card, _, _, text_box Abilities, _, _, _, _]),
%				member(Ability, Abilities), 
%				Ability = add_mana(_,[add,Colour,'to your mana pool'])
				mana_ability(Land_card, _Ability, Colour)

			), Land_colours),
		mana_colours(Land_colours, Land_colours, [], Pairs),
		%write(`UnPairs` - Pairs), nl,
		sort(Pairs, Sorted_pairs), %write(`SPairs` - Sorted_pairs), nl, 
		Title = [`Choose from`],
		Header = [` Name                Mana Colours         `], nl,
		Separator = [`-------------------+-------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Sorted_pairs), nl,
		tab(25),
		write(`Or enter `), write(Cancel), 
		write(` to return to the previous menu`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%mana_colours/3 20/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_colours(+Land_colours, +Land_colours, [], -Pairs) */
	% Returns pairs of Land names and the colour(s) they produce)
	% Pairs are of the form: [ [ [ [I]sland ], [ [u] ] ] ]
	% That's because tabulate/4 is fjarged.

%%%%%%%%%%%%%%mana_colours/3 (0) 20/02/11

	mana_colours(Land_colours, [], Pairs, Pairs).

%%%%%%%%%%%%%%mana_colours/3 (1) 20/02/11

	mana_colours(Land_colours, [Switch - _C | Rest], Temp, Pairs):-
		findall(Colour, member(Switch - Colour, Land_colours), Colours),
		append(Temp, [[[Switch], [Colours]]], New_temp),
		mana_colours(Land_colours, Rest, New_temp, Pairs). 


%%%%%%%%%%%%%%output/2 (15.7) 31/01/11

	output(play_land, ['warning']):-
		tab(25),
		write(`Please enter a valid option`), nl, 
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (15.8) 31/01/11

	output(play_land, ['played_land', Player, Land_card]):-
		tab(25),
		write(Player),
		write(` plays a `), write(Land_card), nl, 
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (16.1) 16/02/11
% 16.* clauses: cast_spell output.

	output(cast_spell, [ 'choose', [ [Other_ | [Cancel_ | Map]] , [Other | [Cancel | Hand]] ] ]):-
		findall(Info,(member(Card - _Switch, Map, Position),
				member(Switch, Hand, Position), 
				card([card_name Card, mana_cost Cost, type_line [_, [Type], _], _, _, _, _, _]),
				Info = [ [Switch], [Type], [Cost] ]), Infos),
		Title = [`Choose the spell to cast`],
		Header = [` Name                Type                 Mana Cost`],
		Separator = [`-------------------+-------------------+--------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Infos), nl,
		tab(25),
		write(`Enter `), write(Cancel), 
		write(` to return to the previous menu `), nl,
%		tab(25),
%		write(`Or `), write(Other), 
%		write(` to cast a spell from another zone than Hand`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.2) 16/02/11

	output(cast_spell, ['warning']):-
		tab(25),
		write(`Please enter a valid option`), nl, 
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.3) 16/02/11

	output(cast_spell, ['spell_cast', Player, Spell, Zone]):-
		tab(25),
		write(Player),
		write(` casts `), write(Spell), 
		write(` from `), 	write(Zone), nl, 
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.4) 16/02/11

	output(cast_spell, ['no_spells', Player, Zone]):-
		tab(25),
		write(`No spells in `), write(Player), 
		write(`'s `), write(Zone), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (16.5) 16/02/11

	output(cast_spell, ['spell_fails', Player, Spell, Zone]):-
		tab(25),
		write(Player),
		write(` fails to cast `), write(Spell), 
		write(` from `), 	write(Zone), nl, 
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (17.1) 20/02/11
% 17.* clauses: pay_cost output.

	output(pay_cost, [Permanent, 'tap_failed']):-
		tab(25),
		write(Permanent), write(` cannot be tapped`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (17.1) 20/02/11

	output(pay_cost, [Permanent, 'untap_failed']):-
		tab(25),
		write(Permanent), write(` cannot be untapped`), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (17.1) 20/02/11

	output(pay_cost, [Object, 'mana_failed']):-
		tab(25),
		write(`Mana cost could not be payed for `), write(Object), nl,
			tab(35),
			write(`* * *`), nl.

%%%%%%%%%%%%%%output/2 (18.1) 20/02/11
% 18.* clauses: draw_mana output.

	%output(draw_mana, [Player, Mana_pool]):-
		%Mana_pool = [c - C,w - W,u - U,b - B,r - R,g - G] ,
	output(draw_mana, [Player, [c - C,w - W,u - U,b - B,r - R,g - G]]):-
		Mana = [
				[['Colourless'], 	[C]], 
				[['White'], 	[W]], 
				[['Blue'], 		[U]], 
				[['Black'], 	[B]], 
				[['Red'], 		[R]], 
				[['Green'], 	[G]]
			],
		tab(25),
		write(`Failed to draw mana from `), nl, 
		Title = [Player, `'s pool`],
		Header = [` Mana                Amount               `],
		Separator = [`-------------------+-------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Mana), nl,
			tab(35),
			write(`* * *`), nl, !. % Green cut. Stops take_from_pool in players.pl from
							%  backtracking over output/2 seven times. O.o


%%%%%%%%%%%%%%output/2 (18.1) 20/02/11
% 19.* clauses: tap_for_mana output.

	% Code repeat; this is same as: output/2 (15.6). Fix please? 
	output(tap_for_mana, ['choose_source', _Player, [_Cancel | [_Skip | Map ]], [Cancel | [Skip | Sources]]]):-
		findall( Switch - Colour , 
				(member(Permanent - _Switch, Map, Position),
				member(Switch, Sources, Position),  
%				card([card_name Permanent, _, _, text_box Abilities, _, _, _, _]),
%				member(Ability, Abilities), 
%				Ability = add_mana([tap],[add,Colour,'to your mana pool'])
				mana_ability(Permanent, _Ability, Colour)
			), Source_colours),
		mana_colours(Source_colours, Source_colours, [], Pairs),
		% ^ Defined in output/2 (15.6) 
		Title = [`Choose any number of permanents to tap for mana`],
		Subtitle = [`(enter [s]witches as a string; no quotes needed)`],
		Header = [` Permanent           Mana Colours         `], nl,
		Separator = [`-------------------+-------------------+`],
		tabulate(Title, [Subtitle, Header, Separator], [5, 20, 0], Pairs), nl,
		tab(25),
		write(`Enter `), write(Skip), 
		write(` to continue casting without tapping for mana`), nl,
		tab(25),
		write(`Or enter `), write(Cancel), 
		write(` to return to the previous menu`), nl,
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (18.1) 20/02/11
% 20.* clauses: spend_mana output.

	output(spend_mana, ['warning']):- 
		tab(25),
		write(`Wrong mana amount`),
			tab(35),
			write(`* * *`), nl.


%%%%%%%%%%%%%%output/2 (18.2) 20/02/11

	output(spend_mana, ['prompt', Mana_pool, Cost]):- 
		Mana_pool = [c - C,w - W,u - U,b - B,r - R,g - G],
		tab(25),
		write(`Please pay `), write(Cost), 
		write(` from your mana poool`), nl,
		Mana = [
				[[`Colourless`], 	[C]],
				[[`White`], 	[W]],
				[[`Blue`],		[U]],
				[[`Black`], 	[B]],
				[[`Red`],		[R]],
				[[`Green`], 	[G]]
			],
		Title = [`Choose from`],
		Header = [` Mana Colour         Amount               `], nl,
		Separator = [`-------------------+-------------------+`],
		tabulate(Title, [Header, Separator], [5, 20, 0], Mana), nl,
		tab(25),
		write(`Or enter [c]ancel to stop casting this spell`), nl,
		tab(25),
		write(`(this will return you to the main player actions menu)`), nl,
			tab(35),
			write(`* * *`), nl.









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% I liked this:
%	output(play_land, ['choose', Land]):-
%		output(play_land, ['warning']), 
%		output(play_land, ['choose', Land]).





