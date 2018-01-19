% First saved: 02/02/2011
% Last saved:  26/02/2011
%
%	Incremental Save
%
% 	Status: 
%	OK, except slight fudge (see notes). 
% Doing:
%	Noticed Inspect clauses should be in player_actions! 
%	removing asserts of active_player (not needed any more?)
% Todo: 
%	game_actions needs to take in player names and possibly more info.
%	add statistics etc output options to debug.
%	receives_priority is hacked (check 30-12-10-3).
%	implement Cleanup step correctly. 
%	Fix the slight fudge.
%	Keep an eye on step_ends. It's a bit confusing. 
%	step_ends can be skipped in [] steps and let phase_ends
%	  do its job (check for [] Stack and 'Full' Cue).
% NOTES: 
%	The slight fudge: [] steps should not exist and Phase
%		actions should be taken before their steps begin. 
%	  noticed that the game doesn't yet know
%	  that the cards in players' stack, exile and battlefield
%	  are also in the _shared_ stack, exile and battlefield. 
% 	At some point I'll need to deal with all those unbound 
% 	  variables in predicate heads... 
%	I'd like to return New_play as a list 
%	  in order to remember playing land this turn without 
%	  using asserts/retracts. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Turn facts              %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- dynamic active_player/1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%phases/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% Phase list
	phases(['Beginning', 'First Main', 'Combat', 'Second Main', 'Ending']).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%phase/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% What phases?
	phase('Beginning').
	phase('First Main').
	phase('Combat').
	phase('Second Main').
	phase('Ending').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%phase/2 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% phase(Name, Steps): steps of a phase.
	phase('Beginning', [
				'Untap',
				'Upkeep',
				'Draw'
				]).
	phase('First Main', []).
	phase('Combat', [
				'Beginning of Combat',
				'Declare Attackers',
				'Declare Blockers',
				'Combat Damage',
				'End of Combat'
				]).
	phase('Second Main', []).
	phase('Ending', [
				'End',
				'Cleanup'
				]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%step/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% What steps?
	step('Untap').
	step('Upkeep').
	step('Draw').
	step('Beginning of Combat').
	step('Declare Attackers').
	step('Declare Blockers').
	step('Combat Damage').
	step('End of Combat').
	step('End').
	step('Cleanup').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%step/2 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% step(Phase, Step): steps by phase.
	step('Beginning', 'Untap').
	step('Beginning', 'Upkeep').
	step('Beginning', 'Draw').
	step('First Main', []).
	step('Combat', 'Beginning of Combat').
	step('Combat', 'Declare Attackers').
	step('Combat', 'Declare Blockers').
	step('Combat', 'Combat Damage').
	step('Combat', 'End of Combat').
	step('Second Main', []).
	step('Ending', 'End').
	step('Ending', 'Cleanup').


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%actions/2 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%actions/2 (1) 16/12/2010

	% actions(Phase, Action), Game Actions by phase.
	actions('Beginning', beginning).
	actions('First Main', first_main).
	actions('Second Main', second_main).

%%%%%%%%%%%%%%actions/2 (2) 16/12/2010

	% actions(Step, Action) Game Actions by step.
	actions('Untap', untap).
	actions('Upkeep', upkeep).
	actions('Draw', draw).
	actions('Beginning of Combat', begin_combat).
	actions('Declare Attackers', declare_attackers).
	actions('Declare Blockers', declare_blockers).
	actions('Combat Damage', damage).
	actions('End of Combat', end_combat).
	actions('End', end).
	actions('Cleanup', cleanup).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%game_action/2 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	game_action(untap).
	game_action(upkeep).
	game_action(draw).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%        Turn rules (1)             %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%turn/7 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	/* turn(+Player, +Phase, +Step, +Priority, +Cue, +Play, +Stack) */
	% Implements a turn sequence. 

%%%%%%%%%%%%%%turn/4 (0) 15/02/2011

	% A player has conceded; the turn sequence ends
	turn(_, _, _, _, _, 67, _). % 67: [C]oncede

%%%%%%%%%%%%%%turn/4 (1) 29/12/2010

	turn(Player, Phase, Step, Priority, Cue, Play, Stack):-	
		turn_begins(Player, Step), !,
			phase_begins(Phase, Step), !,
				step_begins(Step), !,
					game_actions(Phase, Step), !,
					receives_priority(Player, Step, Priority, Play, Stack, New_priority), !,
					player_actions(Player, New_priority, Step, Phase, New_play), !,
					stack_resolves(Stack, New_play, Cue, New_priority, New_cue, New_stack), !,
				step_ends(Step, Phase, New_cue, New_priority, New_stack, New_step), !,
			phase_ends(Phase, Step, New_phase), !,
		turn_ends(Player, Phase, Step, New_player), !,
	turn(New_player, New_phase, New_step, New_priority, New_cue, New_play, New_stack).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%turn_begins/2 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	/* turn_begins(+Player, +Step) */
	% "Every untap step there's a new active player"
% Marks the player whose turn it is as the active player. 

%%%%%%%%%%%%%%turn_begins/2 (1) 25/12/2010

	% If this is the beginning of the first untap phase in a game, 
	%  the active player is the first player in the 
	%  order_of_play, so don't look for a new active player. 
	turn_begins(Player, begins('Untap')):-
		new_game(yes),
		assert_active_player(Player),
		output(turn_begins, ['new', Player]),
		retractall(new_game(_)), 
		assert(new_game(no)).

%%%%%%%%%%%%%%turn_begins/2 (2) 25/12/2010

	% If it's the beginning of the untap step, 
	%  there's a new active player:
	turn_begins(Player, begins('Untap')):-
		assert_active_player(Player),
		output(turn_begins, ['new', Player]).

%%%%%%%%%%%%%%turn_begins/2 (3) 02/01/2011

	% During a Step other than the Untap or Cleanup
	% report the active player 
	turn_begins(Player, State(Step)):-
		(State == begins ; 
		State == ongoing), 
		(Step \== 'Untap',
		Step \== 'Cleanup'),
		assert_active_player(Player),
		output(turn_begins, ['current', Player]).
	% What about Cleanup steps where players receive 
	%  priority? Should report (from another pred). 

%%%%%%%%%%%%%%turn_begins/2 (3) 25/12/2010

	% Otherwise, there is no new active player
	%  and no need to report the active player
	turn_begins(Player, Step):-
		true.


%%%%%%%%%%%%%%assert_active_player/1 28/01/2011

	% If there is an active player in the kb
	assert_active_player(Player):-
		retractall(active_player(_)), 
		asserta(active_player(Player)).

	% If there is no active player in the kb
	assert_active_player(Player):-
		asserta(active_player(Player)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%phase_begins/2 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 
	/* phase_begins(+Phase, +Step) */
% "A phase ends at its last Step and then there is a new phase. 

%%%%%%%%%%%%%%phase_begins/2 (1) 30/12/2010

	% A phase begins when its first step begins
	phase_begins(Phase, begins(First_step)):-
		phase(Phase, [ First_step | Rest ]),
		output(phase_begins, [Phase]).

%%%%%%%%%%%%%%phase_begins/2 (2) 25/12/2010

	% A phase with no steps begins here
	phase_begins(Phase, begins([])):-
		output(phase_begins, [Phase]).

%%%%%%%%%%%%%%phase_begins/3 (2) 25/12/2010

	% Otherwise, it continues.
	phase_begins(Phase, State(Step)):-
		output(phase_begins, [State, Step, Phase]).
	% Could check membership of Step to Phase
	%  to make sure we're in the right phase/ step.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%step_begins/1 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* step_begins(+Step) */
	% Marks the beginning of the Step
	% Also deals with "at end of Step" effects- to be implemented.

%%%%%%%%%%%%%%step_begins/2 (1) 30/12/2010

	% Some phases have no steps: 
	step_begins(State([])):-
	output(step_begins, []).

%%%%%%%%%%%%%%step_begins/2 (2) 30/12/2010

	% A step begins: 
	step_begins(begins(Step)):-
	output(step_begins, [Step]).

%%%%%%%%%%%%%%step_begins/2 (3) 30/12/2010

	% Otherwise, the step goes on 
	step_begins(ongoing(Step)):-
	output(step_begins, [ongoing, Step]).

%%%%%%%%%%%%%%step_begins/2 (4) 30/12/2010

	% The end of the step is reported later.
	step_begins(ends(Step)):-
		true.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Note rule 103.7a: In a two-player game, 
	 the player who plays first skips the draw step 
	 of his or her first turn.
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%game_actions/2 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* game_actions(+Phase, +Step) */
	% Take the necessary game actions in this phase or step.
	% This predicate will need to return a game state/ board position.

%%%%%%%%%%%%%%game_actions/2 (1) 30/12/2010

	% If this is the beginning of the first step of the current phase
	%  and there are turn-based_actions to take in this phase
	%  take them, then take the turn-based actions in the 
	%  first step of that phase. 
	game_actions(Phase, begins(First_step)):-
		phase(Phase, [ First_step | Rest ]),
		phase_actions(Phase),
		step_actions(First_step).

%%%%%%%%%%%%%%game_actions/2 (2) 30/12/2010

	% If the current phase has no steps, 
	%  take the turn-based actions for this phase
	%  when its non-step "begins".
	game_actions(Phase, begins([])):-
		phase_actions(Phase).
	% Slight fudge. State-based actions 
	%  in phases without steps happen
	%  when those phases begin or end,
	%  not when their non-existent steps do. 

%%%%%%%%%%%%%%game_actions/2 (3) 30/12/2010

	% Otherwise, take the turn-based actions that need to 
	%  be taken when the current step begins. 
	game_actions(Phase, begins(Step)):-
		step_actions(Step).

%%%%%%%%%%%%%%game_actions/2 (4) 30/12/2010
	
	% When a phase or step ends, 
	% all mana pools empty. 
	game_actions(Phase, ends(Step)):-
		end_actions, 
		output(end_actions, []).
	% Slight fudge again. end-of-phase
	%  actions will happen at the end
	%  of a phase.

%%%%%%%%%%%%%%game_actions/2 (5) 30/12/2010
	
	% When a non-step ends, 
	% all mana pools empty. 
	game_actions(Phase, ends([])):-
		end_actions, 
		output(end_actions, []).

%%%%%%%%%%%%%%game_actions/2 (6) 30/12/2010
	
	% No turn-based actions are taken
	%  in the middle of a step or phase.
	game_actions(Phase, ongoing(Step)):-
		true.


%%%%%%%%%%%%%%phase_actions/1 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	/* phase_actions(+Phase) */
	% Take the game actions that need to be taken in this phase.

%%%%%%%%%%%%%%phase_actions/1 (1) 25/12/2010

	% Take the necessary phase actions. 
	phase_actions(Phase):-
		actions(Phase,Action),
		output(phase_actions, [Phase]),
		Action.
		% ^^ Reflect-Fu!!
		% There _is_ no separation
		% 'twixt data and operation :P

%%%%%%%%%%%%%%phase_actions/1 (2) 25/12/2010

	% If there are no actions in the current phase just succeed
	phase_actions(Phase):-
		true.


%%%%%%%%%%%%%%beginning/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Beginning phase actions	

	beginning:-
		true. 


%%%%%%%%%%%%%%first_main/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% First Main Phase actions

	first_main:-
		true. 


%%%%%%%%%%%%%%second_main/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% Second Main Phase actions

	second_main:-
		true.


%%%%%%%%%%%%%%%step_actions 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	/* step_actions(+Step) */
	% Take the game actions that need to be taken in the current step

%%%%%%%%%%%%%%step_actions/1 (1) 25/12/2010
	
	step_actions(Step):-
		actions(Step, Action),
		Action, 
		output(step_actions, [Step]).

%%%%%%%%%%%%%%step_actions/1 (2) 25/12/2010

	% If there are no actions in the current step just succeed
	step_actions(Step):-
		true.


%%%%%%%%%%%%%%%untap/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% Untap Step actions 

	untap:-
		active_player(Player),
		zone(Player, 'Battlefield', Permanents),
		untap(Player, Permanents).

	% Untap all of a player's permanents.
	untap(Player, []).
	untap(Player, [Permanent | Rest]):- 
		tap_untap(Permanent, Player, 'untap'), 
		untap(Player, Rest).
	% Needs checks for 'does not untap...' etc. 


%%%%%%%%%%%%%%%upkeep/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% Upkeep step actions

	upkeep:-
		true.


%%%%%%%%%%%%%%%draw/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% Draw step actions

	draw:-
		active_player(Player),
		draw_cards(Player, 1). 


%%%%%%%%%%%%%%%begin_combat/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% Beginning of Combat step actions

	begin_combat:-
		true.


%%%%%%%%%%%%%%%declare_attackers/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% Declare Attackers step actions

	declare_attackers:-
		true.


%%%%%%%%%%%%%%%declare_blockers/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% Declare Blockers step actions

	declare_blockers:-
		true.


%%%%%%%%%%%%%%%damage/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% Damage step actions

	damage:-
		true.


%%%%%%%%%%%%%%%end_combat/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% End Combat step actions

	end_combat:-
		true.


%%%%%%%%%%%%%%%end/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% End step actions 

	end:-
		true.


%%%%%%%%%%%%%%%cleanup/0 25/12/2010
%%%%%%%%%%%%%%%%%%%%%%% 
	% Cleanup step actions

	cleanup:-
		true.


%%%%%%%%%%%%%%end_actions/0 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%
	% At the end of each phase and step, mana pools empty	

	end_actions:-
		true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%receives_priority/3 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%	
	/* receives_priority(+Player, +Step, +Priority, +Play, +Stack, -New_priority) */
	% Determines which player should get priority (=New_priority).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	 I think I should only be assigning priority when 
		ongoing(Step), ongoing(Step)??? 
	 Players do get priority at the beginning of steps, no?
	 Depends. Normally players do receive priority at the beginning
	  of steps/ phases. The difference is that the game doesn't have
	  begins/ongoing/ends states. I use them just to know that it's not
	  time to fire turn-based actions. I could confine priority to 
	  ongoing states... though that's a hack.
	Actually, that's how I have it... change it?
*/

%%%%%%%%%%%%%%receives_priority/3 (1) 01/01/2011

	% No player receives priority at the end of a step
	receives_priority(Player, ends(Step), Priority, Play, Stack, []):-
		true.

%%%%%%%%%%%%%%receives_priority/3 (2) 30/12/2010

	% No player receives priority during the untap step
	receives_priority(Player, Status('Untap'), Priority, Play, [], []):-
		Status == begins
		->  
		output(receives_priority, [])
		; 
		true.

%%%%%%%%%%%%%%receives_priority/3 (3) 30/12/2010

	% No player receives priority in the Cleanup step
	%  unless there are objects on the stack. 	
	receives_priority(Player, Status('Cleanup'), Priority, Play, [], []):-
		Status == begins
		->  
		output(receives_priority, [])
		; 
		true.

	% And this won't work exactly like that- the stack is built 
	%  if there are objects to put on the stack. This should be implemented
	%  later on. 

%%%%%%%%%%%%%%receives_priority/3 (4) 30/12/2010

	% The active Player receives priority at the beginning of
	%  all other steps and phases.
	receives_priority(Player, begins(Step), Priority, Play, Stack, Player):-
		output(receives_priority, [Player]).

%%%%%%%%%%%%%%receives_priority/3 (5) 23/12/2010

%	% The active Player receives priority after a spell or ability
%	%  (other than a mana ability) resolves.
%	receives_priority(Player, Term(Step), Passed, Player):-
%		tab(25),
%		write('Priority player: '),
%		write(Player), nl.
%	% To be implemented- this will need to know that a spell or 
%	%  ability has resolved. I will have to deal with resolving spells
%	%  and abilities first, in player_actions; or maybe resolve_stack?.

%%%%%%%%%%%%%%receives_priority/3 (6) 30/12/2010

	% During the Step, if a player passes, priority is passed 
	%  to the next player in the order_of_play, after 
	%  the current Priority player
	% 'Pass' == 112
	receives_priority(Player, State(Step), Priority, 112, Stack, New_priority):-
		pass_priority(Priority, New_priority),
		output(receives_priority, ['new', New_priority]).

%%%%%%%%%%%%%%receives_priority/3 (7) 30/12/2010

	% During the step, if the current priority player
	%  does not pass, he or she receives priority again. 
	receives_priority(Player, State(Step), Priority, Play, Stack, Priority):-
		output(receives_priority, [Priority]).


%%%%%%%%%%%%%%%pass_priority/2 26/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%pass_priority/2 (1) 26/12/2010

	% Find the next player in the order of play
	pass_priority(Current_player, New_player):-
		order_of_play(Players),
		next(Current_player, New_player, Players).

%%%%%%%%%%%%%%pass_priority/2 (2) 26/12/2010

	% Reset the players pointer
	pass_priority(Current_player, First_player):-
		order_of_play([First_player | Rest]).

	% Note that this fires after all players have
	%  passed in succession when it shouldn't. 
	% It should fire when the next player who should
	%  get priority is the first in the order of play??
	% Fix it. 

	% Actually, when all players have passed in succession, 
	%  we don't need to pass priority. But I'm not checking 
	%  this here so it will have to get fixed later.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%player_actions/5 01/02/2011 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* player_actions(+Active_player, +Priority_player, +Step, +Phase, -New_play) */
	% The priority player can now take the actions allowed for this Phase and Step

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 	 Currently, Step and Phase are unused- play/5 must check that the
	  player actions taken are valid for the Priority player 
	  to take in the current Step or Phase. 
*/


%%%%%%%%%%%%%%player_actions/5 (1) 01/02/2011

	% No player actions are taken at the end of a Step
	%  or phase.
	player_actions(_, _, ends(_), _, []):-
		true.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	I was leaving New_play unbound here and it was getting bound in
	  the first clause of resolve_stack (the debug clause- so it was
	  becoming 100) below. I shouldn't be leaving "don't-know" values
	  in Play, Stack, Cue etc. 
*/

%%%%%%%%%%%%%%player_actions/5 (2) 01/02/2011

		% In steps where no player receives priority
		%  players don't take any actions.
		player_actions(Player, [], State(Step), Phase, []):- 
			State == begins 
			-> 
			output(player_actions, [])
			; true. 

%%%%%%%%%%%%%%player_actions/5 (3) 01/02/2011

   	% In a Step or Phase where a player has received priority, 
	%  that player can take the actions allowed for the Step or Phase. 
	player_actions(Active_player, Priority_player, Step, Phase, New_play):-
		Context = [Active_player, Priority_player, Step, Phase, New_play],
		input(player_actions, Context, Input) -> 	
		play(Active_player, Priority_player, Step, Phase, Input, New_play);
		player_actions(Active_player, Priority_player, Step, Phase, New_play).


%%%%%%%%%%%%%%play/6 01/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* play(+Active_player, +Priority_player, +Step, +Phase, +Play, -New_play) */

%%%%%%%%%%%%%%play/6 (1) 01/02/2011

	% l: 'Play Land'.
	play(Active_player, Priority_player, Step, Phase, 108, Play):- 
		play_land(Active_player, Priority_player, Step, Phase, Play).

%%%%%%%%%%%%%%play/5 (2) 30/12/2010

	% s: 'Cast Spells'
	play(Active_player, Priority_player, Step, Phase, 115, Play):-
		cast_spell(Priority_player, Play).
		%cast_spell(Priority_player, _Saved_state, Play).
%		output(play, [Priority_player, 'spells']).

%%%%%%%%%%%%%%play/5 (3) 30/12/2010

	% a: 'Activate Abilities'
	play(Active_player, Priority_player, Step, Phase, 97, 97):-
		output(play, [Priority_player, 'abilities']).

%%%%%%%%%%%%%%play/1 (4) 26/12/2010

	% t: 'Take Special Action' (other than play land)
	play(Active_player, Priority_player, Step, Phase, 116, 116):-
		output(play, [Priority_player, 'special']).

%%%%%%%%%%%%%%play/1 (5) 26/12/2010

	% h: 'Help'
	play(Active_player, Priority_player, Step, Phase, 104, 104):-
		output(play, [Priority_player, 'help']).

%%%%%%%%%%%%%%play/1 (6) 26/12/2010

	% p: pass
	play(Active_player, Priority_player, Step, Phase, 112, 112):-
	output(play, [Priority_player, 'pass']).

%%%%%%%%%%%%%%play/1 (7) 26/12/2010

	% c: Concede (uppercase c)
	play(Active_player, Priority_player, Step, Phase, 67, 67):-
		output(play, [Priority_player, 'concede']).

%%%%%%%%%%%%%%play/1 (8) 26/12/2010

	% d: debug
	play(Active_player, Priority_player, Step, Phase, 100, 100):-
		output(play, [Priority_player, 'debug']),
		true. 

%%%%%%%%%%%%%%play/1 (X) 26/12/2010

	% i: 'Inspect'
	play(Active_player, Priority_player, Step, Phase, 105, 105):-
		inspect_game(Priority_player).

	
%%%%%%%%%%%%%%%inspect_game/1 28/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* inspect_game(+Priority_player) */
	% Output information about the game state. 

	inspect_game(Priority_player):-
		Context = [Priority_player],
		input(inspect_game, Context, Input) -> 	
		inspect_output(Priority_player, Input);
		inspect_game(Priority_player).
		

%%%%%%%%%%%%%%inspect_output/2 28/01/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* inspect_output(+Priority_player, +Zone)*/
	% Outputs information about cards  in a Zone 
	%  controlled by the Priority_player. 

%%%%%%%%%%%%%%inspect_output/2 (1) 28/01/2011

	% l: library
	inspect_output(Priority_player, 108):-
		output(inspect_output, [Priority_player, 'library']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (2) 28/01/2011

	% h: hand
	inspect_output(Priority_player, 104):-
		output(inspect_output, [Priority_player, 'hand']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (3) 28/01/2011

	% s: stack
	inspect_output(Priority_player, 115):-
		output(inspect_output, [Priority_player, 'stack']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (4) 28/01/2011

	% b: battlefield
	inspect_output(Priority_player, 98):-
		output(inspect_output, [Priority_player, 'battlefield']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (5) 28/01/2011

	% g: graveyard
	inspect_output(Priority_player, 103):-
		output(inspect_output, [Priority_player, 'graveyard']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (6) 28/01/2011

	% e: exile
	inspect_output(Priority_player, 101):-
		output(inspect_output, [Priority_player, 'exile']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (7) 21/02/2011

	% m: mana pools
	inspect_output(Priority_player, 109):-
	output(inspect_output, [Priority_player, 'mana_pools']),
	   	inspect_game(Priority_player).

%%%%%%%%%%%%%%inspect_output/2 (8) 28/01/2011

	% c: cancel
	inspect_output(Priority_player, 99):-
		true.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%stack_resolves/6 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* stack_resolves(+Stack, +New_play, +Cue, +New_priority, -New_cue, -New_stack) */
	% Builds the stack and resolves obejcts from it (resolution to be implemented later)
	% builds the cue and passes it on to step_ends for the step to end.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	The Cue is the list of players who have passed in succession 
	  (without doing anything else in between). 
	If it is full, the Stack resolves. If the Stack is empty, 
	  the step or phase ends. 
	The Stack resolves one object at a time: the top object 
	  in it resolves, then that object's controller receives
	  priority again. 
*/

%%%%%%%%%%%%%%stack_resolves/6 (debug) 01/01/2011
% Debug clause

	stack_resolves(Stack, 100, Cue, New_priority, 'Full', []):-
		true.
		
%%%%%%%%%%%%%%stack_resolves/6 (1) 31/12/2010

	% In a step or phase where no players receive priority
	%  there should be no Stack to build. 
	stack_resolves([], [], [], [], [], []):-
		true.

%%%%%%%%%%%%%%stack_resolves/6 (2) 31/12/2010

	% In a step where players receive priority, 
	%  we need to manage the cue of players passing or playing
	%  and build a stack of objects to be resolved. 
	stack_resolves(Stack, New_play, Cue, New_priority, New_cue, New_stack):-
		cue(Stack, New_play, Cue, New_priority, New_cue), 
		debug_cue(New_cue),
		stack(Stack, New_play, New_cue, New_priority, New_stack). 


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	While debugging, we eventually fall here at the end of
	  a Step because at that point a player has received priority 
	  in the current step but New_play is not 100 any more (it's
	  cleared at ends(Step). This should not be a problem unless
	  subsequent clauses are incorrect. 
*/

%%%%%%%%%%%%%%%cue/4 31/12/2010ppp
%%%%%%%%%%%%%%%%%%%%%%%
	/* cue(+Stack, +New_play, +Cue, +New_priority, -New_cue) */

%%%%%%%%%%%%%%cue/4 (1.1) 02/02/2011

	% If the current priority player has cancelled
	%  an action, don't change the cue. 
	cue(Stack, 99, Cue, New_priority, Cue).
%	cue(Stack, New_play, Cue, New_priority, Cue):- 
%		New_play == 99; 
%		New_play == 105.
	% ^ Acually, this broke Concede, apparently. Wut?


%%%%%%%%%%%%%%cue/4 (1.2) 02/02/2011

	% If the current priority player has simply inspected
	%  the state of the game, don't change the cue. 
	cue(Stack, 105, Cue, New_priority, Cue).

%%%%%%%%%%%%%%cue/4 (1.3) 31/12/2010

	% If the current priority player has taken any action 
	%  but pass, inspect or cancel an action, clear the Cue. 
	cue(Stack, New_play, Cue, New_priority, []):-
		New_play \== 112. 

%%%%%%%%%%%%%%cue/4 (2.1) 31/12/2010
% 2.* clauses: the Priority player has passed
%  and adding him or her to the Cue will fill the Cue.
% If the Cue is not empty, first we need to check that it will not
%  overflow after the current priority player is added to it. 
% Of course this is relevant only if the player has passed- 
%   else the player is not added to the cue!

	% The Stack is empty and the Priority player 
	%  has passed. If adding him or her to the Cue
	%  will fill the Cue, return that the Cue is 'Full'.
	cue([], 112, Cue, New_priority, 'Full'):-
		(Cue \== [], Cue \== 'Full'),
		append(Cue, [New_priority], New_cue),
		full_cue(New_cue). 
	% This should be intercepted by step_ends/5 
	% which should end the step

%%%%%%%%%%%%%%cue/4 (2.2) 31/12/2010
	
	% The Stack is not empty or 'Full' and the Priority player
	%  has passed. If adding him or her to the Cue
	%  will fill the Cue, clear the Cue. 
	cue(Stack, 112, Cue, New_priority, []):-
		Stack \==[],
		(Cue \== [], Cue \== 'Full'),
		append(Cue, [New_priority], New_cue),
		full_cue(New_cue). 
	% This should be intercepted by stack/4, 
	%  which should resolve the first object
	%  on the Stack and allow a new player 
	%  to receive priority in the next level of turn/7.

%%%%%%%%%%%%%%cue/4 (3.1) 31/12/2010
% 3.* clauses: the Priority player has passed
%  and adding him or her to the cue will not fill the Cue.
% At this point the Stack doesn't matter because 
%  it will not begin to resolve until the Cue is full.  

	% The Priority player has passed and the Cue is 
	%  not empty or 'Full'. If adding him or her to the Cue 
	% will not fill the Cue, add that player to the Cue. 
	cue(Stack, 112, Cue, New_priority, New_cue):-
		(Cue \== [], Cue \== 'Full'),
		append(Cue, [New_priority], New_cue),
		not full_cue(New_cue). 

%%%%%%%%%%%%%%cue/4 (3.2) 31/12/2010

	% The Priority player has passed and the Cue is 
	%  empty or 'Full'. Adding that player to the Cue
	%  will not fill it up. Initialise the New_cue 
	%  with the Priority player as its first element.
	cue(Stack, 112, Cue, New_priority, [New_priority]):-
		(Cue == []; Cue == 'Full'). 
	
 
%%%%%%%%%%%%%%%full_cue/ 0 31/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%full_cue/0 (1) 30/12/2010

	% Check that the cue is full.
	full_cue(Cue):-
		order_of_play(Players),
		length(Players, Number_of_players), 
		call((length(Cue, Players_in_cue), !)), 
		Number_of_players == Players_in_cue.


%%%%%%%%%%%%%%%debug_cue/1 31/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

	debug_cue(Cue):-
		tab(25),
		write('Cue : '),
		write(Cue), nl.


%%%%%%%%%%%%%%%stack/4 24/02/2010
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%stack/5 (1) 24/02/2010

	% Previously, inspecting the game on an empty Cue caused
	%  the Stack to resolve. 
	stack(Stack, 105, _New_cue, _New_priority, Stack). %Inspect

%%%%%%%%%%%%%%stack/5 (2) 24/02/2010

	stack(Stack, 99, _New_cue, _New_priority, Stack). %Cancel

%%%%%%%%%%%%%%stack/5 (3) 24/02/2010

%	stack(Stack, 100, _New_cue, _New_priority, Stack). %Debug

%%%%%%%%%%%%%%stack/5 (4) 24/02/2010

	% The stack resolves when both players pass in succession on a
	%  non-empty Stack; ie, Cue == [] (yes indeed), Stack \== [] 
	stack(Stack, New_play, [], _Priority_player, Resolved_stack):-
		Stack \== [],
		resolve(Stack, Resolved_stack).

%%%%%%%%%%%%%%stack/5 (5) 24/02/2010

	% When the turn begins, the Stack is empty. We need to build it. 
	stack(_Stack, _New_play, _Cue, _Priority_player, New_stack):- 
		zone('Stack', New_stack).


%%%%%%%%%%%%%%resolve_stack/2 24/02/2010
%%%%%%%%%%%%%%%%%%%%%%%
	/* resolve_stack(+Object, -Stack) */
	% Resolves the first object from the Stack and returns the 
	%  rest of the Stack. 
	
	resolve([Object | Stack], Stack):- 
		zone('Stack', [Object | Stack]),
		zone(Player, 'Stack', [Object |_]),
		generate_effect(Object, Player).
		% output/2 call? Or in generate_effect?


%%%%%%%%%%%%%%%%%%%%%%%%%%%%step_ends/5 01/01/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* step_ends(+Step, +Phase, +New_cue, +New_priority, +New_stack, -New_step) */

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Values of State(Step) and State(New_step):
																	
	__Step state__	__New_step state__	__bindings__		__step progress__			
	begins(Step)	ongoing(New_step)		Step == New_step		beginning of the step		
	ongoing(Step)	ongoing(New_step)		Step == New_step		course of the step		
	ongoing(Step)	ends(New_step)		Step == New_step		before the end of the step	
	ends(Step)		begins(New_step)		step \== New_step		end of the step			
																	
	Note there is no begins(Step) followed by ends(New_step)!
	A step begins, then goes on, then ends. All three stages must be followed through.
*/

%%%%%%%%%%%%%%step_ends/5 (debug) 01/01/2011
% Debug clause

	step_ends(begins(Step), Phase, 'Full', New_priority, [], ends(Step)):-
		true.

%%%%%%%%%%%%%%step_ends/5 (1.1) 30/12/2010
% step_ends/5 clauses 1.*: players don't receive priority.

	% A step where players don't receive priority begins here.
	step_ends(begins(Step), Phase, [], [], [], ongoing(Step)):-
		true.

%%%%%%%%%%%%%%step_ends/5 (1.2) 30/12/2010

	% A step where players don't receive priority 
	%  continues while there turn-based actions to take
	% to be implemented
%	step_ends(ongoing(Step), Phase, [], [], [], ongoing(Step)):-
%		true.

%%%%%%%%%%%%%%step_ends/5 (1.3) 30/12/2010

	% A step where players don't receive priority 
	%  ends when all game actions have completed
	% Last bit to be implemented... 
	step_ends(ongoing(Step), Phase, [], [], [], ends(Step)):-
		true.
	% The stack is empty and no players have passed.

%%%%%%%%%%%%%%step_ends/5 (1.4) 30/12/2010

	% After a step where players don't receive priority
	%  ends, a new step begins. 
	step_ends(ends(Step), Phase, New_cue, [], New_stack, begins(New_step)):-
		next_step(Step, Phase, New_step),
		output(step_ends, [ends(Step)]).

%%%%%%%%%%%%%%step_ends/5 (2.1) 30/12/2010
% step_ends/5 clauses 2.*: players receive priority.

	% A step where players receive priority begins here
	step_ends(begins(Step), Phase, New_cue, New_priority, New_stack, ongoing(Step)):-
		true.
	% ^ Shouldn't the stack be empty here?

%%%%%%%%%%%%%%step_ends/5 (2.2) 30/12/2010

	% A step where players receive priority
	%  ends when all players pass in succession
	%  on an empty stack. 
	step_ends(ongoing(Step), Phase, 'Full', New_priority, [], ends(Step)):-
		true.

%%%%%%%%%%%%%%step_ends/5 (2.3) 30/12/2010

	% After the Step ends, the next one begins.
	step_ends(ends(Step), Phase, New_cue, New_priority, [], begins(New_step)):-
		next_step(Step, Phase, New_step), 
		output(step_ends, [ends(Step)]).

%%%%%%%%%%%%%%step_ends/5 (2.4) 30/12/2010

	% Otherwise, the Step goes on. 
	step_ends(ongoing(Step), Phase, New_cue, New_priority, New_stack, ongoing(Step)):-
		true.


%%%%%%%%%%%%%%%next_step/3 30/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%next_step/3 (1) 30/12/2010

	% Find the next step in the current Phase
	next_step(Current_step, Phase, Next_step):-
		phase(Phase, Steps_this_phase),
		next(Current_step, Next_step, Steps_this_phase). 

%%%%%%%%%%%%%%next_step/3 (2) 01/01/2011

	% If the current Phase has no more steps, find the first
	%  step in the next phase. 
	next_step(Current_step, This_phase, First_step):-
		phases(Phases), 
		next(This_phase, Next_phase, Phases),
		phase(Next_phase, [ First_step | Rest ]).

%%%%%%%%%%%%%%next_step/3 (3) 01/01/2011

	% The next Phase may have no steps.
	next_step(Current_step, This_phase, []):-
		phases(Phases), 
		next(This_phase, Next_phase, Phases),
		phase(Next_phase, []).

%%%%%%%%%%%%%%next_step/3 (4) 30/12/2010

	% Reset the step pointer to Untap
	%  (this happens when the turn ends)
	next_step(Current_step, Phase, 'Untap'):-
		true. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%phase_ends/2 23/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* phase_ends(+Phase, +Step, -New_phase) */

%%%%%%%%%%%%%%phase_ends/4 (1) 30/12/2010

	% A phase ends at the end of its last step
	%  and then we find a new phase
	phase_ends(Phase, ends(Last_step), Next_phase):-
		phase(Phase, Steps),
		reverse(Steps, [Last_step | Rest]), 
		next_phase(Phase, Next_phase),
		output(phase_ends, [Phase]).

%%%%%%%%%%%%%%phase_ends/4 (2) 30/12/2010

	% If this phase has no steps, 
	%  it ends here (temporarily- it needs to check
	%  for the proper conditions!)
	phase_ends(Phase, ends([]), Next_phase):-
		next_phase(Phase, Next_phase),
		output(phase_ends, [Phase]).

%%%%%%%%%%%%%%phase_ends/4 (3) 30/12/2010

	% If this is not the end of the Phase's last Step, 
	%  continue with this phase
	phase_ends(Phase, Step, Phase):-
		true. 

%%%%%%%%%%%%%%next_phase/2 (1) 25/12/2010

	% Find the next phase
		next_phase(Current_phase, Next_phase):-
		phases(Phases),
		next(Current_phase, Next_phase, Phases). 

%%%%%%%%%%%%%%next_phase/2 (2) 25/12/2010

	% Reset the phase pointer
		next_phase(Current_phase, 'Beginning'):-
		true. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%turn_ends/4 01/01/2011
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* 	turn_ends(+Player, +Phase, +New_step, -New_player) */

%%%%%%%%%%%%%%turn_ends/4 (1) 01/01/2011

	% In the Cleanup step the turn ends, so
	%  find the next player in the order of play
	turn_ends(Current_player, 'Ending', ends('Cleanup'), Next_player):-
		order_of_play(Players),
		next(Current_player, Next_player, Players), 
		cleanup_turn(Current_player),
		output(turn_ends, [Current_player]).

%%%%%%%%%%%%%%turn_ends/4 (2) 01/01/2011

	% Reset the players pointer
	turn_ends(Current_player, 'Ending', ends('Cleanup'), Next_player):-
		order_of_play([Next_player | _Rest]),
		cleanup_turn(Current_player),
		output(turn_ends, [Current_player]).

%%%%%%%%%%%%%%turn_ends/4 (3) 30/12/2010

	% Otherwise, the turn doesn't end 
	turn_ends(Player, Phase, Step, Player):-
		true.
		% I should be asserting the active_player here
		%  so I don't need to have one in the db. 
		% IF! that's a good idea. 	

%%%%%%%%%%%%%%%cleanup_turn/ 02/02/2011
%%%%%%%%%%%%%%%%%%%%%%%

	cleanup_turn(Current_player):-
%		player(Player),
		retractall(played_land((Current_player), _)),
		asserta(played_land((Current_player), 'no')). 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

