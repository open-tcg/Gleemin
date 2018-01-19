% First saved: 15/02/2011
% Last saved:  15/02/2011
%
%	Stable step but I don't know what changed since the last; just in case.
%
% 	Status: 
%	OK
%
% Doing:
%	Fixed concede error (turn_sequence lacked boundary condition).
% Todo: 
%
% NOTES: 
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%         Swi Compatibility         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- set_prolog_flag(back_quotes,string).
	% Swi LPA backtic string compatibility.
	:- style_check(-singleton).
	% Stop Swi warning on singleton vars.
	:- style_check(-discontiguous).
	% Stop Swi warning on discontiguous clauses.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Main Game Loop          %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%main/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%main/1 (1) 16/12/2010

	main:-
		new_run(yes),
		initialise,
		retractall(new_run(yes)),
		welcome,
		fail.

%%%%%%%%%%%%%%main/1 (2) 16/12/2010

	main:-
		input(main, _, Input) -> 
		do(Input);
		main.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%new_run/1 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	new_run(yes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%source_files/X 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
	source_files(['predicates.lb', 
		'players.kb', 
		'zones.kb', 
		'decks.kb',
		'turn_sequence.pl']).
*/

	source_files([
				'swi_compatibility_layer3.pl',
				'decks.pl',
				'mtg_operators.pl',
				'players.pl', 
				'predicates.pl', 
				'testing_cards_27-02-11.pl',
				'testing_input_output_23-02-11.pl',
				'testing_MGL_interpreter_27-02-11-2.pl',
				'testing_player_actions_28-02-11.pl',
				'testing_turn_sequence_26-02-11.pl', 
				'testing_zones_25-02-11.pl'
			]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%initialise/0 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	initialise:-
		source_files(Files),
		load(Files),
		declare_dynamic_facts,
		write(`Initialisation OK`), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%load/1 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%load/1 (1) 18/12/2010

	% Load source files and output their names. 
	load(Files):-
		member(File, Files),
		consult(File),
		write(File), 
		write(` loaded`), nl, 
		fail.

%%%%%%%%%%%%%%load/1 (2) 18/12/2010

	% No more source files to load.
	load(_):-
		write(`Loading files OK`), nl.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%declare_dynamic_facts/0 18/12/2010
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	declare_dynamic_facts:-
		dynamic(new_run/1), 
		write(`Declaring dynamic facts OK`), nl.


%%%%%%%%%%%%%%welcome/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

	welcome:-
		nl,
		write(`Welcome to Gleemin, the Magic Virtual Machine.`).


%%%%%%%%%%%%%%do/1
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%do/1 (1) 16/12/2010

	%s: start
	do(115):-
		reset_game,
		populate_libraries, 
		shuffle_libraries, 
		draw_starting_hand,
		turn('Player 1', 'Beginning', begins('Untap'), [], [], [], []),
		main.

%%%%%%%%%%%%%%do/1 (2) 16/12/2010

	% h: help
	do(104):-
		output(main, ['help']),
		main.	

%%%%%%%%%%%%%%d/1 (3) 16/12/2010

	% q: quit
	do(113):-
		goodbye.


%%%%%%%%%%%%%%goodbye/1 16/12/2010
%%%%%%%%%%%%%%%%%%%%%%%

	goodbye:-
		nl,
		write(`Thank you for playing with Gleemin.`),
		assert(new_run(yes)), nl.


%%%%%%%%%%%%%%reset_game/0	24/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% clear facts from the kb and assert that it's a new game. 

	reset_game:-
		clear_kb,
		retractall(new_game(_)),
		assert(new_game(yes)). 
		% ^ The first turn in a new game needs to know
		%  that this is a new game. 


%%%%%%%%%%%%%%populate_libraries/0	24/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% turn the players' decks to their libraries.

%%%%%%%%%%%%%%populate_libraries/0 (1) 24/01/11

	populate_libraries:- 
		player(Player),
		deck_to_library(Player, Deck, Library), 
		fail.

%%%%%%%%%%%%%%populate_libraries/0 (2) 24/01/11

	populate_libraries:-
		true.


%%%%%%%%%%%%%%shuffle_libraries/0	24/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% shuffle players' libraries

%%%%%%%%%%%%%%shuffle_libraries/0 (1) 24/01/11

	shuffle_libraries:- 
		player(Player),
		shuffle(Player, Shuffled), 
		fail.

%%%%%%%%%%%%%%shuffle_libraries/0 (2) 24/01/11

	shuffle_libraries:-
		true.


%%%%%%%%%%%%%%draw_starting_hand/0	24/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% draw a starting hand (six cards) for all players
	% A little more work needed here. Simply failing just draws
	%  the whole library for all players. 

	draw_starting_hand:-
		order_of_play(Players),
		draw_six(Players).

%%%%%%%%%%%%%%draw_six/1 24/01/11
	
	% Boundary condition... no more players to draw six cards for.
	draw_six([]).

%%%%%%%%%%%%%%draw_six/1 24/01/11

	% draw six cards and proceed with the remaining players.
	draw_six(Players):-
		member(Player, Players), !,
		draw_cards(Player, 7), 
		remove(Player, Players, Rest), !,
		draw_six(Rest).


%%%%%%%%%%%%%%clear_kb/0	23/01/11
%%%%%%%%%%%%%%%%%%%%%%%
	% clear kb of previous game facts.
	% This causes 

%%%%%%%%%%%%%%clear_kb/0 (1) 24/01/11

	clear_kb:-
		source_files(Files),
		member(File, Files),
		consult(File),
		fail.

%%%%%%%%%%%%%%clear_kb/0 (2) 24/01/11

	clear_kb:-
		true.






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%












