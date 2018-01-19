
Files in this archive: 

	decks.pl
		> decklists for each player
	main.pl
		> entry point; enter consult(main.pl) then run with "main." to launch. 
	mtg_operators.pl
		> operator definitions for Gleemin.
	players.pl
		> player data and functions. 
	predicates.pl
		> general utility predicates.
	testing_cards_27-02-11.pl
		> card data and functions. 
	testing_creatures_15-02-11.pl
		> creature combat sandbox.
	testing_input_output_23-02-11.pl
		> i/o predicates (only text i/o for now).
	testing_mgl_interpreter_27-02-11-2.pl
		> MGL syntax and ability/ effect functions.
	testing_player_actions_28-02-11.pl
		> predicates for playing land, casting spells and activating abilities. 
	testing_turn_sequence_26-02-11.pl
		> main turn loop. 
	testing_zones_25-02-11.pl
		> zone data and functions. 

	sorted cardset.txt lists the Oracle text of the cards in the M11 introductory decks. That's the cardpool for my university project. Currently, only basic lands, vanilla creatures and two spells, Unsummon and Boomerang, are actually playable. 

Note that testing_turn_sequence_26-02-11.pl (and possibly others) will cause a lot of warnings about free radical variables on compilation. This reporting can be suppressed from the Win-Prolog main menu: 
	> Options 
		> Prolog Flags
			> Compiler Messages
				> untick Single Variables. 