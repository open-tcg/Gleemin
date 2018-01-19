% First saved: 24/02/2011
% Last saved: 27/02/2011
%
%	Status:
%
%
%
%
% Doing:
%	having fun with brackets.
%		choose_targets looks OK. 
%		generate_effect...
%	trying to fix MGL mana def
%	noticed choose_targets doesn't allow for spells without targets. 
%	noticed that creature_type and permanent_type don't actually 
%	  check that something is on the battlefield (which they should)
%	  see 114.2
%
% Todo
%
%
%
% NOTES:
%	Handles both abilities and effects.
%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          Abilities Syntax         %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	Kind of works-ish. I can ask 
%	phrase(triggered_ability, ['Flying']).
%	and get yes, same for ability_word and some others. 


	ability --> spell_ability.
	ability --> activated_ability.
	ability --> triggered_ability.
	ability --> static_ability.
	ability --> ability_word.	%, "-", ability.
	ability --> keyword_ability.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%spell_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	spell_ability --> effect.
	spell_ability --> effect(Target).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%activated_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	activated_ability --> cost, [:], effect.
	activated_ability --> cost, [:], effect, activation_instructions.
	activated_ability --> mana_ability.
	%%activated_ability --> loyalty_ability.
	activated_ability --> keyword_ability.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%triggered_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% triggered_ability: non-terminals
	triggered_ability --> condition, [','], effect.
	triggered_ability --> mana_ability.
	triggered_ability --> keyword_ability.

% query: phrase(triggered_ability, [when, X]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%static_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	static_ability --> keyword_ability.
	static_ability --> effect.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%ability_word 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	ability_word --> ['Channel'].
	ability_word --> ['Chroma'].
	ability_word --> ['Domain'].
	ability_word --> ['Grandeur'].
	ability_word --> ['Hellbent'].
	ability_word --> ['Imprint'].
	ability_word --> ['Kinship'].
	ability_word --> ['Landfall'].
	ability_word --> ['Metalcraft'].
	ability_word --> ['Radiance'].
	ability_word --> ['Sweep'].
	ability_word --> ['Threshold'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%keyword_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	keyword_ability --> ['Deathtouch'].
	keyword_ability --> ['Defender'].
	keyword_ability --> ['Defender'].
	keyword_ability --> ['Double Strike'].
	keyword_ability --> ['Enchant'].
	keyword_ability --> ['Equip'].
	keyword_ability --> ['First Strike'].
	keyword_ability --> ['Flash'].
	keyword_ability --> ['Flying'].
%	keyword_ability --> ['Flying'], {write('Flyiiiinggg!!'), nl}.
	keyword_ability --> ['Haste'].
	keyword_ability --> ['Intimidate'].
	keyword_ability --> ['Landwalk'].
	keyword_ability --> ['Lifelink'].
	keyword_ability --> ['Protection'].
	keyword_ability --> ['Reach'].
	keyword_ability --> ['Shroud'].
	keyword_ability --> ['Trample'].
	keyword_ability --> ['Vigilance'].
	keyword_ability --> ['Banding'].
	keyword_ability --> ['Rampage'].
	keyword_ability --> ['Cumulative Upkeep'].
	keyword_ability --> ['Flanking'].
	keyword_ability --> ['Phasing'].
	keyword_ability --> ['Buyback'].
	keyword_ability --> ['Shadow'].
	keyword_ability --> ['Cycling'].
	keyword_ability --> ['Echo'].
	keyword_ability --> ['Horsemanship'].
	keyword_ability --> ['Fading'].
	keyword_ability --> ['Kicker'].
	keyword_ability --> ['Flashback'].
	keyword_ability --> ['Madness'].
	keyword_ability --> ['Fear'].
	keyword_ability --> ['Morph'].
	keyword_ability --> ['Amplify'].
	keyword_ability --> ['Provoke'].
	keyword_ability --> ['Storm'].
	keyword_ability --> ['Affinity'].
	keyword_ability --> ['Entwine'].
	keyword_ability --> ['Modular'].
	keyword_ability --> ['Bushido'].
	keyword_ability --> ['Soulshift'].
	keyword_ability --> ['Splice'].
	keyword_ability --> ['Offering'].
	keyword_ability --> ['Ninjutsu'].
	keyword_ability --> ['Epic'].
	keyword_ability --> ['Convoke'].
	keyword_ability --> ['Dredge'].
	keyword_ability --> ['Transmute'].
	keyword_ability --> ['Bloodthirst'].
	keyword_ability --> ['Haunt'].
	keyword_ability --> ['Replicate'].
	keyword_ability --> ['Forecast'].
	keyword_ability --> ['Graft'].
	keyword_ability --> ['Recover'].
	keyword_ability --> ['Ripple'].
	keyword_ability --> ['Split Second'].
	keyword_ability --> ['Suspend'].
	keyword_ability --> ['Vanishing'].
	keyword_ability --> ['Absorb'].
	keyword_ability --> ['Aura Swap'].
	keyword_ability --> ['Delve'].
	keyword_ability --> ['Fortify'].
	keyword_ability --> ['Frenzy'].
	keyword_ability --> ['Gravestorm'].
	keyword_ability --> ['Poisonous'].
	keyword_ability --> ['Transfigure'].
	keyword_ability --> ['Champion'].
	keyword_ability --> ['Changeling'].
	keyword_ability --> ['Evoke'].
	keyword_ability --> ['Hideaway'].
	keyword_ability --> ['Prowl'].
	keyword_ability --> ['Reinforce'].
	keyword_ability --> ['Conspire'].
	keyword_ability --> ['Persist'].
	keyword_ability --> ['Wither'].
	keyword_ability --> ['Retrace'].
	keyword_ability --> ['Devour'].
	keyword_ability --> ['Exalted'].
	keyword_ability --> ['Unearth'].
	keyword_ability --> ['Cascade'].
	keyword_ability --> ['Annihilator'].
	keyword_ability --> ['Level Up'].
	keyword_ability --> ['Rebound'].
	keyword_ability --> ['Totem Armor'].
	keyword_ability --> ['Infect'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%mana_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%	mana_ability --> [mana].
%	mana_ability --> [add,Mana,'to your mana pool'].
	%mana_ability --> ['add'],mana,['to', 'your', 'mana', 'pool'].

/*	mana_ability --> cost, [:], ['add'],mana,['to', 'your', 'mana', 'pool'].
	% Triggered mana ability definitions are probably not correct, see 605.1b
	mana_ability --> trigger_word, trigger_event, ['add'],mana,['to', 'your', 'mana', 'pool'].
	mana_ability --> trigger_word, trigger_event(X), ['add'],mana,['to', 'your', 'mana', 'pool'].
*/	

	mana_ability --> cost, [:], ['add'],mana,['to'], player(Player), ['mana', 'pool'].
	% Triggered mana ability definitions are probably not correct, see 605.1b
	mana_ability --> trigger_word, trigger_event, ['add'],mana,['to'], player(Player), ['mana', 'pool'].
	mana_ability --> trigger_word, trigger_event(X), ['add'],mana,['to'], player(Player), ['mana', 'pool'].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%loyalty_ability 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%%loyalty_ability --> [loyalty Loyalty].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%cost 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	cost --> mana_cost.
	cost --> [tap].
	% ^ Will need to deal with "tap a permanent you control" type
	%  of costs!
	cost --> mana_cost, [tap].
	cost --> [tap], mana_cost.

	mana_cost --> mana.
%	mana_cost --> mana(X).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%condition 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% condition: non-terminals
	condition --> trigger_word, trigger_event(X).
	condition --> trigger_word, trigger_event.

	% condition: terminals
	trigger_word --> ['When'];['Whenever'];['At'].
	%^ needs further disambiguation, otherwise it may accept 
	%  "At enters the battlefield".

	trigger_event(X) --> [X], ['enters', 'the', 'Battlefield'].
	trigger_event --> [placeholder].

% Not sure about those above, try phrase(triggered_ability, X). 

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%effect 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	effect --> effect(X).
	effect(X) --> return(X).
	effect(X) --> tap(X).
	effect(X) --> untap(X).
	effect(X) --> destroy(X).
	effect(X) --> regenerate(X).
	effect(X) --> exile(X).

	% effects
	return(X) --> (['Return'] ; ['return']), all(X), [to, their, 'owner''s', hands].
	return(X) --> (['Return'] ; ['return']), target(X), [to, its, 'owner''s', hand].
	tap(X) --> (['Tap'] ; ['tap']), target(X).
	tap(X) --> [tap], all(X).
	untap(X) --> [untap], target(X).
	untap(X) --> [untap], all(X).
	destroy(X) --> [destroy], target(X).
	destroy(X) --> [destroy], all(X).
	regenerate(X) --> [regenerate], target(X).
	regenerate(X) --> [regenerate], all(X).
	exile(X) --> [exile], target(X).
	exile(X) --> [exile], all(X).

/*
%	effect --> ['Choose one - '], effect(X), [;], [or], effect(Y).
	effect --> mode, effect(X), [;], [or], effect(Y).
	%effect(Player) --> mode(Player), effect(X), [;], [or], effect(Y).
	% ^ backstack full
	mode(Player) --> player(Player), [' chooses one -'].
	mode --> ['Choose one - '] ; ['Choose two - '] ; ['Choose one  or both- '].
%	effect(X) --> ['Choose one'], effect(X).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%activation_instructions 27/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	activation_instructions --> [with], keyword_ability.
	%activation_instructions --> [placeholder].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%target 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	target(Permanent) --> ['target', 'permanent'], {permanent_type(Permanent)}.
	% ^ also check needed that it's on the Battlefield
	target(Permanent) --> ['target', Type], permanent(Permanent, Type).

	%target(Player) --> ['target', 'player'], {player(Player)}.

	%target(Zone) --> ['target', Zone], {zone(Zone, _)}.
	%target(Zone, Player) --> ['target', 'player''s', Zone], {zone(Player, Zone, _)}.
	% ^ call with phrase(target(X, Z), Y).

	permanent(Permanent, Type) --> {check_type(Permanent, _,[Type],_)}.
	% ^ this finds all types, not just permanents.
	player(Player) --> {player(Player)}.
	player(You) --> [you].
	player(You) --> [your].

	all(Creatures) --> ['all', 'creatures'], 
		{findall(Creature, creature_type(Creature), Creatures) }.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%mana 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%	xxy(Y) --> [X], {string_to_list(X, Y)}.
%  	phrase(xxy([23,g,r]), ['23gr']).

%	mana(X) --> [Y], {string_to_list(X,Y)}.
	% ^ Y is the list form of string X

%	mana --> [C], {mana_cost(C)}.
	% OK, but this won't generate mana strings... 


	% mana: non-terminals
	mana --> symbol_string. 
	mana --> colourless_mana. 
	mana --> colourless_mana, symbol_string. 
	symbol_string --> mana_symbol. 
	symbol_string --> mana_symbol, symbol_string. 

	% mana: terminals
	mana_symbol --> [w] ; [u] ; [b] ; [r] ; [g]. 
	colourless_mana --> [C], { number(C) }.
	% ^ but query with phrase(mana, [<number>])!!

	% But u,g,r... etc is not a correct mana string!
	% I have to revise this. Possibly define in tuerms of mana_string/X? 


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	OK; test with: 
	phrase(mana_ability, [add,w,to,your,mana,pool]).
	phrase(mana_ability, [add,1,to,your,mana,pool]).
	phrase(mana_ability, [add,w,u,r,g,g,g,to,your,mana,pool]).
	phrase(mana_ability, [add,10000,w,to,your,mana,pool]).
	phrase(mana_ability, [add,125787,w,u,r,g,g,g,to,your,mana,pool]).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%     Effects & Abilities rules     %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%determine_abilities/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* determine_abilities(+Card, ?Abilities) */
	% Returns a card's abilities, each in a separate list. 

	determine_abilities(Card, Abilities):-
		card([card_name Card, _, _, text_box Text_box, _, _, _, _]),		
		determine_abilities(Text_box, [], Abilities).


%%%%%%%%%%%%%%determine_abilities/3 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%determine_abilities/3 (0) 25/02/11

	determine_abilities([], Abilities, Abilities).

%%%%%%%%%%%%%%determine_abilities/3 (1) 25/02/11

	determine_abilities(Text_box, Temp, Abilities):- 
		member(Ability, Text_box), 
		phrase(ability, Ability), 
		remove(Ability, Text_box, New_text_box), 
		append(Temp, [Ability], New_temp),
		determine_abilities(New_text_box, New_temp, Abilities).


/*
700.2. A spell or ability is modal if it has two or more options 
preceded by "Choose one  -- ," 
"Choose two  -- ," 
"Choose one or both  -- ," 
or "[a specified player] chooses one  -- ." 
Each of those options is a mode.
*/


%%%%%%%%%%%%%%mana_ability/3 20/02/2011
%%%%%%%%%%%%%%%%%%%%%%%
	/* mana_ability(+Permanent, ?Ability, ?Mana) */
	% One mana ability of a permanent and the Mana it generates

	mana_ability(Permanent, Ability, Mana):- 
		card([card_name Permanent, _, _, text_box Abilities, _, _, _, _]),
		member(Ability, Abilities),
		phrase(mana_ability, Ability), 
		Ability = [_Cost, :, 'add', Mana, 'to', 'your', 'mana', 'pool'].

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Mana abilities must be callable independently of 
	  activate_abilities/3, since the same timing rules
	  don't necessarily apply and their Effects don't 
	  use the stack. 

To test without running main: 
asserta(zone( 'Player 2', 'Battlefield', [copy('Plains',1,[]), copy('Air Servant',1,[])] )).
asserta(zone( 'Player 1', 'Battlefield', [copy('Plains',1,[]), copy('Swamplains',1,[])] )).
zone(Player, 'Battlefield', Permanents).
activate_ability('Player 1', ['Swamplains', 1], Object).
add_mana([tap],[add,w,'to your mana pool']).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%determine_cost/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* determine_abilities(+Spell, +Ability, ?Cost) */
	% Returns the mana cost of a spell or the activation cost of one 
	%  ability of a permanent. 

	% The cost is a mana cost. 
	determine_cost(Spell, [], [Cost]):-
		card([card_name Spell, mana_cost Cost, _, _, _, _, _, state State]).
	% Needs to deal with variable (X) and hybrid (g/r) costs. 

	% The cost is an activation cost 
	determine_cost([], Ability, Cost):-
		sublist(Cost, Ability),
		phrase(cost, Cost).
	% ^ Merh. This found that Cost = [tap, u] (on backtracking 
	% after finding Cost = tap). Maybe I should define cost more 
	%  rigorously, as what comes after a colon... 

	% Yep, see? These two determine a mana cost differently; one is a 
	%  mana string: 2341ubgrw, the other is a list of mana symbols: 
	%  2341u,b,g,r,w. That's because I have a new definition in here
	% I should  bring it in line with the older one, I think it's more
	%  convenient. 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Note that for activated abilities, this will return the whole
	  cost. Further processing should sepearate individual costs and 
	  attempt to satisfy them in turn.
	I decided to develop this program because I knew it would let me 
	  say things like that. 
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%additional_cost/2 27/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
	% The Spell has an additional cost
	additional_cost(Spell, [], Cost):-
		card([card_name Spell, _, _, text_box Abilities, _, _, _, _]),
		sublist(Cost, Abilities),
		phrase(additional_cost, Cost).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%alternative_cost/2 27/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
/*
	determine_cost(Spell, [], Cost):-
		card([card_name Spell, _, _, text_box Abilities, _, _, _, _]),
		sublist(Cost, Abilities),
		phrase(additional_cost, Cost).
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%choose_targets/4 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* choose_target(+Controller, +Object, +Ability, -Updated) */
	% Takes in the user's choice of target, checks it for validity 
	%  and returns it. 

% Looks OK. Needs to deal with multi- abilities. 


	% The Object has no abilities, or all its abilities have been 
	%  processed. 
	choose_targets(Player, Object, [], Object). 
	% OK
	
	% The Object is a targeted ability on the stack 
	choose_targets(Player, Object, Ability, Updated):- 
		% Could send in a [] Ability...? Later...
		Object = copy(Ability, _Id, _State), 
		write(`Choose target(s): `), nl, flush, 
		fread(a, 0, -1, Name),  flush, 
		%member(Ability, Text_box), 
		(sublist(Targets, Ability), 
		phrase(target(Name), Targets)),
		% ^ also check for phrase(target(X,Y)...
		write(`Choose Id: `), nl, flush, 
		fread(n, 0, 0, Id), flush, 
		zone('Battlefield', Permanents),
		member(copy(Name, Id, State), Permanents),
		Target = target(copy(Name, Id, State)),
		% ^ Now we have its State, do some checking for 
		%%  "untargettable" etc. 
		add_state(Object, Target, Updated),
		change_state(Object, 'Stack', Player, Updated). 
	% OK
	
	% The Object is a spell on the stack; its first Ability is not
	%  a spell ability 
	choose_targets(Player, Object, Text_box, Updated):- 
		member(Ability, Text_box), 
		\+ phrase(spell_ability, Ability), 
		remove(Ability, Text_box, Rest_box), 
		choose_targets(Player, Object, Rest_box, Updated). 

	% The Object is a spell on the stack and its current ability is 
	%  a targeted spell ability. 
	choose_targets(Player, Object, Text_box, Updated):- 
		write(`Choose target(s): `), nl, flush, 
		fread(a, 0, -1, Name),  flush, 
		member(Ability, Text_box), 
		phrase(spell_ability, Ability),
		(sublist(Targets, Ability),  
		phrase(target(Name), Targets)),
		% ^ also check for phrase(target(X,Y)...
		write(`Choose Id: `), nl, flush, 
		fread(n, 0, 0, Id), flush, 
		zone('Battlefield', Permanents),
		member(copy(Name, Id, State), Permanents),
		Target = target(copy(Name, Id, State)),
		% ^ Now we have its State, do some checking for 
		%%  "untargettable" etc. 
		add_state(Object, Target, Updated),
		change_state(Object, 'Stack', Player, Updated), 
		remove(Ability, Text_box, Rest_box),
		choose_targets(Player, Updated, Rest_box, New_updated). 
									% ^ get rid of it

/*
| ?- Aba = aba, write(Aba) ~> X, fread( a, 0, -1, A ) <~ X.
Aba = A = aba ,
X = `aba`

*/


/*

%%%%%%%%%%%%%%choose_targets/4 (1) 26/02/11

	% A permanent without any Abilities
	choose_targets(_Player, Object, [], Object). 

		
%%%%%%%%%%%%%%choose_targets/4 (1) 26/02/11

	% A non-targeted Spell Ability. 
	choose_targets(_Player, Object, [], Object):- 
		%member(Ability, Text_box), 
		\+ (sublist(Targeted, Ability),
		phrase(target(_Name), Targeted)).

%%%%%%%%%%%%%%choose_targets/4 (1) 26/02/11

	% This Ability needs targets. 
	choose_targets(Player, Object, Text_box, Updated):- 
		write(`Choose target(s): `), nl, flush, 
		fread(a, 0, -1, Name),  flush, 
		%member(Ability, Text_box), 
		(sublist(Targets, Ability), 
		phrase(target(Name), Targets)),
		% ^ also check for phrase(target(X,Y)...
		write(`Choose Id: `), nl, flush, 
		fread(n, 0, 0, Id), flush, 
		zone('Battlefield', Permanents),
		member(copy(Name, Id, State), Permanents),
		Target = target(copy(Name, Id, State)),
		% ^ Now we have its State, do some checking for 
		%%  "untargettable" etc. 
		add_state(Object, Target, Updated),
		change_state(Object, 'Stack', Player, Updated). 

*/

/*
	Test with: 
	choose_targets('Player 1', copy('Unsummon',1,[]), 
		[['Return',target,'Creature',to,its,'owner''s',hand]], Updated).	

	Or with generate_effects:
	choose_targets('Player 1', copy('Unsummon',1,[]), 
		[['Return',target,'Creature',to,its,'owner''s',hand]], Updated), 
		generate_effect(Updated, 'Player 1').
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%generate_effect/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* generate_effect(+Object, +Player) */

	% Temp: permanents will need to have their spell abilities checked
	%  while being cast. 
	generate_effect(Object, Player):- 
		Object = copy(Name, _Id, _State),
		permanent_type(Name),
		move_to_zone(Player, Object, 'Stack', 'Battlefield'), 
		tab(25), write(`Effect: ` - Object), nl,
		output(inspect_output, [_Priority_player, 'battlefield']).

	% Trying with Unsummon . 
	generate_effect(Object, Player):- 
		\+ permanent_type(Object),
		% Object : copy(Name, Id, State)
		%   where member(target(Target), State).
		determine_effect(Object, Effect, Target), 
		gen_effect(Effect, Target),
		% tab(25), write(`Effect: ` - Object), nl,
		output(inspect_output, [_Priority_player, 'battlefield']),
		nl, 
		output(inspect_output, [Player, 'hand']),
		move_to_zone(Player, Object, 'Stack', 'Graveyard'),
		Object = copy(Name, _, _),
		tab(25), write(`Resolved effect: ` - Name), nl,
		output(inspect_output, [_Priority_player, 'graveyard']).


%%%%%%%%%%%%%%determine_effect/3 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* determine_effect(+Object, -Effect, -Recipients) */
	% Determines the Effect that must be generated by an Object on 
	%  the Stac and the Recipients to be affected by it. 

%%%%%%%%%%%%%%determine_effect/3 (1) 25/02/11

	% The Object is a targeted effect (effect(X) and its target is 
	%  in its State, as a copy/3 entry, 
	determine_effect(Object, Effect, Target):- 
		Object = copy(Name, _Id, State), 
		%  eg State: [ target(copy('Maritime Guard', 1, [])) ]
		member(target(Target), State),
		Target = copy(Target_name, _Target_Id, _Target_State), 
		% ^ State could have "untargettable" in, check later
		card([card_name Name, _, _, text_box Text_box, _, _, _, _]),
		%member(Ability, Text_box), 
		sublist(Effect, Ability), 
		phrase(effect(Target_name), Effect).

%%%%%%%%%%%%%%determine_effect/3 (1) 25/02/11

	% The Object is a targeted ability on the stack
	determine_effect(Object, Effect, Target):- 
		Object = copy(Ability, _Id, State), 
		%  eg State: [ target(copy('Maritime Guard', 1, [])) ]
		member(target(Target), State),
		Target = copy(Target_name, _Target_Id, _Target_State), 
		sublist(Effect, Ability), 
		phrase(effect(Target_name), Effect).


%%%%%%%%%%%%%%gen_effect/2 25/02/11
%%%%%%%%%%%%%%%%%%%%%%%

	% "Bounce" effect.
	gen_effect(Effect, Target):- 
		Effect = ['Return',target,Permanent,to,its,'owner''s',hand],
		Target = copy(Name, Id, _State),
		owned_by(Target, Owner),
		move_to_zone(Owner, Target, 'Battlefield', 'Hand'),
		write(Name - Id - ` returned to ` - Owner - `'s hand`), nl.

/*
	Call: 
	generate_effect(copy('Unsummon', 1,
		 [target(copy('Air Servant', 1, []))] ), 'Player 1').

	Use move_to_zone/4 to populate the Battlefield and Stack:

	move_to_zone('Player 2', 'Maritime Guard', 'Hand', 'Battlefield').
	move_to_zone('Player 1', 'Unsummon', 'Hand', 'Stack').

	Remember: 
	add_state(copy('Unsummon',1,[]), target(copy('Maritime Guard',1,[])),Updated),
	change_state(copy('Unsummon',1,[]), 'Stack', 'Player 1', Updated).

*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*

	mana --> mana_symbol. 
	mana --> digit.
	mana --> mana, mana_symbol.
	mana --> digit, mana_symbol.
	%mana --> digit, mana. 
%	mana --> colourless_mana, mana_symbol. 

%	colourless_mana --> [1].
	mana_symbol --> [w] ; [u] ; [b] ; [r] ; [g]. 
	%colourless_mana --> [C], { number(C) }.


	digit --> [N], { N>= 0, N =<9 }.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%             Section               %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%predicate
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%child predicate
%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%clause

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* _*/




