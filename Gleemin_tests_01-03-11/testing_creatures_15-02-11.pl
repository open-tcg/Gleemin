% First saved: 13/02/11
% Last saved: 14/02/11
%
% 	Status:
% 	a) Very Simple combat for testing.
%	  Each player has exactly one of each creature
%  	  already on the battlefield
%	b) Lots of appropriate input/output needed
%	c) Multi-blocking creatures seem to cause trouble.

% Doing:
%	Cleanups
%	Allow the player to assign damage freely; see NOTES
% Todo
%
% NOTES:
/*	This is not how it works. Damage is not assigned automatically.
	Players can choose to assign more than lethal damage to a 
	  creature, even if they don't need to in order to kill it. 
	For example, a player attacking with an Ogre, blocked by a 
	  Soldier and a Bear in that order, can assign all 3 of its
	  damage to the Bear and 0 to the Soldier. In general, 
	  it's the player who decides how much damage to assign, not
	  the game. You should ask the player

	Also note that the flow here, where each step calls the other
	  can't really work like that- in turn_sequence, steps are 
	  separate. 
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Combat facts            %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- dynamic(creature/4).
	:- dynamic(attackers/2).
	:- dynamic(blockers/2).
	:- dynamic(player/2).

% 	Facts only for testing. The right ones are already defined
%	Well, creatures aren't really... maybe I should use these for a while,
%	  only with an Id field.

	% creature(Name, Power, Toughness, State)
	creature('Goblin', 1, 1, []).
	creature('Soldier', 2, 1, []).
	creature('Bear', 2, 2, []).
	creature('Cat', 2, 2, []).
	creature('Ogre', 3, 3, []).
	creature('Giant', 4, 4, []).
	creature('Angel', 5, 5, []).
	creature('Demon', 5, 5, []).
	creature('Dragon', 6, 6, []).
	% Where: blocking is blocking(Attacker) in State
	%  and blocked is blocked_by(Blocker) in State.

	% player(Name, Life)
	player('Player 1', 20).
	player('Player 2', 20).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%attackers/2 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* attackers(+Player, +Creatures) */
	% List of attacking Creatures by Player

	attackers('Player 1', []).
	attackers('Player 2', []).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%blockers/2 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* blockers(+Player, +Creatures) */
	% List of blockers by Player

	blockers('Player 1', []).
	blockers('Player 2', []).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%            Combat rules           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%attacker/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  attacker(+Creature, +Controller, +Opponent) */
	% Adds a Creature to the list of attackers

	attacker(Creature, Controller, Opponent):- 	
		creature(Creature, _Power, _Toughness, _State), 
		player(Controller, _Life_1), 
		player(Opponent, _Life_2), 
		attackers(Controller, Attackers), 
		append(Attackers, [Creature], New_attackers), 
		retractall(attackers(Controller, Attackers)), 
		asserta(attackers(Controller, New_attackers)).
	% Each creature in the attackers list is an attacking creature.
	% Shouldn't need to make it explicit.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%blocker/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* blocker(+Creature, +Controller, +Opponent) */
	% Adds a Creature to the list of blockers

	blocker(Creature, Controller, Opponent):- 	
		creature(Creature, _Power, _Toughness, _State), 
		player(Controller, _Life_1), 
		player(Opponent, _Life_2), 
		blockers(Controller, Blockers), 
		append(Blockers, [Creature], New_blockers), 
		retractall(blockers(Controller, Blockers)), 
		asserta(blockers(Controller, New_blockers)).
	% Each creature in the blockers list is a blocking creature.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%combat_begins/2 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* combat_begins(+Active_player, +Opponent) */

	combat_begins(Active_player, Opponent):- 
		write(`Combat begins`), nl,
		declare_attackers(Active_player, Opponent, _Attackers).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%declare_attackers/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* declare_attackers(+Active_player, +Opponent, +Attackers) */
	% Allows the Active_player to choose Attackers. 

	declare_attackers(Active_player, Opponent, Attackers):- 
		write(`Declare Attackers`), nl, 
		write(`Type "yes" to finish`), nl, 
		read(Attacker), 
		(
			Attacker \== 'yes', 
			attacker(Attacker, Active_player, Opponent), 
			append(Attackers, [Attacker], New_attackers), 
			declare_attackers(Active_player, Opponent, New_attackers);
			write(`Attacking: `), write(Attackers), nl, flush,
			declare_blockers(Active_player, Opponent, Attackers, _Blockers)
		).
	% ^ This needs to take into account Planeswalkers, 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%declare_blockers/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* declare_attackers(+Active_player, +Opponent, +Attackers) */
	% Allows the non active player to choose Blockers. 

	% Remember to check for cases where there are no attacks or blocks, 
	declare_blockers(Active_player, Opponent, Attackers, Blockers):- 
		write(`Declare Blockers`), nl, 
		write(`Type "yes" to finish`), nl, 
		read(Blocker), 
		(
			(	
				Blocker \== 'yes', 
				write(`Block which attacker?`), nl,
				read(Attacker),
				blocks(Blocker, Attacker)
			),
			% Update blockers list >
			blocker(Blocker, Opponent, Active_player), 
			append(Blockers, [Blocker], New_blockers), 
			% Continue declaring blockers >
			declare_blockers(Active_player, Opponent, Attackers, New_blockers);
			write(`Blocking: `), write(Blockers), nl, flush, 
			order_blockers(Attackers, [], Blockers_order), 
			%sort(Blockers, Sorted_blockers), 
			order_attackers(Blockers, [], Attackers_order),	
			combat_damage(Active_player, Opponent, Blockers_order, Attackers_order)
		).


%%%%%%%%%%%%%%blocks/2 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* blocks(+Blocker, +Attacker) */
	% Determines which attacker is blocked by a blocker

	blocks(Blocker, Attacker):-
		creature(Blocker, Blocker_Power, Blocker_Toughness, Blocker_state),
		creature(Attacker, Attacker_Power, Attacker_Toughness, Attacker_state), 
		% Update blocker's state: 
		append(Blocker_state, [blocking(Attacker)], New_blocker_state),
		retractall(creature(Blocker, Blocker_Power, Blocker_Toughness, Blocker_state)), 
		asserta(creature(Blocker, Blocker_Power, Blocker_Toughness, New_blocker_state)), 
		% Update attacker's state:
		append(Attacker_state, [blocked_by(Blocker)], New_attacker_state),
		retractall(creature(Attacker, Attacker_Power, Attacker_Toughness, Attacker_state)), 
		asserta(creature(Attacker, Attacker_Power, Attacker_Toughness, New_attacker_state)).

%%%%%%%%%%%%%%order_blockers/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* order_blockers(+Attackers, [], -Blockers_order) */
	% Allows the active player to choose the damage assignment order for blockers

%%%%%%%%%%%%%%order_blockers/3 (0) 13/02/11

	% No more attackers for which to order blockers
	order_blockers([], Order, Order):- 
		write(`Order of damage to blockers: ` - Order), nl.
	% Incidentally, empty clauses trigger when there are no blockers, 
	%  or attackers, below in order_attackers. 

%%%%%%%%%%%%%%order_blockers/3 (1) 13/02/11

	% Order the blockers for the next attacker
	order_blockers([Attacker | Rest], Temp, Order):-
		creature(Attacker, _Power, _Toughness, State),
		findall(Blocker, member(blocked_by(Blocker), State), Blockers), 
		order_blockers(Attacker, Blockers, [], Order_1), 
		append(Temp, [Order_1], New_temp), 
		order_blockers(Rest, New_temp, Order). 

%%%%%%%%%%%%%%order_blockers/4 (1) 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* order_blockers(+Attacker, +Blockers, [], -Blockers_order) */
	% Allows the active player to choose the damage assignment order
	%  for the creatures blocking one attacker

% Remember to check that the chosen blockers are actually blocking Attacker.	


%%%%%%%%%%%%%%order_blockers/4 (0) 13/02/11

	% No more blockers to order
	order_blockers(Attacker, [], Order, [Attacker, Order]). 

%%%%%%%%%%%%%%order_blockers/4 (0) 13/02/11

	% Choose the next blocker in the damage assignment order
	order_blockers(Attacker, Blockers, Temp, Order):- 
		write(`Order` - Attacker - `'s blockers`: Blockers), nl,
		write(`Choose next blocker for ` - Attacker), nl, 
		read(Blocker), 
			(
				Blocker \== 'yes', 
				append(Temp, [Blocker], New_temp), 
				remove(Blocker, Blockers, New_blockers), % also checks membership btw
				order_blockers(Attacker, New_blockers, New_temp, Order);
				true
			).
	% Damage_to_blockers list: [Blocker_1, Blocker_2, Blocker_3...]


%%%%%%%%%%%%%%order_attackers/3 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* order_attakers(+Blockers, [], -Attackers_order) */
	% Allows the defending player to choose the damage assignment order for attackers
	% Most creatures can only block one attacker so this is not needed often. 

%%%%%%%%%%%%%%order_attakers/3 (0) 13/02/11

	% No more blockers
	order_attackers([], Order, Order):- 
		write(`Order of damage to attackers: ` - Order), nl.

%%%%%%%%%%%%%%order_attakers/3 (1) 13/02/11

	% Take the next Blocker and order the creatures it's blocking	
	order_attackers([Blocker | Rest], Temp, Order):-
		creature(Blocker, _Power, _Toughness, State),
		findall(Attacker, member(blocking(Attacker), State), Attackers), 
		%sort(Attackers, Sorted_attackers),
		order_attackers(Blocker, Attackers, [], Order_1),
		append(Temp, [Order_1], New_temp), 
		order_attackers(Rest, New_temp, Order). 


%%%%%%%%%%%%%%order_attackers/4 (1) 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* order_attackers(+Blocker, +Attackers, [], -Attackers_order) */
	% Order attackers for one blocker. 

%%%%%%%%%%%%%%order_attakers/4 (0) 13/02/11

	% No more Attackers
	order_attackers(Blocker, [], Order, [Blocker, Order]). % No more attackers

%%%%%%%%%%%%%%order_attakers/4 (1) 13/02/11

	order_attackers(Blocker, Attackers, Temp, Order):- 
		write(`Order` - Blocker - `'s attackers`: Attackers), nl,
		write(`Choose next attacker for ` - Blocker), nl, 
		read(Attacker), 
			(
				Attacker \== 'yes', 
				append(Temp, [Attacker], New_temp), 
				remove(Attacker, Attackers, New_attackers), % also checks membership btw
				order_attackers(Blocker, New_attackers, New_temp, Order);
				true
			).
	% Problem here. When the same creature blocks two or more attackers, 
	%  its name is sent on as two different blockers. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%combat_damage/4 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* combat_damage(+Active_player, +Opponent, +Blockers_order, +Attackers_order) */
	% Assigns combat damage to creatures, players and planeswalkers [to be implemented]
	% Both sides use this predicate

	combat_damage(Active_player, Opponent, Blockers_order, Attackers_order):-
		%assign_damage(Order, Player)
		assign_damage(Blockers_order, Opponent),
		assign_damage(Attackers_order, _Active_player),
		combat_ends.
	% Blockers_order and Attackers_order are of the form: 
	% 	[Attacker, [Blockers] ] and [Blocker, [Attackers] ]
	%  so any information about each Attacker and Blocker can be 
	%  extracted from them. 


%%%%%%%%%%%%%%assign_damage/2 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* assign_damage(Bestower, Recipients) */
	% A bestower is a creature that deals damage, a recipient is a
	%  creature, player or planeswalker that receives it.

%%%%%%%%%%%%%%assign_damage/2 (0) 13/02/11

	% No more attackers or blockers.
	assign_damage([], _). 
	
	% Bestower: the creature using its Power to deal damage
	% Recipient: the creature suffering the damage against its Toughness. 

%%%%%%%%%%%%%%assign_damage/2 (1) 13/02/11

	% Bestower is engaged but the Recipient is not there anymore
	assign_damage([ [Bestower, Recipients] | Rest], Opponent):-
		blocked(Bestower), 				
		member(Recipient, Recipients),
		Recipient == '',	% OK, probably stupid
		assign_damage(Rest, Opponent).
		
%%%%%%%%%%%%%%assign_damage/2 (2) 13/02/11

	% Bestower is engaged by exactly one Recipient
	assign_damage([ [Bestower, Recipients] | Rest], Opponent):-
		(blocked(Bestower); blocking(Bestower)),
		length(Recipients, 1),
		Recipients = [Recipient],
		creature(Bestower, Bestower_P, _Bestower_T, _State),
		deal_damage(Bestower, Recipient, Bestower_P), 
		assign_damage(Rest, Opponent).
		
%%%%%%%%%%%%%%assign_damage/2 (3) 15/02/11

	% Bestower is blocked by more than one blocker
	assign_damage([ [Bestower, Recipients] | Rest], Opponent):-
		(blocked(Bestower); blocking(Bestower)), 
		creature(Bestower, Bestower_P, _Bestower_T, _State),
		lethal_damage(Bestower, Recipients, Bestower_P), 
		assign_damage(Rest, _Opponent).

%%%%%%%%%%%%%%assign_damage/2 (4) 13/02/11

	% Bestower is not engaged and is an attacker
	assign_damage([ [Bestower, _] | Rest], Opponent):-
		creature(Bestower, Bestower_P, _Bestower_T, _State),
		attackers(_, Attackers), 
		member(Bestower, Attackers), 
		player(Opponent, _),
		deal_damage(Bestower, Opponent, Bestower_P), 
		assign_damage(Rest, Opponent).

%%%%%%%%%%%%%%assign_damage/2 (5) 13/02/11

	% Bestower is not engaged and is a blocker
	assign_damage(_, _).


%%%%%%%%%%%%%%blocked/1 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* blocked(+Creature) */
	% True if Creature is a blocked creature

	blocked(Bestower):-
		creature(Bestower, _Bestower_P, _Bestower_T, State),
		member(blocked_by(_A_Blocker), State).


%%%%%%%%%%%%%%blocking/1 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* blocking(+Creature) */
	% True if Creature is a blocking creature

	blocking(Bestower):-
		creature(Bestower, _Bestower_P, _Bestower_T, State),
		member(blocking(_An_Attacker), State).


%%%%%%%%%%%%%%deal_damage/3 15/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* deal_damage(+Bestower, +Recipient, +Damage) */
	% Deals an amount of Damage to a Recipient: a player, 
	%   planeswalker or creature.

%%%%%%%%%%%%%%deal_damage/3 (1) 13/02/11

	% Deal damage to another creature
	deal_damage(Bestower, Recipient, Damage):-
		creature(Recipient, Power, Toughness, State),
		New_Toughness is Toughness - Damage, 
		retractall(creature(Recipient, _, _, _)), 
		asserta(creature(Recipient, Power, New_Toughness, State)), 
		write(Bestower - ` deals ` - Damage - ` damage to ` - Recipient), nl. 
		% Also need some clauses for Players and Planeswalkers

%%%%%%%%%%%%%%deal_damage/3 (2) 13/02/11

	% Deal damage to a player
	deal_damage(Bestower, Recipient, Damage):-
		player(Recipient, Life),
		New_life is Life - Damage, 
		retractall(player(Recipient, Life)), 
		asserta(player(Recipient, New_life)), 
		write(Bestower - ` deals ` - Damage - ` damage to ` - Recipient), nl. 
		% Also need some clauses for Planeswalkers. Well, later


%%%%%%%%%%%%%%lethal_damage/3 15/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* lethal_damage(+Bestower, +Recipients, +Bestower_Power) */
	% Causes a creature to deal lethal damage to each of their 
	%  attackers or blockers in turn of their damage assignment order

%%%%%%%%%%%%%%lethal_damage/2 (0) 13/02/11

	% no more Recipients
	lethal_damage(Bestower, [], Bestower_P). 
	
%%%%%%%%%%%%%%lethal_damage/2 (1) 13/02/11

	% no more Power
	lethal_damage(Bestower, Recipients, 0). 

%%%%%%%%%%%%%%lethal_damage/2 (0) 15/02/11

	% Assign lethal damage to each Recipient, before moving on 
	%  to the next; stop when there are no more Recipients or when the
	%  Bestower is out of Power. 
	lethal_damage(Bestower, [Recipient | Rest], Bestower_P):- 
		creature(Recipient, _Recipient_P, Recipient_T, _State_2), 
		lethal_damage(Bestower, Recipient, Bestower_P, Recipient_T, New_bestower_P),   
		lethal_damage(Bestower, Rest, New_bestower_P).

%%%%%%%%%%%%%%lethal_damage/4 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	/* lethal_damage(Recipient, Bestower_power, Recipient_toughness, New_Bestower_toughness) */
	% Assigns lethal damage to a recipient, keeping track of the bestower's power

%%%%%%%%%%%%%%lethal_damage/2 (1) 13/02/11

	% Assign lethal damage to one Recipient
	% The Bestower's Power is not enough or just enough to kill 
	%  the Recipient
	lethal_damage(Bestower, Recipient, Bestower_P, Recipient_T, 0):-
		Bestower_P =< Recipient_T, 
		deal_damage(Bestower, Recipient, Bestower_P).
	% No need to let the player decide- all the Bestower's Power must
	%  be assigned as damage to the first recipient

%%%%%%%%%%%%%%lethal_damage/2 (2) 13/02/11

	% The Attacker's Power is more than enough to kill the Blocker
	lethal_damage(Bestower, Recipient, Bestower_P, Recipient_T, New_bestower_P):-
		Bestower_P > Recipient_T, 
		write(`Assign lethal damage to ` - Recipient), nl, 
		read(Damage), 
		Damage =< Bestower_P, Damage =< Recipient_T, 
		% ^ Damage is no more than the Bestower's Power and is lethal
		deal_damage(Bestower, Recipient, Recipient_T), 
		New_bestower_P is Bestower_P - Recipient_T.
	% In this case, the player can choose to assign enough dmg to 
	%  the Recipient to kill it, or more. 


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	I may be able to use this as a damage assignment algorithm for
	  the AI. 

	% The Attacker's Power is more than enough to kill the Blocker
	lethal_damage(Recipient, Bestower_P, Recipient_T, New_bestower_P):-
		Bestower_P > Recipient_T, 
		deal_damage(Recipient, Recipient_T), 
		New_bestower_P is Bestower_P - Recipient_T.
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%combat_ends/0 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	% Combat ends. This should handle triggers etc.


	combat_ends:-
		write(`Combat ends`), nl.


















%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

/*
Test with: 
asserta(creature( 'Goblin', 1, 1, [blocked_by('Soldier')] )).
asserta(creature( 'Goblin', 1, 1, [blocked_by('Soldier'), blocked_by('Bear')] )).
asserta(creature( 'Ogre', 3, 3, [blocked_by('Soldier'), blocked_by('Bear')] )).

asserta(creature( 'Soldier', 2, 1, [blocking('Goblin')] )).
asserta(creature( 'Bear', 2, 2, [blocking('Goblin')] )).

attackers_damage( [ [ 'Goblin',[ 'Soldier' ] ] ]  )
attackers_damage([ [ 'Goblin',[ 'Soldier' , 'Bear' ] ] ]).
attackers_damage([ [ 'Ogre',[ 'Soldier' , 'Bear' ] ] ]).

*/



/*
%%%%%%%%%%%%%%lethal_damage/4 13/02/11
%%%%%%%%%%%%%%%%%%%%%%%
	lethal_damage(Recipient, Bestower_power, Recipient_toughness, New_Bestower_toughness) 
	% Assigns lethal damage to a recipient, keeping track of the bestower's power

%%%%%%%%%%%%%%lethal_damage/2 (1) 13/02/11

	% Assign lethal damage to one Recipient
	% The Bestower's Power is not enough to kill the Recipient
	lethal_damage(Recipient, Bestower_P, Recipient_T, 0):-
		Bestower_P =< Recipient_T, 
		deal_damage(Recipient, Bestower_P).

%%%%%%%%%%%%%%lethal_damage/2 (2) 13/02/11

	% The Attacker's Power is more than enough to kill the Blocker
	lethal_damage(Recipient, Bestower_P, Recipient_T, New_bestower_P):-
		Bestower_P > Recipient_T, 
		deal_damage(Recipient, Recipient_T), 
		New_bestower_P is Bestower_P - Recipient_T.


*/

