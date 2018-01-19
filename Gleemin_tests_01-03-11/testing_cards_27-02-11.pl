% First saved: 10/01/11
% Last saved:  24/02/11
%
%	Cards 
%
% Doing: 
%	changing mana abilities syntax to pure MGL.
%	putting each ability in a separate list. 
%	noticed card templates have state (might be useful)
%	adding rules text
% Todo
%
% NOTES:
%	Done added check_type pred


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%           Cards Facts             %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	:- ensure_loaded('mtg_operators.pl').
%	:- dynamic card/1.
	%^ Not needed, actually, I think. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%supertypes/1 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  supertypes(+Supertypes) */
	% Lists known supertypes

	supertypes(['Basic', 'Legendary', 'Ongoing', 'Snow', 'World']). 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Supertypes are _not_ formally connected with specific 
	types or subtypes. 
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%card_types/1 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  card_types(+Type) */ 
	% Lists known card Types

	% Known card types. 
	card_types([
			'Artifact',
			'Creature',
			'Enchantment',
			'Instant',
			'Land',
			'Plane',
			'Planeswalker',
			'Scheme',
			'Sorcery',
			'Tribal',
			'Vanguard'
			]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%subtypes/1 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  subtypes(+Type, +Subtypes) */
	% Lists known Subtypes of known Types

%%%%%%%%%%%%%%subtype/2 (1) 03/02/11

	% Artifact subtypes
	subtypes('Artifact', 
			[
				'Equipment',
				'Contraption',
				'Fortification'
			]
		).

%%%%%%%%%%%%%%subtype/2 (2) 03/02/11

	% Enchantment subtypes 
	subtypes('Enchantment', 
			[
				'Aura',
				'Shrine'
			]
		).

%%%%%%%%%%%%%%subtype/2 (3) 03/02/11

	% Land subtypes
	subtypes('Land', 
			[
				'Desert',
				'Forest',
				'Island',
				'Lair',
				'Locus',
				'Mine',
				'Mountain',
				'Plains',
				'Power-Plant',
				'Swamp',
				'Tower',
				'Urza''s'
			]
		).

%%%%%%%%%%%%%%subtype/2 (4) 03/02/11

	% Planeswalker subtypes
	subtypes('Planeswalker', 
			[
				'Ajani',
				'Bolas',
				'Chandra',
				'Elspeth',
				'Garruk',
				'Gideon',
				'Jace',
				'Koth',
				'Liliana',
				'Nissa',
				'Sarkhan',
				'Sorin',
				'Tezzeret',
				'Venser'
			]
		).

%%%%%%%%%%%%%%subtype/2 (5) 03/02/11	

	% Sorcery subtypes
	subtypes('Sorcery',
			 [
				'Arcane', 
				'Trap'
			]
		).

%%%%%%%%%%%%%%subtype/2 (6) 03/02/11

	% Instant subtypes
	subtypes('Instant',
			 [
				'Arcane', 
				'Trap'
			]
		).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Instant and Sorcery types share their subtypes, 
	 which are called spell types. 
	I should represent this with a rule. 
*/

%%%%%%%%%%%%%%subtype/2 (7) 03/02/11

	% Creature subtypes
	subtypes('Creature', 
			[
				'Advisor', 
				'Ally', 
				'Angel',
				'Anteater',
				'Antelope', 
				'Ape', 
				'Archer', 
				'Archon', 
				'Artificer', 
				'Assassin', 
				'Assembly'-
				'Worker', 
				'Atog', 
				'Aurochs', 
				'Avatar', 
				'Badger', 
				'Barbarian', 
				'Basilisk', 
				'Bat', 
				'Bear', 
				'Beast', 
				'Beeble', 
				'Berserker', 
				'Bird', 
				'Blinkmoth', 
				'Boar', 
				'Bringer', 
				'Brushwagg', 
				'Camarid', 
				'Camel', 
				'Caribou', 
				'Carrier', 
				'Cat', 
				'Centaur', 
				'Cephalid', 
				'Chimera', 
				'Citizen', 
				'Cleric', 
				'Cockatrice', 
				'Construct', 
				'Coward', 
				'Crab', 
				'Crocodile', 
				'Cyclops', 
				'Dauthi', 
				'Demon', 
				'Deserter', 
				'Devil', 
				'Djinn', 
				'Dragon', 
				'Drake', 
				'Dreadnought', 
				'Drone', 
				'Druid', 
				'Dryad', 
				'Dwarf', 
				'Efreet', 
				'Elder', 
				'Eldrazi', 
				'Elemental', 
				'Elephant', 
				'Elf', 
				'Elk', 
				'Eye', 
				'Faerie', 
				'Ferret', 
				'Fish', 
				'Flagbearer', 
				'Fox', 
				'Frog', 
				'Fungus', 
				'Gargoyle', 
				'Giant', 
				'Gnome', 
				'Goat', 
				'Goblin', 
				'Golem', 
				'Gorgon', 
				'Graveborn', 
				'Griffin', 
				'Hag', 
				'Harpy', 
				'Hellion', 
				'Hippo', 
				'Hippogriff', 
				'Homarid', 
				'Homunculus', 
				'Horror', 
				'Horse', 
				'Hound', 
				'Human', 
				'Hydra', 
				'Hyena', 
				'Illusion', 
				'Imp', 
				'Incarnation', 
				'Insect', 
				'Jellyfish', 
				'Juggernaut', 
				'Kavu', 
				'Kirin', 
				'Kithkin', 
				'Knight', 
				'Kobold', 
				'Kor', 
				'Kraken', 
				'Lammasu', 
				'Leech', 
				'Leviathan', 
				'Lhurgoyf', 
				'Licid', 
				'Lizard', 
				'Manticore', 
				'Masticore', 
				'Mercenary', 
				'Merfolk', 
				'Metathran', 
				'Minion', 
				'Minotaur', 
				'Monger', 
				'Mongoose', 
				'Monk', 
				'Moonfolk', 
				'Mutant', 
				'Myr', 
				'Mystic', 
				'Nautilus', 
				'Nephilim', 
				'Nightmare', 
				'Nightstalker', 
				'Ninja', 
				'Noggle', 
				'Nomad', 
				'Octopus', 
				'Ogre', 
				'Ooze', 
				'Orb', 
				'Orc', 
				'Orgg', 
				'Ouphe', 
				'Ox', 
				'Oyster', 
				'Pegasus', 
				'Pentavite', 
				'Pest', 
				'Phelddagrif', 
				'Phoenix', 
				'Pincher', 
				'Pirate', 
				'Plant', 
				'Prism', 
				'Rabbit', 
				'Rat', 
				'Rebel', 
				'Reflection', 
				'Rhino', 
				'Rigger', 
				'Rogue', 
				'Salamander', 
				'Samurai', 
				'Sand', 
				'Saproling', 
				'Satyr', 
				'Scarecrow', 
				'Scorpion', 
				'Scout', 
				'Serf', 
				'Serpent', 
				'Shade', 
				'Shaman', 
				'Shapeshifter', 
				'Sheep', 
				'Siren', 
				'Skeleton', 
				'Slith', 
				'Sliver', 
				'Slug', 
				'Snake', 
				'Soldier', 
				'Soltari', 
				'Spawn', 
				'Specter', 
				'Spellshaper', 
				'Sphinx', 
				'Spider', 
				'Spike', 
				'Spirit', 
				'Splinter', 
				'Sponge', 
				'Squid', 
				'Squirrel', 
				'Starfish', 
				'Surrakar', 
				'Survivor', 
				'Tetravite', 
				'Thalakos', 
				'Thopter', 
				'Thrull', 
				'Treefolk', 
				'Triskelavite', 
				'Troll', 
				'Turtle', 
				'Unicorn', 
				'Vampire', 
				'Vedalken', 
				'Viashino', 
				'Volver', 
				'Wall', 
				'Warrior', 
				'Weird', 
				'Whale', 
				'Wizard', 
				'Wolf', 
				'Wolverine', 
				'Wombat', 
				'Worm', 
				'Wraith', 
				'Wurm', 
				'Yeti', 
				'Zombie', 
				'Zubera'
			]
		).
	% Yes, this is my really long list of Mt:G creature subtypes. 
	% I would like to thank Notepad++ for its option to record macros. 

%%%%%%%%%%%%%%subtype/2 (8) 03/02/11

	% Tribal subtypes.
	subtypes('Tribal', 
			[
				'Advisor', 
				'Ally', 
				'Angel',
				'Anteater',
				'Antelope', 
				'Ape', 
				'Archer', 
				'Archon', 
				'Artificer', 
				'Assassin', 
				'Assembly'-
				'Worker', 
				'Atog', 
				'Aurochs', 
				'Avatar', 
				'Badger', 
				'Barbarian', 
				'Basilisk', 
				'Bat', 
				'Bear', 
				'Beast', 
				'Beeble', 
				'Berserker', 
				'Bird', 
				'Blinkmoth', 
				'Boar', 
				'Bringer', 
				'Brushwagg', 
				'Camarid', 
				'Camel', 
				'Caribou', 
				'Carrier', 
				'Cat', 
				'Centaur', 
				'Cephalid', 
				'Chimera', 
				'Citizen', 
				'Cleric', 
				'Cockatrice', 
				'Construct', 
				'Coward', 
				'Crab', 
				'Crocodile', 
				'Cyclops', 
				'Dauthi', 
				'Demon', 
				'Deserter', 
				'Devil', 
				'Djinn', 
				'Dragon', 
				'Drake', 
				'Dreadnought', 
				'Drone', 
				'Druid', 
				'Dryad', 
				'Dwarf', 
				'Efreet', 
				'Elder', 
				'Eldrazi', 
				'Elemental', 
				'Elephant', 
				'Elf', 
				'Elk', 
				'Eye', 
				'Faerie', 
				'Ferret', 
				'Fish', 
				'Flagbearer', 
				'Fox', 
				'Frog', 
				'Fungus', 
				'Gargoyle', 
				'Giant', 
				'Gnome', 
				'Goat', 
				'Goblin', 
				'Golem', 
				'Gorgon', 
				'Graveborn', 
				'Griffin', 
				'Hag', 
				'Harpy', 
				'Hellion', 
				'Hippo', 
				'Hippogriff', 
				'Homarid', 
				'Homunculus', 
				'Horror', 
				'Horse', 
				'Hound', 
				'Human', 
				'Hydra', 
				'Hyena', 
				'Illusion', 
				'Imp', 
				'Incarnation', 
				'Insect', 
				'Jellyfish', 
				'Juggernaut', 
				'Kavu', 
				'Kirin', 
				'Kithkin', 
				'Knight', 
				'Kobold', 
				'Kor', 
				'Kraken', 
				'Lammasu', 
				'Leech', 
				'Leviathan', 
				'Lhurgoyf', 
				'Licid', 
				'Lizard', 
				'Manticore', 
				'Masticore', 
				'Mercenary', 
				'Merfolk', 
				'Metathran', 
				'Minion', 
				'Minotaur', 
				'Monger', 
				'Mongoose', 
				'Monk', 
				'Moonfolk', 
				'Mutant', 
				'Myr', 
				'Mystic', 
				'Nautilus', 
				'Nephilim', 
				'Nightmare', 
				'Nightstalker', 
				'Ninja', 
				'Noggle', 
				'Nomad', 
				'Octopus', 
				'Ogre', 
				'Ooze', 
				'Orb', 
				'Orc', 
				'Orgg', 
				'Ouphe', 
				'Ox', 
				'Oyster', 
				'Pegasus', 
				'Pentavite', 
				'Pest', 
				'Phelddagrif', 
				'Phoenix', 
				'Pincher', 
				'Pirate', 
				'Plant', 
				'Prism', 
				'Rabbit', 
				'Rat', 
				'Rebel', 
				'Reflection', 
				'Rhino', 
				'Rigger', 
				'Rogue', 
				'Salamander', 
				'Samurai', 
				'Sand', 
				'Saproling', 
				'Satyr', 
				'Scarecrow', 
				'Scorpion', 
				'Scout', 
				'Serf', 
				'Serpent', 
				'Shade', 
				'Shaman', 
				'Shapeshifter', 
				'Sheep', 
				'Siren', 
				'Skeleton', 
				'Slith', 
				'Sliver', 
				'Slug', 
				'Snake', 
				'Soldier', 
				'Soltari', 
				'Spawn', 
				'Specter', 
				'Spellshaper', 
				'Sphinx', 
				'Spider', 
				'Spike', 
				'Spirit', 
				'Splinter', 
				'Sponge', 
				'Squid', 
				'Squirrel', 
				'Starfish', 
				'Surrakar', 
				'Survivor', 
				'Tetravite', 
				'Thalakos', 
				'Thopter', 
				'Thrull', 
				'Treefolk', 
				'Triskelavite', 
				'Troll', 
				'Turtle', 
				'Unicorn', 
				'Vampire', 
				'Vedalken', 
				'Viashino', 
				'Volver', 
				'Wall', 
				'Warrior', 
				'Weird', 
				'Whale', 
				'Wizard', 
				'Wolf', 
				'Wolverine', 
				'Wombat', 
				'Worm', 
				'Wraith', 
				'Wurm', 
				'Yeti', 
				'Zombie', 
				'Zubera'
			]
		).

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	And, unfortunately, Creature and Tribal types share their 
	  very many subtypes. The fact that those types are common 
	  can equally be expressed by a rule... but facts may also
	  be needed seperately. 
*/

%%%%%%%%%%%%%%subtype/2 (9) 03/02/11

/*
Alara, Arkhos, Bolas's Meditation Realm, Dominaria, Equilor, Iquatana, 
Ir, Kaldheim, Kamigawa, Karsus, Lorwyn, Luvion, Mercadia, 
Mirrodin, Moag, Muraganda, Phyrexia, Pyrulea, Rabiah, 
Rath, Ravnica, Segovia, Serra's Realm, Shadowmoor, 
Shandalar, Ulgrotha, Valla, Wildfire, and Zendikar
*/

	% Plane types, aka Planar types.
	subtypes('Plane', [
				'Alara',
				'Arkhos',
				'Bolas'' Meditation Realm', 
				'Dominaria',
				'Equilor',
				'Iquatana',
				'Ir', 
				'Kaldheim',
				'Kamigawa',
				'Karsus',
				'Lorwyn',
				'Luvion',
				'Mercadia', 
				'Mirrodin',
				'Moag',
				'Muraganda',
				'Phyrexia',
				'Pyrulea',
				'Rabiah',
				'Rath', 
				'Ravnica',
				'Segovia',
				'Serra''s Realm', 
				'Shadowmoor',
				'Shandalar', 
				'Ulgrotha',
				'Valla',
				'Wildfire',
				'Zendikar'
			]
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%card/1 04/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*	card([
		card_name 		Name, 
		mana_cost 		Cost,
		type_line 		[Supertype, Type, Subtype], % are lists
		text_box 		Text,
		power 		Power, 
		toughness 		Toughness, 
		loyalty 		Loyalty 
		state			[[Status] | Other]
		]).	*/
	% Lists the parts of a card
	% Query with: 
		% card([Name, Cost, Type, Text, P, T, Loyalty, State]).
		% card([Name | _]).
		% card([card_name Name | _]).
	% etc.


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Update: state is not needed for card/1 instances, it's needed for copy/3 
	  ones. I will leave this here for now because everything else is 
	  using it like that, but I should fix it. On the other hand, it may 
	  be useful as a starting state (for "comes into play tapped" etc).
	state is a list of Status = [Tapped, Flipped, Facing, Phased]
	  (where 
		Tapped : 'tapped' or 'untapped'
		Flipped: 'flipped' or 'unflipped'
		Facing: 'face_up' or 'face_down'
		Phased: 'phased_in' or 'phased_out')
	  followed by any number of other states the card can acquire during 
	  the game, eg. damage, counters, destroyed, attacking/blocking, 
	  regenerating etc etc. 
	All permanents normally start in: 
	  state [['untapped', 'unflipped', 'face_up', 'phased_in']]
	Any new state element added will probably have to be a tuple 
	  eg, damage(Amount), counter(Type) etc. 
	Finding the value of a particulare state element of a card can be 
	  done with member/2, eg: 
		state State, member(damage(Amount), State), process(Amount)
	  or for Status specifically:
		state State = [Tapped, Flipped, Facing, Phased | Rest],
			process(Tapped)
	Query with: 
	  card([card_name Name, _, _, _, _, _, _, state State]).
	  card([card_name Name, _, _, _, _, _, _, state [Status|_]]).
	  card([card_name Name, _, _, _, _, _, _, state [[Tapped |_]|_]]).
	  card([card_name Name, _, _, _, _, _, _, state [[Tapped, _, _, Phased]|_]]).
	etc.

	Abilities must be in MGL turned into a comma-separated list of 
	  Edinburgh-syntax Prolog atoms. Each ability must be in a separate
	  sub-list of the text_box field, eg: text_box [[Ability,1] [Ability,2]]. 
	Some MGL terms need to be capitalised to be recognised by Gleemin: 
		Type information ('Creature', not creature)
		Zone information ('Battlefield', not battlefield)
		"When", "Whenever" and "At" (always at the start of sentences?)
*/


	% Land cards.
/*	card([
		card_name 		'Swamplains', 
		mana_cost 		0,
		%illustration 	'Plains'
		type_line 		[['Basic'], ['Land'], ['Plains']],
		%text_box 		[add_mana('tap', w, 'you'), add_mana('tap', b, 'you')],
		text_box 		[add_mana(['1', 'tap'], ['add', w, 'to your mana pool']), 
						add_mana(['tap', '1'], ['add', b, 'to your mana pool'])],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']]
		]).
*/

	card([
		card_name 		'Plains', 
		mana_cost 		0,
		%illustration 	'Plains'
		type_line 		[['Basic'], ['Land'], ['Plains']],
		text_box 		[[tap,:,add,ww,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']]
		]).


	card([
		card_name 		'Island', 
		mana_cost 		0,
		%illustration 	'Island'
		type_line 		[['Basic'], ['Land'], ['Island']],
		text_box 		[[tap,:,add,u,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	card([
		card_name 		'Swamp', 
		mana_cost 		0,
		%illustration 	'Swamp'
		type_line 		[['Basic'], ['Land'], ['Swamp']],
		text_box 		[[tap,:,add,b,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Mountain', 
		mana_cost 		0,
		%illustration 	'Mountain'
		type_line 		[['Basic'], ['Land'], ['Mountain']],
		text_box 		[[tap,:,add,r,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	card([
		card_name 		'Forest', 
		mana_cost 		0,
		%illustration 	'Forest'
		type_line 		[['Basic'], ['Land'], ['Forest']],
		text_box 		[[tap,:,add,g,to,your,mana,pool]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).

	
	% Spells: 

	card([
		card_name 		'Aether Adept', 
		mana_cost 		'1uu',
		%illustration 	'Aether Adept'
		type_line 		[[], ['Creature'], ['Human', 'Wizard']],
		text_box		[['When','Aether Adept',enters,the,'Battlefield',(','),return,target,'Creature',to,its,'owner''s',hand]],
		power 		[2], 
		toughness 		[2], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	card([
		card_name 		'Air Servant', 
		mana_cost 		u, % '4u',
		%illustration 	'Air Servant'
		type_line 		[[], ['Creature'], ['Elemental']],
		text_box		[['Flying'],[2,u,:,tap,target,'Creature',with,'Flying']],
		power 		[4], 
		toughness 		[3], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	card([
		card_name 		'Armored Cancrix', 
		mana_cost 		'4u',
		%illustration 	'Armored Cancrix'
		type_line 		[[], ['Creature'], ['Crab']],
		text_box		[],
		power 		[2], 
		toughness 		[5], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	card([
		card_name 		'Assassinate', 
		mana_cost 		'2b',
		%illustration 	'Assassinate'
		type_line 		[[], ['Sorcery'], []],
		text_box		[['Destroy',target,tapped,'Creature']],
		power 		[2], 
		toughness 		[5], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	card([
		card_name 		'Azure Drake', 
		mana_cost 		'3u',
		%illustration 	'Azure Drake'
		type_line 		[[], ['Creature'], ['Drake']],
		text_box		[['Flying']],
		power 		[2], 
		toughness 		[4], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	card([
		card_name 		'Boomerang', 
		mana_cost 		'uu',
		%illustration 	'Boomerang'
		type_line 		[[], ['Instant'], []],
		text_box		[['Return',target,permanent,to,its,'owner''s',hand]],
		power 		[0], 
		toughness 		[0], 
		loyalty 		[], 
		state 		[[]] 
		]).


	card([
		card_name 		'Maritime Guard', 
		mana_cost 		'u',	%'1u',
		%illustration 	'Maritime Guard'
		type_line 		[[], ['Creature'], ['Merfolk', 'Soldier']],
		text_box		[],
		power 		[1], 
		toughness 		[3], 
		loyalty 		[], 
		state 		[['untapped', 'unflipped', 'face_up', 'phased_in']] 
		]).


	card([
		card_name 		'Unsummon', 
		mana_cost 		'u',
		%illustration 	'Unsummon'
		type_line 		[[], ['Instant'], []],
		text_box		[['Return',target,'Creature',to,its,'owner''s',hand]],
		power 		[], 
		toughness 		[], 
		loyalty 		[], 
		state 		[] 
		]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%card/2 04/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* card(+Name, +ID, ?State) */	
	% This is how permanents and objects should be represented, ie 
	%  cards on the battlefield and on the stack. 
	
	% Example: 
	%card('Plains', 1, ['tapped']).
	%card('Ajani''s Pridemate', 1, [counters('+1/+1', 2), damage(2)]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%        Cards Rules                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%change_state/4 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*change_state(+Object, + Zone, +Player, +New_Object)*/
	% Change the State of an Object
	% Call when any change of state should occur.

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Object, New_Object : copy(Name, Id, State)
*/

	change_state(Object, Zone, Player, New_Object):- 
		%Object = copy(Name, Id, _State), 
		zone(Player, Zone, Partition),
		member(Object, Partition),
		remove(Object, Partition, New_partition),
		New_object = New_Object, % wut?
		append([New_object], New_partition, Full),
		retractall(zone(Player, Zone, _)), 
		asserta(zone(Player, Zone, Full)), 
		update_shared_zone(Zone).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%add_state/3 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* add_state(+Object, +State_to_add, -Updated) */
	% adds a state to an Object's State list

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Object : copy(Name, Id, State)
	State_to_add, Updated : term (valid state)
*/

	add_state(Object, Addend, Updated):- 
		Object = copy(Name, Id, State),
		append([Addend], State, New_state),
		Updated = copy(Name, Id, New_state).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%remove_state/3 23/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* remove_state(+Object, +State_to_remove, -Updated) */
	% removes a state to an Object's State list

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Object : copy(Name, Id, State)
	State_to_remove, Updated : term (must be a valid state)
*/

	remove_state(Object, Subtrahend, Updated):- 
		Object = copy(Name, Id, State),
		remove(Subtrahend, State, New_state),
		Updated = copy(Name, Id, New_state).

	% And some error reporting? 


%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	List of known valid states:

	_State_			_Object type_			_Implemented_
	'tap'				permanent				?/0?/11?
	'untap'			permanent				?/0?/11?
	'countered'			spell	
	'destroyed'			permanent
	'discarded'			card (in Hand)
	'sacrificed'		permanent
	'regenerating'		permanent
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%tap_untap/3 05/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/*  tap_untap(+Permanent, +Controller, +Command) */
	% Taps or untaps a Permanent

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	+Permanent: copy(Name, Id, State)
	+Command:  'tap' or 'untap'
 
	All Permanents begin with an empty State == []. Invoking tap_untap/3 
	  on a Permanent, adds a quoted atom, 'tap' or 'untap' to its State, 
	 depending on the value of Command:
	_Command_		_Added to State_
	'tap'			'tapped'
	'untap'		'untapped'

	A Permanent is only considered tapped if it has the atom 'tap' in its
	  State. A Permanent that is not tapped is considered untapped. Any
	  predicates checking for the tapped/untapped state of a permanent
	  should check for the absence of 'tapped' rather than the presence of 
	  'untapped'. Really, 'untapped' is there only for clarity while 
	  debugging- and playing!  
*/

	% Tap or untap a Permanent controlled by Player. 
 	tap_untap(Permanent, Player, Command):-
		Permanent = copy(_Name, _Id, State),
			(
				Command == 'tap' -> Before = 'untapped', After = 'tapped';
				Command == 'untap' -> Before = 'tapped', After = 'untapped'
			), 
		\+ member(After, State), 		
			(				
				member(Before, State)-> 
				remove_state(Permanent, Before, Updated),
				% Updated = copy(Name, Id, New_state),  
				add_state(Updated, After, Full); 
				add_state(Permanent, After, Full)
			),
		change_state(Permanent, 'Battlefield', Player, Full). 

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/*
	Btw, for testing purposes:
	  asserta(zone( 'Player 2', 'Battlefield', [copy('Plains',3,['untapped'])] )).
*/


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%check_type/4 24/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* check_type(?Object, ?Supertype, ?Type, ?Subtype) */
	% Checks the type line of an Object, or queries the database
	%  for Objects of a given super/type/sub. 

	check_type(Object, Supertype, Type, Subtype):-
		card([card_name Object, _, type_line Type_line, _, _, _, _, _]),
		Type_line = [Supertype, Type, Subtype].

%%%% Notes %%%%
%%%%%%%%%%%%%%%
/* 
	Object : copy(Name, Id, [State])
	Supertype, Type, Subtype: ['Type']
*/

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%permanent_type/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	permanent_type(Object):- 
		check_type(Object, _Supertype, Type, _Subtype),
		(
			Type == ['Artifact'];
			Type == ['Creature'];
			Type == ['Enchantment'];
			Type == ['Land'];
			Type == ['Planeswalker']
		).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%creature_type/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	creature_type(Object):- 
		check_type(Object, _Supertype, ['Creature'], _Subtype).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%check_subtype/3 03/02/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* check_subtype(?Type, ?Subtypes, ?Subtype) */
	% Check an Object's Type or Subtype 
	%  or find the known Subtypes of a Type in the database. 

	check_subtype(Type, Subtypes, Subtype):-
		subtypes(Type, Subtypes), 
		member(Subtype, Subtypes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Notes                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	
/*

	card('Ajani''s Pridemate', 0):- 
		card_name 		'Ajani''s Pridemate', 
		card_copy 		0, 
		mana_cost 		[1, w],
		%illustration 	'Ajani''s Pridemate'
		type_line 		['Creature', 'Cat', 'Soldier'],
		text_box 		['Whenever you gain life, 
			   		you may put a +1/+1 counter 
					on Ajani''s Pridemate.'],
		power 		2, 
		toughness 		2, 
		loyalty 		[]. 


	card('Angelic Arbiter', 0):-
		card_name 		'Angelic Arbiter', 
		card_copy 		0, 
		mana_cost 		[5, w, w],
		%illustration 	'Angelic Arbiter'
		type_line 		['Creature', 'Angel'],
		text_box 		['Flying', 
					'Each opponent who cast a spell 
					this turn can''t attack with creatures.', 
					'Each opponent who attacked with a creature 
					this turn can''t cast spells.'],
		power 		5,
		toughness 		6, 
		loyalty		[]. 
*/


