%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%              Decks                %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- dynamic 
	decklist/3.  


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%decklist/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* deck(+Player, +Name) */	

	% deck(Playername, Deckname).
%	deck('Player 1', 'Blades of Victory').
	deck('Player 1', 'Testdeck Blue').
%	deck('Player 2', 'Power of Prophecy').
	deck('Player 2', 'Testdeck Blue').
%	deck('Player 3', 'Reign of Vampirism').

%	decklist('Testdeck', [['Plains', 2], ['Swamp', 2]]).

	decklist('Testdeck Blue', 
			[
				['Island', 10], 
%				['Swamplains', 4],
				['Air Servant', 4],
%				['Armored Cancrix', 4],
%				['Azure Drake', 4],
%				['Aether Adept', 4],
				['Maritime Guard', 4],
				['Unsummon', 4],
				['Boomerang', 4]
			]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%decklist/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* decklist(+Deck, +Cards) */
	% Cards is a list of [Card, Instances] pairs. 
	% zones.kb should expand this, giving each Instance a unique ID
	% Hm. or could be card(Name, Instance) pairs...?
	% No- actually, you can compound that term later...

	% [Card, Instances] pairs

	% Blades of Victory (black/white)
	decklist('Blades of Victory', 
			[
			['Plains', 			16],
			['Swamp',			 9],
			['Ajani''s Pridemate',	 2],
			['Angelic Arbiter',	 1],
			['Cloud Crusader',	 2],
			['Elite Vanguard',	 2],
			['Infantry Veteran',	 2],
			['Palace Guard',		 2],
			['Serra Angel', 		 2],
			['Siege Mastodon',	 2],
			['Silvercoat Lion', 	 1],
			['Vengeful Archon', 	 1],
			['War Priest of Thune',  1],
			['White Knight', 		 1],
			['Ajani''s Mantra', 	 1],
			['Armored Ascension', 	 2],
			['Assassinate', 		 1],
			['Condemn', 		 1],
			['Doom Blade', 		 1],
			['Duress', 			 1],
			['Inspired Charge', 	 2],
			['Mighty Leap', 		 1],
			['Mind Rot', 		 1],
			['Pacifism', 		 2],
			['Safe Passage', 		 1],
			['Stabbing Pain', 	 1],
			['Warlord''s Axe', 	 1]
			]).

	% Power of Prophecy (blue/white)
	decklist('Power of Prophecy', 
			[
			['Island', 			16],
			['Plains', 			9],
			['Aether Adept', 		2],
			['Air Servant', 		2],
			['Armored Cancrix', 	1],
			['Augury Owl', 		2],
			['Azure Drake', 		1],
			['Blinding Mage', 	1],
			['Cloud Elemental',	2],
			['Conundrum Sphinx', 	1],
			['Harbor Serpent', 	1],
			['Maritime Guard', 	2],
			['Scroll Thief', 		1],
			['Stormtide Leviathan', 1],
			['Wall of Frost', 	1],
			['Water Servant', 	2],
			['Condemn', 		1],
			['Crystal Ball', 		1],
			['Foresee', 		2],
			['Jace''s Ingenuity', 	2],
			['Mighty Leap', 		1],
			['Mind Control', 		1],
			['Negate', 			2],
			['Pacifism', 		1],
			['Safe Passage', 		1],
			['Sleep', 			2],
			['Solemn Offering', 	1]
			]).

	% Reign of Vampirism (green/black)
	decklist('Reign of Vampirism', 
			[
			['Forest', 			9],
			['Swamp',			16],
			['Barony Vampire',	3],
			['Bloodthrone Vampire',	3],
			['Captivating Vampire',	1],
			['Child of Night',	3],
			['Giant Spider',		1],
			['Howling Banshee',	2],
		     ['Reassembling Skeleton', 2],
			['Royal Assassin',	1],
			['Spined Wurm',		1],
			['Viscera Seer',		3],
			['Corrupt',			2],
			['Cultivate',		1],
			['Diabolic Tutor',	2],
			['Doom Blade',		2],
			['Giant Growth',		1],
			['Naturalize',		1],
			['Nature''s Spiral',	1],
			['Quag Sickness',		2],
			['Rise from the Grave',	2],
		     ['Sorcerer''s Strongbox', 1]
			]).

	% Breath of Fire (red/blue)
	decklist('Breath of Fire', 
			[
			['Island', 			9],
			['Mountain', 		16],
			['Ancient Hellkite', 	1],
			['Augury Owl', 		1],
		  ['Berserkers of Blood Ridge', 2],
			['Canyon Minotaur', 	1],
			['Chandra''s Spitfire',	2],
			['Cyclops Gladiator', 	1],
			['Earth Servant', 	1],
			['Ember Hauler', 		2],
			['Fiery Hellhound',	2],
			['Fire Servant', 		1],
			['Goblin Piker', 		1],
			['Goblin Tunneler', 	2],
			['Prodigal Pyromancer',	2],
			['Stone Golem', 		1],
			['Call to Mind', 		1],
			['Chandra''s Outrage',	3],
			['Fireball', 		1],
			['Foresee', 		1],
			['Lava Axe', 		2],
			['Lightning Bolt',	3],
			['Negate', 			1],
			['Preordain', 		1],
			['Shiv''s Embrace', 	1],
			['Unsummon', 		1]
			]).

	% Stampede of Beasts (red/green)
	decklist('Stampede of Beasts', 
			[
			['Forest', 			16],
			['Mountain', 		8],
			['Awakener Druid',	2],
			['Duskdale Wurm', 	2],
			['Garruk''s Packleader', 2],
			['Giant Spider', 		2],
			['Greater Basilisk',	1],
			['Llanowar Elves', 	2],
			['Prized Unicorn', 	2],
			['Protean Hydra', 	1],
			['Runeclaw Bear', 	2],
			['Sacred Wolf', 		1],
			['Spined Wurm', 		1],
			['Sylvan Ranger', 	2],
			['Yavimaya Wurm', 	2],
			['Act of Treason',	1],
			['Back to Nature',	1],
			['Chandra''s Outrage',	1],
			['Cultivate', 		1],
			['Fireball', 		1],
			['Fling', 			1],
			['Giant Growth', 		2],
			['Lava Axe', 		1],
			['Lightning Bolt', 	1],
			['Nature''s Spiral', 	1],
		     ['Overwhelming Stampede', 1],
			['Plummet', 		1],
			['Whispersilk Cloak',	1]
			]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%sideboard/2 09/01/11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	/* sideboard(+Player, +Sideboard) */

	% Cards in a player's sideboard
	sideboard('Player 1', ['card a']).
	sideboard('Player 2', ['card b']).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%            Notes                 %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% testing 
	deck_is(Player, Deck, Cards):-
		deck(Player, Deck),
		decklist(Deck, Cards).
		


