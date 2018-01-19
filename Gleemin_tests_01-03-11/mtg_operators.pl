% First saved: 10/01/11
% Last saved:  04/02/11
%
%	Operators for Card etc. 
%
% Doing: 
%
% Todo
%
% NOTES:


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%          Card operators           %%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	% Precedence 1000 caused errors in cards.pl. :?
	:- op(100, fx, card_name).
	:- op(100, fx, card_copy).	% unique ID, not used
	:- op(100, fx, mana_cost).
	:- op(100, fx, illustration). % for the GUI
	:- op(100, fx, expansion). 	% just in case
	:- op(100, fx, type_line).
	:- op(100, fx, text_box).
	:- op(100, fx, power).
	:- op(100, fx, toughness).
	:- op(100, fx, loyalty).
	:- op(100, fx, state).		% Status and damage, counters etc.

	% ability operators. Useless? Possibly.
%	:- op(100, fx, keyword).
	:- op(100, fx, modes).		% "mode" is a built-in operator 
	:- op(100, fx, condition).	% For triggered abilities
	:- op(100, fx, cost).
	:- op(100, fx, recipients).	% targets or not
	:- op(100, fx, effect).
	:- op(100, fx, duration).	

	
	%:- op(1000, xfx, [when, whenever, at]).
	%:- op(1000, yfx, [:, -]).










