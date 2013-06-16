OCaml Pokemon AI
====
Introduction
----
This OCaml program approximately simulates a battle environment for everyone's favorite RPG, Pokemon (specifically, the second generation, i.e. Gold/Silver/Crystal). Creating the battle environment is mostly just bookkeeping - the creative part of the assignment, and the main challenge, was to create an AI that played the game against other players or programs. Like in Pokemon, each team selects a group (5, in this case) Pokemon. These Pokemon have hard-coded stats and moves, located in steammon.txt. Each player sends out one Pokemon at a time, and they battle each other. During the battle phase, a Pokemon can attack, use an item, or switch out and be replaced by another Pokemon.

Setup
----
*	Compile with [fill me in]
*   Run the game with [fill me in]
*   Run the GUI with [fill me in]
*   Press the connect button on the GUI
*   Run a bot with [fill me in]
*   Repeat for a second bot, i.e. [fill me in]

Important Files
----
*   team/smellYaLater.ml    #Our bot's code
*   game/state.ml           #A module to record the state of a game
*   game/game.ml            #A module for manipulating the state of the gaemy

Mechanics
----
Overall the bot favors a greedy strategy - it will generally do as much damage as it can at the time. This is generally a good idea in Pokemon - you won't win by doing less damage than you could have done. However, this is a predictable strategy, with many flaws. For instance, what happens if none of your moves are capable of doing a significant amount of damage? What happens if, while you can do damage, your opponent is faster and will KO you before you get to attack? What should you do if your opponent is so outmatched, he would be foolish not to switch? The AI addresses these and other issues in the following ways:
