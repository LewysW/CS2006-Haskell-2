Cabal Instructions:
1. If when running the program any dependencies are not installed, run:
	cabal install 

2. If the graphics package gloss is not installed, run:
	cabal install gloss

3. If cabal is not properly configured, run:
	cabal configure

Execution Instructions:

1. Open a terminal window.

2. Navigate in the terminal to the Code/ folder 

3. To run the program without command-line arguments, proceed to step 4. Otherwise, run the command (substituting the appropriate arguments) and proceed to step 6:
	cabal run <board_size (Int)> <target (Int)> <player (White/Black)> <game_type (normal/4x4)> <ai_level (beginner/intermediate/pvp)>

3. To run the program without command line arguments, run the command:
	cabal run

5. After running the program without command line arguments click the buttons representing the desired:
	- Board size
	- Target (i.e. goal to win)
	- Colour to play as
	- AI mode (beginner is a random AI, intermediate is a heuristic based AI, and PVP is player vs player)
	- Game type (normal Gomoku, or rule of fours)

6. To play the game, click on the corner of a square to place a piece. Place enough pieces in a row to match the target to win. Stop the AI (or other player) from doing the same.

7. While playing the game, a number of options can be set using the buttons on the left hand side of the display:
	- Undo, allows the player to undo the last player and AI move.
	- Save Game, saves the current game state in the save.dat file located in the Code/ folder.
	- Load Game, loads the current game state from the save.dat file and allows the player to continue from that state.
	- Toggle Hints, displays a red/pink piece on the board which is a suggestion for where the player should play their next piece (results may vary).

8. Once the game has ended ((n)either player has won) then the program must be re-run to play again.

Version Notes:
See the Gomoku.cabal file in the Code/ directory.

Game Assets:
Located in the Assets/ folder within the Code/ directory.
	
