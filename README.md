# Sample Haskell Console App: tic-tac-toe
I have created this project to demonstrate the project structure of a simple Stack-based Haskell app. The project also demonstrates some unit testing capabilities in Haskell using QuickCheck and HSpec. Some basic I/O testing is also shown.

## How to run?
```
stack exec tic-tac-toe-exe
```

## Example run
```
➜  tic-tac-toe git:(master) stack exec tic-tac-toe-exe
Welcome to tic-tac-toe!
Press Return key to start a new game.
Starting new game
. . .
. . .
. . .

Enter move for X: 00
X . .
. . .
. . .

Enter move for O: 10
X . .
O . .
. . .

Enter move for X: 02
X . X
O . .
. . .

Enter move for O: 02
Error: Cell (0,2) is already occupied.
Enter move for O: 30
Error: PointOutOfBoard (3,0)
Enter move for O: 01
X O X
O . .
. . .

Enter move for X: 22
X O X
O . .
. . X

Enter move for O: 11
X O X
O O .
. . X

Enter move for X: 12
X O X
O O X
. . X

Winner is X
Do you want to play again? [Y/N]: N 
Bye!
```
