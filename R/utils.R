.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Welcome to the gameRoom.
Use play() to get playing, for example play(\"tictactoe\").
Games currently available: tictactoe, hangman, blackjack. Good luck!")
}
