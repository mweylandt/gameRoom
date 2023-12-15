#' Play Games
#'
#' This function calls the code required to run the available games
#'
#' @param game string input. One of 'tictactoe', 'hangman', or 'blackjack'
#'
#'
#' @export
#'
#' @examples
#' \dontrun{play('tictactoe')}


play <- function(game = NULL) {
  if (is.null(game)) {
    return(errorCondition(message = "You haven't specified a game. Current options are: tictactoe, hangman, blackjack."))
    } else if (game == "tictactoe") {
    tictactoe()
  } else if (game == "hangman") {
    hangman()
  } else if (game == "blackjack") {
    blackjack()
  }
}
