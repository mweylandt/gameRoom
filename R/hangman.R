#' Hangman game in R
#' @keywords hangman
#' @export
#' @import glue





hangman <- function() {
  hangmen <- c(
    "  +---+\n  |   |\n      |\n      |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n      |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n  |   |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n /|   |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n /|\\  |\n      |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n /|\\  |\n /    |\n      |\n=========",
    "  +---+\n  |   |\n  O   |\n /|\\  |\n / \\  |\n      |\n========="
  )


words <- readLines(system.file("words.txt", package = "gameRoom"))





  # check guess -------------------------------------------------------------


  check_guess <- function(x) {
    ifelse(x %in% strsplit(hidden_word, "")[[1]], TRUE, FALSE)
  }


  # check win/loss ----------------------------------------------------------

  check_outcome <- function(counter, old_guesses) {
    render_field(hangmen)
    if (counter == length(hangmen)) {
      cat("\n")
      cat(glue::glue("YOU LOSE!! The word was *{hidden_word}*"))
      game <- FALSE
    } else {
      if (sum(hidden_word_split %in% old_guesses) == nchar(hidden_word)) {
        cat("\n WELL DONE")
        game <- FALSE
      } else {
        # continue with game
        game <- TRUE
      }
    }
  }


  # render the field --------------------------------------------------------


  render_field <- function(hangmen) {
    cat("\014") # clears console

    cat("***********************************************\n")
    cat("****************** HANGMAN ********************\n")
    cat("***********************************************\n")
    cat("\n")
    cat(glue::glue("Secret Word: ", current_display_word))
    cat("\n")
    cat("Your guesses so far:", old_guesses)
    cat("\n")
    cat("\n")
    cat("\n")
    cat("\n")

    scaffolding <- glue::glue(
      hangmen[counter]
    )

    cat(scaffolding)
  } # end of render_field



  # game_loop ---------------------------------------------------------------





  hidden_word <- sample(words, 1)
  hidden_word_split <- unlist(strsplit(hidden_word, split = ""))
  current_display_word <- paste0(rep("* ", nchar(hidden_word)), collapse = "")
  old_guesses <- vector()
  counter <- 1
  game <- TRUE

  suppressWarnings(
    while (game == TRUE) {{  render_field(hangmen)

      guess <- readline(prompt = "Guess a letter:  ")

      if (!guess %in% letters) {
        readline(prompt = "That's not a letter ")
        next
        # if it is a number
      } else if (guess %in% letters) {
        # check it hasn't been used already
        if (guess %in% old_guesses) {
          readline(prompt = "You guessed that already! Press enter to try again ")
          next
        } else {
          guess <- guess
        }
      }
    } # end of input
    if (check_guess(guess)) {
      # if true, reveal on the word
      locations <- which(hidden_word_split == guess)
      split_display <- unlist(strsplit(current_display_word, " "))
      split_display[locations] <- guess
      current_display_word <- paste0(split_display, collapse = " ")
      old_guesses <- c(old_guesses, guess)
      game <- check_outcome(counter, old_guesses)
      # if not
    } else {
      counter <- counter + 1
      old_guesses <- c(old_guesses, guess)
      game <- check_outcome(counter, old_guesses)
    }
    } # end of game loop
  )
} # end of function
