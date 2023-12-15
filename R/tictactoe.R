#' tic tac toe game in R
#' @keywords tic tac toe
#' @export
#' @import glue



# get data storage ready --------------------------------------------------

tictactoe <- function() {
  # This is used to convert the User choices into the correct
  # values in the DB -- face value of field will be converted to
  # index value, and that will be edited in the DB

  field <- seq(1:9)

  # the db in which the game data is stored
  db <- rep(NA, 9)



  # functions ---------------------------------------------------------------





  # render field ------------------------------------------------------------

  render_field <- function(db) {
    m <- rep(" ", 9)
    m[which(db == 1)] <- "X"
    m[which(db == 0)] <- "O"

    cat("\014")

    cat("***********************************************\n")
    cat("**************** TIC TAC TOE ******************\n")
    cat("***********************************************\n")
    cat("\n")
    cat("You are X and get to go first")
    cat("\n")
    cat("Enter the number of the square you want to hit \n")

    cat("Good luck!")
    cat("\n")
    cat("\n")
    cat("\n")


    scaffolding <- glue::glue("
 _____ _____ _____                            Square Numbers:
|     |     |     |                            ___ ___ ___
|  {m[1]}  |  {m[2]}  |  {m[3]}  |                           | 1 | 2 | 3 |
|_____|_____|_____|                            --- --- ---
|     |     |     |                           | 4 | 5 | 6 |
|  {m[4]}  |  {m[5]}  |  {m[6]}  |                            --- --- ---
|_____|_____|_____|                           | 7 | 8 | 9 |
|     |     |     |                            --- --- ---
|  {m[7]}  |  {m[8]}  |  {m[9]}  |
|_____|_____|_____|
")

    cat(scaffolding)
  }

  # update list of rows to check --------------------------------------------

  check_list <- list(
    d1 = c(`1` = db[1], `5` = db[5], `9` = db[9]),
    d2 = c(`7` = db[7], `5` = db[5], `3` = db[3]),
    r1 = c(`1` = db[1], `2` = db[2], `3` = db[3]),
    r2 = c(`4` = db[4], `5` = db[5], `6` = db[6]),
    r3 = c(`7` = db[7], `8` = db[8], `9` = db[9]),
    c1 = c(`1` = db[1], `4` = db[4], `7` = db[7]),
    c2 = c(`2` = db[2], `5` = db[5], `8` = db[8]),
    c3 = c(`3` = db[3], `6` = db[6], `9` = db[9])
  )




  # Computer: determine next mark -----------------------------------------------


  # see if opponent has 2 in a row

  find_2 <- function(x) {
    for (i in 1:length(x)) {
      vec <- x[[i]]
      if (sum(is.na(vec)) == 0) { # if line completely full already
        next
      }

      # if there are two slots taken
      # check if they are by player; if so return the square to block
      if (length(vec[!is.na(vec)]) == 2 & sum(vec[!is.na(vec)]) == 2) {
        row_loc <- which(is.na(vec))
        square_to_block <- as.numeric(names(check_list[[i]][row_loc]))
        return(square_to_block)
      }
    }
  }

  # see if computer has 2 in a row

  find_2_own <- function(x) {
    for (i in 1:length(x)) {
      vec <- x[[i]]
      if (sum(is.na(vec)) == 0) { # if line is already full, go on
        next
      }

      # if there are two slots taken
      # if they belong to the other guy, block
      # This is separate because I don't want to block first
      # if I have the opportunity to win, so have to check all for win
      # condition first
      if (length(vec[!is.na(vec)]) == 2 & sum(vec[!is.na(vec)]) == 0) {
        row_loc <- which(is.na(vec))
        square_to_block <- as.numeric(names(check_list[[i]][row_loc]))
        return(square_to_block)
      }
    }
  }


  make_pc_mark <- function(x) {
    marker <- find_2_own(x)

    if (length(marker)) {
      db[marker] <- 0

      return(db)
    }

    marker <- find_2(x)
    if (length(marker)) {
      db[marker] <- 0
      return(db)
    }

    # else hit random one
    marker <- sample(which(is.na(db)), 1)
    if (length(marker)) {
      db[marker] <- 0
      return(db)
    }
  }



  # Store user mark --------------------------------------------------------------


  make_player_mark <- function(x) {
    # store in matrix
    db[x] <- 1
    return(db)
  }

  # check for stalemate ------------------------------------------------------------

  check_stalemate <- function(x) {
    if (game == FALSE) {
      return(FALSE)
    }
    counter <- 0

    for (i in 1:length(x)) {
      # print(i)
      vec <- x[[i]]

      if (sum(is.na(vec)) == 0) {
        counter <- counter + 1
      }
    }

    if (counter == 8) {
      print("it's a draw!")
      return(FALSE)
    } else {
      return(TRUE)
    }
  }


  # Check for winners -------------------------------------------------------

  check_winner <- function(x) {
    for (i in 1:length(x)) {
      # print(i)
      vec <- x[[i]]

      if (sum(is.na(vec)) == 0 & sum(vec[!is.na(vec)]) == 3) { # if line is already full, go on
        print("You win!")
        return(FALSE)
      } else if (sum(is.na(vec)) == 0 & sum(vec[!is.na(vec)]) == 0) {
        print("computer wins :( ")
        return(FALSE)
      }
    } # end of win check

    return(TRUE)
  }




  # game loop ---------------------------------------------------------------


  game <- TRUE
  suppressWarnings(
    while (game == TRUE) {
      render_field(db)
      {
        n1 <- readline(prompt = "select target: ")
        selection <- "invalid"

        if (is.na(as.integer(n1))) {
          readline(prompt = "Not a valid square! Press enter to try again ")
          next
          # if it is a number
        } else if (as.integer(n1) %in% seq(1:9)) {
          # check it hasn't been used already
          if (!is.na(db[which(field == as.integer(n1))])) {
            readline(prompt = "Already taken! Press enter to try again ")
            next
          } else {
            n1 <- as.integer(n1)
          }
        }
      }

      db <- make_player_mark(n1)
      render_field(db)


      # update checklist
      check_list <- list(
        d1 = c(`1` = db[1], `5` = db[5], `9` = db[9]),
        d2 = c(`7` = db[7], `5` = db[5], `3` = db[3]),
        r1 = c(`1` = db[1], `2` = db[2], `3` = db[3]),
        r2 = c(`4` = db[4], `5` = db[5], `6` = db[6]),
        r3 = c(`7` = db[7], `8` = db[8], `9` = db[9]),
        c1 = c(`1` = db[1], `4` = db[4], `7` = db[7]),
        c2 = c(`2` = db[2], `5` = db[5], `8` = db[8]),
        c3 = c(`3` = db[3], `6` = db[6], `9` = db[9])
      )



      game <- check_winner(check_list)
      game <- check_stalemate(check_list)

      if (!game) {
        break
      }
      print("The computer is making its decision...")
      Sys.sleep(stats::runif(1, 1, 3))

      # do computer play
      db <- make_pc_mark(check_list)

      # update checklist
      check_list <- list(
        d1 = c(`1` = db[1], `5` = db[5], `9` = db[9]),
        d2 = c(`7` = db[7], `5` = db[5], `3` = db[3]),
        r1 = c(`1` = db[1], `2` = db[2], `3` = db[3]),
        r2 = c(`4` = db[4], `5` = db[5], `6` = db[6]),
        r3 = c(`7` = db[7], `8` = db[8], `9` = db[9]),
        c1 = c(`1` = db[1], `4` = db[4], `7` = db[7]),
        c2 = c(`2` = db[2], `5` = db[5], `8` = db[8]),
        c3 = c(`3` = db[3], `6` = db[6], `9` = db[9])
      )



      render_field(db)
      cat("")
      game <- check_winner(check_list)
      game <- check_stalemate(check_list)
    }
  )
}
