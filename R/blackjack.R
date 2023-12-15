#' blackjack
#'
#' A game of blackjack!
#'
#' @details
#' \describe{
#' \item{- minimum bet is 5}{}
#' \item{- no splits}{}
#' \item{- one deck, shuffled once down to 20 cards or fewer}{}
#'
#'
#'}
#' @export
#'
#'
blackjack <- function() {
  # get art -----------------------------------------------------------------


  back <- card_back

  # Make object with all the game info
  gamestate <- list(
    deck = cardlist,
    player_hand = "",
    player_score = 0,
    player_score_list = NULL,
    dealer_hand = "",
    dealer_score = 0,
    bet = "",
    total_bank = 100,
    game = TRUE,
    round = TRUE,
    player = TRUE,
    dealer = TRUE,
    message = "",
    counter = 1
  )



  # game loop ---------------------------------------------------------------


  check_outcome <- function(gamestate, p = "player") {
    if (p == "player") {
      gamestate <- check_player_bust(gamestate)
      gamestate <- check_draw(gamestate)
      gamestate <- check_player_blackjack(gamestate)
      return(gamestate)
    } else if (p == "dealer") {
      gamestate <- check_dealer_bust(gamestate)
      gamestate <- check_dealer_win(gamestate)
      gamestate <- check_draw(gamestate)
    }
  }

  check_draw <- function(gamestate) {
    if (gamestate$player_score == gamestate$dealer_score) {
      # return that it's a draw and round is over
      gamestate$player <- FALSE
      gamestate$dealer <- FALSE
      gamestate$round <- FALSE
      gamestate$message <- "It's a draw!"
      gamestate$total_bank <- gamestate$total_bank + gamestate$bet
      gamestate$bet <- 0
      return(gamestate)
    } else {
      return(gamestate)
    }
  }

  check_player_blackjack <- function(gamestate) {
    if (gamestate$player_score == 21) {
      gamestate$outcome <- "p_win"
      gamestate$message <- "BLACKJACK!"
      gamestate$dealer <- FALSE
      gamestate$player <- FALSE
      gamestate$round <- FALSE
      gamestate$total_bank <- gamestate$total_bank + gamestate$bet * 2
      gamestate$bet <- 0
      return(gamestate)
    } else {
      return(gamestate)
    }
  }

  check_player_bust <- function(gamestate) {
    if (gamestate$player_score > 21) {
      # don't forget to amend here for aces
      gamestate$player <- FALSE
      gamestate$dealer <- FALSE
      gamestate$round <- FALSE
      gamestate$message <- "You went Bust!"
      gamestate$bet <- 0

      return(gamestate)
    } else {
      return(gamestate)
    }
  }


  check_dealer_bust <- function(gamestate) {
    if (gamestate$dealer_score > 21) {
      gamestate$dealer <- FALSE
      gamestate$dealer <- FALSE
      gamestate$round <- FALSE
      gamestate$message <- "Dealer Bust - You win!"
      gamestate$total_bank <- gamestate$total_bank + gamestate$bet * 2

      return(gamestate)
    } else {
      return(gamestate)
    }
  }

  check_dealer_win <- function(gamestate) {
    if (gamestate$dealer_score <= 21 &
      gamestate$dealer_score > gamestate$player_score) {
      gamestate$player <- FALSE # just in case
      gamestate$dealer <- FALSE
      gamestate$round <- FALSE
      gamestate$outcome <- "d_win"
      gamestate$message <- "DEALER WINS"
      return(gamestate)
    } else {
      return(gamestate)
    }
  }

  hit <- function(gamestate, player = "player") {
    newcard <- sample(gamestate$deck, 1)
    gamestate[[paste0(player, "_hand")]] <- append(gamestate[[paste0(player, "_hand")]], newcard)
    total <- sum(sapply(gamestate[[paste0(player, "_hand")]], `[[`, 2))

    if (player == "player") {
      gamestate <- calculate_player_score_list(gamestate)
    }


    gamestate[[paste0(player, "_score")]] <- total
    return(gamestate)
  }


  calculate_player_score_list <- function(gamestate) {
    values <- sapply(gamestate$player_hand, `[[`, 3)
    points <- sapply(gamestate$player_hand, `[[`, 2)

    if ("A" %in% values) {
      # calculate all possible score options

      indices <- which(values == "A" & points == 11)
      score_options <- rep(NA, length(indices) + 1)
      score_options[1] <- sum(points)

      # go through the values list and successively replace Aces until there
      # are none left
      # each time add to score options
      for (i in 1:length(indices)) {
        points[indices[i]] <- 1
        score_options[i + 1] <- sum(points)
      }

      gamestate$player_score_list <- score_options
      # out of the ones below 21, pick the highest one
      # first one always highest seeing as we replace 1 Ace at a time
      gamestate$player_score <- gamestate$player_score_list[which(gamestate$player_score_list <= 21)[1]]


      # if (sapply(gamestate$player_hand, `[[`, 2) > 21) {
      #    # replace the first of the ones with A and 11 with 1.
      #    # next one only replaced next time you go over 21
      #    if(length(indices)!=0){
      #      gamestate$player_hand[[indices[1]]]$points <- 1
      #    }
      #    total = sum(sapply(gamestate$player_hand, `[[`, 2))
      #    gamestate$player_score
      # }
    } else { # if no aces involved
      gamestate$player_score <- sum(sapply(gamestate$player_hand, `[[`, 2))
      gamestate$player_score_list <- gamestate$player_score
    }

    return(gamestate)
  }

  update_deck <- function(gamestate) {
    cards_out <- c(names(gamestate$player_hand), names(gamestate$dealer_hand))
    taken_card_indices <- which(names(cardlist) %in% cards_out)
    deck <- cardlist[-c(taken_card_indices)]
    gamestate$deck <- deck
    return(gamestate)
  }




  # rendering ---------------------------------------------------------------


  render_hand <- function(hand) {
    lines <- list()
    for (i in 1:length(hand)) {
      lines[[i]] <- unlist(strsplit(hand[[i]]$image, "\n"))
    }


    # make a matrix, and then print it row by row
    ma <- matrix(nrow = 7, ncol = length(hand))
    for (n in 1:ncol(ma)) {
      for (i in 1:7) {
        ma[i, n] <- paste0("{lines[[", n, "]]", "[", i, "]}")
      }
    }

    ma[1, 2:ncol(ma)] <- paste0(" ", ma[1, 2:ncol(ma)])

    pattern <- paste0(
      paste0(ma[1, ], collapse = " "), "\n",
      paste0(ma[2, ], collapse = " "), "\n",
      paste0(ma[3, ], collapse = " "), "\n",
      paste0(ma[4, ], collapse = " "), "\n",
      paste0(ma[5, ], collapse = " "), "\n",
      paste0(ma[6, ], collapse = " "), "\n",
      paste0(ma[7, ], collapse = " "), "\n"
    )

    hand <- glue::glue(pattern)
  }


  hand <- gamestate$dealer_hand
  render_dealer_hand <- function(hand) {
    lines <- list()
    lines[[1]] <- unlist(strsplit(hand[[1]]$image, "\n"))
    lines[[2]] <- unlist(strsplit(back, "\n"))



    # make a matrix, and then print it row by row
    ma <- matrix(nrow = 7, ncol = 2)
    for (n in 1:ncol(ma)) {
      for (i in 1:7) {
        ma[i, n] <- paste0("{lines[[", n, "]]", "[", i, "]}")
      }
    }

    ma[1, 2] <- paste0(" ", ma[1, 2])

    pattern <- paste0(
      paste0(ma[1, ], collapse = " "), "\n",
      paste0(ma[2, ], collapse = " "), "\n",
      paste0(ma[3, ], collapse = " "), "\n",
      paste0(ma[4, ], collapse = " "), "\n",
      paste0(ma[5, ], collapse = " "), "\n",
      paste0(ma[6, ], collapse = " "), "\n",
      paste0(ma[7, ], collapse = " "), "\n"
    )

    hand <- glue::glue(pattern)
    hand
  }

  render_table <- function(gamestate) {
    if (gamestate$player == TRUE) {
      dealer_p <- render_dealer_hand(gamestate$dealer_hand)
      points_d <- gamestate$dealer_hand[[1]]$points
    } else if (gamestate$player == FALSE) {
      dealer_p <- render_hand(gamestate$dealer_hand)
      points_d <- gamestate$dealer_score
    }

    player_p <- render_hand(gamestate$player_hand)
    points_p <- gamestate$player_score
    pointlist_p <- paste0(gamestate$player_score_list, collapse = "/")


    field <- glue::glue("
                {blackjack_logo}
    DEALER

    {dealer_p}
    Total: {points_d}
    -----------------------------------------------
     Total Money: ${gamestate$total_bank}       {gamestate$message}
     Current Bet: ${gamestate$bet}
    -----------------------------------------------
    Total: {points_p} ({pointlist_p})
    {player_p}

    PLAYER")

    cat("\014")
    cat(field)
  }


  newcards <- function(gamestate) {
    if (length(gamestate$deck) < 20) {
      gamestate$deck <- cardlist
      return(gamestate)
    } else {
      return(gamestate)
    }
  }

  # set up gamestate --------------------------------------------------------

  library(conText)

  fresh_gamestate <- function(gamestate) {
    gamestate$round <- TRUE
    gamestate$player <- TRUE
    gamestate$bet <- ""
    gamestate$message <- ""
    gamestate$dealer <- TRUE

    gamestate$dealer_hand <- sample(gamestate$deck, 2)
    gamestate <- update_deck(gamestate)
    gamestate$player_hand <- sample(gamestate$deck, 2)
    gamestate <- update_deck(gamestate)
    gamestate$player_score <- sum(sapply(gamestate$player_hand, `[[`, 2))
    gamestate$dealer_score <- sum(sapply(gamestate$dealer_hand, `[[`, 2))
    gamestate <- calculate_player_score_list(gamestate)

    return(gamestate)
  }

  # game loop ---------------------------------------------------------------


  gamestate <- fresh_gamestate(gamestate)
  game <- TRUE
  suppressWarnings(
    while (game == TRUE) {
      gamestate <- newcards(gamestate)

      gamestate <- fresh_gamestate(gamestate)
      gamestate$bet <- ""

      while (gamestate$bet == "") {
        render_table(gamestate)

        # get input to bet
        {
          input <- readline(prompt = "Place a bet: ")


          if (is.na(as.numeric(input))) {
            gamestate$message <- "That's not a number. "
            next
            # if it is a number
          } else if (as.numeric(input) & as.numeric(input) > gamestate$total_bank) {
            gamestate$message <- "You don't have that much.  "
            next
          } else if (as.numeric(input) & as.numeric(input) < 5) {
            gamestate$message <- "The minimum bet is 5.  "
            next
          } else if (as.numeric(input) & as.numeric(input) <= gamestate$total_bank) {
            gamestate$bet <- as.numeric(input)
            gamestate$total_bank <- gamestate$total_bank - gamestate$bet
            gamestate$message <- ""
          }
        } # input end
      } # bet check end

      while (gamestate$round == TRUE) {
        while (gamestate$player == TRUE) {
          render_table(gamestate)

          input <- readline(prompt = "player move. Press 'h' to Hit or 's' to stand:")
          if (input == "h" | input == "H") {
            gamestate <- hit(gamestate, "player")
            gamestate <- update_deck(gamestate)
            gamestate <- calculate_player_score_list(gamestate)
            gamestate <- check_outcome(gamestate, p = "player")
            render_table(gamestate)


          } else if (input == "s" | input == "S") {
            gamestate$player <- FALSE
            Sys.sleep(stats::runif(1, min = 1, max = 2.5))
            gamestate <- check_outcome(gamestate, p = "player")
            render_table(gamestate)
            gamestate$message <- "Drawing another card..."
            gamestate <- check_outcome(gamestate, p = "dealer")
            render_table(gamestate)
          }
        } # end of player input
        counter <- 1
        # still in the same round
        while (gamestate$dealer == TRUE) {
          Sys.sleep(stats::runif(1, min = 1, max = 2.5))
          gamestate <- hit(gamestate, "dealer")
          gamestate <- update_deck(gamestate)
          gamestate <- check_outcome(gamestate, p = "dealer")
          render_table(gamestate)
        }
      } # end of round
      input <- readline("Play again? Y/N ")
      if (input == "y" | input == "Y") {
        gamestate$player <- TRUE
        gamestate$dealer <- TRUE
        gamestate$round <- TRUE
        next
      } else {
        break
      }
    }
  ) # end of suppressWarnings
} # end of function
