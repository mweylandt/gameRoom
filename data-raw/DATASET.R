## BLACKJACK ingredients

library(glue)


values <- c(1:10, "J", "Q", "K", "A")
suits <- c("♠", "♥", "♦", "♣")

cards <- expand.grid(v = values, s = suits)
cards$v <- as.character(cards$v)
cards$s <- as.character(cards$s)
cards$points <- as.numeric(cards$v)
cards$points[cards$v == "A"] <- 11 # this is for blackjack, so this is good initial
cards$points[is.na(cards$points)] <- 10

cardlist <- list()

for (i in 1:nrow(cards)) {
  s <- cards$s[i]
  v <- cards$v[i]
  v <- ifelse(nchar(v) == 1, paste0(" ", v), v)

  cat("\n")
  card <- glue("
 _______
|{s}     {s}|
|       |
|  {v}   |
|       |
|{s}     {s}|
|_______|")

  cat(card)
  cat("\n")

  thiscard <- list(
    image = card,
    points = cards$points[i],
    value = cards$v[i]
  )

  cardname <- paste0(cards$v[i], " of ", cards$s[i])

  cardlist[[cardname]] <- thiscard
}

blackjack_logo <- "
 _     _            _     _            _
| |__ | | __ _  ___| | __(_) __ _  ___| | __
| '_ \\| |/ _` |/ __| |/ /| |/ _` |/ __| |/ /
| |_) | | (_| | (__|   < | | (_| | (__|   <
|_.__/|_|\\__,_|\\___|_|\\_\\/ |\\__,_|\\___|_|\\_\\
                       |__/
"

card_back <- " _______
|xxxxxxx|
|xxxxxxx|
|xxxxxxx|
|xxxxxxx|
|xxxxxxx|
|_______|"






# HANGMAN inputs ----------------------------------------------------------
words <- readLines("data/words.txt")

usethis::use_data(cardlist, blackjack_logo, card_back, words, overwrite = TRUE, internal = TRUE)

