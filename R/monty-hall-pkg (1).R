#' @title Create a Monty Hall Game
#' @description Generates a game with three doors, one of which has a car behind it.
#' @details The game setup replicates the classic Monty Hall problem, where a contestant chooses a door, the host opens a door revealing a goat, and the contestant decides whether to stay or switch.
#' @return A character vector of length 3 representing the contents behind each door (e.g., "goat", "goat", "car").
#' @examples
#' create_game()
#' @export
create_game <- function() {
  doors <- c("goat", "goat", "car")
  sample(doors, 3)
}

#' @title Select a Door
#' @description Randomly selects one of the three doors.
#' @details This function simulates the contestant's initial door selection.
#' @return An integer between 1 and 3 representing the selected door.
#' @examples
#' select_door()
#' @export
select_door <- function() {
  doors <- c(1, 2, 3)
  a.pick <- sample(doors, size = 1)
  return(a.pick)  # number between 1 and 3
}

#' @title Open a Goat Door
#' @description Opens a door that has a goat behind it and is not the contestant's initial pick.
#' @details The host opens a door that is not the contestant's initial pick and does not contain the car.
#' @param game A character vector representing the game setup (e.g., the output of `create_game()`).
#' @param a.pick An integer representing the contestant's initial door selection.
#' @return An integer between 1 and 3 representing the opened door.
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' open_goat_door(game, pick)
#' @export
open_goat_door <- function(game, a.pick) {
  doors <- c(1, 2, 3)
  if (game[a.pick] == "car") {
    goat.doors <- doors[game != "car"]
    opened.door <- sample(goat.doors, size = 1)
  }
  if (game[a.pick] == "goat") {
    opened.door <- doors[game != "car" & doors != a.pick]
  }
  return(opened.door)  # number between 1 and 3
}

#' @title Change Door
#' @description Allows the contestant to stay with their initial pick or switch to the remaining unopened door.
#' @details The contestant can choose to stay with their initial pick or switch to the other unopened door.
#' @param stay A logical value indicating whether the contestant stays with their initial pick (`TRUE`) or switches (`FALSE`).
#' @param opened.door An integer representing the door opened by the host.
#' @param a.pick An integer representing the contestant's initial door selection.
#' @return An integer between 1 and 3 representing the contestant's final choice.
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' opened.door <- open_goat_door(game, pick)
#' change_door(stay = TRUE, opened.door, pick)  # Stay with initial pick
#' change_door(stay = FALSE, opened.door, pick) # Switch to another door
#' @export
change_door <- function(stay = TRUE, opened.door, a.pick) {
  doors <- c(1, 2, 3)
  if (stay) {
    final.pick <- a.pick
  } else {
    final.pick <- doors[doors != opened.door & doors != a.pick]
  }
  return(final.pick)  # number between 1 and 3
}

#' @title Determine Winner
#' @description Determines whether the contestant wins or loses based on their final door selection.
#' @details The function checks if the contestant's final pick contains the car (win) or a goat (lose).
#' @param final.pick An integer representing the contestant's final door selection.
#' @param game A character vector representing the game setup (e.g., the output of `create_game()`).
#' @return A character string: "WIN" if the contestant wins, "LOSE" if the contestant loses.
#' @examples
#' game <- create_game()
#' pick <- select_door()
#' opened.door <- open_goat_door(game, pick)
#' final.pick <- change_door(stay = TRUE, opened.door, pick)
#' determine_winner(final.pick, game)
#' @export
determine_winner <- function(final.pick, game) {
  if (game[final.pick] == "car") {
    return("WIN")
  } else {
    return("LOSE")
  }
}

#' @title Play a Monty Hall Game
#' @description Simulates a full Monty Hall game, including the contestant's initial pick, the host's action, and the final outcome.
#' @details The function simulates a single game and returns the results of both staying and switching strategies.
#' @return A data frame with two rows (one for "stay" and one for "switch") and two columns: "strategy" and "outcome".
#' @examples
#' play_game()
#' @export
play_game <- function() {
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door(new.game, first.pick)

  final.pick.stay <- change_door(stay = TRUE, opened.door, first.pick)
  final.pick.switch <- change_door(stay = FALSE, opened.door, first.pick)

  outcome.stay <- determine_winner(final.pick.stay, new.game)
  outcome.switch <- determine_winner(final.pick.switch, new.game)

  strategy <- c("stay", "switch")
  outcome <- c(outcome.stay, outcome.switch)
  game.results <- data.frame(strategy, outcome, stringsAsFactors = FALSE)
  return(game.results)
}

#' @title Play Multiple Monty Hall Games
#' @description Simulates multiple Monty Hall games and summarizes the results.
#' @details The function simulates `n` games and calculates the proportion of wins for both staying and switching strategies.
#' @param n An integer specifying the number of games to simulate (default is 100).
#' @return A data frame summarizing the results of the simulations.
#' @examples
#' play_n_games(n = 100)
#' @export
play_n_games <- function(n = 100) {
  results.list <- list()  # collector
  loop.count <- 1

  for (i in 1:n) {  # iterator
    game.outcome <- play_game()
    results.list[[loop.count]] <- game.outcome
    loop.count <- loop.count + 1
  }

  results.df <- dplyr::bind_rows(results.list)

  table(results.df) %>%
    prop.table(margin = 1) %>%  # row proportions
    round(2) %>%
    print()

  return(results.df)
}
