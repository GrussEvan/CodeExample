# Goal: Create a minimum set of parlay bets covering all possible outcomes within specified parameters.
# Parameters: Maximum legs, maximum win
# Strategy: Remove the leg with the shortest odds (heaviest favorite) when necessary to meet parameters.

# Load required library
library(dplyr)

# Function to safely get user input as integer
get_integer_input <- function(prompt_message) {
  while (TRUE) {
    input <- readline(prompt = prompt_message)
    num <- suppressWarnings(as.integer(input)) # Avoid stopping on non-numeric
    if (!is.na(num)) {
      return(num)
    }
    cat("Invalid input. Please enter a valid integer.\n")
  }
}

# Function to safely get user input as double
get_double_input <- function(prompt_message) {
  while (TRUE) {
    input <- readline(prompt = prompt_message)
    num <- suppressWarnings(as.double(input))
    if (!is.na(num)) {
      return(num)
    }
    cat("Invalid input. Please enter a valid number.\n")
  }
}

# Get number of games from user
num_games <- get_integer_input("Enter the number of games: ")

# Initialize and populate the games data frame
games <- data.frame(
  Game = 1:num_games,
  Away = character(num_games),
  Home = character(num_games),
  Away_Odds = integer(num_games),
  Home_Odds = integer(num_games),
  stringsAsFactors = FALSE # Important for consistency
)

for (i in 1:num_games) {
  games$Away[i] <- readline(prompt = paste0("Game ", i, " Away Team: "))
  games$Home[i] <- readline(prompt = paste0("Game ", i, " Home Team: "))
  games$Away_Odds[i] <- get_integer_input(paste0("Game ", i, " Away Odds: "))
  games$Home_Odds[i] <- get_integer_input(paste0("Game ", i, " Home Odds: "))
}

# Calculate decimal odds using a vectorized approach
games <- games %>%
  mutate(
    Away_Odds_Dec = ifelse(Away_Odds < 0, (100 / -Away_Odds) + 1, (Away_Odds / 100) + 1),
    Home_Odds_Dec = ifelse(Home_Odds < 0, (100 / -Home_Odds) + 1, (Home_Odds / 100) + 1)
  )

# Function to generate all possible combinations recursively
generate_combinations <- function(game_data, current_combo = data.frame(stringsAsFactors = FALSE), game_index = 1) {
  if (game_index > nrow(game_data)) {
    return(current_combo)
  }
  
  away_pick <- game_data[game_index, "Away"]
  away_odds <- game_data[game_index, "Away_Odds_Dec"]
  home_pick <- game_data[game_index, "Home"]
  home_odds <- game_data[game_index, "Home_Odds_Dec"]
  
  if (nrow(current_combo) == 0) {
    combos_away <- data.frame(G1 = away_pick, G1_Odds = away_odds, stringsAsFactors = FALSE)
    combos_home <- data.frame(G1 = home_pick, G1_Odds = home_odds, stringsAsFactors = FALSE)
  } else {
    away_col_name <- paste0("G", game_index)
    away_odds_col_name <- paste0("G", game_index, "_Odds")
    home_col_name <- paste0("G", game_index)
    home_odds_col_name <- paste0("G", game_index, "_Odds")
    
    combos_away <- current_combo %>%
      mutate(!!away_col_name := away_pick, !!away_odds_col_name := away_odds)
    combos_home <- current_combo %>%
      mutate(!!home_col_name := home_pick, !!home_odds_col_name := home_odds)
  }
  
  
  all_combos <- bind_rows(combos_away, combos_home)
  
  if (game_index == 1) {
    all_combos$Total_Odd <- all_combos$G1_Odds
  } else {
    prev_col_odds <- paste0("G", game_index - 1, "_Odds")
    all_combos <- all_combos %>% mutate(Total_Odd = Total_Odd * all_combos[[prev_col_odds]])
  }
  
  generate_combinations(game_data, all_combos, game_index + 1)
}

# Generate all possible combinations
bets <- generate_combinations(games)

bets <- bets %>% mutate(Combo_Number = 1:n())


# Get betting parameters from user
bet_amount <- get_double_input("Enter Bet Amount: ")
max_win_amount <- get_double_input("Enter Maximum Win Amount: ")
max_legs_allowed <- get_integer_input("Enter Maximum Legs Allowed: ")

max_odds_allowed <- (max_win_amount - bet_amount) / bet_amount

# Adjust max_legs_allowed if necessary
max_legs_allowed <- min(max_legs_allowed, num_games)

# Function to remove the leg with the minimum odds
remove_min_odds_leg <- function(df) {
  odds_cols <- grep("_Odds$", names(df), value = TRUE)  # Get column names ending with "_Odds"
  game_cols <- gsub("_Odds", "", odds_cols) # Get corresponding game columns
  
  min_odds_info <- apply(df[, odds_cols], 1, function(row_odds) {
    valid_odds <- row_odds[row_odds > 1 & !is.na(row_odds)]
    if (length(valid_odds) > 0) {
      min_odd <- min(valid_odds, na.rm = TRUE)
      min_odd_index_in_valid <- which.min(valid_odds)
      original_indices <- which(row_odds > 1 & !is.na(row_odds))
      min_odd_index <- original_indices[min_odd_index_in_valid]
      new_total_odds <- max(valid_odds, na.rm = TRUE) / min_odd
      c(min_odd, min_odd_index)
    } else {
      c(NA, NA) # Return NAs if no valid odds
    }
  })
  
  min_odds_values <- min_odds_info[1,]
  min_odds_indices <- min_odds_info[2,]
  
  for (i in 1:nrow(df)) {
    if (!is.na(min_odds_indices[i])) { # Only modify if a valid min_odd was found
      
      min_odds_col_name <- odds_cols[min_odds_indices[i]]
      min_game_col_name <- game_cols[min_odds_indices[i]]
      
      df[i, min_odds_col_name] <- 1
      df[i, min_game_col_name] <- "N/A"
      
    }
  }
  
  new_total_odds <- apply(df[,odds_cols], 1, function(x){
    prod(x[x!= "N/A" & !is.na(x)])
  })
  
  if("Total_Odd" %in% colnames(df)){
    df$Total_Odd <- new_total_odds
  }
  
  return(df)
}



# Remove legs if max legs is less than number of games
bets_adjusted <- bets
current_legs <- num_games

while (current_legs > max_legs_allowed) {
  bets_adjusted <- remove_min_odds_leg(bets_adjusted)
  matching_rows <- match(bets_adjusted$Combo_Number, bets$Combo_Number)
  bets[matching_rows, ] <- bets_adjusted
  current_legs <- current_legs - 1
}


# Remove legs if total odds exceed maximum allowed odds
bets_adjusted <- bets %>% filter(Total_Odd > max_odds_allowed)
while (nrow(bets_adjusted) > 0) {
  bets_adjusted <- remove_min_odds_leg(bets_adjusted)
  matching_rows <- match(bets_adjusted$Combo_Number, bets$Combo_Number)
  bets[matching_rows, ] <- bets_adjusted
  bets_adjusted <- bets %>% filter(Total_Odd > max_odds_allowed)
}

# Remove records containing the same outcomes
# Repeat until starting records = final records
bets <- bets[ , !grepl( "_Odds" , names( bets ) ) ]
bets <- bets[ ,!names(bets) %in% c("Combo_Number")]


startingrowcount <- nrow(bets)
endingrowcount <- 0
iterations <- 1
bets[,"Bet_Number"] <- 1:startingrowcount
while (startingrowcount != endingrowcount) {
  startingrowcount <- nrow(bets)
  x <- 1
  while (x < nrow(bets)) {
    bets2 <- bets
    if (x%%1000 == 0) {
      print(paste0(x, " out of ", nrow(bets)))
    }
    for (z in 1: num_games) {
      col1 <- paste0("G",z)
      curr_bet <- bets[x, col1]
      if (curr_bet != "N/A") {
        bets2 <- subset(bets2, bets2[,col1] == curr_bet)
      }
      if (nrow(bets2) <= 1) {break}
    }
    if (nrow(bets2) > 1) {
      curr_no <- bets[x, "Bet_Number"]
      bets2 <- subset(bets2, Bet_Number != curr_no)
      bets <- anti_join(bets, bets2, by = names(bets))
    }
    x <- x+1
  }
  iterations <- iterations +1
  endingrowcount <- nrow(bets)
}
rm(bets2)
bets$Bet_Number <- 1:nrow(bets)

# Save results to CSV
output_path <- file.path(Sys.getenv("USERPROFILE"), "Documents", "bets.csv")
write.csv(bets, output_path, row.names = FALSE)
cat(paste0("Bets saved to: ", output_path, "\n"))