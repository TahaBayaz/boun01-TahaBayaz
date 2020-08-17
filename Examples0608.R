convert_temperature = function(value, F_to_C) {
  if (!F_to_C) {
    result = value * 9 / 5 + 32
  } else {
    result = (value - 32) * 5 / 9
  }
  return(result)
}

convert_temperature(30,F_to_C = FALSE)
convert_temperature(86,F_to_C = TRUE)

calculate_future_value = function(investment, interest, duration_in_years) {
  FV = investment * (1 + interest) ^ duration_in_years
  return(FV)
}

calculate_future_value(investment = 100, interest = 0.07, duration_in_years = 5)

generate_hex_code = function(n) {
  options = c(letters, 0:9)
  result = c()
  for (i in 1:n) {
    result = c(result, paste0('#', paste0(sample(options, 6, replace = TRUE), collapse =  "")))
  }
  return(result)
}

generate_hex_code(n=3)

get_prob_dice = function(k, n) {
  result = dbinom(k, n, prob = 1/6)
  return(result)
}

get_prob_dice(3, 5)

winner_check = function(comp, input) {
  if (comp == input) {
    print(paste("I choose ", comp, ". It is tie!"))
  } else if ((comp == "rock" & input == "scissors") | (comp == "scissors" & input == "paper") | (comp == "paper" & input == "rock")) {
    print(paste("I choose ", comp, ". You lose!"))
  } else {
    print(paste("I choose ", comp, ". You win!"))
  }
}

rsp_game = function(input) {
  options = c("rock", "paper","scissors")
  comp = options[sample(1:3, 1)]
  winner_check(comp, input)
}

rsp_game("rock")

