#Code for Multiple Monte Carlo Games
n_stay <- 0 # set number of stays to 0
n_switch <- 0 # set number of switches to 0
n_wins <- 0 # set number of wins to 0

for (i in 1:100) {
  door <- c(1, 2, 3) # set door values to a vector with values between 1 and 3
  cardoor <- sample(door, 1) # set car door to a random door value
  choice <- sample(door, 1) # set player choice to a random door value
  goatdoors <- setdiff(door, cardoor) # set goat doors to whichever values in door car door is not
  reveal_options <- setdiff(goatdoors, choice) # set reveal options to which ever goat door choice is not
  if (choice == cardoor) {
    reveal <- sample(reveal_options, 1) # if choice is the car door, set reveal to one other random goat door
  }
  else {
    reveal <- reveal_options # if choice is not the car door, set a new variable reveal to reveal options
  }
  remaining_doors <- setdiff(door, reveal) # set remaining doors to whichever doors that have not yet been revealed
  newchoice <- setdiff(remaining_doors, choice) # set new choice to a value in remaining doors that isn't the old choice
  pick <- sample(remaining_doors, 1) # pick a random door out of what is remaining
  if (choice == cardoor) {
    # if the original random choice is the car door
    if (pick == cardoor) {
      # if the random pick is the cardoor
      n_wins <- n_wins + 1 # increase number of wins by 1
    }
    n_stay <- n_stay + 1 # if the old choice is the car door, increase number of stays by 1
  }

  if (newchoice == cardoor) {
    # if the new choice is the car door
    if (pick == cardoor) {
      # if the random pick is the cardoor
      n_wins <- n_wins + 1 # increase number of wins by 1
    }
    n_switch <- n_switch + 1 # if the new choice is the car door, increase number of switches by 1
  }
}


print(n_stay / 100) # print out number of stays / 100
print(n_switch / 100) # print out number of switches / 100
print(n_wins / 100) # print out number of wins / 100