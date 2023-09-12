n_stay <- 0
n_switch <- 0

for (i in 1:100) {
  door <- c(1, 2, 3)
  cardoor <- sample(door, 1)
  choice <- sample(door, 1)
  goatdoors <- setdiff(door, cardoor)
  reveal_options <- setdiff(goatdoors, choice)
  if (choice == cardoor) {
    reveal <- sample(reveal_options, 1)
  }
  else {
    reveal <- reveal_options
  }
  remaining_doors <- setdiff(door, reveal)
  newchoice <- setdiff(remaining_doors, choice)

  if (choice == cardoor) {
    n_stay <- n_stay + 1
  }

  if (newchoice == cardoor) {
    n_switch <- n_switch + 1
  }
}
print(n_stay / 100)
print(n_switch / 100)
