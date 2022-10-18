# Project set-up

library(palmerpenguins) # Access R package, if not installed start with install.packages("palmerpenguins")
data("penguins") # Load penguin data
penguins = na.omit(penguins) # Remove missing rows

my.student.number = 210431461 # My student number
set.seed(my.student.number) #
my.penguins = penguins[sample(nrow(penguins), 100), ] # Create my unique subset of the penguin dataset


