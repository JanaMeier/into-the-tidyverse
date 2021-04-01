
# Into the Tidyverse Tutorial by Jae-Young Son
# Exercise 2

mean(2,6)
# funktioniert nicht, weil 2, 6 kein Vektor ist
mean(c(2,6))

# eigene Funktion erstellen:
custom_mean <- function(this_vector){
  sum(this_vector) / length(this_vector)
}

custom_mean(c(2,6))
my_vector <- c(2,6)
custom_mean(my_vector)
custom_mean(this_vector = my_vector)

library("readr")
library("dplyr")
library("here")

