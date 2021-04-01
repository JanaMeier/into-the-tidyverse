
# Into the Tidyverse by Jae-Young Son
# Exercise 1: Loading Data

library("readr")
library("here") #This library starts with the folder where your code is stored, then checks whether there is a file with the extension .Rproj. 
#If there isn’t, it goes up to the parent folder and does the same thing
library("janitor")
library("tictoc")

x <- 12
y <- 4
x/y

this_year <- "2020"
is.character(this_year)
is.numeric(this_year)
this_year==2020 # base R ist "weakly coded", ändert Datenformate, ohne zu warnen. Tidyverse ist nicht so, deshalb sind Tibbles (tidyverse-Format) besser als dataframes

## Exercise ----------------------

here("Data", "PVD_2020_Property_Tax_Roll.csv") %>% #sucht den Pfad raus
  read.csv() %>% #nimm den vorher generierten Pfad und lies die Tabelle ein
  head() #zeig nur die ersten Zeilen

# reading data with the base-R function read.csv
tic()
base_read <- here("Data", "PVD_2020_Property_Tax_Roll.csv") %>%
  read.csv()
toc()
str(base_read)  

# reading data with the readr-function read_csv
tic()
tidy_read <- here("Data", "PVD_2020_Property_Tax_Roll.csv") %>%
  read_csv()
toc()
tidy_read %>% 
  str()
tidy_read_mod <- here("Data", "PVD_2020_Property_Tax_Roll.csv") %>%
  read_csv(col_types = cols(ZIP_POSTAL=col_character(), plat=col_character())) %>% 
  str()

covid_usa <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv")
bank_branches <- here("Data", "BankBranchesData.txt") %>% 
  read_tsv()
