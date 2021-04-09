
# Into the Tidyverse Tutorial by Jae-Young Son
# Exercise 3: Tidying Data (tidyr)

## 0. Preparation -----------------

library("tidyr")
library("dplyr")
library("readr")
library("here")
library("janitor")


## 1. pivot_longer ----------------

billboard %>% slice(1:10) # This is a built in dataset in tidyr. It is in wide format (multiple observations in each row)
# we want each row to contain only one observation
billboard %>% 
  pivot_longer(cols=starts_with("wk"),
               names_to = "week",
               values_to ="ranking") %>% 
  drop_na() %>%  # drops any row with missing values
  group_by(track) %>% 
  slice(1:5) %>% # keeps only the first 5 weeks of each song
  ungroup() %>% 
  slice(1:30)

## 2. pivot_wider ------------------

# sometimes, datasets are too long

us_rent_income # here we have the median estimate and 90% margin of error for the yearly income and monthly rent for various US states

# rather than have rent and income values in one column, we want them in separate columns
us_rent_income %>% 
  pivot_wider(names_from = "variable", values_from = c("estimate", "moe"))

# now we can analyze the income to rent ratio for different regions:
us_rent_income %>% 
  pivot_wider(names_from = "variable", values_from = c("estimate", "moe")) %>% 
  select(locale = NAME, estimate_income, estimate_rent) %>% 
  group_by(locale) %>% 
  summarize(p_income_spent_on_rent = 12*estimate_rent / estimate_income) %>% 
  arrange(p_income_spent_on_rent)

## 3. separate --------------------

# Looks for common characters that may separate data (e.g. - _ . / etc.) and separates it into different variables

conformity <- here("Data", "JustCon5_TPP_Order1.csv") %>% 
  read_csv() %>% 
  select(sub_id = mTurkCode,
         starts_with("assault"),
         starts_with("theft")) %>% 
  slice(-1) %>% 
  type_convert()
conformity
# problem wiht this dataset: not tidy! Rows are not unique observations, columns do not encode unique variables
conformity %>% 
  pivot_longer(cols = -sub_id,               # pivot everything exept for sub_id
               names_to = "condition",
               values_to = "rating")
# problem now: The "condition" column contains information about various conditions --> we need to separate this
conformity %>% 
  pivot_longer(cols = -sub_id,
               names_to = "condition",
               values_to = "rating") %>% 
  separate( col = condition,
            into = c("crime_type", "crime_severity", "n_endorsing_punishment", "repetition_number", "qualtrics_junk")) %>% 
  select(-qualtrics_junk)
# now it's tidy!

## 4. unite ----------------------

elections <- here("Data", "countypres_2000-2016.csv") %>% 
  read_csv() %>% 
  select(year, county, state, candidate, party, candidatevotes, totalvotes)
elections               
# we want to combine information from the columns for state and county (to avoid confusion with e.g. the county Dallas that exists in multiple states)
elections %>% 
  unite(col = "location",
        county, state)
# Now we want a comma instead of the default underscore separator
elections %>% 
  unite(col = "location",
        county, state,
        sep = ", ")

## 5. Bonus: janitor -------------

# Tidyverse preferred way of formatting variable names is with lower case and underscores (snake_case)

banks <- here("Data", "BankBranchesData.txt") %>% 
  read_tsv()
banks
# As we see, this dataset uses camelCase for the variable names. We can change this with janitor (although not really necessary)
banks %>% 
  clean_names()
# Now, let's look at a dataset with truly terrible names
candy <- here("Data", "candyhierarchy2017.csv") %>% 
  read_csv()
candy # the variable names are so bad that r console won't display them ;D
candy <- candy %>% 
  clean_names() 
# now it's better, I guess...

## 6. Exercises ------------------

# Make Candy dataset tidy with one observation per row
candy_long <- candy %>% 
  pivot_longer(cols = starts_with ("q6"),
               names_to = "candy",
               values_to = "rating")

# Get Johns Hopkins covid dataset and tidy it to a long format. Also, clean variable names
covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>% 
  read_csv()
# Change format to long
covid_long <- covid %>% 
  pivot_longer(cols = 12:length(colnames(covid)),
               names_to = "date",
               values_to = "covid_rate")
# clean names
covid_long <- covid_long %>% 
  clean_names()

# We have to clean the variable names AFTERWARDS, because otherwise, the dates get changed to a shitty format, as we can see here:
covid %>% 
  clean_names()
