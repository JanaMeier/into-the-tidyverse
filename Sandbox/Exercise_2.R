
# Into the Tidyverse Tutorial by Jae-Young Son
# Exercise 2: Dplyr

## 0. Intro: random stuff -----------------

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

## 1. prepare libraries and data ------------------

library("readr")
library("dplyr")
library("here")
covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>%  #search for file in Data folder, read it in as tibble and show first 10 rows
  read_csv()
covid %>% 
  slice(1:10) #slice lets us look at only some rows

## 2. filter and select ------------

# filter lets us choose rows (=observations)
# select lets us choose columns (=variables)

covid %>% 
  filter(Province_State=="California") %>% #show counties in California with rates above 1000 at 24.09.2020
  select(fips=FIPS, county=Admin2, "9/18/20":"9/24/20") %>% # select can also be used to rename variables
  filter('9/14/20'>=1000)

covid %>% filter(Province_State=="California") %>% #show rates of county Yolo in California for dates 18.-24.09.2020
  filter(Admin2=="Yolo") %>% 
  select(FIPS, Admin2, "9/18/20":"9/24/20")

covid %>% filter(Province_State=="California") %>% 
  filter(Admin2=="Yolo") %>% 
  select(state=Province_State, fips=FIPS, county=Admin2, latest_cases=`9/24/20`) %>% #now we changed our mind and want to get rid of state
  select(-state)

## 3. mutate ---------------------

# mutate transfomrs variables and adds new columns to dataframes

covid %>% filter(Province_State=="California") %>% #keep only observations from California
  slice(1:20) %>%  #keep only first 20 observations
  select(fips=FIPS, county=Admin2, latest_cases=`9/24/20`) %>% # keep only a few columns of interest
  #apply logarithmic computation of cases before statistical testing b/c count data is ususally not normally distributed
  mutate(latest_cases_log=log(latest_cases)) 
  
# mutate can also be used to create a variable that tells us from what dataset this data comes
covid %>% filter(Province_State=="California") %>% 
  slice(1:5) %>% 
  select(fips=FIPS, county=Admin2, `9/18/20`:`9/24/20`) %>% #pretend that we're getting this data from a CSV that contains no state info
  mutate(state="California")

# when we want to apply a function to multiple variables, we can use 'across'
covid %>% select(state=Province_State, county=Admin2, `9/18/20`: `9/24/20`) %>% 
  filter(state=="California") %>% 
  slice(1:10) %>% 
  mutate(across(.cols=`9/18/20`: `9/24/20`,
                .fns = ~log(.x+1))) # this is a lambda function that takes every x, adds 1 (to avoid log(0)=inf and takes the log

# alternative way of doing this: create a function

avoid_log_trap <- function(x) {
  log(x+1)
}

covid %>% select(state=Province_State, county=Admin2, `9/18/20`: `9/24/20`) %>% 
  filter(state=="California") %>% 
  slice(1:10) %>% 
  mutate(across(.cols=`9/18/20`: `9/24/20`,
                .fns = avoid_log_trap))

## 4. summarize --------------

# gives summary statistics of variables

covid %>% filter(Province_State=="California") %>% 
  select(fips=FIPS, county=Admin2, latest_cases=`9/24/20`) %>% 
  summarize(total_cases=sum(latest_cases))

# we can use 'across' to get summary statistics of several variables
covid %>% filter(Province_State=="California") %>% 
  select(fips=FIPS, county=Admin2, `9/18/20`: `9/24/20`) %>% 
  summarize(across(.cols =`9/18/20`: `9/24/20`,
                   .fns = sum))

## 5. arrange ---------------

# lets us reorder our dataframe

# order by amount of latest cases
covid %>% filter(Province_State=="California") %>% 
  slice(1:5) %>% 
  select(fips=FIPS, county=Admin2, latest_cases=`9/24/20`) %>%
  arrange(latest_cases)

# order by county name in reverse order
covid %>% filter(Province_State=="California") %>% 
  slice(1:5) %>% 
  select(fips=FIPS, county=Admin2, latest_cases=`9/24/20`) %>%
  arrange(desc(county))

## 6. group by ----------------

# lets us group together observations

# e.g. we can summarize observations from the same state
covid %>% 
  rename(state=Province_State, latest_cases=`9/24/20`) %>% 
  group_by(state) %>% 
  summarize(n_cases = sum(latest_cases)) %>% 
  ungroup() %>% #ungroup when done manipulating data to avoid problems later on
  arrange(desc(n_cases))

## 7. join -------------------

# lets us combine separate dataframes

# e.g. we want to find out whether number of inhabitants, urbanizity and political leaning have an impact on covid19 prevalence

# get urbanicity index
urbanicity <- here("Data", "NCHSURCodes2013.xlsx") %>% 
  readxl::read_excel(na = c(".")) %>% 
  janitor::clean_names() %>% 
  select(fips_code, urbanicity = x2013_code, population = county_2012_pop)
urbanicity %>% slice(1:5)

# get voting data and compute Trump/Clinton index
elections <- here("Data", "countypres_2000-2016.csv") %>% #search for election data in folder "Data"
  read_csv() %>% # read file into R
  filter(year==2016) %>% # keep only data for the year 2016
  filter(party %in% c("democrat", "republican")) %>% # keep only data for reps and democrats, not others
  group_by(state, county, FIPS) %>% 
  # for each group (=FIPS within county and state) take votes for candidate and divide by votes for first candidate
  # democrats come first, therefore there is a 1 in each row with democrat votes, whereas the rows with the republican votes contain the ratio rep/democ
  mutate(lean_republican = candidatevotes/first(candidatevotes)) %>% 
  ungroup() %>% 
  filter(party=="republican") %>% #now keep only the rows with republican votes since they contain the ratio in column "lean_republican"
  select(state, county, FIPS, lean_republican) # keep only relevant columns

# now let's join the parties!
covid %>% select(FIPS, county=Admin2, state=Province_State, latest_cases=`9/24/20`) %>% #start with covid dataset
  filter(state=="California") %>% 
  slice(1:10) %>% 
  left_join(elections) %>% #join with elections information
  left_join(urbanicity, by=c("FIPS"="fips_code")) #join with population data
# notice that we joined only the rows that we kept in the covid dataset, that is b/c we used left_join
# also notice the output "Joining, by = c("county", "state"), this tells us that county and state are present in both datasets
# ("FIPS" is too, but we had to rename it in urbanicity dataset)

# left_join: keeps all observations from the left-hand side dataset
# right_join: of course keeps all observations from the added (right-hand side) dataset
# inner_join: keeps only rows that have values in both dataframes
# full_join: keeps all rows

## 8. Exercises -------------------------------------------------------------------------------------------------------------

# load dataset with incarceration trends
incarcerations <- here("Data", "incarceration_trends.csv") %>%
  read_csv()
# keep only data for California and only some relevant columns, compute proportion of incarcerated people in total population
unique(incarcerations$state)
ca_jail <- incarcerations %>% filter(state=="CA" & year=="2018") %>% 
  select(fips, total_pop, total_jail_pop) %>% 
  mutate(prop_jail = total_jail_pop/total_pop)
# does a county's political leaning affect its tendency to jail people?
ca_jail <- ca_jail %>% 
  left_join(elections, by= c("fips"="FIPS")) %>% 
  arrange(lean_republican)

# first try: qualitative comparision of 10 least and 10 most republican counties 
overview <- ca_jail %>% slice_head(n=10) %>% 
  bind_rows(slice_tail(ca_jail, n=10))

# second try: (had to look this up, stupid me)
ca_jail_polit <- ca_jail %>% 
  mutate(more_trump=lean_republican >= 1) %>% 
  group_by(more_trump) %>% 
  summarize(mean_prop_jail = mean(prop_jail, na.rm=T), sd_prop_jail=sd(prop_jail, na.rm=T))

# now do this for the whole USA
jail_polit <- incarcerations %>%
  filter(year=="2018") %>%
  select(fips, total_pop, total_jail_pop) %>%
  mutate(prop_jail=total_jail_pop/total_pop) %>% 
  left_join(elections, by=c("fips"="FIPS")) %>% 
  arrange(lean_republican) %>%
  mutate(more_trump = lean_republican >= 1) %>% 
  group_by(more_trump) %>% 
  summarize(mean_prop_jail = mean(prop_jail, na.rm=T), sd_prop_jail = sd(prop_jail, na.rm=T))

jail_polit[is.na(jail_polit$more_trump)==T,]
# aggregate at the state level
jail_polit_states <- incarcerations %>%
  filter(year=="2018") %>%
  select(fips, total_pop, total_jail_pop) %>%
  mutate(prop_jail=total_jail_pop/total_pop) %>% 
  left_join(elections, by=c("fips"="FIPS")) %>%
  arrange(lean_republican) %>%
  mutate(more_trump = lean_republican >= 1) %>% 
  group_by(state, more_trump) %>% 
  summarize(mean_prop_jail = mean(prop_jail, na.rm=T), sd_prop_jail = sd(prop_jail, na.rm=T))

# --> Louisianna has very high rates
elections %>% 
  filter(state=="Louisiana") %>% 
  group_by (state) %>% 
  summarize(lean_republican = mean(lean_republican))

# now do this for the whole USA
jail_polit <- incarcerations %>%
  filter(year=="2018" & state != "LA") %>%
  select(fips, total_pop, total_jail_pop) %>%
  mutate(prop_jail=total_jail_pop/total_pop) %>% 
  left_join(elections, by=c("fips"="FIPS")) %>% 
  arrange(lean_republican) %>%
  mutate(more_trump = lean_republican >= 1) %>% 
  group_by(more_trump) %>% 
  summarize(mean_prop_jail = mean(prop_jail, na.rm=T), sd_prop_jail = sd(prop_jail, na.rm=T))


