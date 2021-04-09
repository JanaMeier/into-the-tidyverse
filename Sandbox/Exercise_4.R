

# Into the Tidyverse Tutorial by Jae-Young Son
# Exercise 4: Plotting Data I (ggplot2)



## ggplot and the grammar of graphics

# All plots have an x- and y-axis that require mapping of data to a plotting aesthetic

# Plotting number of covid cases over time:
# We map time to the x-axis and case counts to the y-axis

## 0. Preparation -------------------------------------

library("tidyverse")
library("lubridate")
library("janitor")
library("here")

## 1. get dataset and bring it into a tidy format -----

covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>% 
  read_csv() %>% 
  clean_names() %>% 
  select(-c(uid:fips, country_region:combined_key)) %>% # drop unnecessary columns
  rename (county = admin2, state = province_state) %>%
  pivot_longer(cols = -c(county, state), names_to = "date", values_to = "cases") %>% 
  mutate(date = str_remove(date, "x"),
         date = mdy(date)) %>% # use lubridate to fix dates (after we f*cked them up with clean_names)
  group_by(state, date) %>% 
  summarize(cases = sum(cases)) %>% # get cases per state and date (sum across counties)
  ungroup()

covid %>% 
  slice_head(n=10)

## 2. Now let's try plotting it ----------------------

covid %>% 
  ggplot(mapping = aes(x=date, y=cases))
# well okay the axes are mapped correctly, but the data is still missing ;D
# the reason for this is that we can freely decide in what way we want to represent the data without affecting, what the axes are (time and case count)
# we could choose a bar plot or a line plot or whatever we like. Those forms are calles geometries
covid %>% 
  filter(state == "California") %>% 
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_line()

covid %>%
  filter(state == "California") %>%
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_bar(stat = "identity") 
# we need to specify stat = identity, because geom_bar by default maps the number of cases to bar height. 
# We want it to take the number in the cell itself (=its identity) instead. We could also just use geom_col()



