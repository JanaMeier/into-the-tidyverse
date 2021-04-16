
# Into the Tidyverse Tutorial by Jae-Young Son
# Exercise 5: Plotting Data II (ggplot2)

# Let's give our plots a little more polish so that they are basically publication ready

## 0. Preparation -------------------------------------

library("tidyverse")
library("lubridate")
library("janitor")
library("here")

covid <- here("Data", "time_series_covid19_confirmed_US.csv") %>% 
  read_csv() %>% 
  clean_names() %>% 
  select(-c(uid:fips, country_region:combined_key)) %>% 
  rename(county = admin2, state = province_state) %>% 
  pivot_longer(cols = -c(county, state), names_to = "date", values_to = "cases") %>% 
  mutate(date = str_remove(date, "x"),
         date = mdy(date)) %>% 
  group_by(state, date) %>% 
  summarize(cases = sum(cases)) %>% 
  ungroup()

elections <- here("Data", "countypres_2000-2016.csv") %>% 
  read_csv() %>% 
  filter(year == 2016) %>% 
  filter(party %in% c("democrat", "republican")) %>% 
  group_by(state, candidate) %>% 
  summarize(candidatevotes = sum(candidatevotes, na.rm=T)) %>% 
  group_by(state) %>% 
  mutate(lean_democrat = candidatevotes/first(candidatevotes)) %>% 
  filter(candidate == "Hillary Clinton") %>% 
  ungroup() %>% 
  select(state, lean_democrat)

regions <- here("Data", "state_region_division.csv") %>% 
  read_csv()

## 1. Axis labels -----------------------------------------------

covid %>% 
  ggplot(mapping = aes(x=date, y=cases)) +
  xlab("Time") +
  ylab("Raw case numbers") +
  ggtitle(label="Covid-19 cases over time",
          subtitle = "(not normalized by state population)")

## 2. Facets ---------------------------------------------------

# problem last time: we overplotted. With a line for every single state, it was impossible to pick out a single one

covid %>% 
  ggplot(mapping = aes(x=date, y=cases, color=state)) +
  geom_line() +
  theme(legend.position = "bottom")

# It might be useful to separate the states into their own panels, using facets
# the ~ basically means "by"

covid %>% 
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_line() +
  facet_wrap(~state) +
  theme(legend.position = "bottom")

covid %>% 
  inner_join(elections) %>% 
  mutate(ideology = if_else(
    lean_democrat > 1,
    "Democrat-leaning",
    "Republican-leaning")) %>% 
  inner_join(regions) %>% 
  ggplot(mapping = aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_wrap(~region + ideology, ncol = 2)





