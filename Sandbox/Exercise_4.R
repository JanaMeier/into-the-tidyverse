

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

# other geometries:

covid %>% 
  filter(state=="California") %>% 
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_area()
covid %>% 
  filter(state=="California") %>% 
  ggplot(mapping = aes(x=date, y=cases)) +
  geom_point()
covid %>% 
  mutate(time = month(date, label =TRUE)) %>% # lubridate function to extract the month from the date
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_boxplot() +
  ggtitle("covid-19 cases in the USA over time")
covid %>% 
  mutate(time = month(date, label =TRUE)) %>% 
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_violin(scale = "width") + 
  ggtitle("covid-19 cases in the USA over time")
covid %>% 
  mutate(time = month(date, label =TRUE)) %>% 
  filter(time > "Jun") %>% 
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_dotplot(binaxis = "y", stackdir ="center", dotsize = 5, binwidth = 1000) + 
  ggtitle("covid-19 cases in the USA over time")
covid %>% 
  mutate(time = month(date, label =TRUE)) %>% 
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_point() + 
  ggtitle("covid-19 cases in the USA over time")

# all of those have represented bivariate data, now to univariate data:

covid %>% 
  filter(date == as.Date("2020-09-24")) %>% 
  ggplot(mapping = aes(x=cases)) +
  geom_histogram() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")
covid %>% 
  filter(date == as.Date("2020-09-24")) %>% 
  ggplot(mapping = aes(x=cases)) +
  geom_density() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")
covid %>% 
  filter(date == as.Date("2020-09-24")) %>% 
  ggplot(mapping = aes(x=cases)) +
  geom_area(stat = "bin") +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")
covid %>% 
  filter(date == as.Date("2020-09-24")) %>% 
  ggplot(mapping = aes(x=cases)) +
  geom_freqpoly() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")
covid %>% 
  filter(date == as.Date("2020-09-24")) %>% 
  ggplot(mapping = aes(x=cases)) +
  geom_dotplot() +
  ggtitle("Distribution of covid-19 cases in the USA on 09/24/2020")

# This was not so impressive. What makes GGPLOT special?

## 3. Layering! ------------------------------

# A barplot is a nice way to get an overview of data but obscures important information:

covid %>% 
  mutate(time = month(date, label=TRUE)) %>% 
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("covid-19 cases om the USA over time")

# On the other hand, dotplots are nice for showing the shape of a distribution but not good at displaying central tendencies. Solution: take both

covid %>% 
  mutate(time = month(date, label=TRUE)) %>% 
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_bar(stat = "summary", fun = "mean") +
  geom_dotplot(binaxis ="y", stackdir = "center", # define  axis that is divided into bins (default is x) and  direction in which dots are stacked
               dotsize = 2, binwidth = 1000) +
  ggtitle("covid-19 cases om the USA over time")
# Chaning the order of the geometries in the code also changes order of layers in the plot:
covid %>% 
  mutate(time = month(date, label=TRUE)) %>% 
  ggplot(mapping = aes(x=time, y=cases)) +
  geom_dotplot(binaxis ="y", stackdir = "center", # define  axis that is divided into bins (default is x) and  direction in which dots are stacked
               dotsize = 2, binwidth = 1000) +
  geom_bar(stat = "summary", fun = "mean") +
  ggtitle("covid-19 cases om the USA over time")

## 4. Other aesthetics ----------------------------

# Aside from the axes, we can map other aesthetics to the plot like color, fill, alpha (transparency), size, linetype, group...

# Split up the plot in different states:
covid %>% 
  ggplot(mapping = aes(x=date, y=cases, fill=state)) +
  geom_area() +
  theme(legend.position = "bottom")

# Let#s pull another dataset to play with
elections <- here("Data", "countypres_2000-2016.csv") %>% 
  read_csv() %>% 
  filter(year == 2016) %>% 
  filter(party %in% c("democrat", "republican")) %>% # %in% helps filter several options instead of only one
  group_by(state, candidate) %>% 
  summarize(candidatevotes = sum(candidatevotes, na.rm=T)) %>% 
  group_by(state) %>% 
  mutate(lean_democrat = candidatevotes / first(candidatevotes)) %>% 
  filter(candidate == "Hillary Clinton") %>% 
  ungroup() %>% 
  select(state, lean_democrat)

# We want to see whether covid cases differ depending on how Democrat-friendly a state is
covid %>% 
  inner_join(elections) %>% 
  filter(state != "District of Columbia") %>% #exclude Washington DC because they favored Clinton so heavily that it breaks the plot (all other are one color then)
  ggplot(mapping = aes(x=date, y=cases, color=lean_democrat, group=state)) + # we have to group by state to get a single line
  geom_line()

# notice: we used color instead of fill, like in the last plot. Color is for outlines, lines and dots,  fill is for filling in shapes
covid %>% 
  filter(state %in% c("Tennessee", "California", "Rhode Island")) %>% 
  mutate(date = month(date, label=T)) %>% 
  group_by(state, date) %>% 
  summarize(cases=sum(cases)) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases, fill=state)) +
  geom_bar(stat="summary", fun="mean") +
  theme(legend.position="bottom")
# the same plot with color:
covid %>% 
  filter(state %in% c("Tennessee", "California", "Rhode Island")) %>% 
  mutate(date = month(date, label=T)) %>% 
  group_by(state, date) %>% 
  summarize(cases=sum(cases)) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases, color=state)) +
  geom_bar(stat="summary", fun="mean") +
  theme(legend.position="bottom")

## 5. Inheritance and overriding aesthetics ---------

# Usually, every geometry inherits the aesthetic mapping of the initial ggplot call

# Plot covid cases by regions (Northeast, Midwest, South and West)(btw what kind of compass do they use?!)
regions <- here("Data", "state_region_division.csv") %>% 
  read_csv
covid %>% 
  inner_join(regions) %>% 
  group_by(region, date) %>% 
  summarize(cases_mean = mean(cases, na.rm=T),
            cases_sd = sd(cases, na.rm=T),
            cases_n = n(), # gives current group size, works only inside grouping functions
            cases_se = cases_sd/cases_n) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases_mean, color=region)) +
  geom_line() # we don't have to specify the mapping because it is inherited from the ggplot call
# we could also specify the color mapping separately:
covid %>% 
  inner_join(regions) %>% 
  group_by(region, date) %>% 
  summarize(cases_mean = mean(cases, na.rm=T),
            cases_sd = sd(cases, na.rm=T),
            cases_n = n(), # gives current group size, works only inside grouping functions
            cases_se = cases_sd/cases_n) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases_mean)) +
  geom_line(aes(color=region))

# Now we might want to show central tendency and variability of the data
covid %>% 
  inner_join(regions) %>% 
  group_by(region, date) %>% 
  summarize(cases_mean = mean(cases, na.rm=T),
            cases_sd = sd(cases, na.rm=T),
            cases_n = n(), # gives current group size, works only inside grouping functions
            cases_se = cases_sd/cases_n) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases_mean, color=region)) +
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se)) +
  geom_line()

# There is room for improvement in this plot:
covid %>% 
  inner_join(regions) %>% 
  group_by(region, date) %>% 
  summarize(cases_mean = mean(cases, na.rm=T),
            cases_sd = sd(cases, na.rm=T),
            cases_n = n(), # gives current group size, works only inside grouping functions
            cases_se = cases_sd/cases_n) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases_mean, color=region, fill=region)) +
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se),
              alpha=0.25) +
  geom_line(size=1)
# Just for fun let's also use linetype
covid %>% 
  inner_join(regions) %>% 
  group_by(region, date) %>% 
  summarize(cases_mean = mean(cases, na.rm=T),
            cases_sd = sd(cases, na.rm=T),
            cases_n = n(), # gives current group size, works only inside grouping functions
            cases_se = cases_sd/cases_n) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases_mean, color=region, fill=region, linetype=region)) +
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se),
              alpha=0.25) +
  geom_line(size=1)
# We can tell ggplot NOT to inherit the linetype aesthetic for the ribbon geometry
covid %>% 
  inner_join(regions) %>% 
  group_by(region, date) %>% 
  summarize(cases_mean = mean(cases, na.rm=T),
            cases_sd = sd(cases, na.rm=T),
            cases_n = n(), # gives current group size, works only inside grouping functions
            cases_se = cases_sd/cases_n) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases_mean, color=region, fill=region, linetype=region)) +
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se),
              alpha=0.25, linetype="solid") +
  geom_line(size=1)
# Or we could remove the ribbon lines by overriding the color aesthetic:
covid %>% 
  inner_join(regions) %>% 
  group_by(region, date) %>% 
  summarize(cases_mean = mean(cases, na.rm=T),
            cases_sd = sd(cases, na.rm=T),
            cases_n = n(), # gives current group size, works only inside grouping functions
            cases_se = cases_sd/cases_n) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=date, y=cases_mean, color=region, fill=region, linetype=region)) +
  geom_ribbon(aes(ymin=cases_mean-cases_se, ymax=cases_mean+cases_se),
              alpha=0.25, color=NA) +
  geom_line(size=1)

## 6. Exercises ----------------------------

# In the Data folder, use the file nst-est2019-modified.csv to pull in the U.S. Census' most recent population estimates for each state. 
# Then create a new variable that indexes covid counts per 1,000 people. Find a compelling way to plot these data.

census <- here("Data", "nst-est2019-modified.csv") %>% 
  read_csv() %>% 
  select(state="State", pop = "2019")

covid %>% 
  filter(state %in% c("California", "Vermont")) %>% 
  inner_join(census) %>% 
  mutate(case_rate = cases/pop*1000) %>% 
  ggplot(mapping = aes(x=date, y=case_rate, color=state)) +
  geom_line() +
  theme(legend.position="bottom")
covid %>% 
  inner_join(census) %>% 
  inner_join(regions) %>% 
  mutate(case_rate = cases/pop*1000) %>%
  group_by(state) %>% 
  summarize(case_rate = sum(case_rate)) %>% 
  mutate(No = 1) %>% # suuuper umständliche Art, jeder Zeile eine Zeilennummer zuzuordnen, damit die x-Achse übersichtlicher wird
  mutate(No = cumsum(No)) %>% 
  ggplot(mapping = aes(x=No, y=case_rate, fill=state)) +
  geom_col() +
  theme(legend.position="bottom")

# Your friend has a hypothesis that covid counts tend to be especially high in states where there are many incarcerated people. 
# We have previously used the incarceration_trends.csv dataset to examine incarceration trends. Since we haven't yet covered techniques
# for performing statistical analysis, find ways to plot the data in a way that visually tests your friend's hypothesis.

incarceration <- here("Data", "incarceration_trends.csv") %>% 
  read_csv %>% 
  filter(year==2018) %>% 
  select(state, fips, total_pop, total_jail_pop) %>% 
  group_by(fips) %>% 
  summarize(total_pop=sum(total_pop, na.rm=T),
            total_jail_pop = sum(total_jail_pop, na.rm=T))

# we need a common variable to join the datasets, since covid uses full state names and incarcerations uses abbreviations --> fips
covid_2 <- here("Data", "time_series_covid19_confirmed_US.csv") %>%
  read_csv() %>%
  clean_names() %>%
  select(-c(uid:code3, country_region:combined_key)) %>% # drop unnecessary columns
  rename (county = admin2, state = province_state) %>%
  pivot_longer(cols = -c(fips, county, state), names_to = "date", values_to = "cases") %>%
  mutate(date = str_remove(date, "x"),
         date = mdy(date)) # use lubridate to fix dates (after we f*cked them up with clean_names)

covid_2 %>% 
  group_by(state, county, fips) %>% 
  summarize(cases = max(cases)) %>% # Achtung: wenn man die Summe aller Covid-Cases über die Daten nimmt, bekommt man mehr, als die Bevölkerung von Californien ist --> die cases sind wahrscheinlich schon aufsummert! (ist dann auch in ihren Beispielen teilweise falsch)
  ungroup() %>% 
  inner_join(incarceration, by = "fips") %>% 
  group_by(state) %>% 
  summarize(cases = sum(cases),
            total_pop = sum(total_pop),
            total_jail_pop = sum(total_jail_pop)) %>% # get cases per state and date (sum across counties)
  mutate(case_rate = cases/total_pop*1000, jail_rate = total_jail_pop/total_pop*1000) %>% 
  ungroup() %>% 
  ggplot(mapping = aes(x=jail_rate, y=case_rate, color=state)) +
  geom_point()+
  stat_smooth(method = "lm",
              col = "#000000",
              se = FALSE,
              size = 0.5)+
  theme(legend.position = "bottom")
