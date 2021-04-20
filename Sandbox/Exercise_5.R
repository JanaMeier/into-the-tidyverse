
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
# by default, ggplot uses variable names as axis labels. We can also manually change them:
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

# Still pretty messy, let's try to group it with some other variable:

#This is upposed to work in one go, without defining covid_test. But somehow it doesn't, not even when I copy the code from the tutorial
covid_fix <- covid %>% 
  inner_join(elections) %>% 
  mutate(ideology = if_else( # create a dichotome variable that separates states into democrat or republican leaning
    lean_democrat > 1,
    "Democrat-leaning",
    "Republican-leaning")) %>% 
  inner_join(regions)
covid_fix %>%  
  ggplot(mapping = aes(x=date, y=cases, group=state)) + # we need to group by state in order to get a line for every state
  geom_line() +
  facet_wrap(~region + ideology, ncol = 2) # if we don't specify ncol=2, then the plots are arranged differently

# If we don't linke the way the variable names are stacked one on top of the other, we can also try the same with facet_grid:

# covid %>% 
#   inner_join(elections) %>% 
#   mutate(ideology = if else(
#     lean_democrat > 1,
#     "Democrat-leaning",
#     "Republican-leaning")) %>% 
#   inner_join(regions) 
covid_fix %>%  # same problem as earlier, we have to use covid_fix
  ggplot(mapping = aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(rows = vars(region), cols = vars(ideology)) # older spelling: fecet_grid(region ~ ideology)

## 3. Scales and legends ---------------------------------------

# We have used the default settings for the scales, which assume that we want to plot the scales a) continuous on a b) linear scale
# We could also specify this manually and, while we're at it, change the axis title (we could also do this with ylab)

covid %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  scale_y_continuous(name = "Raw covid-19 case counts")

# We might also want to use a logarithmic scale (makes sense when plotting data with a very large range)

covid %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  scale_y_log10(name = "log-transformed covid-19 case counts")

## Other Options:
# Square Root
covid %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  scale_y_sqrt(name = "Square-root-transformed covid-19 case counts")
# Binned, so that it is no longer continuous:
covid %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  scale_y_binned(name = "Binned covid-19 case counts")
# Or reversed
covid %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  scale_y_reverse(name = "Inexplicably-plotted covid-19 case counts")

### 3.1 Discrete color scales ------------------------------------------

# The same is true for other aesthetics: we can use the default settings or change them

# E.g. region is a discrete variable, so ggplot uses a discrete color palette for it:
covid %>% 
  inner_join(regions) %>% 
  ggplot(aes(x=date, y=cases, color=region, group=state)) +
  geom_line() +
  scale_color_discrete(name = "Census-designated region")

# We could use another color palette, that is accessible for users with color blindness and can be transformed to black-and-white: 
# viridis, subpalettes are magma, inferno, plasma, cividis

covid %>% 
  inner_join(regions) %>% 
  ggplot(aes(x=date, y=cases, color=region, group=state)) +
  geom_line() +
  scale_color_viridis_d(name = "Census-designated region",
                        begin = 0.1, end = 0.8, option = "plasma") # the -d specifies that we are using a discrete palette, also, the range is specified

# We can also use color_brewer https://colorbrewer2.org/
covid %>%
  inner_join(regions) %>%
  ggplot(mapping = aes(x=date, y=cases, color=region, group=state)) +
  geom_line() +
  scale_color_brewer(name = "Census-designated region:",
                     palette = "GnBu")

# Or just pick colors manually:
covid %>% 
  inner_join(regions) %>% 
  ggplot(aes(x=date, y=cases, color=region, group=state)) +
  geom_line() +
  scale_color_manual(name = "Census-designated region:",
                     values = c("#3182bd", "#de2d26", "#31a354", "#756bb1"))

### 3.2 Continuous color scales ---------------------------------

# Of course, we can also use continuous color scales

covid %>%
  inner_join(elections) %>%
  filter(state != "District of Columbia") %>% # DC is excluded b/c it leaned democrat so heavily that it makes the plot unreadable
  ggplot(mapping = aes(x=date, y=cases, color=lean_democrat, group=state)) +
  geom_line() +
  scale_color_continuous(name = "Democrat-voting ratio:")

# Here it can be usefull to bin the continuous scale:

covid %>%
  inner_join(elections) %>%
  filter(state != "District of Columbia") %>% # DC is excluded b/c it leaned democrat so heavily that it makes the plot unreadable
  ggplot(mapping = aes(x=date, y=cases, color=lean_democrat, group=state)) +
  geom_line() +
  scale_color_binned(name = "Democrat-voting ratio:")

covid %>%
  inner_join(elections) %>%
  filter(state != "District of Columbia") %>% # DC is excluded b/c it leaned democrat so heavily that it makes the plot unreadable
  ggplot(mapping = aes(x=date, y=cases, color=lean_democrat, group=state)) +
  geom_line() +
  scale_color_viridis_c(name = "Democrat-voting ratio:")

### 3.3 Other aesthetics ---------------------------------------

# The same is true for other aesthetics. All aesthetics are always represented by a scale. Ggplot is usually good at picking a good scale,
# but we can always specify it differently, if we wish

## 4. Coordinates ----------------------------------------------

# When we draw a quare on a balloon and then inflate it, the square doesn't look the same. We have warped the background.
# The same is true for the coordinate system of our plot. We can modify it using scale_y_something and scale_x_something, but sometimes
# we want to change the coordinate system itself

### 4.1 Zooming ------------------------------------------------

# We know that the first covid wave only started in March, so we might want to zoom into our data, plotting only relevant months

covid %>% 
  inner_join(regions) %>% 
  ggplot(aes(x=date, y=cases, color=region, group=state)) +
  geom_line() +
  scale_color_viridis_d(name = "Census-designated region:",
                        begin = 0.1, end = 0.8, option = "plasma") +
  coord_cartesian(xlim = c(mdy("03/01/2020"), mdy("09/24/2020"))) # added argument to default coordinate system "cartesian" to limit x-axis

### 4.2 Aspect ratio -------------------------------------------

# When we don't want the plot to change its form when we change the size of the plot window, we can define a constant aspect ratio

covid %>% 
  inner_join(regions) %>% 
  ggplot(aes(x=date, y=cases, color=region, group=state)) +
  geom_line() +
  scale_color_viridis_d(name = "Census-designated region:",
                        begin = 0.1, end = 0.8, option = "plasma") +
  coord_fixed(ratio = 1/1000)
covid %>% 
  inner_join(regions) %>% 
  ggplot(aes(x=date, y=cases, color=region, group=state)) +
  geom_line() +
  scale_color_viridis_d(name = "Census-designated region:",
                        begin = 0.1, end = 0.8, option = "plasma") +
  coord_fixed(ratio = 1/10000, xlim = c(mdy("03/01/2020"), mdy("09/24/2020")))

### 4.3 Polar coordinates ---------------------------------------

# ggplot by default returns a stacked bar when using a barplot, this makes it easy to illustrate proportions:

covid %>% 
  filter(date== mdy("09/24/2020")) %>% 
  filter(state %in% c("Tennessee", "California", "Rhode Island")) %>% 
  ggplot(aes(x=date, y=cases, fill=state)) +
  geom_bar(stat="identity")

# We can make a pie chart by transforming the cartesian coordinate system to a polar one:
covid %>% 
  filter(date== mdy("09/24/2020")) %>% 
  filter(state %in% c("Tennessee", "California", "Rhode Island")) %>% 
  ggplot(aes(x=date, y=cases, fill=state)) +
  geom_bar(stat="identity") +
  coord_polar(theta = "y")

## 5. Themes ---------------------------------------------------

### 5.1 Pre-made themes ----------------------------------------

# the default theme has a grey background and white lines. We can change that, if we don't like it

#let's save us some typing by creating a dataframe that has the covid data joined with the regions data
covid_regions <- covid %>% 
  inner_join(regions)

# We can also call the default theme, although we don't have to
covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_grey()
## Other themes:
# Black-and-white
covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_bw()
# Dark
covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_dark()
#Light
covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_light()
#Classic
covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_classic()
#Linedraw (ewwww)
covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_linedraw()
#Minimal
covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_minimal()
#Void
covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_void()

### 5.2 Manual adjustments ------------------------------------

# Other changes can be made manually, if we want that

# Change theme, remove grid lines, change titel and legend position
covid_regions %>% 
  ggplot(aes(x=date, y=cases, color=region, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5)
  )

## 6. Miscellany ----------------------------------------------

### 6.1 Position adjustments ----------------------------------

# Barplot with side by side bars, not a stacked bar:
covid %>%
  filter(date == mdy("09/24/2020")) %>%
  filter(state %in% c("Tennessee", "California", "Rhode Island")) %>%
  ggplot(mapping = aes(x=date, y=cases, fill=state)) +
  geom_bar(stat = "identity", position = position_dodge()) # bars "dodge" each other

# This can also help with overlapping datapoints:

overlap <- tibble(
  x = rep(1, times=20),
  y = rep(1, times=20)
)

overlap %>% 
  ggplot(aes(x,y)) +
  geom_point() +
  coord_cartesian(xlim = c(0.95, 1.05))
# all data is compressed in one single dot --> bad

overlap %>% 
  ggplot(aes(x,y)) +
  geom_point(position = position_jitter(width = 0.01, height = 0)) +
  coord_cartesian(xlim = c(0.95, 1.05))

### 6.2. Lists + style consistency ------------------------------

# When a ggplot object is assigned to a variable, it is stored in memory as a list:

my_plot <- covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  geom_line() +
  facet_grid(cols = vars(region))

my_plot

# This means we can create a list that can act as our custom theme when we add a new plot:

theme_jana <- list(
  # start off with the black-and-white theme
  theme_bw(),
  # make adjustments
  theme(
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
)

covid_regions %>% 
  ggplot(aes(x=date, y=cases, group=state)) +
  ggtitle("covid-19 cases over time") +
  geom_line() +
  facet_grid(cols = vars(region)) +
  theme_jana
# or:
my_plot +
  theme_jana

### 6.3. Saving plots -----------------------------------------

# we might want to save plots. We can use ggsave, this will save the last visualization made by ggplot, if not otherwise specified
# It's recommendet to save plots as vectorized images, to avoid information loss or problems with resizing the image
# Vektorizing images can be harder to work with. Vectorized PDF is the happy medium (but I really don't know how to get this into a document).

ggsave(
  filename = here("Output", "ggsave_example.pdf"), #output dir and file name
  plot = my_plot + theme_jana, #input
  width = 8,
  height = 4,
  units = "in",
  dpi = 300, #300 dots per inch is ususally the minimum requirement of publishers
  useDingbats = FALSE #dingbats are fonts with icons instead of characters. They might make it hard to after edit your image
)

## 7. Exercises -----------------------------------------------

# load data
candy <- here("Data", "candyhierarchy2017.csv") %>% 
  read_csv

# tidy up data
candy %>% 
  clean_names() %>% 
  pivot_longer(cols = starts_with("Q6"), names_to("candy"), values_to("rating"))




