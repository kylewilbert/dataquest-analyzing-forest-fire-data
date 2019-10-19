library(dplyr)
library(ggplot2)
library(purrr)
library(readr)

forest_fires <- read_csv('forestfires.csv')

# During which months are forest fires most common?
# Mutate, group, and summarize into dataframe needed for visualization
fires_by_month <- forest_fires %>% 
  mutate(month = factor(month, levels = c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"))) %>% 
  group_by(month) %>%
  summarize(total = n())

# create bar chart for months

ggplot(data = fires_by_month) +
  aes(x = month, y = total) +
  geom_bar(stat='identity') +
  labs(title = "Forest Fires, by Month", y = "Number of Fires") +
  theme(panel.background = element_rect(fill='white'))

# On which days of the week are forest fires most common?
# Mutate, group, and summarize into dataframe needed for visualization
fires_by_day <- forest_fires %>% 
  mutate(day = factor(day, levels = c('mon','tue','wed','thu','fri','sat','sun'))) %>% 
  group_by(day) %>%
  summarize(total = n())

# create bar chart for days
ggplot(data = fires_by_day) +
  aes(x = day, y = total) +
  geom_bar(stat='identity') +
  labs(title = "Forest Fires, by Day", y = "Number of Fires") +
  theme(panel.background = element_rect(fill='white'))

# Fires are most common in the Summer months (August and September). 
# And fires occur most often on the weekend days (fri, sat, sun). Most likely this is the result of people spending
# leisure time in the forest on the weekends and during the Summer.

# Create function to generate multiple graphs by month
create_boxplots <- function(x,y) {
  ggplot(data = forest_fires) +
    aes_string(x=x, y=y) +
    geom_boxplot() +
    theme(panel.background = element_rect(fill='white'))
}

# Assign x and y variables
x_var_month <- names(forest_fires)[3] ## month
x_var_day <- names(forest_fires)[4] ## day
y_var <- names(forest_fires)[5:12]

# use the map function to apply the function to the variables
month_box <- map2(x_var_month, y_var, create_boxplots) # visualize by month
day_box <- map2(x_var_day, y_var, create_boxplots) # visualize by day

# Create scatter plots to see which variables may affect forest fire size: 

## write the function 
create_scatterplots = function(x, y) {
  ggplot(data = forest_fires) + 
    aes_string(x = x, y = y) +
    geom_point() +
    theme(panel.background = element_rect(fill = "white"))
}

## Assign x and y variable names 
x_var_scatter <- names(forest_fires)[5:12]
y_var_scatter <- names(forest_fires)[13]
## use the map() function to apply the function to the variables of interest
scatters <- map2(x_var_scatter, y_var_scatter, create_scatterplots)