# A4 Data Wrangling

# We provide this line to delete all variables in your workspace.
# This will make it easier to test your script.
rm(list = ls())

# Loading and Exploring Data -------------------------------- (**29 points**)

# First, search online for a dplyr cheatsheet and put the link to one you
# like in the comments here (it's ok if the link makes the line too long):
# - <put the link here>
# https://rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

# To begin, you'll need to download the Kickstarter Projects data from the
# Kaggle website: https://www.kaggle.com/kemical/kickstarter-projects
# Download the `ks-projects-201801.csv` file into a new folder called `data/`

# Load the `dplyr` package
install.packages("dplyr")
library("dplyr")

# If your computer isn't in English, you made need to use this line of code
# to get the csv to load correctly (if the data gets messed up a few rows in):
# Sys.setlocale("LC_ALL", "English")

# Load your data, making sure to not interpret strings as factors.
dataset <- read.csv("data/ks-projects-201801.csv", stringsAsFactors = FALSE)
dataset = tbl_df(dataset)

# To start, write the code to get some basic information about the dataframe:
# - What are the column names?
# - How many rows is the data frame?
# - How many columns are in the data frame?
colnames(dataset)
nrow(dataset)
ncol(dataset)

# Use the `summary` function to get some summary information
summary(dataset)

# Unfortunately, this doesn't give us a great set of insights. Let's write a
# few functions to try and do this better.
# First, let's write a function `get_col_info()` that takes as parameters a
# column name and a dataframe. If the values in the column are *numeric*,
# the function should return a list with the keys:
# - `min`: the minimum value of the column
# - `max`: the maximum value of the column
# - `mean`: the mean value of the column
# If the column is *not* numeric and there are fewer than 10 unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `unique_values`: a vector of each unique value in the column
# If the column is *not* numeric and there are 10 or *more* unique values in
# the column, you should return a list with the keys:
# - `n_values`: the number of unique values in the column
# - `sample_values`: a vector containing a random sample of 10 column values
# Hint: use `typeof()` to determine the column type
get_col_info <- function(col_name, df) {
  column <- df$col_name
  if (typeof(column) == "integer") {
    result <- list(min_value = min(column), max_value = max(column),
                   mean_value = mean(column))
    return(result)
  } else {
      if (typeof(column)) {
        if (n_distinct(column) < 10) {
          result <- list(n_values = n_distinct(column), unique_values =
                           c(distinct(column)))
          return(result)              
        } else {
          result <- list(n_values = n_distinct(column), 
                         sample_values = sample_n(column, 10))
          return(result)
        }
      }
  }
}

# Demonstrate that your function works by passing a column name of your choice
# and the kickstarter data to your function. Store the result in a variable
# with a meaningful name
pledged_info <- get_col_info("pledged", dataset)

# To take this one step further, write a function `get_summary_info()`,
# that takes in a data frame  and returns a *list* of information for each
# column (where the *keys* of the returned list are the column names, and the
# _values_ are the summary information returned by the `get_col_info()` function
# The suggested approach is to use the appropriate `*apply` method to
# do this, though you can write a loop
get_summary_info <- function(df) {
  col_names <- as.list(colnames(df))
  result <- lapply(col_names, get_col_info)
  return(result)
}

# Demonstrate that your function works by passing the kickstarter data
# into it and saving the result in a variable
data_summary <- summary(dataset)

# Take note of 3 observations that you find interesting from this summary
# information (and/or questions that arise that want to investigate further)
# YOUR COMMENTS HERE
# LIKELY ON MULTIPLE LINES

# Asking questions of the data ----------------------------- (**29 points**)

# Write the appropriate dplyr code to answer each one of the following questions
# Make sure to return (only) the desired value of interest (e.g., use `pull()`)
# Store the result of each question in a variable with a clear + expressive name
# If there are multiple observations that meet each condition, the results
# can be in a vector. Make sure to *handle NA values* throughout!
# You should answer each question using a single statement with multiple pipe
# operations!
# Note: For questions about goals and pledged, use the usd_pledged_real
# and the usd_goal_real columns, since they standardize the currancy.


# What was the name of the project(s) with the highest goal?
highest_goal <- dataset %>% filter(goal == max(goal)) %>% pull(name)

# What was the category of the project(s) with the lowest goal?
lowest_goal_category <- dataset %>% filter(goal == min(goal)) %>% pull(category)

# How many projects had a deadline in 2018?
# Hint: start by googling "r get year from date" and then look up more about
# different functions you find
num_2018 <- nrow(dataset %>% filter(substring(deadline, 1, 4) == "2018"))

# What proportion of projects weren't marked successful (e.g., failed or live)?
# Your result can be a decimal
num_failed <- nrow(dataset %>% filter(state == "failed"))
proportion_failed <- num_failed / nrow(dataset)

# What was the amount pledged for the project with the most backers?
max_backers_pledged <- dataset %>% filter(backers == max(backers)) %>%
                        pull(pledged)

# Of all of the projects that *failed*, what was the name of the project with
# the highest amount of money pledged?
highest_pledged_failed <- dataset %>% filter(state == "failed") %>%
                          filter(pledged == max(pledged)) %>% pull(name)

# How much total money was pledged to projects that weren't marked successful?
total_pledged_failed <- sum(dataset %>% filter(state == "failed") %>% select(pledged))

# Performing analysis by *grouped* observations ----------------- (31 Points)

# Which category had the most money pledged (total)?
categories <- group_by(dataset, main_category)
most_pledged_category <- categories %>% filter(pledged == max(pledged)) %>%
                          pull(main_category)

# Which country had the most backers?
most_backed_country <- dataset %>% filter(backers == max(backers)) %>% pull(country)

# Which year had the most money pledged (hint: you may have to create a new
# column)?
# Note: To answer this question you can choose to get the year from either
# deadline or launched dates.
dataset <- mutate(dataset, year = as.integer(substring(launched, 1, 4)))
most_pledged_year <- dataset %>% filter(pledged == max(pledged)) %>% pull(year)

# Write one sentance below on why you chose deadline or launched dates to
# get the year from:
# The money is pledged upon the project's launch, so it would make sense
# to use the launch date as the parameter.

# What were the top 3 main categories in 2018 (as ranked by number of backers)?
top_3_2018 <- dataset %>% filter(year == 2018) %>% select(category) %>% top_n(3)

# What was the most common day of the week on which to launch a project?
# (return the name of the day, e.g. "Sunday", "Monday"....)
dataset <- mutate(dataset, day = weekdays(as.Date(launched)))
day_counts <- tbl_df(data.frame(dataset %>% count(day)))
most_common_day <- day_counts %>% filter(n == max(n)) %>% pull(day)

# What was the least successful day on which to launch a project? In other
# words, which day had the lowest success rate (lowest proportion of projects
# that were marked successful )? This might require creative problem solving...
# Hint: Try googling "r summarize with condition in dplyr"
dataset <- dataset %>% group_by(day) %>% summarize(success_rate = 
                                                     nrow(state == "successful") / nrow(dataset))
