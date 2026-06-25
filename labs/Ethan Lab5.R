#install.packages('dplyr')
library(dplyr)



# Import data 
#install.packages('foreign')
library(foreign)
data <- read.dta("https://minusthemath.com/data/nyc_schools.dta")

# View data structure: names and types of variables
str(data)

colSums(is.na(data)) # View number of NAs in each column

head(data) # View of first 6 columns
tail(data) # View of last 6 columns

# Rename variable
data <- data |>
  rename(school = schoolname)

head(data$school) # Look at first 6 schools in dataframe

tail(data$school) # Look at last 6 schools in dataframe


summary(data$overallscore) # Get summary statistics for variable `overallscore`

# Get mean overall score for each school type in dataset
data |>
  group_by(schooltype) |>
  summarize(mean = mean(overallscore, na.rm = TRUE))

data |>
  group_by(schooltype) |>
  filter(!is.na(overallscore)) |> # Remove all columns without data in `overallscore`
  summarize(
    n = n(), # Number of observations by school type
    mean = mean(overallscore, na.rm = TRUE), # Mean score by school type
    sd = sd(overallscore, na.rm = TRUE), # Standard deviation of the score by school type
    min = min(overallscore, na.rm = TRUE), # Minimum score by school type
    max = max(overallscore, na.rm = TRUE)) # Maximum score by school type


# The overall score ranges from 0 to 100 (plus bonus points, which cause some observations to be greater than 100).
# Say we want to rescale the variable so that it ranges from 0 to 1. We simply divide by 100.
# This operation is useful for converting percentages to ratios.
data$overallscore_ratio <- data$overallscore/100

summary(data$overallscore) # Summary of the overall score

summary(data$overallscore_ratio) # Summary of new created ratio

# Note: All of the summary statistics from the original `overall_score` were divided by 100 in the ratio variable


# Generate a dummy (0-1) variable indicating whether or not the school is an elementary school
table(data$schooltype, exclude=NULL)

data <- data |>
  mutate(elementary=ifelse(schooltype=="Elementary" | schooltype=="K-8", 1, 0)) # `|` in an if-else station means "or"

# Now, we do the same thing for middle schools
data <- data |>
  mutate(middle=ifelse(schooltype=="Middle" | schooltype=="K-8", 1, 0))

# To double-check our work, we can look at the cross-tabulations of school type and the new dummy variable
table(data$schooltype, data$elementary, exclude=NULL)
table(data$schooltype, data$middle, exclude=NULL)

data <- data |>
  mutate(elementary = factor(elementary),
         middle = factor(middle)) # Make each of the dummy variables a factor

# Finally, we create a variable to indicate if the letter grade is missing.
data <- data |>
  mutate(grade_missing1=ifelse(overallgrade=="",NA,overallgrade)) |> # NA or the overall grade itself
  mutate(grade_missing2=ifelse(overallgrade=="",1,0)) # Binary indicator if grade is missing


# If we try to look at the summary statistics for the variable blackhispanic, R gives us nothing because
# it is treating the variable as text (as a string variable) since it includes the percentage sign (%).
summary(data$blackhispanic) 
str(data$blackhispanic)

# We need to convert this variable to just numbers.
#install.packages('stringr')
library(stringr)

data$blackhispanic <- gsub("%","",data$blackhispanic) # Finds "%" in the data and removes it
data$blackhispanic <- as.numeric(data$blackhispanic) # Converts the string into a number after the percent sign is removed

names(data$blackhispanic) <- "blackhispanic_per"  ## This is from the previous code, is there a reason we have the names if no visualizations are being made?

str(data$blackhispanic) # See how it is a number variable now?


# If we want to move a set of variables to be the first columns in the dataset, we run the following.
data <- data |> 
  dplyr::select(schooltype, overallgrade, everything()) # "schooltype" and "overallgrade" become the first two columns

# If we want a variable to come after another one, we use the following.
data <- data |>
  dplyr::relocate(district, .after=dbn) # "district" now goes after "dbn"

# Also works with the command .before


# Now, we want to sort the data by the type of school.
data <- data |>
  dplyr::arrange(schooltype)

# If we want to sort by the grade within each school type, 
# we list both variables (with the primary sorting variable listed first).
data <- data |> 
  arrange(schooltype, overallgrade)

data_2 <- data |> ## The group_by was here as well, any reason?
  group_by(district) |>
  arrange(schooltype, overallgrade, .by_group = TRUE) 

# Let's say that we only want to use the elementary schools in our dataset. We can delete the others by
# running the following line of code.
data_elementary <- data |>
  dplyr::filter(elementary == 1) # Searches for where the previously created dummy variable "elementary" is equal to one

# Alternatively, we could have run the following (which does exactly the same thing as the line above in this case).
data_elementary <- data |>
  dplyr::filter(elementary != 0) # Searhes for where the previously created dummy variable "elementary" does not equal zero


# Now, let's create an index of the progress grade and the performance grade. 
# We first convert the grades to numeric variables.
# We assign a score of 5 to schools with an A, 4 for a B, etc.
data_index <- data_elementary |> 
  mutate(progress = case_when( 
          progressgrade == "A" ~ 5, # Left side is the qualifier, right side is the new value
          progressgrade == "B" ~ 4,
          progressgrade == "C" ~ 3,
          progressgrade == "D" ~ 2,
          progressgrade == "F" ~ 1,
          TRUE ~ NA_real_      # Required at end of statement in case there are any columns that do not fit the criteria
    )
  ) |>
  mutate(performance = case_when( # Rinse and repeat for performance!
          performancegrade == "A" ~ 5,
          performancegrade == "B" ~ 4,
          performancegrade == "C" ~ 3,
          performancegrade == "D" ~ 2,
          performancegrade == "F" ~ 1,
          TRUE ~ NA_real_
  )
)
table(data_index$progress, exclude=NULL)
table(data_index$performance, exclude=NULL)

# Now we want to combine the two measures into a single index by adding them, creating a scale ranging from 1 to 10.
data_index <- data_index |>
  mutate(index = progress + performance)

# The variable dbn contains the district, borough, and school number. The first 2 digits are the district number.
# The third digit is the borough. And the fourth through sixth digits are the school number.
data_index$distnum <- substr(data_index$dbn,1,2) 
data_index$distnum <- str_sub(data_index$dbn,1,2) # Can use either function, where the first number is the starting index and the second number is the stopping index

# For the rest of the 
data_index$borough <- str_sub(data_index$dbn,3,3) # Only third index for the borough
data_index$schoolnum <- str_sub(data_index$dbn,4,6) # Fourth through sixth digit for the school number
# cbind!!!

# Note: we do not want to conver these strings to numbers. 
# Take a look at the following school number.
data_index$schoolnum[3]
# 022, if converted to a number, would become 22, and all school numbers are three digits.
# Knowing your data in cleaning and analysis is very important!

# Don't overwrite the original file! Save a new copy in case we realize later that we made a mistake and
# want to go back to the original data.

#save(data_index1, file='data_index1.rda')
# load(file='data_index1.rda') 
readr::write_rds(data_index, "data_index.rds")

rm(list = ls(all.names = TRUE)) # Clear R console

