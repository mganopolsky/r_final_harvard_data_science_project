##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()

download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later
#mutate to add the "year" column, and remove the (year) pattern from the title 

movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres)) %>%
                                           mutate(release_year =  str_extract(title, "(\\(\\d{4}\\)$)") ) %>%
              mutate(release_year = as.numeric(str_remove(str_remove(release_year, "[\\(]"), "[\\)]")) ,
                     title = str_trim(str_remove(title, "\\(\\d{4}\\)$")))

# add the date field , converting the timestamp to an actual date, and rounding to the nearest day
#use inner_join instead of provided left_join to make sure the movie titles and genres are populated
movielens <- inner_join(ratings, movies, by = "movieId") %>% 
  mutate(rating_date = round_date(as_datetime(timestamp), unit="day"),
         age_of_movie =  year(as_date(Sys.Date())) - release_year,
         rating_year = year(rating_date))


#separate the genres from the combined values into separate ones
movielens <- movielens %>% separate_rows(genres, sep ="\\|")

#Before separating the dataset into test, train and validation data, we provide some visualuzation for the report to get a better view of the data


# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#remove all the objects from memory
rm(dl, ratings, movies, test_index, temp, removed) #, movielens, )

#Let's show the movie ratings by genre
#qqplot()


