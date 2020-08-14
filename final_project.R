##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
marinas_directory = "/Users/marina/Documents/DevProjects/R/projects/r_final_harvard_data_science_project"

#specifying not needing to download the .zip file if located on personal computer
if (file.exists(marinas_directory)){
  setwd(marinas_directory)
}


if (file.exists("ml-10m.zip")) {
  
  file.link("./ml-10m.zip", dl)
} else {
  #only download the file if it doesn't exist in the current working directory.
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
}

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

#add in ratings average per movie
rating_avgs <- ratings %>% group_by(movieId) %>% summarise(avg_rating = mean(rating))

ratings <- inner_join(ratings, rating_avgs, by = "movieId")


# add the date field , converting the timestamp to an actual date, and rounding to the nearest day
#use inner_join instead of provided left_join to make sure the movie titles and genres are populated
movielens <- inner_join(ratings, movies, by = "movieId") %>% 
  mutate(rating_date = round_date(as_datetime(timestamp), unit="day"),
         age_of_movie =  year(as_date(Sys.Date())) - release_year,
         rating_year = year(rating_date))

#Before splitting the data into individual rows per genre, I will make some data visualization procedures in order 
#to draw some conclusions. First, there are currently this amount of ratings in the dataset:
nrow(movielens)

#We check to see how many unique movies and users we have in the system
data_summary <- movielens %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

data_summary

#If every user rated every movie, we would have approximately data_summary$n_users X data_summary$n_movies entries in the dataset; however, as shown above, we only have 
#However, the data set is actually less then 1.4% populated, which we can see from the following calculation:
nrow(movielens)/(data_summary$n_users* data_summary$n_movies)

#Visually, we can show the sparseness with the following image graph:
users <- sample(unique(movielens$userId), 100)
rafalib::mypar()
movielens %>% filter(userId %in% users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users", col="blue")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")


#now we plot out a histogram of users to examine how many ratings per user exists in our system 
users <- movielens %>% group_by(userId) %>% summarize(n = n())

#Summarizing the users in the system, we have users with a minimum of 20 reviews; however, the maximum is 7359 reviews per user
users %>% summarise( max(n), min(n))

#Viewing the ratings' density distribution, it is obvious that most of the ratings come from a small percentage of users
users %>%
  ggplot(aes(n)) + 
  geom_density(fill = "blue", color="black") + 
  scale_x_log10() + 
  ggtitle("Users log10 Density Distribution")

#We can also show that each movie is not rated as many times - the popular movies will be seen by many more people and will therefor be rated by many more people
#Meanwhile, there are also many indpeendant films that will only be seen by a few people and therefore will only be rated by a small percentage of those.
movielens %>%  group_by(movieId) %>% summarise(n = n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(fill = "blue", color="black", bins = 40) +
  scale_x_log10() + 
  ggtitle("Movie Ratings With log10 Density Distribution")

bigger_films <- movielens %>% group_by(movieId) %>% summarise(n = n()) %>% filter(n > 20)

#The users set came with a spceifications that only users with 20 or more reviews are included; however, no such specification was done for movies; I believe that movies that have only a few reviews may very well be outliers and so should be excluded from the database.
movielens <- subset(movielens, (movieId %in% bigger_films$movieId))

#In terms of ratings, we can that the ratings are on a scale of 0-5, in increments of 0.5. There are 10 discrete options, and the ratings are not continous.

types_of_ratings <- sort(unique(movielens$rating))
#Graph a bar graph of the distribution of the ratings. Full-grade ratings are much more common then the half-grades
movielens %>% group_by(rating) %>% summarize(count = n()) %>% ggplot(aes(rating, count)) + geom_bar(stat="identity", fill="maroon", color="black") + xlab("Movie Rating") + 
  ylab("Rating Count") + ggtitle("Movie Ratings Summary")

#Plotting release year vs avg movie ratings - it seems movies are either getting worse with time, or the reviewers are getting pickier.
movielens %>%
  ggplot(aes(release_year, avg_rating)) + stat_bin_hex(bins = 100) + scale_fill_distiller(palette = "PuBuGn") +
  stat_smooth(method = "lm", color = "magenta", size = 1) +
  ylab("Average Movie Rating") + xlab("Release Year") + ggtitle("Release Year vs Average Movie Ratings")


ratings_summary_by_movie <- ratings %>% group_by(movieId) %>% summarise(n = n()) 
ratings_summary_by_movie



#separate the genres from the combined values into separate ones
movielens <- movielens %>% separate_rows(genres, sep ="\\|")

#We will now evaluate how many genres are present.
genres <- unique(movielens$genres)
genres

#I would like to examine the patterns of movie reviews by genre; Do people tend to review certain genres more then others?
movies_by_genre <- movielens %>% group_by(genres) %>% summarize(count = n()) %>% arrange(count)
movies_by_genre %>% 
  mutate(genres = factor(genres, levels = unique(as.character(genres)))) %>%
  ggplot(aes(x = genres, y= count, fill=count)) + geom_bar( stat = "identity" ) + scale_y_log10() + coord_flip() +
  ggtitle("Total Reviews By Genre") + theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) 
  
#The answer seems to be a resounding YES! Drama seems to have the most reviews, while IMAX has by far the least; this pattern makes sense, since only a small percentage of movies gets released in IMAX (although the ones that are are super popular, and will thus get more reviews.)


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


#adjusted the RMSE function to account for NA and Null values
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2,  na.rm = TRUE))
}

#splitting the edx data set into testing and training , making sure to exclude the validation data created before.
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.15, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]


#start out slow with the most basic ratings 
mu_hat <- mean(train_set$rating)
mu_hat

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

#adding the basic results to the output table; first try
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)


#mu us the true rating for all movies
#b_i is the average Bias for the movie i - calulcated per movie. no user is used to calculate this yet
#Y_i (calculated rating per movies)
#e_u_i - (independant errors for each movie, per user) - this, in essense is the RMSE
# for this calculation, Y_i = mu + b_i + e_u_i 
#so, to calculate the error, the RMSE, we transform the formula to :
# e_u_i = Y_i - mu - b_i

mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) #here we're calculating the b_i, avg bias per movie 

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(test_set$rating, predicted_ratings)

#get the RMSE results from from the b_i predective model
tmp_rmse_results <- tibble(method = "Just Movie Effect Model b_i average", RMSE = model_1_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)

user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i)) #here, we're calculating the bias per movie, per user


predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, test_set$rating)

tmp_rmse_results <- tibble(method = "User Affect + Movie Effect Model", RMSE = model_2_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)


rm(tmp_rmse_results)


#function that will create the loose labmdas, and then come up with a more detailed lambda evaluation when we have an idea where the lowest value is
find_generic_lambda <- function(seq_start, seq_end, seq_increment, FUN, detailed_flag = FALSE, training_set, testing_set)
{
  lambdas <- seq(seq_start, seq_end, seq_increment)
  print("lambdas")
  print(lambdas)
  RMSE <- sapply(lambdas, FUN)
  #find the smallest lamdba
  print(qplot(lambdas, RMSE))
  #saving the first round lambda 
  min_lambda_first_try <- lambdas[which.min(RMSE)]
  print("rough lamdbda is:")
  print(min_lambda_first_try)
  
  if (detailed_flag)
  {
    #if this is the first iteration of the function, continue with taking a 10% lower and 10% higher lambda value to iterate through new lambdas that are much more granuluar, with increments at 10% of what they were previously.
    new_lambda_range = (seq_end - seq_start)/20
    print("new lamdbda ramge is:")
    print(new_lambda_range)
    min_lambda_first_try <- find_generic_lambda(seq_start = min_lambda_first_try - new_lambda_range, seq_end = min_lambda_first_try + new_lambda_range, 
                                                seq_increment = seq_increment/10, FUN, detailed_flag = FALSE, training_set = training_set, testing_set = testing_set)
  }
  return (min_lambda_first_try)
  
}

#trying regularization next; first need to find the correct lambda - tuning parameter:
#The testing set is very large so if we want to evaluate lambdas, we first run the data set with broad intervals, and then zoom in on the best performing section

regularized_rmse_3 <- function(l, training_set, testing_set)
{
  print(l)
  mu <- mean(training_set$rating)
  just_the_sum <- training_set %>% 
    group_by(movieId) %>% 
    summarize(s = sum(rating - mu), n_i = n())
  
    predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
    
    l_rmse <- RMSE(predicted_ratings, testing_set$rating)
    print(l_rmse)
    return (l_rmse)
}

#testing out the regularization with lamdba - 
rmse3_lambda <- find_generic_lambda(seq_start=0, seq_end=10, seq_increment=0.5, 
                                    FUN= function(x) regularized_rmse_3(x, train_set, test_set ), 
                                    detailed_flag = TRUE, training_set=train_set, testing_set=test_set)

rmse_3 <- regularized_rmse_3(rmse3_lambda, train_set, test_set)


tmp_rmse_results <- tibble(method = "Regularized + Movie Effect", RMSE = rmse_3)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)

# The results from the regularization approach don't seem to be doing any better then the movie affect model, 
#and significantly worse then the user affect + movie effect model. We will now try the regularization option with both movie and user bias effect
#Therefor, we will next attempt to tune the model more with the year parameter

#the lambda needs to be selected using cross-validation, as well. We do this as follows, using the same lambda-creation sequence:
regularized_movie_and_user <- function(l, training_set, testing_set)
{
  
  print(l)
  mu <- mean(training_set$rating)
  
  b_i <- training_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- training_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    testing_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
  
}

rmse4_lambda <- find_generic_lambda(seq_start=0, seq_end=10, seq_increment=0.5, 
                                    FUN= function(x) regularized_movie_and_user(x, training_set=train_set, testing_set=test_set ), 
                                    detailed_flag = TRUE, training_set=train_set, testing_set=test_set)

rmse_4 <- regularized_movie_and_user(rmse4_lambda, train_set, test_set)

tmp_rmse_results <- tibble(method = "Regularized Movie + User Effect Model", RMSE = rmse_4)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)
rmse_results
#But can we do even better?

#Next, I will attempt to add in the year into the mix, testing whether the age of the movie makes a difference
#For this we have created the field "age_of_movie"

regularized_movie_and_user_and_year <- function(l, training_set, testing_set)
{
  mu <- mean(training_set$rating)
  print(l)
  
  b_i <- training_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- training_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- training_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by="userId") %>%
    group_by(age_of_movie) %>%
    summarize( b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings <- 
    testing_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "age_of_movie") %>%
    mutate(pred = mu + b_i + b_u + b_y) %>%
    pull(pred)
 
  rmse <- RMSE(predicted_ratings, testing_set$rating)
  print(rmse) 
  return(rmse)
}
#now we plot the new lambdas vs. RMSE's
#qplot(lambdas, rmses_5, color="red", main="Regularized Lambdas Predicted With User Bias, Movie Bias, and Release Year Bias.")  

#we pick up the most accurate lambda - 
#lambda_5 <- lambdas[which.min(rmses_5)]
#lambda_5


#model_5_rmse <- min(rmses_5)
model_5_lamdba <- find_generic_lambda(seq_start=0, seq_end=10, seq_increment=0.5, 
                                    FUN= function(x) regularized_movie_and_user_and_year(x, training_set=train_set, testing_set=test_set ), 
                                    detailed_flag = TRUE, training_set=train_set, testing_set=test_set)

model_5_rmse <- regularized_movie_and_user_and_year(model_5_lamdba, train_set, test_set)

tmp_rmse_results <- tibble(method = "Regularized Movie + User Effect Model + Year Effect Model", RMSE = model_5_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)

rm(tmp_rmse_results)

#The year seems to have made a difference, but a fairly insignificant one in our model; 
#The next item to be tested is the genre. 


regularized_movie_and_user_and_year_and_genre <- function(l, training_set, testing_set)
{

  mu <- mean(training_set$rating)
  
  b_i <- training_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- training_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- training_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by="userId") %>%
    group_by(age_of_movie) %>%
    summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  
  #adding in the genre bias here
  b_g <- training_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="age_of_movie") %>%
    group_by(genres) %>%
    summarize( b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
  
  
  predicted_ratings <- 
    testing_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "age_of_movie") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
    pull(pred)
  
  rmse <- RMSE(predicted_ratings, testing_set$rating)
  print(rmse)
  return(rmse)
}
#now we plot the new lambdas vs. RMSE's
#qplot(lambdas, rmses_6, color="turquoise", main="Regularized Lambdas Predicted With User Bias, Movie Bias, and Release Year Bias.")  

#we pick up the most accurate lambda - 
#lambda_6 <- lambdas[which.min(rmses_6)]
#lambda_6

model_6_lambda <- find_generic_lambda(seq_start=0, seq_end=10, seq_increment=0.5, 
                                      FUN= function(x) regularized_movie_and_user_and_year_and_genre(x, training_set=train_set, testing_set=test_set ), 
                                      detailed_flag = TRUE, training_set=train_set, testing_set=test_set)

model_6_rmse <- regularized_movie_and_user_and_year_and_genre(model_6_lambda, train_set, test_set)

tmp_rmse_results <- tibble(method = "Regularized Movie + User Effect Model + Year Effect Model + Genre Effect Model", RMSE = model_6_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)

rm(tmp_rmse_results)


final_rmse <- regularized_movie_and_user_and_year(model_6_lambda, train_set, validation)

tmp_rmse_results <- tibble(method = "Final Model Tested on Validation Set", RMSE = final_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)

rm(tmp_rmse_results)
  

# The final output 
rmse_results %>% knitr::kable()




