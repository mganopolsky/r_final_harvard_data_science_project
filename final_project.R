##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################
## ---- libraries --------
# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(hexbin)) install.packages("hexbin", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(cowplot)) install.packages("cowplot", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(here)) install.packages("here", repos = "http://cran.us.r-project.org")
if(!require(formatR)) install.packages("formatR", repos = "http://cran.us.r-project.org")
if(!require(archivist)) install.packages("archivist", repos = "http://cran.us.r-project.org")
if(!require(devtools)) install.packages("devtools", repos = "http://cran.us.r-project.org")


library(tidyverse)
library(caret)
library(data.table)
library(stringr)
library(lubridate)
library(ggrepel)
library(ggplot2)
library(cowplot)
library(dplyr)
library(here)
library(formatR)
library(archivist)
library(devtools)


# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
marinas_directory <- "/Users/marina/Documents/DevProjects/R/projects/r_final_harvard_data_science_project"


## ---- dataset --------
#specifying not needing to download the .zip file if located on personal computer
if (file.exists(marinas_directory)){
  setwd(marinas_directory)
} 
repo <- getwd()

createLocalRepo(repoDir = repo, default = TRUE, force=TRUE)


if (file.exists("ml-10m.zip")) {
  
  file.link("./ml-10m.zip", dl)
} else {
  #only download the file if it doesn't exist in the current working directory.
  download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)
}

#downloading the ratings info
ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

#downloading the movie info
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")


#mutate the movies dataframe to add the "release_year" column, format it as a numeric, and remove the (year) pattern from the title 
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres)) %>%
  mutate(release_year =  str_extract(title, "(\\(\\d{4}\\)$)") ) %>%
  mutate(release_year = as.numeric(str_remove(str_remove(release_year, "[\\(]"), "[\\)]")) ,
         title = str_trim(str_remove(title, "\\(\\d{4}\\)$")))

#add in ratings average and ratings_count per movie, into the dataset
rating_avgs <- ratings %>% group_by(movieId) %>% summarise(avg_rating = mean(rating), rating_count = n())
archivist::saveToLocalRepo(rating_avgs, repoDir = repo)
ratings <- inner_join(ratings, rating_avgs, by = "movieId")

# add the rating date field , converting the timestamp to an actual date, and rounding to the nearest day
#adding the rating year field for possible use
#use inner_join instead of provided left_join to make sure the movie titles and genres are populated
movielens <- inner_join(ratings, movies, by = "movieId") %>% 
  mutate(rating_date = round_date(as_datetime(timestamp), unit="day"),
         age_of_movie =  year(as_date(Sys.Date())) - release_year,
         rating_year = year(rating_date))

#As per the documentation of the ```createDataPartition {caret} ``` function, items with 3 counts or less won't show up in both testing and training data.
#so we're going to remove the movies that have 3 or less reviews.
movielens <- movielens %>% filter(rating_count > 3)

saveToLocalRepo(movielens, repoDir = repo)

#Are there any missing values? 
## ---- summary_movielens --------
summary(movielens)

#Before splitting the data into individual rows per genre, I will make some data visualization procedures in order 
#to draw some conclusions. First, there are currently this amount of ratings in the dataset:
#nrow(movielens)

## ---- summary_data_summary_movielens --------
#We check to see how many unique movies and unique users we have in the system
data_summary <- movielens %>% summarize(n_users = n_distinct(userId), n_movies = n_distinct(movieId))

data_summary
saveToLocalRepo(data_summary, repoDir = repo)

#If every user rated every movie, we would have approximately data_summary$n_users X data_summary$n_movies entries in the dataset; 
#however, as shown above, we only have nrow(movielens) ratings.
#However, the data set is actually less then 1.4% populated, which we can see from the following calculation:
nrow(movielens)/(data_summary$n_users* data_summary$n_movies)

#Visually, we can show the sparseness on a small sample of the data, with the following image graph:
## ---- sparse_summary --------
sample_users <- sample(unique(movielens$userId), 100)
saveToLocalRepo(sample_users, repoDir = repo)

## ---- sparse_plot --------
rafalib::mypar()
movielens %>% filter(userId %in% sample_users) %>% 
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , 
        xlab="Movies", ylab="Users", col="blue", 
        main="Sparsely Populated Ratings Sample, Movies vs. Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey") 


## ---- users_summary --------
#now we plot out a histogram of users to examine how many ratings per user exists in our system 
users <- movielens %>% group_by(userId) %>% summarize(n = n(), avg_user_rating = mean(rating))
#Summarizing the users in the system, we have users with a minimum of 20 reviews; however, the maximum is over 7000 reviews per user
saveToLocalRepo(users, repoDir = repo)
users_summary <- users %>% summarise( max(n), min(n))
saveToLocalRepo(users_summary, repoDir = repo)

## ---- user_density_distribution --------
#Viewing the ratings' density distribution, it is obvious that most of the ratings come from a small percentage of users
users %>%
  ggplot(aes(n)) + 
  geom_density(fill = "blue", color="black") + 
  scale_x_log10() + xlab("Long 10 User Count") +
  ggtitle("Users log10 Density Distribution")

## ---- users_4_graphs --------
#Variability in all reviews 
gu_1 <- users %>%
  ggplot(aes(n, avg_user_rating)) + 
  geom_point(color="blue", alpha=.3) + 
  xlab("Ratings Per User") +ylab("Avg Rating") + geom_smooth(color='red') +
  ggtitle("Ratings Count vs. Mean Rating") + scale_y_continuous(limits = c(0, 5)) +
  theme(legend.position = "none", plot.title = element_text(size = 11, face="bold")) 

gu_2 <- users %>% filter(n > 50) %>%
  ggplot(aes(n, avg_user_rating)) + 
  geom_point(color="blue", alpha=.3) + 
  xlab("Ratings Per User") +ylab("Avg Rating") + geom_smooth(color='red') +
  ggtitle("Ratings Count > 50 vs. Mean Rating")+ scale_y_continuous(limits = c(0, 5)) +
  theme(legend.position = "none", plot.title = element_text(size = 11, face="bold")) 

gu_3 <- users %>% filter(n > 100) %>%
  ggplot(aes(n, avg_user_rating)) + 
  geom_point(color="blue", alpha=.3) + 
  xlab("Ratings Per User") +ylab("Avg Rating") + geom_smooth(color='red') +
  ggtitle("Ratings Count > 100 vs. Mean Rating")+ scale_y_continuous(limits = c(0, 5)) +
  theme(legend.position = "none", plot.title = element_text(size = 11, face="bold")) 

gu_4 <- users %>% filter(n > 200) %>%
  ggplot(aes(n, avg_user_rating)) + 
  geom_point(color="blue", alpha=.3) + 
  xlab("Ratings Per User") +ylab("Avg Rating") + geom_smooth(color='red') +
  ggtitle("Ratings Count > 200 vs. Mean Rating")+ scale_y_continuous(limits = c(0, 5)) +
  theme(legend.position = "none", plot.title = element_text(size = 11, face="bold")) 


plot_grid(gu_1, gu_2, gu_3, gu_4)


#We can also show that each movie is not rated as many times - the popular movies will be seen by many more people and will therefor be rated by many more people
#Meanwhile, there are also many independant films that will only be seen by a few people and therefore will only be rated by a small percentage of those.
## ---- movie_histogram --------

movielens %>%  group_by(movieId) %>% summarise(n = n()) %>%
  ggplot(aes(n)) + 
  geom_histogram(fill = "blue", color="black", bins = 40) +
  scale_x_log10() + xlab("Log10 Rating Count Per Movie") +
  ggtitle("Movie Ratings With log10 Density Distribution")


## ---- ratings_4_graphs_preface --------
####################
#New evaluations since last version
# The variability in ratings is much larger when the movie only has very few reviews. This is evident when the amount of a film's 
#reviews is plotted against the film's mean ratings:

#ratings for all movies
## ---- ratings_4_graphs --------
g1 <- rating_avgs %>% ggplot(aes(rating_count, avg_rating)) + geom_point(color='blue', alpha=.5) + 
  geom_smooth(aes(color='red')) + xlab("Cumulative Ratings Per Movie") + 
  ylab("Average Movie Rating") + ggtitle("Ratings Variability") + 
  scale_y_continuous(limits = c(0, 5)) + 
  theme(legend.position = "none", plot.title = element_text(size = 11, face="bold")) 

g2 <- rating_avgs %>% filter(rating_count > 100) %>%
  ggplot(aes(rating_count, avg_rating)) + geom_point(color='blue', alpha=.5) + 
  geom_smooth(aes(color='red')) + xlab("Cumulative Ratings Per Movie") + 
  ylab("Average Movie Rating") + ggtitle("Ratings Variability") + 
  ggtitle("Ratings Variability in films with > 100 ratings") +
  scale_y_continuous(limits = c(0, 5))+ 
  theme(legend.position = "none", plot.title = element_text(size = 11, face="bold")) 

g3 <- rating_avgs %>% filter(rating_count > 200) %>%
  ggplot(aes(rating_count, avg_rating)) + geom_point(color='blue', alpha=.5) + 
  geom_smooth(aes(color='red')) + xlab("Cumulative Ratings Per Movie") + 
  ylab("Average Movie Rating") + ggtitle("Ratings Variability") + 
  ggtitle("Ratings Variability in films with > 200 ratings") +
  scale_y_continuous(limits = c(0, 5))+ 
  theme(legend.position = "none", plot.title = element_text(size = 11, face="bold")) 

g4 <- rating_avgs %>% filter(rating_count > 300) %>%
  ggplot(aes(rating_count, avg_rating)) + geom_point(color='blue', alpha=.5) + 
  geom_smooth(aes(color='red')) + xlab("Cumulative Ratings Per Movie") + 
  ylab("Average Movie Rating") + ggtitle("Ratings Variability") + 
  ggtitle("Ratings Variability in films with > 300 ratings") +
  scale_y_continuous(limits = c(0, 5))+ 
  theme(legend.position = "none", plot.title = element_text(size = 11, face="bold")) 


plot_grid(g1, g2, g3, g4)


#cumulative ratings per genre, once separated
## ---- cumulative_ratings_separated --------
movie_ratings_variability_facet <- movielens %>% distinct(movieId, avg_rating, rating_count, genres) %>%
  separate_rows(genres, sep = "\\|") %>%
  ggplot(aes(x = rating_count, y = avg_rating)) + 
  geom_point(alpha=.5, color="blue") + geom_smooth(color='red') +
  xlab("Cumulative Ratings Per Movie") + ylab("Average Movie Rating") + 
  ggtitle("Movie Ratings Variability") + facet_wrap(~genres) 

#saveToLocalRepo(movie_ratings_variability_facet, repoDir = repo)
####################

## ---- ratings_distributions --------
#Next we examine the ratings system. In terms of ratings, we can that the ratings are on a scale of 0-5, in increments of 0.5. There are 10 discrete options, and the ratings are not continous.
types_of_ratings <- sort(unique(movielens$rating))


movielens %>%
  ggplot(aes(rating)) +  xlab("Movie Rating") + ylab("Cumulative Rating Count") +
  geom_histogram(binwidth = 0.25, fill = "blue") +
  scale_y_continuous(breaks = c(seq(0, 3000000, 500000))) +
  ggtitle("Votes By Volume, Per Discrete Rating")


#From the data and the bar graph of the ratings, we can make one 2 conclusions: 
#(1) In terms of ratings, we can that the ratings are on a scale of 0.5-5, in increments of 0.5. There are 10 discrete options, and the ratings are not continous.
#(2) Full-grade ratings are much more common then the half-grades

#Plotting release year vs avg movie ratings - it seems movies are either getting worse with time, or the reviewers are getting pickier.
## ---- rls_yar_vs_avg --------
movielens %>%
  ggplot(aes(release_year, avg_rating)) + 
  stat_bin_hex(bins = 100) + 
  scale_fill_distiller(palette = "PuBuGn") +
  stat_smooth(method = "lm", color = "magenta", size = 1) +
  ylab("Average Movie Rating") + xlab("Release Year") + 
  ggtitle("Release Year vs Average Movie Ratings")


#next, we'll explore how movies have faired over time by Genre.
#Based on the information, it is fairly obvious that most of the average ratings across genres have declined over the years. The notable exception to this is IMAX movies, and in a small way, animation. These trends make sense, as both IMAX and animated films have benefited greatly from technological advancements over the years.
#The notable exception to those are IMAX mocies 
## ---- genres_ratings_facet --------
movielens %>% na.omit() %>% separate_rows(genres, sep = "\\|") %>%
  ggplot(aes(release_year, avg_rating)) + stat_bin_hex(bins = 100) + #scale_fill_distiller(palette = "PuBuGn") +
  stat_smooth(method = "lm", color = "magenta", size = 1) +
  ylab("Average Movie Rating") + xlab("Release Year") + ggtitle("Release Year vs Average Movie Ratings") +
  facet_wrap(~genres) + theme(axis.text.x = element_text(angle = 90))     



## ---- print_genres_totals --------
#I would like to examine the patterns of movie reviews by genre; Do people tend to review certain genres more then others?
movies_by_genre <- movielens %>% separate_rows(genres, sep ="\\|") %>% group_by(genres) %>% 
  summarize(count = n(), avg_rating = mean(rating)) %>% arrange(count)

saveToLocalRepo(movies_by_genre, repoDir = repo)

## ---- print_genres_text --------
print(unique(movies_by_genre$genres))


## ---- print_movie_count_by_genre --------
movies_by_genre %>% 
  mutate(genres = factor(genres, levels = unique(as.character(genres)))) %>%
  ggplot(aes(x = genres, y= count, fill=count)) + geom_bar( stat = "identity" ) +  
  scale_y_continuous(n.breaks = 5, breaks = c(0,  1000000, 2000000, 3000000, 4000000), labels = c("0", "1,000,000", "2,000,000", "3,000,000", "4,000,000")) +
  coord_flip() +
  ggtitle("Total Reviews By Genre") + theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) 



#The answer seems to be a resounding YES! Drama seems to have the most reviews, while IMAX has by far the least; this pattern makes sense, since only a small percentage of movies gets released in IMAX (although the ones that are are super popular, and will thus get more reviews.)

## ---- avg_rating_by_genre_plots --------
#ratings_summary_by_genre <- movielens %>% separate_rows(genres, sep ="\\|") %>% group_by(genres) %>% summarise(avg_rating = mean(rating)) 
#ratings_summary_by_genre %>% ggplot(aes(genres, avg_rating, size = 3, col=genres)) + 
movies_by_genre %>% ggplot(aes(genres, avg_rating, size = 3, col=genres)) + 
    geom_point() +   theme(axis.title.x=element_blank(), legend.title = element_blank(),
                         axis.text.x=element_blank(), legend.key= element_blank(),
                         axis.ticks.x=element_blank(), legend.text = element_blank(), legend.position="none") +
  geom_label_repel(aes(label = genres),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = "blue") + ggtitle("Average Rating By Genre") + ylab("Average Rating")

#saveToLocalRepo(avg_rating_by_genre_plot, repoDir = repo)

# Validation set will be 10% of MovieLens data
## ---- data_setup_test_train_validation --------
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")


saveToLocalRepo(validation, repoDir = repo)

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

#TODO: remove the movielens object as well 
#remove all the objects from memory
#rm(dl, ratings, movies, test_index, temp, removed, movielens )


## ---- rmse_formula --------
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

## ---- test_train_break --------
#splitting the edx data set into testing and training , making sure to exclude the validation data created before.
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.1, list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

saveToLocalRepo(train_set, repoDir = repo)
saveToLocalRepo(test_set, repoDir = repo)

## ---- mu_hat --------
#start out slow with the most basic ratings 
mu_hat <- mean(train_set$rating)
mu_hat

#we will run the RMSE algorithm on the test set, and compare it with the mean of the ratings. This is the most basic of predictions, and will give us a baseline to beat.
naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

saveToLocalRepo(naive_rmse, repoDir = repo)

#adding the basic results to the output table; first try
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)


## ---- naive_rmse_printout --------
rmse_results_output_0 <- rmse_results %>% knitr::kable(row.names=TRUE)
rmse_results_output_0
saveToLocalRepo(rmse_results_output_0, repoDir = repo)

#TODO : continue RMD from here

#mu us the true rating for all movies
#b_i is the average Bias for the movie i - calulcated per movie. no user is used to calculate this yet
#Y_i (calculated rating per movies)
#e_u_i - (independant errors for each movie, per user) - this, in essense is the RMSE
# for this calculation, Y_i = mu + b_i + e_u_i 
#so, to calculate the error, the RMSE, we transform the formula to :
# e_u_i = Y_i - mu - b_i

## ---- minus_mu_minus_bi --------
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu)) #here we're calculating the b_i, avg bias per movie 

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(test_set$rating, predicted_ratings)
saveToLocalRepo(model_1_rmse, repoDir = repo)
#get the RMSE results from from the b_i predective model
rmse_results_output_1 <- tibble(method = "Just Movie Effect Model b_i average", RMSE = model_1_rmse)
saveToLocalRepo(rmse_results_output_1, repoDir = repo)
#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, rmse_results_output_1)
rmse_results %>% knitr::kable(row.names=TRUE)

## ---- minus_mu_minus_bi_minus_bu --------
#Now we will add in the user bias, b_u
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
saveToLocalRepo(model_2_rmse, repoDir = repo)
rmse_results_output_2 <- tibble(method = "User Affect + Movie Effect Model", RMSE = model_2_rmse)
saveToLocalRepo(rmse_results_output_2, repoDir = repo)
#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, rmse_results_output_2)
rmse_results %>% knitr::kable(row.names=TRUE)

#Next we will attempt to improve the algorithms with regularization.
#For this purpose, we will calculate a range of lambda values, and pick the smallest one in order to calculate the lowest RMSE

#This is a function that will create the loose labmdas, by using large intervals, and then come up with a more detailed lambda evaluation when we have an idea where the lowest value is

#The function below will be used by every regularized algorithm; it will take in the following parameters :
#seq_start - start of the lambda range
#seq_end - end of the lambda range
#seq_increment - the increments to use in order to traverse the lambda range
#FUN - function passed in as a parameter (this is how find_generic_lambda is able to be used in every regularized algorithm calculation)
#detailed_flag - a boolean flag that will indicate whether a recursive call will be needed to find more granular lambdas. 
# training_set, testing_set - the data sets passed in to evaluate the testing set 
#The function is meant to be called exactly twice - once to find the rough range of the lambda, and once again to find a more granular detailed lambda.
#The recursive call is set up in order to optimize performance - if we were to run the algorithm with the final granularity, it will be an order of magnitude slower.

## ---- find_generic_lambda --------
find_generic_lambda <- function(seq_start, seq_end, seq_increment, FUN, 
        detailed_flag = FALSE, training_set, testing_set, plot_title="")
{
  lambdas <- seq(seq_start, seq_end, seq_increment)
  #calculate the RMSEs for the FUN - regularized function passed in - 
    #with the lambda range created above
  RMSE <- sapply(lambdas, FUN)
  print(RMSE)
  #plot the output to see the smallest lamdba
  plot <- qplot(lambdas, RMSE, main=plot_title)
  print(plot)
  saveToLocalRepo(plot, repoDir = repo, userTags = c(plot_title))
  #saving the first round lambda - the lowest so far
  min_lambda_first_try <- lambdas[which.min(RMSE)]
  
  if (detailed_flag)
  {
    #if this is the first iteration of the function, continue with taking a 
    #10% lower and 10% higher lambda value to iterate through new lambdas 
    #that are much more granuluar, with increments at 10% of what they were previously.
    new_lambda_range = (seq_end - seq_start)/40
    min_lambda_first_try <- find_generic_lambda(
     seq_start = min_lambda_first_try - new_lambda_range, 
     seq_end = min_lambda_first_try + new_lambda_range, 
     seq_increment = seq_increment/10, FUN, detailed_flag = FALSE, 
      training_set = training_set,  testing_set = testing_set, 
     plot_title=paste("Narrowed :" , plot_title, sep=" "))
  }
  return (min_lambda_first_try)
}

## ---- regularized_rmse_3 --------
#trying regularization next; first need to find the correct lambda - 
#tuning parameter: The testing set is very large so if we want to evaluate 
#lambdas, we first run the data set with broad intervals, and then 
#zoom in on the best performing section
regularized_rmse_3 <- function(l, training_set, testing_set)
{
  #print(l)
  mu <- mean(training_set$rating)
  just_the_sum <- training_set %>% 
    group_by(movieId) %>% 
    summarize(s = sum(rating - mu), n_i = n())
  
  predicted_ratings <- testing_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  l_rmse <- RMSE(predicted_ratings, testing_set$rating)
  #print(l_rmse)
  return (l_rmse)
}

## ---- regularized_rmse_3_calcs --------
#testing out the regularization with lamdba - 
rmse3_lambda <- find_generic_lambda(seq_start=0, seq_end=10, seq_increment=0.5, 
                                    FUN= function(x) regularized_rmse_3(x, train_set, test_set ), 
                                    detailed_flag = TRUE, training_set=train_set, 
                                    testing_set=test_set, 
                                    plot_title = "Testing Lambdas for Movie Affect")
saveToLocalRepo(rmse3_lambda, repoDir = repo)
rmse_3 <- regularized_rmse_3(rmse3_lambda, train_set, test_set)
saveToLocalRepo(rmse_3, repoDir = repo)

rmse_results_output_3 <- tibble(method = "Regularized + Movie Effect", RMSE = rmse_3)
saveToLocalRepo(rmse_results_output_3, repoDir = repo)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, rmse_results_output_3)
rmse_results %>% knitr::kable(row.names=TRUE)


# The results from the regularization approach don't seem to be doing any better then the movie affect model, 
#and significantly worse then the user affect + movie effect model. We will now try the regularization option with both movie and user bias effect
#Therefor, we will next attempt to tune the model more with the year parameter

## ---- regularized_movie_and_user --------
#the lambda needs to be selected using cross-validation, as well. 
#We do this as follows, using the same lambda-creation sequence:
regularized_movie_and_user <- function(l, training_set, testing_set)
{
  
  #print(l)
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
  
  return(RMSE(predicted_ratings, testing_set$rating))
  
}

## ---- regularized_movie_and_user_evals --------

rmse4_lambda <- find_generic_lambda(seq_start=0, seq_end=10, seq_increment=0.5, 
            FUN= function(x)  
              regularized_movie_and_user(x, training_set=train_set, testing_set=test_set ), 
            detailed_flag = TRUE, training_set=train_set, testing_set=test_set, 
            plot_title = "Testing Lambdas for Movie Affect and User Bias Effect")
saveToLocalRepo(rmse4_lambda, repoDir = repo)
rmse_4 <- regularized_movie_and_user(rmse4_lambda, train_set, test_set)
saveToLocalRepo(rmse_4, repoDir = repo)
tmp_rmse_results_4 <- tibble(method = "Regularized Movie + User Effect Model", RMSE = rmse_4)
saveToLocalRepo(tmp_rmse_results_4, repoDir = repo)
#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results_4)
rmse_results %>% knitr::kable(row.names=TRUE)
#But can we do even better?

## ---- regularized_movie_and_user_and_year --------
#Next, I will attempt to add in the year into the mix, 
#testing whether the age of the movie makes a difference
#For this we have created the field "age_of_movie"

regularized_movie_and_user_and_year <- 
  function(l, training_set, testing_set)
{
  mu <- mean(training_set$rating)
  #print(l)
  
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
  return(rmse)
}

## ---- regularized_movie_and_user_and_year_eval --------

model_5_lamdba <- find_generic_lambda(seq_start=0, seq_end=10, seq_increment=0.5, 
                      FUN= function(x) regularized_movie_and_user_and_year(x, 
                      training_set=train_set, testing_set=test_set ), 
                      detailed_flag = TRUE, training_set=train_set, 
                      testing_set=test_set, plot_title = 
                        "Testing Lambdas for Movie Affect, User Bias Effect and Film Age Affect")
saveToLocalRepo(model_5_lamdba, repoDir = repo)
model_5_rmse <- regularized_movie_and_user_and_year(model_5_lamdba, train_set, test_set)
saveToLocalRepo(model_5_rmse, repoDir = repo)
rmse_results_output_5 <- tibble(method = "Regularized Movie + User Effect Model + Year Effect Model", 
                                RMSE = model_5_rmse)
saveToLocalRepo(rmse_results_output_5, repoDir = repo)
#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, rmse_results_output_5)
rmse_results %>% knitr::kable(row.names=TRUE)
#rm(tmp_rmse_results)

#The year seems to have made a difference, but a fairly insignificant one in our model; 
#The next item to be tested is the genre. 

## ---- regularized_movie_and_user_and_year_and_genre --------
regularized_movie_and_user_and_year_and_genre <- 
  function(l, training_set, testing_set)
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
  return(rmse)
}

## ---- regularized_movie_and_user_and_year_and_genre_eval --------
model_6_lambda <- find_generic_lambda(seq_start=0, seq_end=10, seq_increment=0.5, 
          FUN= function(x) regularized_movie_and_user_and_year_and_genre(x, 
          training_set=train_set, testing_set=test_set ), 
          detailed_flag = TRUE, training_set=train_set, testing_set=test_set, 
          plot_title = "Testing Lambdas for Movie Affect, User Bias , Film Age And Genre Effect")
saveToLocalRepo(model_6_lambda, repoDir = repo)
model_6_rmse <- regularized_movie_and_user_and_year_and_genre(model_6_lambda, train_set, test_set)
saveToLocalRepo(model_6_rmse, repoDir = repo)
rmse_results_output_6 <- tibble(method = "Regularized Movie + User Effect Model + Year Effect Model + Genre Effect Model", RMSE = model_6_rmse)
saveToLocalRepo(rmse_results_output_6, repoDir = repo)
#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, rmse_results_output_6)
rmse_results %>% knitr::kable(row.names=TRUE)


## ---- regularized_movie_and_user_and_year_and_genre_validation --------
final_rmse <- regularized_movie_and_user_and_year_and_genre(model_6_lambda, 
                                              train_set, validation)
saveToLocalRepo(final_rmse, repoDir = repo)


final_rmse_results <- tibble(method = "Final Model Tested on Validation Set", RMSE = final_rmse)
saveToLocalRepo(final_rmse_results, repoDir = repo)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, final_rmse_results)
saveToLocalRepo(rmse_results, repoDir = repo)

# The final output 
## ---- regularized_movie_and_user_and_year_and_genre_validation_results_final_ouput --------
rmse_results %>% knitr::kable(row.names=TRUE)




