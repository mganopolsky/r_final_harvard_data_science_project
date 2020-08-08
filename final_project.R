
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
#e_u_i - (independant errors for each movie, per user) - this, in essential is the RMSE
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

#trying regularization next; first need to find the correct lambda - tuning parameter:
#Here we use a much more granular approach of stepping in increments of .05 (instead of .5)
lambdas <- seq(0, 10, 0.05)

mu <- mean(train_set$rating)
just_the_sum <- train_set %>% 
  group_by(movieId) %>% 
  summarize(s = sum(rating - mu), n_i = n())

rmses_2 <- sapply(lambdas, function(l){
  predicted_ratings <- test_set %>% 
    left_join(just_the_sum, by='movieId') %>% 
    mutate(b_i = s/(n_i+l)) %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  return(RMSE(predicted_ratings, test_set$rating))
})
#If you visually display the lambda's on a plot, you can spot the lowest point somewhere 
# between 1.5 and 2
qplot(lambdas, rmses_2)  
#Saving the latest lambda 
lambda_2 <- lambdas[which.min(rmses_2)]
lambda_2

#now that we know the best lambda is 2.45 - thia accounts for the smallest RMSE value - 
# - we use that to make some predictions with regularization


movie_reg_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+lambda_2), n_i = n()) 

#To view the output of this we can plot it and see the difference regularization makes:
tibble(original = movie_avgs$b_i, 
       regularlized = movie_reg_avgs$b_i, 
       n = movie_reg_avgs$n_i) %>%
  ggplot(aes(original, regularlized, size=sqrt(n))) + 
  geom_point(shape=1, alpha=0.5)


#after creating the regularization parameters, we can now test our predictions with the RMSE on the test_set
predicted_ratings <- test_set %>% 
  left_join(movie_reg_avgs, by = "movieId") %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)

model_3_rmse <- RMSE(predicted_ratings, test_set$rating)

tmp_rmse_results <- tibble(method = "Regularized with lambda", RMSE = model_3_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)

# The results from the regularization approach don't seem to be doing any better then the movie affect model, and significantly worse then the user affect + movie effect model.
#Therefor, we will next attempt to tune the model more with the year parameter

#the lambda needs to be selected using cross-validation, as well. We do this as follows, using the same lambda-creation sequence:
lambdas <- seq(0, 10, 0.05)

rmses_4 <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
#now we plot the new lambdas vs. RMSE's
qplot(lambdas, rmses_4)  

#we pick up the most accurate lambda - 
lambda_4 <- lambdas[which.min(rmses_4)]
lambda_4


model_4_rmse <- min(rmses_4)
#This final outcome is .8642080, which is better then the requested 0.86490.
tmp_rmse_results <- tibble(method = "Regularized Movie + User Effect Model", RMSE = model_4_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)

#This final outcome is .8642080, which is better then the requested 0.86490.
#But can we do even better?

#Next, I will attempt to add in the year into the mix, testing whether the age of the movie makes a difference
#For this we have created the field "age_of_movie"

#l = 4.8

rmses_5 <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by="userId") %>%
    group_by(age_of_movie) %>%
    summarize( b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "age_of_movie") %>%
    mutate(pred = mu + b_i + b_u + b_y) %>%
    pull(pred)
 
#  rmse <- RMSE(predicted_ratings, test_set$rating)
   
  return(RMSE(predicted_ratings, test_set$rating))
})
#now we plot the new lambdas vs. RMSE's
qplot(lambdas, rmses_5, color="red", main="Regularized Lambdas Predicted With User Bias, Movie Bias, and Release Year Bias.")  

#we pick up the most accurate lambda - 
lambda_5 <- lambdas[which.min(rmses_5)]
lambda_5


model_5_rmse <- min(rmses_5)
tmp_rmse_results <- tibble(method = "Regularized Movie + User Effect Model + Year Effect Model", RMSE = model_5_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)
rm(tmp_rmse_results)

#The year seems to have made a difference, but a fairly insignificant one in our model; 
#The next item to be tested is the genre. Currently, the genres are listed as a string combination of all the genres a movie belongs to; we will attempt to measure the difference this way to see if it makes a significant difference first.


rmses_6 <- sapply(lambdas, function(l){
  
#l <- 4.8

  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_y <- train_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by="userId") %>%
    group_by(age_of_movie) %>%
    summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+l))
  
  #adding in the genre bias here
  b_g <- train_set %>% 
    left_join(b_i, by='movieId') %>%
    left_join(b_u, by="userId") %>%
    left_join(b_y, by="age_of_movie") %>%
    group_by(genres) %>%
    summarize( b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+l))
  
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_y, by = "age_of_movie") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
    pull(pred)
  
  #  rmse <- RMSE(predicted_ratings, test_set$rating)
  
  return(RMSE(predicted_ratings, test_set$rating))
})
#now we plot the new lambdas vs. RMSE's
qplot(lambdas, rmses_6, color="turquoise", main="Regularized Lambdas Predicted With User Bias, Movie Bias, and Release Year Bias.")  

#we pick up the most accurate lambda - 
lambda_6 <- lambdas[which.min(rmses_6)]
lambda_6


model_6_rmse <- min(rmses_6)
tmp_rmse_results <- tibble(method = "Regularized Movie + User Effect Model + Year Effect Model + Genre Effect Model", RMSE = model_6_rmse)

#add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)
rm(tmp_rmse_results)

#the results don't see to have made a huge difference in this case. but the numbers are below the expected RMSE number, so we will try to apply this algorithm to the validation data
mu <- mean(train_set$rating)

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda_6))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda_6))

b_y <- train_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by="userId") %>%
  group_by(age_of_movie) %>%
  summarize(b_y = sum(rating - b_i - b_u - mu)/(n()+lambda_6))

#adding in the genre bias here
b_g <- train_set %>% 
  left_join(b_i, by='movieId') %>%
  left_join(b_u, by="userId") %>%
  left_join(b_y, by="age_of_movie") %>%
  group_by(genres) %>%
  summarize( b_g = sum(rating - b_i - b_u - b_y - mu)/(n()+lambda_6))


predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_y, by = "age_of_movie") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_y + b_g) %>%
  pull(pred)

  final_rmse <- RMSE(predicted_ratings, validation$rating)

tmp_rmse_results <- tibble(method = "Final Evaluation of the algorithm on the Validation Data", RMSE = final_rmse)
  
  #add to existing rmse results table
rmse_results <- bind_rows(rmse_results, tmp_rmse_results)
rm(tmp_rmse_results)
  

# The final output 
rmse_results %>% knitr::kable()




