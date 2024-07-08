##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################
# Note: this process could take a couple of minutes

# installing nessesary packages in our R environment
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.rproject.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.rproject.org")

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

#Downloading the  movielens(ml-10m.zip) file if not avaliable in our device
options(timeout = 120)
dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

# Unzipping the ml-10m.zip file and attaching the ratings.dat file to  ratings_file
ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

# Already we have Unzipped the ml-10m.zip file and Now simply attaching  the movies.dat file to  movies_file
movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

# Converting the ratings_files to the Dataset
ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)

# Adding the coloumn names to the  rating dataset
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")

# Coverting the coloumns values into the respective datatype 
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

# Converting the movies_files to the Dataset
movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)

# Adding the coloumn names to the  rating dataset
colnames(movies) <- c("movieId", "title", "genres")

# Coverting the coloumns values into the respective datatype 
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

#Left joining between the ratings dataset with movies dataset by the movieId
movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later

# set.seed(1) # if using R 3.5 or earlier
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

# Deleting the some Rows in the movielens data and defined with the edx dataset
edx <- movielens[-test_index,]

# Extracting the Dataset with inverse of the edx dataset from movielens data
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set

final_holdout_test <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)

#Adding the new Rows to the edx from removed dataset
edx <- rbind(edx, removed)

# Removing the  extra following  dl, ratings, movies, test_index, temp, movielens, removed data files and datasets from system
rm(dl, ratings, movies, test_index, temp, movielens, removed)



if(!require(tidyverse)) install.packages("tidyverse") 
if(!require(kableExtra)) install.packages("kableExtra")
if(!require(tidyr)) install.packages("tidyr")
if(!require(tidyverse)) install.packages("tidyverse")
if(!require(stringr)) install.packages("stringr")
if(!require(forcats)) install.packages("forcats")
if(!require(ggplot2)) install.packages("ggplot2")

library(dplyr)
library(tidyverse)
library(kableExtra)
library(tidyr)
library(stringr)
library(forcats)
library(ggplot2)


summary(edx)


anyNA(edx)

edx %>% summarize(users_count = n_distinct(userId))


edx %>%summarize( movies_count = n_distinct(movieId))

#Rating Distribution

#Overview of Rating Distribution

edx %>% 
  ggplot(aes(rating))+
  geom_histogram(bins= 25 ,color="lightblue")+
                   labs(title="Distribution of movie Ratings",
                        x= "Rating",
                        y="Frequency")

#Top 10  Movies with  highest no.of ratings per movie
edx %>%
  group_by(title) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=10) %>%
  ggplot(aes(title, count)) +
  theme_classic()  +
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 7)) +
  labs(title = "Ratings Frequency Distribution Per Title - TOP 10 Movies",
       x = "Title",
       y = "Frequency")


# average rating for movie
avg_rating<-edx %>%
  group_by(title) %>%
  summarise(average_rating = mean(rating))
 
avg_rating %>%
  ggplot(aes(average_rating)) +
  theme_classic()  +
  geom_histogram(bins=12) +
  labs(title = "average rating for movieId",
       x = "Mean",
       y = "Frequency")

#top 10 movies with highest rating
top_movies<- avg_rating %>%
  arrange(desc(average_rating)) %>%
  head(n=10)
top_movies


#meadian distribution of rating with title

edx %>%
  group_by(title) %>%
  summarise(median = median(rating)) %>%
  ggplot(aes(median)) +
  theme_classic()  +
  geom_histogram(bins=12) +
  labs(title = "Median Distribution per Title",
       x = "Median",
       y = "Frequency")


# Modelling Approach #

# Avg. movie rating model #

# Compute the dataset's mean rating
mu <- mean(edx$rating)
mu

# Test results based on simple prediction
simple_rmse <- RMSE(validation$rating, mu)
simple_rmse

# Check results
# Save prediction in data frame
rmse_results <- data.frame(method = "Avg. movie rating model", RMSE = simple_rmse)
rmse_results %>% knitr::kable()

## Movie effect model ##

# Simple model taking into account the movie effect b_i
# Subtract the rating minus the mean for each rating for a movie
# Plot no.of movies with the computed b_i
movie_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = mean(rating - mu))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"),
                     ylab = "No.of movies", main = "No.of movies with the computed b_i")


# Test and save rmse results 
predicted_ratings <- mu +  validation %>%
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie effect model",  
                                     RMSE = model_1_rmse ))
# Check results
rmse_results %>% knitr::kable()

## Movie and user effect model ##

# Plot penaly term user effect #
user_avgs<- edx %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  filter(n() >= 100) %>%
  summarize(b_u = mean(rating - mu - b_i))
user_avgs%>% qplot(b_u, geom ="histogram", bins = 30, data = ., color = I("black"))


user_avgs <- edx %>%
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))


# Test and save the rmse results 
predicted_ratings <- validation%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, validation$rating)
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Movie and user effect model",  
                                     RMSE = model_2_rmse))

# Check result
rmse_results %>% knitr::kable()

## Regularised movie and user effect model ##
# lambdas is a tuning parameter
# use cross-validation to choose it.
lambdas <- seq(0, 10, 0.25)


# For each lambda,calculate b_i & b_u, the rating prediction & testing
rmses <- sapply(lambdas, function(l){
  
  mu <- mean(edx$rating)
  
  b_i <- edx %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    test %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test$rating))
})

# Plot lambdas vs rmses to select the optimal lambdas                                                            
qplot(lambdas, rmses)  


# The optimal lambda                                                             
lambda <- lambdas[which.min(rmses)]
lambdas

# Test and save results                                                             
rmse_results <- bind_rows(rmse_results,
                          data_frame(method="Regularised movie and user effect model",  
                                     RMSE = min(rmses)))

# Check result
rmse_results %>% knitr::kable()

#### Results ####                                                            
# RMSE results overview                                                          
rmse_results %>% knitr::kable()
