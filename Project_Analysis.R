# Load edx and partition Temp
#########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# if using R 4.0 or later
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

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

rm(dl, ratings, movies, test_index, temp, movielens, removed)


# Partition edx in to test & train

set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)
train_set <- edx[-test_index,]
temp1 <- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp1 %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test_set set back into train_set set
removed1 <- anti_join(temp1, test_set)
train_set <- rbind(train_set, removed1)

rm(test_index, temp1,removed1)

# RSME Function

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

#  Create a copy of train_set and test_set with an additional column of "week" of rating derived from "timestamp" column

train_set_m <- train_set  %>% mutate (date = round_date(as_datetime(timestamp), unit = "week"))
test_set_m <- test_set %>% mutate (date = round_date(as_datetime(timestamp), unit = "week"))

#  Create a copy of "edx" and "validation"  with an additional column of "week" of rating derived from "timestamp" column

edx_m <- edx  %>% mutate (date = round_date(as_datetime(timestamp), unit = "week"))
validation_m <- validation %>% mutate (date = round_date(as_datetime(timestamp), unit = "week"))

# Final run with different lambdas

set.seed(1, sample.kind="Rounding")
library(lubridate)
library(tidyverse)

rmses <- sapply(4.95, function(l){
  
  mu <- mean(edx_m$rating)
  b_i <- edx_m %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_m %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+5.1))
  b_day <- edx_m %>% 
    left_join(b_i, by="movieId") %>% left_join(b_u, by="userId")%>%
    group_by(date) %>%
    summarize(b_day = sum(rating - b_i - b_u -mu)/(n()+2.5))
  b_ge <- edx_m %>% 
    left_join(b_i, by="movieId") %>% left_join(b_u, by="userId")%>%left_join(b_day, by="date")%>%
    group_by(genres) %>%
    summarize(b_ge = sum(rating - b_i - b_u -b_day-mu)/(n()+1.9))
  
  predicted_ratings <- 
    validation_m %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_day,by="date")%>%
    left_join(b_ge,by="genres") %>%
    mutate(pred = mu + b_i + b_u + b_day+b_ge) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, validation_m$rating))
})
rmses  


   