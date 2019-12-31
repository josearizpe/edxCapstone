# CREATE SOURCE FILES
################################
# Create edx set, validation set
################################

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
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

suppressWarnings(set.seed(1, sample.kind="Rounding"))
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

# Create copies of the files for use later
edxCopy <- edx
validationCopy <- validation

# ----------------------------------------------------------------------
################################
# START PROCESSING
################################

# ----------------------------------------------------------------------
# Augment data - CREATE DATE FIELD
#
library(lubridate)
edx <- edx %>% mutate(date = as_datetime(timestamp))
# EXTRACT YEAR AND TITLE FROM ORIGINAL “title” COLUMN
y <- str_match(edx$title, "^(.*)\\s\\((\\d{4})\\)$")[,3] ## Extract the year into vector “y”
edx$year <- y ## Attach “y” as a new column to “edx”
movieTitles <- str_match(edx$title, "^(.*)\\s\\((\\d{4})\\)$")[,2] ## Elimiate year data from title
edx$title <- movieTitles ## Replace title with the stripped down new title

# ----------------------------------------------------------------------
# Augment data - split genres into columns
#
movieGenres <- edx %>% select(movieId, genres) %>% unique() ## Create new data frame including only “movieId” and “genres”
movieGenres <- suppressWarnings(movieGenres %>% separate("genres", c("g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8", "g9", "g10"), "\\|")) ## separate genres into different columns, assume no more than 10 genres per movie. 
t <- gather(movieGenres, "key", "genre", -movieId, na.rm = TRUE) ## Create 1-n table for “movieId” and “genre”. Use intermediate table t
allGenres <- t %>% select(genre) %>% unique() ## List all different genres found
allGenres <- allGenres %>% filter(!str_detect(allGenres$genre, "^\\(.*\\)$")) ## Delete records with text in parenthesis, assuming these are not valid genre names
# Create colums with genres
for (g in allGenres[,1]) { 
  print(g)
  edx <- cbind(edx, data.frame(nom = str_detect(edx$genres, g)))
  names(edx)[names(edx) == "nom"] <- str_replace(tolower(g), "-", "_")
}

# ----------------------------------------------------------------------
# DO THE SAME AS ABOVE FOR VALIDATION DATA
#
# Augment data - CREATE DATE FIELD
#
validation <- validation %>% mutate(date = as_datetime(timestamp))

y <- str_match(validation$title, "^(.*)\\s\\((\\d{4})\\)$")[,3] ## Extract the year into vector “y”
validation$year <- y ## Attach “y” as a new column to “edx”
movieTitles <- str_match(validation$title, "^(.*)\\s\\((\\d{4})\\)$")[,2] ## Elimiate year data from title
validation$title <- movieTitles ## Replace title column with the stripped down new title

# Augment data - split genres into columns
movieGenres <- validation %>% select(movieId, genres) %>% unique() ## Create new data frame including “movieId” and “genres”
movieGenres <- suppressWarnings(movieGenres %>% separate("genres", c("g1", "g2", "g3", "g4", "g5", "g6", "g7", "g8", "g9", "g10"), "\\|")) ## separate genres into different columns
t <- gather(movieGenres, "key", "genre", -movieId, na.rm = TRUE) ## Create 1-n table for “movieId” and “genre”
allGenres <- t %>% select(genre) %>% unique() ## List all different genres found
allGenres <- allGenres %>% filter(!str_detect(allGenres$genre, "^\\(.*\\)$")) ## Delete records with text in parenthesis
# Create colums with genres
for (g in allGenres[,1]) { 
  print(g)
  validation <- cbind(validation, data.frame(nom = str_detect(validation$genres, g)))
  names(validation)[names(validation) == "nom"] <- str_replace(tolower(g), "-", "_")
}


# ----------------------------------------------------------------------
# START FITTING
#
# Create RMSE function
#
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# ----------------------------------------------------------------------
# Naïve RSME
#
mu_hat <- mean(edx$rating)
naive_rmse <- RMSE(validation$rating, mu_hat)
naive_rmse
results <- data.frame(method = "Naive", rmse = naive_rmse) # Add entry in matrix

# ----------------------------------------------------------------------
# Fit ratings using “rpart”
#
set.seed(1)
library(rpart)
fit <- rpart(rating~., data = edx)
predicted_ratings <- predict(fit, validation)
model_0_rmse <- RMSE(predicted_ratings, validation$rating)
model_0_rmse
results <- suppressWarnings(bind_rows(results, data_frame(method="rpart ratings fit", rmse = model_0_rmse)))

# ----------------------------------------------------------------------
# Movie effects
#
mu <- mean(edx$rating)
movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
predicted_ratings <- mu + validation %>% left_join(movie_avgs, by='movieId') %>% pull(b_i)
model_1_rmse <- RMSE(predicted_ratings, validation$rating)
model_1_rmse
results <- suppressWarnings(bind_rows(results, data_frame(method="Naive+movie effects", rmse = model_1_rmse)))

# ----------------------------------------------------------------------
# User effects
#
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)
model_2_rmse <- RMSE(predicted_ratings, validation$rating)
model_2_rmse
results <- suppressWarnings(bind_rows(results, data_frame(method="Naive+movie&user effects", rmse = model_2_rmse)))

# ----------------------------------------------------------------------
# Movie effect with regularisation
#
# Partition data
set.seed(1)
test_index <- createDataPartition(edx$rating, times=1, p=0.9, list=FALSE)
edx_test <- edx[-test_index,]
edx_train <- edx[test_index,]
edx_test <- edx_test %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Determine the better lambda based on lower RMSE
lambdas <- seq(0, 10, 0.25)
mu <- mean(edx_train$rating)
just_the_sum <- edx_train %>% group_by(movieId) %>% summarize(s = sum(rating - mu), n_i = n())
rmses <- sapply(lambdas, function(l1){
  predicted_ratings <- edx_test %>% left_join(just_the_sum, by='movieId') %>% mutate(b_i = s/(n_i+l1)) %>% mutate(pred = mu + b_i) %>% pull(pred)
  return(RMSE(predicted_ratings, edx_test$rating))
})
qplot(lambdas, rmses)
l1 <- lambdas[which.min(rmses)]
l1
# Calculate RMSE for better lambda
mu <- mean(edx$rating)
just_the_sum <- edx %>% group_by(movieId) %>% summarize(s = sum(rating - mu), n_i = n())
predicted_ratings <- validation %>% left_join(just_the_sum, by='movieId') %>% mutate(b_i = s/(n_i+l1)) %>% mutate(pred = mu + b_i) %>% pull(pred)
model_6_rmse <- RMSE(predicted_ratings, validation$rating)
model_6_rmse
results <- suppressWarnings(bind_rows(results, data_frame(method="Naive+reg'd movie effects", rmse = model_6_rmse)))

# ----------------------------------------------------------------------
# Movie and user effect with regularisation
#
# Partition data
set.seed(1)
test_index <- createDataPartition(edx$rating, times=1, p=0.9, list=FALSE)
edx_test <- edx[-test_index,]
edx_train <- edx[test_index,]
edx_test <- edx_test %>% 
  semi_join(edx_train, by = "movieId") %>%
  semi_join(edx_train, by = "userId")

# Determine the better lambda based on lower RMSE
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l2){
  mu <- mean(edx_train$rating)
  b_i <- edx_train %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+l2))
  b_u <- edx_train %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+l2))
  predicted_ratings <- edx_test %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)
  return(RMSE(predicted_ratings, edx_test$rating))
})
qplot(lambdas, rmses)
l2 <- lambdas[which.min(rmses)]
l2
# Calculate RMSE for better lambda
mu <- mean(edx$rating)
b_i <- edx %>% group_by(movieId) %>% 
  summarize(b_i = sum(rating - mu)/(n()+l2))
b_u <- edx %>% left_join(b_i, by="movieId") %>% 
  group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+l2))
predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% 
  pull(pred)
model_3_rmse <- RMSE(predicted_ratings, validation$rating)
model_3_rmse
results <- suppressWarnings(bind_rows(results, data_frame(method="Naive+reg'd movie&user effects", rmse = model_3_rmse)))

# ----------------------------------------------------------------------
# Model residuals using rpart on original dataset
#
# Identify the winning parameters for the calculations to follow
mu <- mean(edxCopy$rating)
b_i <- edxCopy %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+l2))
b_u <- edxCopy %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% 
  summarize(b_u = sum(rating - b_i - mu)/(n()+l2))
# Create residuals table with residuals on ratings made by users
residuals <- edxCopy %>% left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% mutate(residual = rating - mu - b_i - b_u) %>% 
  pull(residual)
edxCopy <- bind_cols(edxCopy, data.frame(residuals = residuals))
library(rpart)
fit <- rpart(residuals~., data = edxCopy) # Fit residuals using “rpart”
r_hat <- predict(fit, validationCopy) # Estimate residuals for validation set
validationCopy <- bind_cols(validationCopy, data.frame(residuals = r_hat))

# Calculate predicted ratings using mu + b_i + b_u + residuals, i.e. adding residuals 
# to previous best estimate
predicted_ratings <- validationCopy %>% left_join(b_i, by = "movieId") %>% 
  left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u + residuals) %>% 
  pull(pred)
model_7_rmse <- RMSE(predicted_ratings, validationCopy$rating)
model_7_rmse
results <- suppressWarnings(bind_rows(results, data_frame(method="Naive+reg'd movie&user effects+rpart residuals fit", rmse = model_7_rmse)))

# ----------------------------------------------------------------------
# Model residuals using rpart on augmented dataset
#
# Identify the winning parameters for the calculations to follow
mu <- mean(edx$rating)
b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating - mu)/(n()+l2))
b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+l2))
# Create residuals table with residuals on ratings made by users
residuals <- edx %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(residual = rating - mu - b_i - b_u) %>% pull(residual)
edx <- bind_cols(edx, data.frame(residuals = residuals))
library(rpart) 
fit <- rpart(residuals~., data = edx) # Fit residuals using “rpart”
r_hat <- predict(fit, validation) # Estimate residuals for validation set
validation <- bind_cols(validation, data.frame(residuals = r_hat))

# Calculate predicted ratings using mu + b_i + b_u + residuals, i.e. adding residuals to previous best estimate
predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u + residuals) %>% pull(pred)
model_4_rmse <- RMSE(predicted_ratings, validation$rating)
model_4_rmse
results <- suppressWarnings(bind_rows(results, data_frame(method="Naive+reg'd movie&user effects+rpart residuals fit on augmented set", rmse = model_4_rmse)))

# ----------------------------------------------------------------------
# Model residuals on augmented data set without taking into account regularisation
#
# Identify the winning parameters for the calculations to follow
mu <- mean(edx$rating)
b_i <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating - mu))
b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = mean(rating - b_i - mu))
# Create residuals table with residuals on ratings made by users
residuals <- edx %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(residual = rating - mu - b_i - b_u) %>% pull(residual)
edx <- edx[ ,-which(names(edx)=="residuals")]
edx <- bind_cols(edx, data.frame(residuals = residuals))
fit <- rpart(residuals~., data = edx) # Fit residuals using “rpart”
r_hat <- predict(fit, validation) # Estimate residuals for validation set
validation <- validation[ ,-which(names(validation)=="residuals")]
validation <- bind_cols(validation, data.frame(residuals = r_hat))

# Calculate predicted ratings using mu + b_i + b_u + residuals, i.e. adding residuals 
# to previous best estimate

predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u + residuals) %>% pull(pred)
model_5_rmse <- RMSE(predicted_ratings, validation$rating)
model_5_rmse

results <- suppressWarnings(bind_rows(results, data_frame(method="Naive+movie&user effects+rpart residuals fit on augmented set", rmse = model_5_rmse)))

# ----------------------------------------------------------------------
# Show results in order of increasing RMSE
#
results %>% arrange(rmse)

# ----------------------------------------------------------------------
# Show the winning method and final RMSE value
#
message("Final RMSE = ", min(results$rmse), " delivered by '", results$method[which.min(results$rmse)],"'")
