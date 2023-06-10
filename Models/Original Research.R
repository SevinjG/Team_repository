#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("fastDummies")


library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(fastDummies)


# Load data
data <- read.csv('Data/kaggle_survey_2021_responses.csv', fileEncoding = 'UTF-8')

# Subset data with an initial list of a few features
df <- data[, c(2:20, 128)]

cols <- c('Age', 'Gender', 'Country_of_residence', 'Education', 'Job_title', 'Experience', 
          'Python', 'R', 'SQL', 'C', 'C++', 'Java', 'Javascript', 'Julia', 'Swift', 
          'Bash', 'MATLAB', 'None', 'Other', 'Salary_range')
colnames(df) <- cols



# Get rid of the first (questions) row
df <- df[-1, ]

# Filter USA data
df <- df[df$Country_of_residence == 'United States of America', ]

# Keep just non-null values of salary
df <- df[df$Salary_range != "", ]

# Checking out unique categories of categorical columns
cat_cols <- sapply(df, class) %in% c('character', 'factor')

for (column in names(df)[cat_cols]) {
  cat('----------------------------------------\n')
  cat(column, '\n')
  cat('***********\n')
  print(table(df[,column]))
}


# An attempt to convert categorical salary range into a single numeric value
temp1 <- strsplit(df$Salary_range, '-', fixed = TRUE)
df$Salary_min <- sapply(temp1, '[', 1)
df$Salary_max <- sapply(temp1, '[', 2)

# Some cleanups
replace_symbols <- function(x) {
  gsub(',', '', gsub('$', '', gsub('>', '', x), fixed = TRUE))
}
df$Salary_min <- as.integer(replace_symbols(df$Salary_min))
df$Salary_max <- as.integer(replace_symbols(df$Salary_max))

# Additional cleanups
df$Salary_max <- replace_na(df$Salary_max, 1000001)

# Estimated salary (mid value of the range)
df$Salary_est <- (df$Salary_min + df$Salary_max + 1) / 2

# Filter data for salary range 30-300k
df <- df[df$Salary_est > 30000 & df$Salary_est < 300000, ]

# Recode rows of language columns
languages <- c('Python', 'R', 'SQL', 'C', 'C++', 'Java', 'Javascript',
               'Julia', 'Swift', 'Bash', 'MATLAB', 'None', 'Other')

for (i in languages) {
  df[[i]] <- ifelse(df[[i]] == i, 1, 0)
}

# Box plots
ggplot(df, aes(x = Salary_est, y = Gender)) +
  geom_boxplot() +
  ggtitle('Salary vs Gender')

ggplot(df, aes(x = Salary_est, y = Education)) +
  geom_boxplot() +
  ggtitle('Salary vs Education')

ggplot(df, aes(x = Salary_est, y = Job_title)) +
  geom_boxplot() +
  ggtitle('Salary vs Job Title')

ggplot(df, aes(x = Salary_est, y = Experience)) +
  geom_boxplot() +
  ggtitle('Salary vs Experience')

# Train and evaluate the model
X <- df[, !(names(df) %in% c('Gender', 'Salary_range', 'Salary_min', 'Salary_max', 'Salary_est'))]
y <- df$Salary_est


X <- dummy_cols(X, select_columns = colnames(X), remove_first_dummy = TRUE)

X <- X[,-c(1:18)]

# Set the random seed for reproducibility
set.seed(1)  

# Splitting the data into training and testing sets
train_indices <- createDataPartition(y, p = 0.7, list = FALSE)
X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]


# Instantiate
original_lm <- lm(y_train ~ ., data = X_train)

save(original_lm, file = "original_lm.RData")

# Predict
y_pred <- predict(original_lm, newdata = X_test)

# Evaluate
mse <- mean((y_test - y_pred)^2)

rmse <- sqrt(mean((y_test - y_pred)^2))

r2_score <- R2(y_pred, y_test)

mae <- MAE(y_pred, y_test)

