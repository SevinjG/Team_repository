install.packages("dplyr")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("caret")



library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)


library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)

# Load data
data <- read.csv('RRproject.csv', stringsAsFactors = FALSE)

# Subset data with an initial list of a few features
df <- data[, c(2:19, 128)]

cols <- c('Age', 'Gender', 'Country_of_residence', 'Education', 'Job_title', 'Experience', 
          'Python', 'R', 'SQL', 'C', 'C++', 'Java', 'Javascript', 'Julia', 'Swift', 
          'Bash', 'MATLAB', 'None', 'Salary_range')
colnames(df) <- cols



# Get rid of the first (questions) row
df <- df[-1, ]

# Filter USA data
df <- df[df$Country_of_residence == 'United States of America', ]

# Keep just non-null values of salary
df <- df[!is.na(df$Salary_range), ]

# Checking out unique categories of categorical columns
cat_cols <- sapply(df, class) %in% c('character', 'factor')

for (column in names(df)[cat_cols]) {
  cat('----------------------------------------\n')
  cat(column, '\n')
  cat('***********\n')
  cat(table(df[[column]]), '\n')
}
cat('----------------------------------------\n')

# An attempt to convert categorical salary range into a single numeric value
temp1 <- strsplit(df$Salary_range, '-', fixed = TRUE)
df$Salary_min <- sapply(temp1, '[', 1)
df$Salary_max <- sapply(temp1, '[', 2)

# Some cleanups
replace_symbols <- function(x) {
  gsub(',', '', gsub('$', '', gsub('>', '', x)))
}

# Check the unique values in the Salary_min column
unique_values <- unique(df$Salary_min)

# Identify non-numeric values
non_numeric_values <- unique_values[!is.na(as.numeric(unique_values))]

# Print non-numeric values
print(non_numeric_values)


df$Salary_min <- as.integer(sapply(df$Salary_min, replace_symbols))

# Additional cleanups
clean_text <- function(x) {
  if (is.null(x)) {
    x <- '0,000'
  }
  x
}

df$Salary_max <- sapply(df$Salary_max, clean_text)
df$Salary_max <- as.integer(sapply(df$Salary_max, replace_symbols))

# Estimated salary (mid value of the range)
df$Salary_est <- (df$Salary_min + df$Salary_max + 1) / 2

# Filter data for salary range 30-300k
df <- df[df$Salary_est > 30000 & df$Salary_est < 300000, ]




languages <- c('Python', 'R', 'SQL', 'C', 'C++', 'Java', 'Javascript',
               'Julia', 'Swift', 'Bash', 'MATLAB', 'None')

for (i in languages) {
  regex_pattern <- paste0("^Q7_Part_.*", gsub("\\+", "\\\\+", i), "$")
  column_name <- colnames(df)[grep(regex_pattern, colnames(df))]
  
  if (length(column_name) > 0) {
    df[[i]] <- ifelse(!is.na(df[[column_name]]) & df[[column_name]] != 0, 1, 0)
  } else {
    print(paste("Column for", i, "not found in the dataframe."))
  }
}

for (i in languages) {
  regex_pattern <- paste0("^", gsub("\\+", "\\\\+", i), "$")
  column_name <- colnames(df)[grep(regex_pattern, colnames(df))]
  
  if (length(column_name) > 0) {
    df[[i]] <- ifelse(!is.na(df[[column_name]]) & df[[column_name]] != 0, 1, 0)
  } else {
    print(paste("Column for", i, "not found in the dataframe."))
  }
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


# Check levels of each variable
for (col in colnames(X)) {
  if (is.factor(X[[col]]) && length(levels(X[[col]])) < 2) {
    print(paste("Variable", col, "has less than 2 levels."))
  }
}
# Check levels of each factor variable
for (col in colnames(X)) {
  if (is.factor(X[[col]])) {
    var_levels <- levels(X[[col]])
    if (length(var_levels) < 2) {
      print(paste("Variable", col, "has less than 2 levels."))
    } else {
      print(paste("Variable", col, "levels:", var_levels))
    }
  }
}

# Exclude variables with less than 2 levels from dummyVars
valid_cols <- colnames(X)[sapply(X, function(x) is.factor(x) && length(levels(x)) >= 2)]
X <- dummyVars(as.formula(paste("~ .", paste(valid_cols, collapse = " + "))), data = X) %>% predict(X)

X <- dummyVars(~., data = X) %>% predict(X)
# Exclude variables with less than 2 levels from dummyVars
valid_cols <- colnames(X)[sapply(X, function(x) is.factor(x) && length(levels(x)) >= 2)]
X <- dummyVars(as.formula(paste("~ .", paste(valid_cols, collapse = " + "))), data = X) %>% predict(X)

# Check levels of each factor variable in valid columns
for (col in valid_cols) {
  if (is.factor(X[[col]])) {
    var_levels <- levels(X[[col]])
    print(paste("Variable", col, "levels:", var_levels))
  }
}

# Check levels of each factor variable in valid columns
for (col in valid_cols) {
  if (is.factor(X[[col]])) {
    var_levels <- levels(X[[col]])
    print(paste("Variable", col, "levels:", var_levels))
  }
}

valid_cols <- colnames(X)[sapply(X, function(x) is.factor(x) && length(levels(x)) >= 2)]


set.seed(1)
X <- X[complete_cases, ]

train_indices <- createDataPartition(y[complete_cases], p = 0.75, list = FALSE)


complete_cases <- complete.cases(y)

X_train <- X[train_indices, ]
X_test <- X[-train_indices, ]
y_train <- y[train_indices]
y_test <- y[-train_indices]

# Check levels of each factor variable
for (col in colnames(X)) {
  if (is.factor(X[[col]]) && length(levels(X[[col]])) < 2) {
    print(paste("Variable", col, "has less than 2 levels."))
  }
}

# Remove rows with missing values in the target variable
complete_cases <- complete.cases(y)
X <- X[complete_cases, ]
y <- y[complete_cases, ]

# Remove rows with missing values in the target variable
complete_cases <- complete.cases(y)
X <- X[complete_cases, ]
y <- y[complete_cases]



# Handle missing values
data <- na.omit(data)  # Remove rows with any missing values

# Split the data into predictors (X) and response variable (y)
target_column_index <- 1  # Replace with the actual index of the response variable column
X <- data[, -target_column_index]  # Specify the columns of predictors
y <- data[, target_column_index]   # Specify the column of the response variable


# Train the model
model <- train(X, y, method = "rf")
warnings()
# View the model summary
print(model)

# View the model summary
summary(model$finalModel)


# Train the model
model <- train(X, y, method = "lm")

# View the model summary
summary(model$finalModel)


# Train the model
model <- train(x = X_train, y = y_train, method = 'lm')
warnings()
# View model performance
print(model$results)


# Instantiate
model <- train(x = X_train, y = y_train, method = 'lm')


# Predict
y_pred <- predict(model, newdata = X_test)

# Evaluate
mean_squared_error <- mean((y_test - y_pred)^2)
rmse <- sqrt(mean_squared_error)
r2 <- cor(y_test, y_pred)^2
mae <- mean(abs(y_test - y_pred))

mean_squared_error
rmse
r2
mae

