#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("ggplot2")
#install.packages("caret")
#install.packages("fastDummies")
#install.packages("randomForest")

library(dplyr)
library(tidyr)
library(ggplot2)
library(caret)
library(fastDummies)
library(randomForest)


# Load data
load('Data/kaggle_survey_2021_responses.RData')
data_old <- data
rm(data)

# data_new <- read.csv('Data/kaggle_survey_2022_responses.csv', encoding = "UTF-8")
# save(data_new, file = 'Data/kaggle_survey_2022_responses.RData')
load('Data/kaggle_survey_2022_responses.RData')

### Data Preparation --------------------------------------------------------------------------
df_old <- data_old[, c(2:20, 128)]
df_new <- data_new[, c(2:4, 25, 146, 30:45, 159)]

cols <- c('Age', 'Gender', 'Country_of_residence', 'Education', 'Job_title', 'Experience', 
          'Python', 'R', 'SQL', 'C', 'C++', 'Java', 'Javascript', 'Julia', 'Swift', 
          'Bash', 'MATLAB', 'None', 'Other', 'Salary_range')
colnames(df_old) <- cols

cols <- c('Age', 'Gender', 'Country_of_residence', 'Education', 'Job_title', 'Experience', 
          'Python', 'R', 'SQL', 'C', 'C#', 'C++', 'Java', 'Javascript', 'Bash','PHP', 'MATLAB', 
          'Julia', 'Go', 'None', 'Other', 'Salary_range')
colnames(df_new) <- cols


# Get rid of the first (questions) row
df_old <- df_old[-1, ]
df_new <- df_new[-1, ]

# Filter USA data
df_old <- df_old[df_old$Country_of_residence == 'United States of America', ]
df_new <- df_new[df_new$Country_of_residence == 'United States of America', ]

# Keep just non-null values of salary
df_old <- df_old[df_old$Salary_range != "", ]
df_new <- df_new[df_new$Salary_range != "", ]

# Checking out unique categories of categorical columns
for (column in names(df_old)) {
  cat('----------------------------------------\n')
  cat(column, '\n')
  cat('***********\n')
  print(table(df_old[,column]))
}

for (column in names(df_new)) {
  cat('----------------------------------------\n')
  cat(column, '\n')
  cat('***********\n')
  print(table(df_new[,column]))
}

# An attempt to convert categorical salary range into a single numeric value
temp1 <- strsplit(df_old$Salary_range, '-', fixed = TRUE)
df_old$Salary_min <- sapply(temp1, '[', 1)
df_old$Salary_max <- sapply(temp1, '[', 2)

temp2 <- strsplit(df_new$Salary_range, '-', fixed = TRUE)
df_new$Salary_min <- sapply(temp2, '[', 1)
df_new$Salary_max <- sapply(temp2, '[', 2)

# Some cleanups
replace_symbols <- function(x) {
  gsub(',', '', gsub('$', '', gsub('>', '', x), fixed = TRUE))
}

df_old$Salary_min <- as.integer(replace_symbols(df_old$Salary_min))
df_old$Salary_max <- as.integer(replace_symbols(df_old$Salary_max))

df_new$Salary_min <- as.integer(replace_symbols(df_new$Salary_min))
df_new$Salary_max <- as.integer(replace_symbols(df_new$Salary_max))

# Additional cleanups
df_old$Salary_max <- replace_na(df_old$Salary_max, 1000001)
df_new$Salary_max <- replace_na(df_new$Salary_max, 1000001)

# Estimated salary (mid value of the range)
df_old$Salary_est <- (df_old$Salary_min + df_old$Salary_max + 1) / 2
df_new$Salary_est <- (df_new$Salary_min + df_new$Salary_max + 1) / 2

# Filter data for salary range 30-300k
df_old <- df_old[df_old$Salary_est > 30000 & df_old$Salary_est < 300000, ]
df_new <- df_new[df_new$Salary_est > 30000 & df_new$Salary_est < 300000, ]

# Recode rows of language columns
languages_old <- c('Python', 'R', 'SQL', 'C', 'C++', 'Java', 'Javascript',
               'Julia', 'Swift', 'Bash', 'MATLAB', 'None', 'Other')

for (i in languages_old) {
  df_old[[i]] <- ifelse(df_old[[i]] == i, 1, 0)
}

languages_new <- c('Python', 'R', 'SQL', 'C', 'C#', 'C++', 'Java', 'Javascript', 
                   'Bash','PHP', 'MATLAB', 'Julia', 'Go', 'None', 'Other')

for (i in languages_new) {
  df_new[[i]] <- ifelse(df_new[[i]] == i, 1, 0)
}


# Being able to run the original model, we have to match the columns (programming languages)
# We do not have "Swift" in our new data
# We do not have "PHP", "C#" and "Go" in our original data
# That's why we will combine "PHP", "C#" and "Go" results with "Others" column.
# After creating dummy variable, we will add dummy for Swift as well with 0s. 
# As in original data, we had only 18 positive asnwers for "Swift", 
# this addition will not create a significant impact.

df_new$Other <- df_new$PHP + df_new$`C#` + df_new$Go + df_new$Other
df_new$Other <- ifelse(df_new$Other >= 1, 1, 0)
df_new$PHP <- NULL
df_new$`C#` <- NULL
df_new$Go <- NULL


### Data Visualization --------------------------------------------------------------------------
# Box plots - original data
ggplot(df_old, aes(x = Salary_est, y = Gender)) +
  geom_boxplot() +
  ggtitle('Salary vs Gender')

ggplot(df_old, aes(x = Salary_est, y = Education)) +
  geom_boxplot() +
  ggtitle('Salary vs Education')

ggplot(df_old, aes(x = Salary_est, y = Job_title)) +
  geom_boxplot() +
  ggtitle('Salary vs Job Title')

ggplot(df_old, aes(x = Salary_est, y = Experience)) +
  geom_boxplot() +
  ggtitle('Salary vs Experience')

# Box plots - new data
ggplot(df_new, aes(x = Salary_est, y = Gender)) +
  geom_boxplot() +
  ggtitle('Salary vs Gender')

ggplot(df_new, aes(x = Salary_est, y = Education)) +
  geom_boxplot() +
  ggtitle('Salary vs Education')

ggplot(df_new, aes(x = Salary_est, y = Job_title)) +
  geom_boxplot() +
  ggtitle('Salary vs Job Title')

ggplot(df_new, aes(x = Salary_est, y = Experience)) +
  geom_boxplot() +
  ggtitle('Salary vs Experience')


### Modelling --------------------------------------------------------------------------
# Preparation of data for modelling
X_old <- df_old[, !(names(df_old) %in% 
                      c('Gender', 'Salary_range', 'Salary_min', 'Salary_max', 'Salary_est'))]
y_old <- df_old$Salary_est

X_old <- dummy_cols(X_old, select_columns = colnames(X_old), remove_first_dummy = TRUE)
X_old <- X_old[,-c(1:18)]

X_new <- df_new[, !(names(df_new) %in% 
                      c('Gender', 'Salary_range', 'Salary_min', 'Salary_max', 'Salary_est'))]
y_new <- df_new$Salary_est

X_new <- dummy_cols(X_new, select_columns = colnames(X_new), remove_first_dummy = TRUE)
X_new <- X_new[,-c(1:17)]

# Add job title "Teacher/Professor" and "Engineer (non-software)" to "Other" 
# and remove as we did not have that column in original model.
X_new$Job_title_Other <- X_new$`Job_title_Teacher / professor` + 
                          X_new$`Job_title_Engineer (non-software)`

X_new$`Job_title_Teacher / professor` <- NULL
X_new$`Job_title_Engineer (non-software)` <- NULL

# Matching the job titles
colnames(X_new)[
  which(names(X_new) == "Job_title_Data Analyst (Business, Marketing, Financial, Quantitative, etc)")
  ] <- "Job_title_Data Analyst"

colnames(X_new)[
  which(names(X_new) == "Job_title_Data Architect")
  ] <- "Job_title_DBA/Database Engineer"

colnames(X_new)[
  which(names(X_new) == "Job_title_Developer Advocate")
  ] <- "Job_title_Developer Relations/Advocacy"

colnames(X_new)[
  which(names(X_new) == "Job_title_Machine Learning/ MLops Engineer")
  ] <- "Job_title_Machine Learning Engineer"

colnames(X_new)[
  which(names(X_new) == "Job_title_Manager (Program, Project, Operations, Executive-level, etc)")
  ] <- "Job_title_Program/Project Manager"


# Add dummy column for Swift and Job_title_Product Manager
X_new$Swift_1 <- 0
X_new$`Job_title_Product Manager` <- 0

# Set the random seed for reproducibility
set.seed(1)  

# Splitting the data into training and testing sets
train_indices <- createDataPartition(y_old, p = 0.7, list = FALSE)
X_train_old <- X_old[train_indices, ]
X_test_old <- X_old[-train_indices, ]
y_train_old <- y_old[train_indices]
y_test_old <- y_old[-train_indices]

# Modelling using original model ----------------------------------------------------
load("original_lm.RData")

# Predict
y_pred_old <- predict(original_lm, newdata = X_test_old)
y_pred_new <- predict(original_lm, newdata = X_new)

# Evaluate
mse_old <- mean((y_test_old - y_pred_old)^2)
rmse_old <- sqrt(mean((y_test_old - y_pred_old)^2))
r2_score_old <- R2(y_pred_old, y_test_old)
mae_old <- MAE(y_pred_old, y_test_old)

mse_new <- mean((y_new - y_pred_new)^2)
rmse_new <- sqrt(mean((y_new - y_pred_new)^2))
r2_score_new <- R2(y_pred_new, y_new)
mae_new <- MAE(y_pred_new, y_new)

mse <- c(mse_old, mse_new)
rmse <- c(rmse_old, rmse_new)
r2_score <- c(r2_score_old, r2_score_new)
mae <- c(mae_old, mae_new)
data_name <- c("Original data", "New data")

lm_results <- data.frame(data_name, mse, rmse, r2_score, mae)

# Random Forest Model ------------------------------------------------------------------
## Making additional data preparation
### Original Data
df_old$Country_of_residence <- NULL
df_old$Gender <- as.factor(df_old$Gender)
df_old$Age <- factor(df_old$Age, 
                     levels = c("18-21", "22-24", "25-29", "30-34", "35-39", 
                                "40-44", "45-49", "50-54", "55-59", "60-69", "70+"),
                     ordered = TRUE)
df_old$Education <- factor(df_old$Education,
                           levels = c("I prefer not to answer",
                                     "No formal education past high school",
                                     "Some college/university study without earning a bachelor's degree",
                                     "Bachelor's degree", "Master's degree",
                                     "Doctoral degree", "Professional doctorate"),
                           ordered = TRUE)
df_old$Experience <- factor(df_old$Experience,
                            levels = c("< 1 years", "1-3 years", "3-5 years",
                                       "5-10 years", "10-20 years", "20+ years",
                                       "I have never written code"),
                            ordered = TRUE)

rf_old <- df_old[,-c(19,20,21)]
colnames(rf_old)[which(names(rf_old) == "C++")] <- "C_plus_plus"

### New data
df_new$Country_of_residence <- NULL
df_new$Gender <- as.factor(df_new$Gender)
df_new$Age <- factor(df_new$Age, 
                     levels = c("18-21", "22-24", "25-29", "30-34", "35-39", 
                                "40-44", "45-49", "50-54", "55-59", "60-69", "70+"),
                     ordered = TRUE)
df_new$Education <- factor(df_new$Education,
                           levels = c("I prefer not to answer",
                                      "No formal education past high school",
                                      "Some college/university study without earning a bachelor's degree",
                                      "Bachelor's degree", "Master's degree",
                                      "Doctoral degree", "Professional doctorate"),
                           ordered = TRUE)
df_new$Experience <- factor(df_new$Experience,
                            levels = c("< 1 years", "1-3 years", "3-5 years",
                                       "5-10 years", "10-20 years", "20+ years",
                                       "I have never written code"),
                            ordered = TRUE)

rf_new <- df_new[,-c(18,19,20)]
rf_new$Swift <- 0
colnames(rf_new)[which(names(rf_new) == "C++")] <- "C_plus_plus"

# Train test split
rf_train_old <- rf_old[train_indices, ]
rf_test_old <- rf_old[train_indices, ]

rf_model <- randomForest(Salary_est ~ ., data = rf_train_old)

# Predict
rf_predict_old <- predict(rf_model, rf_test_old)
rf_predict_new <- predict(rf_model, rf_new)

# Evaluate
mse_old <- mean((rf_test_old$Salary_est - rf_predict_old)^2)
rmse_old <- sqrt(mean((rf_test_old$Salary_est - rf_predict_old)^2))
r2_score_old <- R2(rf_predict_old, rf_test_old$Salary_est)
mae_old <- MAE(rf_predict_old, rf_test_old$Salary_est)

mse_new <- mean((rf_new$Salary_est - rf_predict_new)^2)
rmse_new <- sqrt(mean((rf_new$Salary_est - rf_predict_new)^2))
r2_score_new <- R2(rf_predict_new, rf_new$Salary_est)
mae_new <- MAE(rf_predict_new, rf_new$Salary_est)

mse <- c(mse_old, mse_new)
rmse <- c(rmse_old, rmse_new)
r2_score <- c(r2_score_old, r2_score_new)
mae <- c(mae_old, mae_new)
data_name <- c("Original data", "New data")

rf_results <- data.frame(data_name, mse, rmse, r2_score, mae)



