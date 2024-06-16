#setwd("C:/Users/mkim3/OneDrive - University of Florida/teaching_UG research/Battery prognostics Bubis")
#setwd("/Users/beccabubis/Desktop/Undergraduate Research/Datasets/Calce Data")


setwd("/Users/beccabubis/Desktop/Undergraduate Research")

library(tidyverse) # transforms data
library(openxlsx) # writes .xlsx files
library(readxl) # get data from Excel into R
library(ggplot2) # data visualization
library(emmeans)  # estimated marginal means and p values
library(sjstats) # partial eta squared and cohens f effect size
library(lme4) # estimated the multi level model (random intercept)
library(lmerTest) # gives more comprehensive anova output with p values
library(MuMIn) # R2 for the model
library(Matrix) # linear algebra operations (i.e., vectors)
library(pbkrtest) # computes p-values from mixed-effect models
library(GauPro) # GPR
library(neuralnet) # neural network
library(kernlab) # kernel for GPR model
library(caret) # k-fold cross validation 

##### ----------------------------------------------------------------------------------------------------------------
# refer to v15 for code to manually extract data from excel files
##### ----------------------------------------------------------------------------------------------------------------

all_data <- read_csv("all_data.csv")
counter <- 1  # Initialize counter

all_data <- all_data %>%
  mutate(
    "Ambient_Temperature" = case_when(
      Condition %in% c(1, 2, 3, 4, 5, 6) ~ "10",
      Condition %in% c(7, 8, 9, 10, 11, 12) ~ "25",
      Condition %in% c(13, 14, 15, 16, 17, 18) ~ "45",
      Condition %in% c(19, 20, 21, 22, 23, 24) ~ "60"
    ),
    "Charge_Cut_Off_Current" = case_when(
      Condition %% 2 == 1 ~ ".20",
      TRUE ~ ".025"
    ),
    "Discharge_Rate" = case_when(
      Condition %in% c(1, 2, 7, 8, 13, 14, 19, 20) ~ "0.7",
      Condition %in% c(3, 4, 9, 10, 15, 16, 21, 22) ~ "1",
      TRUE ~ "2"
    )
  )

##### ----------------------------------------------------------------------------------------------------------------

sum_data <- read_csv("sum_data.csv")

sum_data <- sum_data %>%
  mutate(
    "Ambient_Temperature" = case_when(
      Condition %in% c(1, 2, 3, 4, 5, 6) ~ "10",
      Condition %in% c(7, 8, 9, 10, 11, 12) ~ "25",
      Condition %in% c(13, 14, 15, 16, 17, 18) ~ "45",
      Condition %in% c(19, 20, 21, 22, 23, 24) ~ "60"
    ),
    "Charge_Cut_Off_Current" = case_when(
      Condition %% 2 == 1 ~ ".20",
      TRUE ~ ".025"
    ),
    "Discharge_Rate" = case_when(
      Condition %in% c(1, 2, 7, 8, 13, 14, 19, 20) ~ "0.7",
      Condition %in% c(3, 4, 9, 10, 15, 16, 21, 22) ~ "1",
      TRUE ~ "2"
    )
  )

# Initial graphs 
sum_data$Condition <- factor(sum_data$Condition)

ggplot(sum_data[sum_data[,"Ambient_Temperature"]==10,], aes(x = Cycle, y = Discharge_Capacity.Ah., color = Condition)) +
  geom_point() +
  geom_line(aes(color=Condition)) +
  theme_minimal() 

##### ----------------------------------------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------------------------
### LMM
##### ----------------------------------------------------------------------------------------------------------------
##### ----------------------------------------------------------------------------------------------------------------

# Fit the linear mixed-effects model with individual factors
LLM_model <- lmer(Discharge_Capacity.Ah. ~ Ambient_Temperature + Charge_Cut_Off_Current + Discharge_Rate + (1|Sample_ID), data = all_data)

# Print the summary of the model
summary(LLM_model)

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################

# Check for missing values and replace NA with 0 (kept getting errors so added this check)
if (anyNA(sum_data)) {
  sum_data[is.na(sum_data)] <- 0 
}

# Convert non-numeric columns to numeric (kept getting errors so added this step)
sum_data <- as.data.frame(sapply(sum_data, as.numeric))

# Normalize data
sum_data_norm <- as.data.frame(scale(sum_data))
sum_data_norm <- sum_data_norm[sum_data_norm[,3]>-3,]  # removes outliers

# START EPIC LOOP
for (i in 1:24) {
  train_data <- sum_data_norm[sum_data["Condition"]!=i, ]
  test_data <- sum_data_norm[sum_data["Condition"]==i, ]
  
  # Select columns to assign as variables
  train_matrix <- train_data[, c(1, 4:6)]  # Excluding column 2 ("Condition") and column 3 "Discharge_Capacity.Ah."
  response_vector <- train_data[, 3] 
  test_matrix <- test_data[, c(1, 4:6)] 
  test_response_vector <- test_data[, 3] 
  
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  ### GPR
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  
#Kernels
  #Matern52$new(0)
  #Exponential$new(0)
  #Periodic$new(0)
  #rbfdot(0)
  
  GPR_model <- GauPro(as.matrix(train_matrix), response_vector, kernel = Matern52$new(0))  
  print(GPR_model)
  GPR_predictions <- predict(GPR_model, as.matrix(test_matrix))  # Convert train_matrix back to matrix for predictions
  
  # Find mean and sd of each column in sum_data
  mean_vector <- colMeans(sum_data)
  sd_vector <- apply(sum_data, 2, sd)
  
  # Reverse normalization for test_response_vector
  unnormalized_test_response_vector <- test_response_vector * sd_vector + mean_vector
  
  # Reverse normalization for GPR_predictions
  unnormalized_GPR_predictions <- GPR_predictions * sd_vector + mean_vector
  
  # Metrics
  GPR_median_absolute_error <- median(abs(unnormalized_test_response_vector - unnormalized_GPR_predictions))
  GPR_R_squared <- cor(unnormalized_test_response_vector, unnormalized_GPR_predictions)^2
  
  # Print the results
  cat("GPR Median Absolute Error (MAE):", GPR_median_absolute_error, "\n")
  cat("GPR R-squared (R^2):", GPR_R_squared, "\n")
  
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  ### NNM
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  
  NN_model = neuralnet(
    Discharge_Capacity.Ah. ~ Cycle + Ambient_Temperature + Discharge_Rate + Charge_Cut_Off_Current,
    data=train_data,
    hidden=c(3,2,4), # multiples of 4, 3, 2
    threshold = 0.01,
    act.fct = "logistic", #logistic, tanh
    linear.output = FALSE
  )
  
  plot(NN_model,rep = "best")
  NN_predictions <- predict(NN_model, test_data)
  
  # Metrics 
  NN_median_absolute_error <- median(abs(test_response_vector - NN_predictions))
  #NN_mean_squared_error <- mean((NN_predictions - test_response_vector[,'Discharge_Capacity.Ah.'])^2)
  #NN_root_mean_squared_error <- sqrt(mean_squared_errors)
  NN_R_squared <- cor(test_response_vector[,'Discharge_Capacity.Ah.'], NN_predictions)^2
  
  #cat("Root Mean Squared Error (RMSE):", NN_root_mean_squared_error, "\n")
  cat("R-squared (R2):", NN_R_squared, "\n")
  cat("NN Median Absolute Error (MAE):", NN_median_absolute_error, "\n")
  
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  ### Evaluation Metrics Matrix
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  
  # Make dataframe to store evaluation metrics for both models
  evaluation_metrics <- data.frame(
    Model = c("GPR", "NN"),
    R_squared = numeric(2),
    Median_Absolute_Error = numeric(2)
  )
  
  # Assign metrics for GPR model
  evaluation_metrics[1, "R_squared"] <- GPR_R_squared
  evaluation_metrics[1, "Median_Absolute_Error"] <- GPR_median_absolute_error
  
  # Assign metrics for NN model
  evaluation_metrics[2, "R_squared"] <- NN_R_squared
  evaluation_metrics[2, "Median_Absolute_Error"] <- NN_median_absolute_error
  
  # Print new dataframe
  print(evaluation_metrics)
  
}



# The warning messages you're seeing indicate that the Cholesky decomposition encountered numerical difficulties, likely due to the matrix not being positive definite. This can happen when the covariance matrix is ill-conditioned or nearly singular.

# Here are some steps you can take to address this issue:

#Increase Numerical Stability: Consider increasing the nugget parameter or adding jitter to the diagonal of the covariance matrix to improve numerical stability. This can help prevent the matrix from becoming numerically singular.
#Check Data: Ensure that your input data is correctly formatted and does not contain any missing or invalid values. Also, verify that the data is appropriate for the specific operations you're performing.
#Regularization: If your model supports it, consider adding regularization to the covariance matrix to help stabilize the computations.
#Review Model Assumptions: Review the assumptions of your Gaussian Process Regression model and verify that they hold for your data. For example, ensure that the covariance function you're using is appropriate for your data.
#Debugging: Use debugging techniques such as printing intermediate results, inspecting variable values, or simplifying the problem to isolate the source of the issue.




