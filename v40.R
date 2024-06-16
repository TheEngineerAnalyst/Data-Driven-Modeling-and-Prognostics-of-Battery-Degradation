#setwd("C:/Users/mkim3/OneDrive - University of Florida/teaching_UG research/Battery prognostics Bubis")
#setwd("/Users/beccabubis/Desktop/Undergraduate Research/Datasets/Calce Data")


#setwd("/Users/beccabubis/Desktop/Undergraduate Research")
setwd("C:/Users/Rebecca Bubis/OneDrive - University of Florida/Desktop/Undergraduate Research")

#install.packages(c('tidyverse', 'openxlsx', 'readxl', 'ggplot2', 'Matrix', 'GauPro','neuralnet', 'kernlab', 'lme4'))

library(tidyverse) # transforms data
library(openxlsx) # writes .xlsx files
library(readxl) # get data from Excel into R
library(ggplot2) # data visualization
library(Matrix) # linear algebra operations (i.e., vectors)
library(GauPro) # GPR
library(neuralnet) # neural network
library(kernlab) # kernel for GPR model
library(lme4) # linear mixed-effect model

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
LLM_model <- lmer(Discharge_Capacity.Ah. ~ Ambient_Temperature + Charge_Cut_Off_Current + Discharge_Rate + Cycle + (1|Sample_ID), data = all_data)

# Print the summary of the model
summary(LLM_model)

#version 37 for normalized metrics

######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################
######################################################################################################################


### CHECK V31 FOR ORIGINAL CODE



# Check for missing values and replace NA with 0 (kept getting errors so added this check)
if (anyNA(sum_data)) {
  sum_data[is.na(sum_data)] <- 0 
}

# Convert non-numeric columns to numeric (kept getting errors so added this step)
sum_data <- as.data.frame(sapply(sum_data, as.numeric))

# Normalize data
#sum_data_norm <- as.data.frame(scale(sum_data))
normalized_columns <- scale(sum_data[, !(names(sum_data) %in% c("Cycle", "Condition"))])  #normalize everything except Cycle and Condition
sum_data_norm <- cbind(sum_data[c("Cycle", "Condition")], as.data.frame(normalized_columns)) # combine Cycle and Condition with normalized data
sum_data_norm <- sum_data_norm[sum_data_norm[,3]>-3,]  # removes outliers

# Define evaluation metrics dataframe outside the loop so it doesn't make a new one each time
evaluation_metrics <- data.frame(
  Model = rep(c("GPR", "NN"), each = 24),
  R_squared = numeric(24),
  Median_Absolute_Error = numeric(24),
  Median_Percentage_Error = numeric(24)
)


# START EPIC LOOP
for (i in 1:24) {
  train_data <- sum_data_norm[sum_data_norm$Condition != i, ] 
  test_data <- sum_data_norm[sum_data_norm$Condition == i, ] 
  
  # Select columns to assign as variables
  train_matrix <- train_data[, c(1, 4:6)]  # Independent variable for training model
  response_vector <- train_data[, 3] # Dependent variable for training model
  test_matrix <- test_data[, c(1, 4:6)] # Independent variable for testing model
  test_response_vector <- test_data[, 3] # Dependent variable for testing model
  
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  ### GPR
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  
  GPR_model <- GauPro(as.matrix(train_matrix), response_vector, kernel = rbfdot$new(2.5), nugget = 0.1) # changed kernel because more consistent across observations, higher R-squared, lower MAE & MPE
  GPR_predictions <- predict(GPR_model, as.matrix(test_matrix))  #TODO: $mean?
  
  # Plot only when testing data condition is 1
  if(i==24){
    insert_imputed_rows <- function(df) {
      n <- nrow(df)
      for (i in seq(1, n - 1, by = 2)) {
        next_cycle <- (df$Cycle[i] + df$Cycle[i + 1]) / 2
        new_row <- df[i, ]
        new_row$Cycle <- next_cycle
        df <- rbind(df[1:i, ], new_row, df[(i + 1):n, ])
        n <- nrow(df)  # Update the number of rows after insertion
      }
      return(df[order(df$Cycle), ])  # Return the dataframe sorted by Cycle
    }
    
    # Insert the imputed rows
    test_matrix_imputed <- insert_imputed_rows(test_matrix)
    test_matrix_imputed <- insert_imputed_rows(test_matrix_imputed)
    
    GPR_predictions_imputed <- predict(GPR_model, as.matrix(test_matrix_imputed), se = T)  
    
    plot(test_matrix$Cycle,test_response_vector, main = "Actual vs. Predicted Values for Condition 24", xlab = "Cycle", ylab = "Normalized Discharge Capacity")
    lines(test_matrix_imputed$Cycle, GPR_predictions_imputed[,1], col = "red")
    lines(test_matrix_imputed$Cycle, GPR_predictions_imputed[,1]+GPR_predictions_imputed[,3], col = "blue")
    lines(test_matrix_imputed$Cycle, GPR_predictions_imputed[,1]-GPR_predictions_imputed[,3], col = "blue")
    # Red: Mean prediction
    # Blue: Mean prediction +- S.d. prediction
  }
  
  # Find mean and sd of each column in sum_data
  mean_vector <- colMeans(sum_data)[3] 
  sd_vector <- apply(sum_data, 2, sd)[3]
  
  # Reverse normalization for test_response_vector
  unnormalized_test_response_vector <- test_response_vector * sd_vector + mean_vector
  
  # Reverse normalization for GPR_predictions
  unnormalized_GPR_predictions <- GPR_predictions * sd_vector + mean_vector
  
  # Metrics
  GPR_median_absolute_error <- median(abs(unnormalized_test_response_vector - unnormalized_GPR_predictions))
  GPR_median_percentage_error <- median(abs((unnormalized_test_response_vector- unnormalized_GPR_predictions)/unnormalized_test_response_vector)) * 100
  GPR_R_squared <- cor(unnormalized_test_response_vector, unnormalized_GPR_predictions)^2
  cat("GPR Median Absolute Error (MAE):", GPR_median_absolute_error, "\n")
  cat("GPR Median Percentage Error (MPE):", GPR_median_percentage_error, "\n")
  cat("GPR R-squared (R^2):", GPR_R_squared, "\n")
  
  
  
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  ### NNM
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  
  NN_model <- neuralnet(
    Discharge_Capacity.Ah. ~ Cycle + Ambient_Temperature + Discharge_Rate + Charge_Cut_Off_Current,
    data = train_data,
    hidden = c(16, 12, 8), # 8, 6, 4 worked
    threshold = 0.01,
    act.fct = "logistic", #logistic, tanh
    linear.output = FALSE
  )
  
  # IF want to plot a NN model for PAPER
  # plot(NN_model,rep = "best")
  
  #NN_predictions <- predict(NN_model, test_data)
  NN_predictions <- predict(NN_model, as.matrix(test_matrix))
  
  NN_median_absolute_error <- median(abs(test_response_vector - NN_predictions))
  NN_median_percentage_error <- median(abs((test_response_vector- NN_predictions)/test_response_vector)) *100
  NN_R_squared <- cor(test_response_vector, NN_predictions)^2
  cat("NN Median Absolute Error (MAE):", NN_median_absolute_error, "\n")
  cat("NN Median Percentage Error (MPE):", NN_median_percentage_error, "\n")
  cat("R-squared (R2):", NN_R_squared, "\n")
  
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  ### Evaluation Metrics Matrix
  ##### ----------------------------------------------------------------------------------------------------------------
  ##### ----------------------------------------------------------------------------------------------------------------
  
  # Assign metrics for GPR model
  evaluation_metrics[i, "R_squared"] <- GPR_R_squared
  evaluation_metrics[i, "Median_Absolute_Error"] <- GPR_median_absolute_error
  evaluation_metrics[i, "Median_Percentage_Error"] <- GPR_median_percentage_error
  evaluation_metrics[i+24, "R_squared"] <- NN_R_squared
  evaluation_metrics[i+24, "Median_Absolute_Error"] <- NN_median_absolute_error
  evaluation_metrics[i+24, "Median_Percentage_Error"] <- NN_median_percentage_error
  
  # Print dataframe
  print(evaluation_metrics)
}









