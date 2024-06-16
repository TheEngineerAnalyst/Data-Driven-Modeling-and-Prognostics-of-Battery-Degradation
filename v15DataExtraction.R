#setwd("C:/Users/mkim3/OneDrive - University of Florida/teaching_UG research/Battery prognostics Bubis")
#setwd("/Users/beccabubis/Desktop/Undergraduate Research/Datasets/Calce Data")


setwd("/Users/beccabubis/Desktop/Undergraduate Research")

# install.packages(c("tidyverse", "openxlsx", "readxl")) 
library(tidyverse)
library(openxlsx)
library(readxl)
library(ggplot2)

#Note: recursive to check subdirectories
#excel_files <- list.files(path = "Peoriodical_Characterization_Data", pattern = ".xlsx", full.names = TRUE, recursive = TRUE)
#excel_files <- list.files(path = "Peoriodical Characterization Data", pattern = ".xlsx", full.names = TRUE, recursive = TRUE)
#all_data <- tibble()

all_data <- read_csv("all_data.csv")
counter <- 1  # Initialize counter

#for (file in excel_files) {
# Read each sheet directly using readxl library
#  sheet_names <- excel_sheets(file)
#  cycle <- as.numeric(sub(".*?/DOE-([0-9]+)-capacity.*", "\\1", file))
#  condition <- as.numeric(sub(".*-capacity ([0-9]+).*", "\\1", file))
#  global <- read_excel(file, "Global_Info")
#  cat("Cycle:", cycle, "\n")

#  for (sheet in sheet_names) {
#    if (grepl("^Statistics_[0-9]+$", sheet) | grepl("^StatisticsByCycle-Chan_[0-9]+$", sheet)) {
#      data <- read_excel(file, sheet)
#      channel <- as.numeric(gsub("[^0-9]", "", sheet))
#      sample <- global[global[,"Channel"]==channel,"Sample No"]
#      sample <- as.numeric(gsub(".*-", "", sample))
#      
# Check if there is data in the sheet since some sheets had none
#      if (nrow(data) > 0) {
# Print the sheet name and column names, acts as a check to identify the correct names
#        capacity = data[1,'Discharge_Capacity(Ah)']
#        selected_data <- data.frame(Cycle = cycle, Sample_ID = sample, Condition = condition, Capacity = capacity)
#        all_data <- bind_rows(all_data, selected_data)
#      } else {
#        cat("Sheet", sheet, "in file", file, "has no data.\n")
#      }
#   }
#  }
#}

#write.csv(all_data, "/Users/beccabubis/Desktop/Undergraduate Research/Coding Portion\\all_data.csv", row.names=FALSE)

# Add new columns to all_data dataframe
# Used case_when function to vectorise if_else() statements
# Used mutate function to add columns to dataframe based on existing columns in existing one
# For Stress Factor 1, assigned specific value based on range
# For Stress Factor 2, assigned value base don odd or even coniditon
# For Stress Factor 3, assigned specific value based on range

all_data <- all_data %>%
  mutate(
    "SF1" = case_when(
      Condition %in% c(1, 2, 3, 4, 5, 6) ~ "10",
      Condition %in% c(7, 8, 9, 10, 11, 12) ~ "25",
      Condition %in% c(13, 14, 15, 16, 17, 18) ~ "45",
      Condition %in% c(19, 20, 21, 22, 23, 24) ~ "60"
    ),
    "SF2" = case_when(
      Condition %% 2 == 1 ~ "1/5",
      TRUE ~ "1/40"
    ),
    "SF3" = case_when(
      Condition %in% c(1, 2, 7, 8, 13, 14, 19, 20) ~ "0.7",
      Condition %in% c(3, 4, 9, 10, 15, 16, 21, 22) ~ "1",
      TRUE ~ "2"
    )
  )

# sum_data <- all_data %>%
#  group_by(Cycle, Condition) %>%
#  summarize(
#    Cycle = mean(Cycle, na.rm = TRUE),
#    Discharge_Capacity.Ah. = mean(Discharge_Capacity.Ah., na.rm = TRUE),
#    Condition  = mean(Condition , na.rm = TRUE)
#  )

#write.csv(sum_data, "/Users/beccabubis/Desktop/Undergraduate Research/Coding Portion\\sum_data.csv", row.names=FALSE)
sum_data <- read_csv("sum_data.csv")

sum_data <- sum_data %>%
  mutate(
      "SF1" = case_when(
        Condition %in% c(1, 2, 3, 4, 5, 6) ~ "10",
        Condition %in% c(7, 8, 9, 10, 11, 12) ~ "25",
        Condition %in% c(13, 14, 15, 16, 17, 18) ~ "45",
        Condition %in% c(19, 20, 21, 22, 23, 24) ~ "60"
      ),
      "SF2" = case_when(
        Condition %% 2 == 1 ~ "1/5",
        TRUE ~ "1/40"
      ),
      "SF3" = case_when(
        Condition %in% c(1, 2, 7, 8, 13, 14, 19, 20) ~ "0.7",
        Condition %in% c(3, 4, 9, 10, 15, 16, 21, 22) ~ "1",
        TRUE ~ "2"
      )
    )

sum_data$Condition <- factor(sum_data$Condition)

ggplot(sum_data[sum_data[,"Stress Factor 1"]==10,], aes(x = Cycle, y = Discharge_Capacity.Ah., color = Condition)) +
  geom_point() +
  geom_line(aes(color=Condition)) +
  theme_minimal() 


# Common to use discharge capacity since it reflects the health of the battery
# Currently using Cycle_Index column to graph, will change it later
plot(all_data$Cycle, all_data$`Stress Factor 1 (ºC)`, type = "l", col = "blue", xlab = "Cycle", ylab = "Temperature", main = "Discharge Capacity vs. Cycle for RUL Estimation")


# Model 1: Linear Mixed-Effect Regression

# install.packages("emmeans")
# install.packages("sjstats")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("MuMIn")
# install.packages("Matrix")

library(emmeans)  # estimated marginal means and p values
library(sjstats) # partial eta squared and cohens f effect size
library(lme4) # estimated the multi level model (random intercept)
library(lmerTest) # gives more comprehensive anova output with p values
library(MuMIn) # R2 for the model
library(Matrix)

# discharge capacity predicted by SF1
SF1eff <- lmer(sum_data$Discharge_Capacity.Ah. ~ as.factor(sum_data$SF1) + (1|sum_data$SF2)) # SF2 is random effect/variable

# summary of model
summary(SF1eff)

# show model as anova: statistically significant?
anova(SF1eff)

# partial eta sq: how large of an effect the independent variable(s) had on the dependent variable
effectsize::eta_squared(SF1eff, partial = TRUE)



# Model 2: Gaussian Process Regression


