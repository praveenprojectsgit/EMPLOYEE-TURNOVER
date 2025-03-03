#PACKAGES
if (!require(corrplot)) install.packages("corrplot")
if (!require(lsr)) install.packages("lsr")
if (!require(reshape2)) install.packages("reshape2")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(plotly)) install.packages("plotly")
if (!require(scales)) install.packages("scales")
if (!require(randomForest)) install.packages("randomForest")
if (!require(rpart)) install.packages("rpart")
if (!require(rpart.plot)) install.packages("rpart.plot")
if (!require(caret)) install.packages("caret")
if (!require(class)) install.packages("class")
if (!require(e1071)) install.packages("e1071")

#LIBRARIES
library(corrplot)
library(lsr)
library(reshape2)
library(ggplot2)
library(dplyr)
library(plotly)
library(scales)
library(randomForest)
library(rpart)
library(rpart.plot)
library(caret)
library(class)
library(e1071)

Employee <- read.csv("C:/Users/prave/OneDrive/Desktop/3-1/AP/mine/Employee.csv")

#DATA EXPLORATION#
head(Employee)
sum(is.na(Employee))
str(Employee)
dim(Employee)
summary(Employee)
sum(is.null(Employee))
zero_counts <- sapply(Employee, function(x) sum(x == 0, na.rm = TRUE))
zero_counts

#FEATURE EXPLORATION#
# Combined function to create plots
create_plot <- function(data, column_name, title, x_label, binwidth = NULL) {
  p <- ggplot(data, aes_string(x = column_name)) +
    theme_minimal() +
    ggtitle(title) +
    ylab("Count") +
    xlab(x_label)
  
  if (!is.null(binwidth)) {
    # If binwidth is provided, create a histogram
    p <- p + geom_histogram(binwidth = binwidth, fill = "cyan", color = "black") +
      stat_bin(binwidth = binwidth, geom = "text", aes(label = ..count..), vjust = -0.5, color = "black")
  } else {
    # Otherwise, create a bar plot
    p <- p + geom_bar(fill = "cyan", color = "black") +
      geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5, color = "black")
  }
  
  return(p)
}

# Convert categorical variables to factors with specified order if needed
Employee$salary <- factor(Employee$salary, levels = c("low", "medium", "high"))

# Generate histograms
satisfaction_level <- create_plot(Employee, "satisfaction_level", "Distribution of Satisfaction Level", "Satisfaction Level", binwidth = 0.05)
last_evaluation <- create_plot(Employee, "last_evaluation", "Distribution of Last Evaluation", "Last Evaluation", binwidth = 0.05)
number_project <- create_plot(Employee, "number_project", "Distribution of Number of Projects", "Number of Projects", binwidth = 1)
average_montly_hours <- create_plot(Employee, "average_montly_hours", "Distribution of Average Monthly Hours", "Average Monthly Hours", binwidth = 10)
time_spend_company <- create_plot(Employee, "time_spend_company", "Distribution of Time Spent at Company", "Time Spent at Company (Years)", binwidth = 1)

# Generate bar plots
work_accident_plot <- create_plot(Employee, "factor(Work_accident)", "Distribution of Work Accidents", "Work Accident (0 = No, 1 = Yes)")
left_plot <- create_plot(Employee, "factor(left)", "Distribution of Employees Who Left", "Left (0 = No, 1 = Yes)")
promotion_plot <- create_plot(Employee, "factor(promotion_last_5years)", "Distribution of Promotions in Last 5 Years", "Promotion in Last 5 Years (0 = No, 1 = Yes)")
sales_plot <- create_plot(Employee, "sales", "Distribution of Employees Across Departments", "Department")
salary_plot <- create_plot(Employee, "salary", "Distribution of Salary Levels", "Salary Level")

# Display the plots
satisfaction_level
last_evaluation
number_project
average_montly_hours
time_spend_company
work_accident_plot
left_plot
promotion_plot
sales_plot
salary_plot
#PLOT SHOWING RELEVANCY BETWEEN TARGET OUTCOME (LEFT) AND CATEGORICAL VARIABLES.
categorical_columns <- c("Work_accident", "promotion_last_5years", "sales", "salary", "left")
cramers_v_matrix <- matrix(NA, nrow = length(categorical_columns), ncol = length(categorical_columns),
                           dimnames = list(categorical_columns, categorical_columns))

# Calculate Cramér's V for each pair of categorical variables
for (i in 1:length(categorical_columns)) {
  for (j in i:length(categorical_columns)) {
    if (i != j) {
      cramers_v_matrix[i, j] <- cramersV(table(Employee[[categorical_columns[i]]], Employee[[categorical_columns[j]]]))
      cramers_v_matrix[j, i] <- cramers_v_matrix[i, j]
    } else {
      cramers_v_matrix[i, j] <- 1  # Diagonal set to 1
    }
  }
}

# Convert the matrix to a long format for ggplot
cramers_v_long <- melt(cramers_v_matrix)
# Plot the heatmap
ggplot(cramers_v_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "steelblue") +
  theme_minimal() +
  labs(title = "Heatmap of Cramér's V Between Categorical Variables", x = "", y = "")
#____________________
#PLOT SHOWING RELEVANCY BETWEEN TARGET OUTCOME (LEFT) AND NUMERICAL VARIABLES.
numeric_data <- Employee[, c('satisfaction_level', 'last_evaluation', 'number_project', 'average_montly_hours', 'time_spend_company', 'left')]
correlations <- cor(numeric_data)
corrplot(correlations, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
#LEFT VS OTHER PREDICTOR'S PROPORTION
# Ensure 'left' variable is treated as a factor
Employee$left <- as.factor(Employee$left)

#___________interactive_plot_sales_____
plot <- ggplot(Employee, aes(x = sales, fill = factor(left))) + 
  geom_bar(position = "fill") +  # Use 'fill' for proportions
  theme_minimal() +
  ggtitle("Proportion of Employees Who Left by Department") +
  xlab("Department") +
  ylab("Proportion") +
  scale_fill_manual(values = c("cyan", "blue"), name = "Left (0 = No, 1 = Yes)") +
  geom_text(stat = "count", aes(label = scales::percent(..count../tapply(..count.., ..x.., sum)[..x..])), 
            position = position_fill(vjust = 0.5), color = "red", size = 3)

# Convert the ggplot object to an interactive plotly object
interactive_plot_sales <- ggplotly(plot)

# Display the interactive plot
interactive_plot_sales
#______ interactive_plot_salary _______

# Create the bar plot for 'left' vs. 'salary'
plot_salary <- ggplot(Employee, aes(x = salary, fill = factor(left))) + 
  geom_bar(position = "fill") +  
  theme_minimal() +
  ggtitle("Proportion of Employees Who Left by Salary Level") +
  xlab("Salary Level") +
  ylab("Proportion") +
  scale_fill_manual(values = c("cyan", "blue"), name = "Left (0 = No, 1 = Yes)") +
  geom_text(stat = "count", aes(label = after_stat(scales::percent(count / sum(count)))), 
            position = position_fill(vjust = 0.5), color = "red", size = 3)

# Convert to an interactive plotly object
interactive_plot_salary <- ggplotly(plot_salary)

# Display the interactive plot
interactive_plot_salary

#______ interactive_plot_accident ______

# Calculate the counts and proportions for 'left' vs. 'Work_accident'
Employee_plot_data_accident <- Employee %>%
  group_by(Work_accident, left) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count), 
         proportion = count / total)

# Create the bar plot for 'left' vs. 'Work_accident' with counts and proportions
plot_accident <- ggplot(Employee_plot_data_accident, aes(x = factor(Work_accident), 
                                                         y = proportion, fill = factor(left))) + 
  geom_bar(stat = "identity", position = "fill") +  
  theme_minimal() +
  ggtitle("Proportion of Employees Who Left by Work Accident") +
  xlab("Work Accident (0 = No, 1 = Yes)") +
  ylab("Proportion") +
  scale_fill_manual(values = c("cyan", "blue"), name = "Left (0 = No, 1 = Yes)") +
  # Adjust label positions dynamically to place them outside the bar
  geom_text(aes(label = paste0(count, " (", scales::percent(proportion), ")")), 
            position = position_stack(vjust = 0.98), color = "red", size = 3)

# Convert to an interactive plotly object
interactive_plot_accident <- ggplotly(plot_accident)

# Display the interactive plot
interactive_plot_accident

#______ interactive_plot_promotion ______

# Calculate the counts and proportions for 'left' vs. 'promotion_last_5years'
Employee_plot_data_promotion <- Employee %>%
  group_by(promotion_last_5years, left) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(total = sum(count), 
         proportion = count / total)

# Create the bar plot for 'left' vs. 'promotion_last_5years' with counts and proportions
plot_promotion <- ggplot(Employee_plot_data_promotion, aes(x = factor(promotion_last_5years),
                                                           y = proportion, fill = factor(left))) + 
  geom_bar(stat = "identity", position = "fill") +  
  theme_minimal() +
  ggtitle("Proportion of Employees Who Left by Promotion in Last 5 Years") +
  xlab("Promotion in Last 5 Years (0 = No, 1 = Yes)") +
  ylab("Proportion") +
  scale_fill_manual(values = c("cyan", "blue"), name = "Left (0 = No, 1 = Yes)") +
  # Adjust label positions dynamically to place them outside the bar
  geom_text(aes(label = paste0(count, " (", scales::percent(proportion), ")")), 
            position = position_stack(vjust = 0.98), color = "red", size = 3) # Negative vjust to push labels outside

# Convert to an interactive plotly object
interactive_plot_promotion <- ggplotly(plot_promotion)

# Display the interactive plot
interactive_plot_promotion

#______ interactive_density_satisfaction ______

# Create a density plot for 'satisfaction_level' by 'left' with computed counts
Employee_density_data_satisfaction <- Employee %>%
  group_by(left) %>%
  mutate(count = n())  # Compute counts for each group

# Create filled density plot
plot_density_satisfaction <- ggplot(Employee_density_data_satisfaction, aes(x = satisfaction_level, fill = factor(left))) + 
  geom_density(alpha = 0.5, position = "identity") +  # Filled plot with semi-transparent fill
  theme_minimal() +
  ggtitle("Density Plot of Satisfaction Level by Employee Turnover Status") +
  xlab("Satisfaction Level") +
  ylab("Density") +
  scale_fill_manual(values = c("cyan", "blue"), name = "Left (0 = No, 1 = Yes)")

# Convert the ggplot object to an interactive plotly object
interactive_density_satisfaction <- ggplotly(plot_density_satisfaction)

# Display the interactive plot
interactive_density_satisfaction


#______ interactive_density_hours ______

# Create a density plot for 'average_montly_hours' by 'left' with computed counts
Employee_density_data <- Employee %>%
  group_by(left) %>%
  mutate(count = n())  # Compute counts for each group

# Create density plot
plot_density_hours <- ggplot(Employee_density_data, aes(x = average_montly_hours, fill = factor(left))) + 
  geom_density(alpha = 0.5) +  # Semi-transparent fill
  theme_minimal() +
  ggtitle("Density Plot of Average Monthly Hours by Employee Turnover Status") +
  xlab("Average Monthly Hours") +
  ylab("Density") +
  scale_fill_manual(values = c("cyan", "blue"), name = "Left (0 = No, 1 = Yes)")

# Convert the ggplot object to an interactive plotly object
interactive_density_hours <- ggplotly(plot_density_hours)

# Display the interactive plot
interactive_density_hours

#______ interactive_density_projects ______

# Create a density plot for 'number_project' by 'left' with computed counts
Employee_density_data_projects <- Employee %>%
  group_by(left) %>%
  mutate(count = n())  # Compute counts for each group

# Create filled density plot
plot_density_projects <- ggplot(Employee_density_data_projects, aes(x = number_project, fill = factor(left))) + 
  geom_density(alpha = 0.5, position = "identity") +  # Filled plot with semi-transparent fill
  theme_minimal() +
  ggtitle("Density Plot of Number of Projects by Employee Turnover Status") +
  xlab("Number of Projects") +
  ylab("Density") +
  scale_fill_manual(values = c("cyan", "blue"), name = "Left (0 = No, 1 = Yes)")

# Convert the ggplot object to an interactive plotly object
interactive_density_projects <- ggplotly(plot_density_projects)

# Display the interactive plot
interactive_density_projects

#______ interactive_density_evaluation ______

# Create a density plot for 'last_evaluation' by 'left' with computed counts
Employee_density_data_evaluation <- Employee %>%
  group_by(left) %>%
  mutate(count = n())  # Compute counts for each group

# Create filled density plot
plot_density_evaluation <- ggplot(Employee_density_data_evaluation, aes(x = last_evaluation, fill = factor(left))) + 
  geom_density(alpha = 0.5, position = "identity") +  # Filled plot with semi-transparent fill
  theme_minimal() +
  ggtitle("Density Plot of Last Evaluation by Employee Turnover Status") +
  xlab("Last Evaluation") +
  ylab("Density") +
  scale_fill_manual(values = c("cyan", "blue"), name = "Left (0 = No, 1 = Yes)")

# Convert the ggplot object to an interactive plotly object
interactive_density_evaluation <- ggplotly(plot_density_evaluation)

# Display the interactive plot
interactive_density_evaluation

#___________ DIMENSION_REDUCTION ___________

######################### RANDOM FOREST FOR  DIMENSION_REDUCTION ##################################
# Train the Random Forest model
rf_model <- randomForest(left ~ ., data = Employee, importance = TRUE, ntree = 500)

# Extract feature importance
importance <- importance(rf_model)
feature_importance <- data.frame(Feature = row.names(importance), Importance = importance[, 1])

# Sort feature importance in ascending order
feature_importance <- feature_importance[order(feature_importance$Importance), ]

# Print feature importance in ascending order
print(feature_importance)

#All variables are having the predictive power that is needed for model to classify employee turnover.







##________MODEL FOR VARIABLE IMPORTANCE________

# Build the Random Forest model
set.seed(123)  # For reproducibility
rf_model <- randomForest(left ~ ., data = Employee, importance = TRUE, ntree = 500)

# Extract feature importance
importance_values <- importance(rf_model)
feature_importance <- data.frame(Feature = row.names(importance_values), 
                                 MeanDecreaseAccuracy = importance_values[, "MeanDecreaseAccuracy"],
                                 MeanDecreaseGini = importance_values[, "MeanDecreaseGini"])

# Sort feature importance in descending order
feature_importance <- feature_importance[order(-feature_importance$MeanDecreaseAccuracy), ]

# Print feature importance
print(feature_importance)

# Plot feature importance
varImpPlot(rf_model, main = "Feature Importance using Random Forest")

#_______DECISION TREES__________


# Apply Decision Tree Model using the original categorical variables
decision_tree_model <- rpart(left ~ ., data = Employee, method = "class")

# Enhanced plot for the Decision Tree
rpart.plot(decision_tree_model, 
           main = "Decision Tree for Employee Turnover", 
           extra = 106,             # Display both class and probability percentages
           type = 4,                # Make the layout horizontal and more compact
           fallen.leaves = TRUE,     # Position the leaves at the bottom
           cex = 0.6,               # Adjust text size
           box.palette = "RdBu",     # Add color to nodes using Red-Blue color scheme
           shadow.col = "gray",      # Add shadows to boxes for better visibility
           tweak = 1.2)             # Further tweak for better spacing and size

# Print the summary of the Decision Tree
printcp(decision_tree_model)
summary(decision_tree_model)

# Display the variable importance from the Decision Tree
variable_importance <- decision_tree_model$variable.importance
print(variable_importance)

# Optional: Cross-Validation and Pruning
plotcp(decision_tree_model) # Plot cross-validated error rates
pruned_tree <- prune(decision_tree_model, cp = decision_tree_model$cptable[which.min(decision_tree_model$cptable[,"xerror"]),"CP"])

# Enhanced plot for the Pruned Decision Tree
rpart.plot(pruned_tree, 
           main = "Pruned Decision Tree for Employee Turnover", 
           extra = 106, 
           type = 4, 
           fallen.leaves = TRUE, 
           cex = 0.6, 
           box.palette = "GnBu",    # Change palette to Green-Blue for pruned tree
           shadow.col = "gray", 
           tweak = 1.2)


#####################################DATA TRANSFORMATION####################################################


#no data transformation is needed.

#######################################DATA PARTITION #######################

# No need to oversample or under sample the dataset, as the dataset has enough proportion of outcome values needed for model to classify.
# Random partitioning is done at 60% of training,20% of validation and 20% of holdout proportions.
# Split the data into training (60%), validation (20%), and holdout (20%) sets
Employee1 <- Employee
set.seed(123)  # For reproducibility
trainIndex <- createDataPartition(Employee$left, p = 0.6, list = FALSE)
training_data <- Employee[trainIndex, ]
temp_data <- Employee[-trainIndex, ]

validationIndex <- createDataPartition(temp_data$left, p = 0.5, list = FALSE)
validation_data <- temp_data[validationIndex, ]
holdout_data <- temp_data[-validationIndex, ]
#__________ MODEL FOR PREDICTING TURNOVER _______________
#___________ LOGISTIC REGRESSION _________

# Fit the logistic regression model on the training data
logistic_model <- glm(left ~ ., data = training_data, family = binomial)
summary(logistic_model)
# Predict on validation set
validation_predictions <- predict(logistic_model, newdata = validation_data, type = "response")
validation_pred_class <- ifelse(validation_predictions > 0.5, 1, 0)

# Evaluate on validation set
confusion_matrix_validation <- confusionMatrix(factor(validation_pred_class), validation_data$left, positive = "1")
print(confusion_matrix_validation)

# Predict on holdout set
holdout_predictions <- predict(logistic_model, newdata = holdout_data, type = "response")
holdout_pred_class <- ifelse(holdout_predictions > 0.5, 1, 0)

# Evaluate on holdout set
confusion_matrix_holdout <- confusionMatrix(factor(holdout_pred_class), holdout_data$left, positive = "1")
print(confusion_matrix_holdout)

#___________ Naive Bayes ______________

# Train the Naive Bayes model
nb_model <- naiveBayes(left ~ ., data = training_data)
# Predict on validation data
validation_predictions_nb <- predict(nb_model, newdata = validation_data)

# Evaluate the Naive Bayes model on the validation set
confusion_matrix_validation_nb <- confusionMatrix(as.factor(validation_predictions_nb), validation_data$left, positive = "1")
print(confusion_matrix_validation_nb)

# Predict on holdout data
holdout_predictions_nb <- predict(nb_model, newdata = holdout_data)

# Evaluate the Naive Bayes model on the holdout set
confusion_matrix_holdout_nb <- confusionMatrix(as.factor(holdout_predictions_nb), holdout_data$left, positive = "1")
print(confusion_matrix_holdout_nb)

#___________ KNN ________________

# Create dummy variables for 'salary' and 'sales'
Employee_dummies <- model.matrix(~ sales + salary - 1, data = Employee1) %>% 
  as.data.frame()

# Combine the dummy variables with the original dataset, excluding 'sales' and 'salary'
Employee_prepared <- Employee1 %>%
  select(-sales, -salary) %>%
  cbind(Employee_dummies)

# Exclude 'left' column before scaling
data_for_scaling <- Employee_prepared %>% select(-left)

# Scale all variables using Z-score (standardization)
scaled_data <- data_for_scaling %>%
  mutate(across(everything(), scale))

# Add back the 'left' column after scaling
scaled_data <- cbind(scaled_data, left = Employee1$left)

# Split the data into training (60%), validation (20%), and holdout (20%) sets
set.seed(123)  # For reproducibility
train_knn <- createDataPartition(scaled_data$left, p = 0.6, list = FALSE)
trainingknn <- scaled_data[train_knn, ]
training_label <- scaled_data$left[train_knn]

temp <- scaled_data[-train_knn, ]
temp_label <- scaled_data$left[-train_knn]

validation_knn <- createDataPartition(temp_label, p = 0.5, list = FALSE)
validationknn <- temp[validation_knn, ]
validation_label <- temp_label[validation_knn]

holdoutknn <- temp[-validation_knn, ]
holdout_label <- temp_label[-validation_knn]

# Perform KNN classification (k = 5)
k <- 5
validation_predictions <- knn(train = trainingknn %>% select(-left), 
                              test = validationknn %>% select(-left), 
                              cl = training_label, 
                              k = k)
summary(validation_predictions)
# Evaluate the KNN model on the validation set
confusion_matrix_validation <- confusionMatrix(as.factor(validation_predictions), validation_label,positive = "1")
print(confusion_matrix_validation)

# Perform predictions on the holdout set
holdout_predictions <- knn(train = trainingknn %>% select(-left), 
                           test = holdoutknn %>% select(-left), 
                           cl = training_label, 
                           k = k)

# Evaluate the KNN model on the holdout set
confusion_matrix_holdout <- confusionMatrix(as.factor(holdout_predictions), holdout_label,positive = "1")

#___________________________________MODEL FITTING

print(confusion_matrix_holdout)

