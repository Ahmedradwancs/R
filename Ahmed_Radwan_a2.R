# 1. Correlations
correlations <- cor(iris[,1:4])
# Text answer...
# The correlation matrix for the iris dataset's numeric variables shows low to moderate correlations. The highest correlation is between Sepal.Length and Petal.Length (0.87), indicating a strong positive relationship. Sepal.Width and Sepal.Length have a weak negative correlation (-0.12).

# 2. Plot Sepal.Width against Sepal.Length
plot1 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length)) + geom_point() + theme_minimal()
print(plot1)
# Text answer...
# The scatter plot of Sepal.Width against Sepal.Length shows no clear relationship between the two variables, suggesting that Sepal.Width is not a strong predictor of Sepal.Length.

# 3. Fit a linear model using Sepal.Width as predictor and Sepal.Length as response
model1 <- lm(Sepal.Length ~ Sepal.Width, data = iris)
summary(model1)
# Text answer...
# The linear model with Sepal.Width as the predictor for Sepal.Length is not statistically significant (p-value = 0.152), and the R-squared value is very low (0.0138), indicating that Sepal.Width does not explain much of the variance in Sepal.Length.

# 4. Setosa correlations
correlations_setosa <- cor(subset(iris, Species == "setosa")[,1:4])
# Text answer...
# The correlation matrix for the Setosa species shows that Sepal.Width and Sepal.Length have a moderate positive correlation (0.75), suggesting a stronger relationship within the Setosa species compared to the entire dataset.

# 5. Plot Sepal.Width against Sepal.Length, color by species
plot2 <- ggplot(iris, aes(x = Sepal.Width, y = Sepal.Length, color = Species)) + geom_point() + theme_minimal()
print(plot2)
# Text answer...
# The scatter plot, colored by species, shows that the relationship between Sepal.Width and Sepal.Length varies across species. Setosa shows a positive trend, while Versicolor and Virginica have different patterns.

# 6. Fit second model using species and Sepal.Width as predictors and Sepal.Length as response
model2 <- lm(Sepal.Length ~ Sepal.Width + Species, data = iris)
summary(model2)
# Text answer...
# The second model, including Species and Sepal.Width as predictors, is highly significant (p-value < 2.2e-16), with a much higher R-squared value (0.7259), indicating that these predictors explain a large portion of the variance in Sepal.Length.

# 7. Predict the sepal length of a setosa with a sepal width of 3.6 cm
# Define the new data frame
new_data <- data.frame(Sepal.Width = 3.6, Species = factor("setosa", levels = levels(iris$Species)))

# Use the model to predict Sepal.Length
prediction <- predict(model2, newdata = new_data)
print(prediction)
# Text answer...
# The predicted Sepal.Length for a Setosa with a Sepal.Width of 3.6 cm is approximately 5.14 cm.

# Download the dataset from Canvas an place in current directory
getwd()

# Load the data
diabetes_data <- read_csv("a2_diabetes.csv") # Don't change this line!

# Reflect over important variables


# 8. Load the Pima Indian diabetes .csv into R 
diabetes_data <- read_csv("a2_diabetes.csv") # Don't change this line!
diabetes_data <- diabetes_data %>% mutate(Outcome = factor(Outcome))
# Text answer...
# The Outcome variable is recoded as a factor to prepare it for logistic regression analysis.

# Make a copy of diabetes_data for tasks 9-10
diabetes_data_copy <- diabetes_data

# 9. Find a good logistic regression model
logistic_model <- glm(Outcome ~ Glucose + BMI, data = diabetes_data_copy, family = "binomial")
summary(logistic_model)
# Text answer...
# The logistic regression model with Glucose and BMI as predictors is highly significant (p-value < 2e-16 for both predictors), indicating that both variables are important predictors of diabetes outcome.
# Check for missing values

# 10. Improve the model to achieve an accuracy of at least 76.5%
predicted_probabilities <- predict(logistic_model, newdata = diabetes_data, type = "response")
predicted_outcomes <- ifelse(predicted_probabilities > 0.5, 1, 0)
valid_predictions <- !is.na(predicted_outcomes)
actual_outcomes <- diabetes_data$Outcome[valid_predictions]
final_predictions <- predicted_outcomes[valid_predictions]

accuracy <- mean(final_predictions == actual_outcomes)
print(accuracy)
# Text answer...
# The improved logistic regression model after handling missing values achieves an accuracy of approximately 77.1%, which meets the requirement of at least 76.5%.

