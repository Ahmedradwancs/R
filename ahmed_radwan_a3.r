# Load necessary libraries
library(ggplot2)
library(corrplot)
library(car)

# Read the dataset
dataset='C:/Users/AHRA0004/Desktop/real-estate-taiwan.csv'
data <- read.csv(dataset)
print("Data loaded successfully.")

# View the first few rows of the dataset
print(head(data))

# Summary statistics
print("Summary statistics of the dataset:")
print(summary(data))

# Check for missing values
print(paste("Number of missing values in the dataset:", sum(is.na(data))))

# EDA: Histogram of house prices
print("Generating histogram of house prices...")
ggplot(data, aes(x = house_price)) + 
  geom_histogram(binwidth = 5, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution of House Prices", x = "House Price", y = "Count")

# Scatter plot of house price vs house age with linear regression line
print("Generating scatter plot for house price vs house age...")
ggplot(data, aes(x = house_age, y = house_price)) +
  geom_point() +
  geom_smooth(method = "lm", color = "red") +
  theme_minimal() +
  labs(title = "House Price vs House Age", x = "House Age", y = "House Price")

# Correlation matrix
print("Calculating correlation matrix...")
cor_matrix <- cor(data[, -1])  # Exclude the first column (transaction_date)
print(cor_matrix)

print("Generating correlation plot...")
# Reset graphic device
dev.off()

# Open a new graphics window with larger dimensions
windows(width = 10, height = 10)  # On Windows (quartz() for macOS, x11() for Linux)

# Increase margins and reduce text size
corrplot(cor_matrix, method = "circle", tl.cex = 0.6, tl.col = "black", mar = c(5, 5, 5, 5))

ggsave("correlation_plot.png", width = 10, height = 10)

# Linear regression model with all predictors
print("Building full linear regression model...")
model_full <- lm(house_price ~ ., data = data)
print("Summary of the full model:")
print(summary(model_full))

# Using VIF to check for multicollinearity
print("Calculating VIF for the full model:")
print(vif(model_full))

# Based on VIF and significance levels, refine the model
# High VIF and p-values indicate multicollinearity or non-significance
print("Building refined linear regression model...")
model_refined <- lm(house_price ~ house_age + mrt_distance + convenience_stores, data = data)
print("Summary of the refined model:")
print(summary(model_refined))

# Save model summary to a file
summary_file <- capture.output(summary(model_refined))
writeLines(summary_file, "refined_model_summary.txt")

# Model diagnostics
print("Generating model diagnostic plots...")
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
plot(model_refined)
