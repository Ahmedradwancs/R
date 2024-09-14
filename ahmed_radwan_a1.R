library(tidyverse)
data(msleep)
?msleep

# 1. Convert into factor
msleep <- msleep %>%
  mutate(
    genus = as.factor(genus),
    vore = as.factor(vore),
    order = as.factor(order),
    conservation = as.factor(conservation)
  )


# 2. Shortest sleep time
shortest_sleep_animal <- msleep %>% arrange(sleep_total) %>% slice(1)
shortest_sleep <- shortest_sleep_animal$sleep_total
shortest_sleep_mammal <- shortest_sleep_animal$name


# 3. Most missing
missing_data <- colSums(is.na(msleep))
most_missing <- names(which.max(missing_data))
missing_values <- max(missing_data)


# 4. Correlations
numeric_cols <- sapply(msleep, is.numeric)
correlations <- cor(msleep[, numeric_cols], use = "pairwise.complete.obs")


# 5. Highest correlation
# Create a copy for modification
correlations2 <- correlations
correlations2[upper.tri(correlations2, diag = TRUE)] <- NA

correlations2["sleep_total", "awake"] <- NA
correlations2["awake", "sleep_total"] <- NA

flat_corr <- as.vector(correlations2)
flat_corr <- flat_corr[!is.na(flat_corr)]

sorted_corr <- sort(flat_corr, decreasing = TRUE)

# The highest correlation
highest_corr <- sorted_corr[1]


# 6. Sleep time distribution
sleep_histogram <- ggplot(msleep, aes(x = sleep_total)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black")


# 7. Bar chart for food categories
food_barchart <- ggplot(msleep, aes(x = vore)) +
  geom_bar(fill = "red", color = "black")


# 8. Grouped box plot for sleep time
sleep_boxplot <- ggplot(msleep, aes(x = vore, y = sleep_total)) +
  geom_boxplot(fill = "green", color = "black")


# 9. Longest average sleep time
average_sleep <- aggregate(sleep_total ~ vore, data = msleep, mean, na.rm = TRUE)
highest_average <- max(average_sleep$sleep_total, na.rm = TRUE)


# 10. REM sleep vs. total sleep, colored by order
sleep_scatterplot <- ggplot(msleep, aes(x = sleep_total, y = sleep_rem, color = order)) +
  geom_point(na.rm = TRUE) 


# 11. REM sleep vs. total sleep for the order most common in the data
common_order <- names(which.max(table(msleep$order)))
sleep_scatterplot2 <- ggplot(msleep %>% filter(order == common_order), aes(x = sleep_total, y = sleep_rem)) +
  geom_point(na.rm = TRUE)


