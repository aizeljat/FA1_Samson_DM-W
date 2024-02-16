#Lecture 2 (Data Visualiation)

library(tidyverse)
diamonds

#From lecture 3.2 Continuous variables
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat))

#Covariation
#From Lecture 4.1 Discrete versus discrete
ggplot(data = diamonds) +
  geom_count(mapping = aes(x = cut, y = clarity))

# Function to calculate the number of bins for a histogram using the Freedman-Diaconis rule
num_bins_freedman_diaconis <- function(x) {
  IQR_x <- IQR(x)
  n <- length(x)
  if (IQR_x == 0) {
    bins <- 1
  } else {
    bins <- ceiling((max(x) - min(x)) / (2 * IQR_x / nth(sqrt(n), 4)))
  }
  return(bins)
}

# Calculate the mean and standard deviation of a variable
summary_stats <- function(x) {
  mean_x <- mean(x)
  sd_x <- sd(x)
  return(c(mean_x = mean_x, sd_x = sd_x))
}

# Plot: Histogram of 'carat' with custom number of bins and summary statistics
ggplot(data = diamonds) +
  geom_histogram(mapping = aes(x = carat), bins = num_bins_freedman_diaconis(diamonds$carat), fill = "skyblue", color = "black") +
  labs(title = "Distribution of Diamond Carat Weights",
       x = "Carat Weight",
       y = "Count") +
  theme_minimal() +
  geom_vline(aes(xintercept = mean(carat)), color = "red", linetype = "dashed") +
  annotate("text", x = mean(diamonds$carat), y = 5000, label = "Mean", color = "red") +
  annotate("text", x = mean(diamonds$carat), y = 4500, label = paste0(round(summary_stats(diamonds$carat)["mean_x"], 2), " ± ", round(summary_stats(diamonds$carat)["sd_x"], 2)), color = "red")

# Analysis: 
# This plot shows the distribution of diamond carat weights in the dataset.
# The x-axis represents the carat weight of diamonds, and the y-axis represents the count of diamonds falling into each bin.
# In summary, data visualization in R Studio is a versatile and essential tool for exploratory data analysis, insight generation, and communication of findings. With its flexibility, interactivity, and integration with other R packages, ggplot2 enables users to create compelling visualizations that drive understanding and decision-making in data-driven fields.
###############################################################

#Lecture 3 (Data Transformation)
library(tidyverse)
diamonds

#From Lecture 2.2 select()
select(diamonds, carat, cut, color, clarity)

#From Lecture 2.3 arrange()
arrange(diamonds, carat) # sort diamonds by carat (ascending)

library(tidyverse)

# Load the diamonds dataset from ggplot2 package
data("diamonds")

# Use select() function to select specific columns
selected_columns <- select(diamonds, carat, cut, color, clarity)

# Use arrange() function to sort the dataset by carat
sorted_diamonds <- arrange(diamonds, carat)

# Print the selected columns
print("Selected columns:")
print(head(selected_columns))

# Print the sorted dataset
print("Sorted dataset by carat:")
print(head(sorted_diamonds))

# These data manipulation operations (selecting and arranging) are fundamental for preparing data for analysis or visualization.
# Selecting specific columns helps in focusing on relevant variables, while sorting the dataset provides insights into trends or patterns based on the sorted variable.
# In summary, data transformation in RStudio includes a variety of tasks such as cleaning, restructuring, generating, collecting, and summarizing data in preparation for analysis or visualization. RStudio's extensive set of functions and packages for modifying data and alteration makes it an effective tool for data scientists and analysts to discover and derive insights from their data.
#############################################################

#Lecture 4 (Data Wrangling)

#From Lecture Pivoting 4.1 Longer
table4a %>%
  pivot_longer(cols = c(`1999`, `2000`), names_to = "year", values_to = "cases")

#From Lecture Joining
tidy4a <- table4a %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")
tidy4b <- table4b %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")
tidy4a


library(tidyverse)

# Create example data tables
table3 <- tibble(
  Country = c("A", "B", "C"),
  `1999` = c(100, 200, 300),
  `2000` = c(150, 250, 350)
)

table4 <- tibble(
  Country = c("X", "Y", "Z"),
  `1999` = c(500, 600, 700),
  `2000` = c(550, 650, 750)
)

# Apply pivot_longer to table3
tidy_table3 <- table3 %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "cases")

# Apply pivot_longer to table4
tidy_table4 <- table4 %>%
  pivot_longer(c(`1999`, `2000`), names_to = "year", values_to = "population")

# Print the resulting tidy tables
print("Tidy table 3:")
print(tidy_table3)

print("Tidy table 4:")
print(tidy_table4)

# Analysis:
# This example demonstrates the flexibility of the pivot_longer function to pivot multiple columns at once, converting data from wide to long format.
# By transforming the data into a tidy format, it becomes easier to perform analysis and create visualizations that effectively communicate trends and patterns present in the data.
# The resulting tidy tables can be used for further analysis, such as calculating growth rates or comparing values across different years or countries.
# Data wrangling is an important process in the final analysis that lays the foundation for successful data analysis. By purifying, altering, remodeling, gathering, and implementing data, analysts may guarantee that the data set is ready for further analysis, resulting in more precise and significant insights.
