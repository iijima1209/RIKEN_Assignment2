# --- Basic R Script to Calculate the Square Root of 10 ---
# Step 1: Define the number
number <- 10
# Step 2: Calculate the square root of the number
sqrt_result <- sqrt(number)
# Step 3: Display the result in the console
print(sqrt_result)


# ---2を底とする32の対数を求めるRのコマンド ---
# Step 1: Calculate the base-2 logarithm of 32
log_result <- log(32, base = 2)
# Step 2: Print the result
print(log_result)


# Create a sequence from 1 to 1000
numbers <- 1:1000
# Step 1: Calculate the sum
total_sum <- sum(numbers)
# Step 2: Display the result
total_sum


# Create a sequence of even numbers from 2 to 1000
even_numbers <- seq(2, 1000, by = 2)
# Display the first few numbers
head(even_numbers)
# Sum all even numbers from 2 to 1000
sum_even <- sum(even_numbers)
# Display the result
sum_even


# --- Calculate the numbur of pairwise comparisons for 100 genes ---
# Step 1: Define the number of genes
num_genes <- 100
# Step 2: Calculate the number of Calpairwise comparisons (n choose 2)
pairwise_comparisons <- choose(num_genes, 2)
# Step 3: Display the result
print(pairwise_comparisons)


# --- How many ways are there to choose 3 genes at a time from 100 genes ---
# Step 1: Define the number of genes
num_genes <- 100
# Step 2: Calculate the number of combinations choosing 3 genes at a time
triplet_combinations <- choose(num_genes, 3)
# Step 3: Display the result
print(triplet_combinations)


# ---task 5-2 ---
help("CO2")


# --- task 5-3 ---
# Step 1: Load the CO2 dataset (included in R by default)
data("CO2")
# Step 2: Load the dplyr package for data manipulation
library(dplyr)
# Step 3: Group the data by plant Type and calculate summary statistics
CO2 %>%
  group_by(Type) %>%
  summarise(
    mean_uptake = mean(uptake),
    median_uptake = median(uptake)
  )


# ---task 6-2 ---
# Define a function to return mean, median, and ratio in a data frame
mean_median_ratio_df <- function(mean_value, median_value) {
  # Calculate ratio
  ratio <- mean_value / median_value
  
  # Return as a data frame
  result <- data.frame(
    mean = mean_value,
    median = median_value,
    ratio = ratio
  )
  
  return(result)
}

# Example usage 1
result1 <- mean_median_ratio_df(mean_value = 33.5, median_value = 37.2)
print(result1)
# Example usage 2
result2 <- mean_median_ratio_df(mean_value = 20.9, median_value = 19.3)
print(result2)



# ---task 6-3 ---
# Step 1: Load the CO2 dataset (included in R by default)
data(CO2)

# Step 2: Load the dplyr package for data manipulation
library(dplyr)

# Step 3: Group the data by plant Type and calculate summary statistics
CO2 %>%
  group_by(Type) %>%
  summarise(
    mean_uptake = mean(uptake),
    median_uptake = median(uptake)
  )




