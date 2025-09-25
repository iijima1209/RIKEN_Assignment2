# --- Basic R Script to Calculate the Square Root of 10 ---
# Step 1: Define the number
number <- 10
# Step 2: Calculate the square root of the number
sqrt_result <- sqrt(number)
# Step 3: Display the result in the console
print(sqrt_result)
#[1] 3.162278

#comment
# The square root of the number 10 is approximately 3.162.
# In R, the sqrt() function allows us to calculate square roots easily.
# R provides a reliable and quick way to perform mathematical calculations.
# This result confirms the mathematical fact that √10 ≈ 3.162.


# ---R command to calculate the base-2 logarithm of 32 ---
# Step 1: Calculate the base-2 logarithm of 32
log_result <- log(32, base = 2)
# Step 2: Print the result
print(log_result)
#[1] 5

#comment
# The base-2 logarithm of 32 is 5.
# This confirms the exponential relationship that 2^5 = 32.
# Using the log() function in R, we can easily compute logarithms with any base.
# The result demonstrates the correctness of the logarithmic-exponential relationship.


# ---Create a sequence from 1 to 1000---
numbers <- 1:1000
# Step 1: Calculate the sum
total_sum <- sum(numbers)
# Step 2: Display the result
total_sum
#[1] 500500

#comment
# From this output, we learn that the sum of the integers from 1 to 1000 is 500,500.
# This matches the mathematical formula for the sum of the first n natural numbers:
# Sum = n(n + 1) / 2


# Create a sequence of even numbers from 2 to 1000
even_numbers <- seq(2, 1000, by = 2)
# Display the first few numbers
head(even_numbers)
# Sum all even numbers from 2 to 1000
sum_even <- sum(even_numbers)
# Display the result
sum_even
#[1] 250500

#comment
# From this output, we learn that the sum of all even numbers from 2 to 1000 is 250,500.
# Sum = n/2 * (first_term + last_term)
# So, Sum = 500/2 * (2 + 1000) = 250 * 1002 = 250,500.
# Therefore, the R calculation confirms the arithmetic sequence formula.


# --- Calculate the numbur of pairwise comparisons for 100 genes ---
# Step 1: Define the number of genes
num_genes <- 100
# Step 2: Calculate the number of Calpairwise comparisons (n choose 2)
pairwise_comparisons <- choose(num_genes, 2)
# Step 3: Display the result
print(pairwise_comparisons)
#[1] 4950

#comment
# From this output, we learn that the number of pairwise comparisons among 100 genes is 4,950.
# This result comes from the combinatorial formula "n choose 2":
# C(n, 2) = n * (n - 1) / 2
# C(100, 2) = 100 * 99 / 2 = 4,950
# Therefore, the R calculation confirms how combinatorics can be used to determine the number of unique pairs in a dataset.


# --- How many ways are there to choose 3 genes at a time from 100 genes ---
# Step 1: Define the number of genes
num_genes <- 100
# Step 2: Calculate the number of combinations choosing 3 genes at a time
triplet_combinations <- choose(num_genes, 3)
# Step 3: Display the result
print(triplet_combinations)
#[1] 161700

#comment
# From this output, we learn that the number of ways to choose 3 genes 
# at a time from 100 genes is 161,700.
# This result comes from the combinatorial formula "n choose k":
#     C(n, k) = n! / (k! * (n - k)!)
# Substituting n = 100 and k = 3 gives:
# C(100, 3) = 100 * 99 * 98 / (3 * 2 * 1) = 161,700
# Therefore, the R calculation confirms that combinatorics can be used to determine the number of unique triplets in a dataset.


# ---task 5-2 ---
help("CO2")
#CO2 is a dataset containing the results of an experiment on the carbon dioxide uptake of grass plants under different treatment conditions and CO2 concentrations. It has 84 observations on 5 variables: Plant, Type, Treatment, conc, and uptake.

#comment
# From the output of help("CO2"), we learn that the CO2 dataset is a built-in dataset in R, containing information on the effect of different CO2 
# concentrations on the growth of grass plants.
# The dataset is structured as a data frame with variables such as:
# - Plant
# - Type
# - Treatment
# - conc
# - uptake


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
## A tibble: 2 × 3
#Type              mean_uptake median_uptake
#<fct>                <dbl>         <dbl>
#1 Quebec             33.5          37.2
#2 Mississippi        20.9          19.3

# comment
# Quebec plants show a higher mean uptake (33.5) and median uptake (37.2).
# Mississippi plants show a lower mean uptake (20.9) and median uptake (19.3).
# The conclusion is that plant type plays a crucial role in CO2 uptake efficiency.


# ---task 6-1 ---
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
#  mean   median     ratio
#1 33.5    37.2.   0.9005376
# Example usage 2
result2 <- mean_median_ratio_df(mean_value = 20.9, median_value = 19.3)
print(result2)
#  mean   median     ratio
#2 20.9    19.3     1.082902

#comment
# From the results, we learn the relationship between the mean and median 
# values for the two plant types:
# - For Quebec plants (mean = 33.5, median = 37.2), the ratio is less than 1 (0.90).
#   This indicates that the distribution is slightly left-skewed, with the median being higher than the mean.
# - For Mississippi plants (mean = 20.9, median = 19.3), the ratio is greater than 1 (1.08).
#   This suggests a right-skewed distribution, with the mean being higher than the median.


# ---task6-2---
# ---- Function: mean excluding min & max ----
mean_excl_minmax <- function(x, remove = c("all", "one"), na.rm = TRUE) {
  stopifnot(is.numeric(x))               # Ensure input is numeric
  remove <- match.arg(remove)            # Match argument ("all" or "one")
  if (na.rm) x <- x[!is.na(x)]           # Remove NA values if specified
  n <- length(x)
  if (n < 3) return(NA_real_)            # Not enough values to compute
  if (remove == "all") {
    r <- range(x)
    x2 <- x[x > r[1] & x < r[2]]         # Remove all values equal to min or max
  } else {
    x_ord <- sort(x)
    x2 <- x_ord[2:(n - 1)]               # Remove only one min and one max
  }
  if (length(x2) == 0) return(NA_real_)  # If no values left (e.g., all equal)
  mean(x2)                               # Compute mean of remaining values
}

#comment
#The mean computed after excluding the minimum and maximum is a more robust summary than the ordinary mean because it reduces the influence of outliers.
#If results from remove = "one" and "all" are similar, only a few extremes are present; a large gap indicates multiple extremes or strong skewness.
#Reporting ordinary mean, min/max-excluded mean, and median side by side demonstrates that your conclusions do not hinge on outliers.


# ---task6-3---
#Why we didn’t use pipes here
#Few steps; explicit intermediates make intent clearer
#We wanted to debug step-by-step (inspect outputs)
#Some tasks stand alone as single functions; little benefit from chaining

# Example: summarize CO2 by Type × Treatment
library(dplyr)
CO2 |>
  group_by(Type, Treatment) |>
  summarise(uptake_mean = mean(uptake), .groups = "drop")
# Equivalent (non-pipe)
tmp <- group_by(CO2, Type, Treatment)
summarise(tmp, uptake_mean = mean(uptake), .groups = "drop")

#comment
#When to use them
#For 3–5 sequential transforms to improve readability
#When no intermediate objects are needed
#To express data → transform → plot in one flow


# ---task6-4---
#I primarily use single-cell analysis in my research, so I consider lapply the most useful for my work. lapply excels at applying the same procedure to each sample, making it well suited for analyzing large numbers of samples.


# --- task7-1-a ---
#hist
# Arrange plots side by side
par(mfrow = c(1, 2))

# Histogram of Length
hist(magic_guys$length,
     main = "Histogram of Length",
     xlab = "Length",
     col = "skyblue", border = "white",
     cex.main = 0.9,    # Make the title smaller
     cex.lab  = 0.9,    # Make axis labels smaller
     cex.axis = 0.8)    # Make axis tick labels smaller

# Histogram of Weight
hist(magic_guys$weight,
     main = "Histogram of Weight",
     xlab = "Weight",
     col = "orange", border = "white",
     cex.main = 0.9,    # Make the title smaller
     cex.lab  = 0.9,    # Make axis labels smaller
     cex.axis = 0.8)    # Make axis tick labels smaller
#ggplot, genom_histogram
library(ggplot2)
library(tidyr)
library(dplyr)

# Convert to long format
magic_long <- magic_guys %>%
  pivot_longer(cols = c(length, weight),
               names_to = "Variable",
               values_to = "Value")

# Side-by-side histograms with improved style
ggplot(magic_long, aes(x = Value, fill = Variable)) +
  geom_histogram(color = "white", bins = 20) +
  facet_wrap(~Variable, scales = "free_x") +
  labs(title = "Distributions of Length and Weight",
       x = "Value", y = "Count") +
  theme_minimal(base_size = 12) +   # Set a smaller overall base text size
  theme(
    plot.title = element_text(size = 14, face = "bold"),   # Smaller, bold title
    axis.text.x = element_text(angle = 30, hjust = 1),     # Rotate x-axis labels for readability
    axis.title.x = element_text(size = 12),                # Adjust x-axis label size
    axis.title.y = element_text(size = 12)                 # Adjust y-axis label size
  )

#comment
#From these histograms, we learn how the distributions of "length" and "weight" in the dataset differ in their spread and central tendency.
#Using base R histograms is simple and effective for quick inspection, while ggplot2 provides more flexibility, customization, and publication-ready graphics.


# ---task7-1-b---
library(ggplot2)

ggplot(magic_long, aes(x = Variable, y = Value, fill = Variable)) +
  geom_boxplot(width = 0.3, outlier.shape = NA, size = 0.7) +   # Hide outliers
  facet_wrap(~Variable, ncol = 2, scales = "free_y") +          # Arrange side by side
  theme_minimal(base_size = 14) +
  labs(
    title = "Slim and Taller Boxplots of Length and Weight (Outliers Hidden)",
    x = "Variable",
    y = "Value"
  ) +
  theme(
    strip.text = element_text(size = 10),                       # Font size of facet labels
    axis.text.y = element_text(size = 12),                      # Font size of y-axis labels
    plot.title = element_text(size = 10, hjust = 0.5)           # Font size and centered title
  )

#comment
#From these side-by-side boxplots, we can observe the central tendency and variability of both "length" and "weight."
#By hiding the outliers, the visualization emphasizes the main body of the data without distraction.
#In this dataset, "length" and "weight" exhibit distinct scales and variability, making boxplots a compact and effective tool for comparison.


# ---task7-1-c---
#PING:Used for presentations or as quick screenshots of analyses
# Save as PNG
ggsave("plot.png", width = 6, height = 4, dpi = 300)
#PDF:Used for papers, posters, or print-ready materials
# Save as PDF
ggsave("plot.pdf", width = 6, height = 4)
#SVG:Used for web visualization, further editing, or interactive figures
# Save as SVG
ggsave("plot.svg", width = 6, height = 4)

#comment
#The conclusion is that choosing the right format depends on the target audience and usage.


# ---task7-2-a---
# 1. Read the data
data_raw <- read.table("/Users/iijimayui/Desktop/microarray_data.tab",
                       header = TRUE, sep = "\t", stringsAsFactors = FALSE)
# 2. Replace NA in the first column (gene IDs) with unique names
gene_ids <- as.character(data_raw[,1])
gene_ids[is.na(gene_ids)] <- paste0("Gene_", which(is.na(gene_ids)))
# 3. Make all gene IDs unique
gene_ids <- make.unique(gene_ids)
# 4. Set row names
rownames(data_raw) <- gene_ids
# 5. Remove the first column and create the expression matrix
expr_matrix <- data_raw[,-1]
# 6. Check the dimensions of the matrix
dim(expr_matrix)
#[1] 553 999

#comment
# This indicates that the data contains expression measurements for 553 unique genes across 999 experimental observations. 
#The conclusion is that the preprocessing steps successfully standardized the gene identifiers, removed missing values, and produced a structured dataset that is ready for downstream statistical analysis and visualization.


# ---task7-b---
# Make a gene-level summary dataframe
gene_df <- data.frame(
  Gene = rownames(expr_matrix),
  Mean_Expression = rowMeans(expr_matrix, na.rm = TRUE),
  NA_Count = rowSums(is.na(expr_matrix))
)
gene_df_clean <- gene_df %>%
  filter(!is.na(Mean_Expression) & !is.na(NA_Count))

top_na_genes <- gene_df_clean %>%
  arrange(desc(NA_Count)) %>%
  slice_head(n = 10)

ggplot(gene_df_clean, aes(x = Mean_Expression, y = NA_Count)) +
  geom_point(alpha = 0.7, color = "grey") +
  geom_point(data = top_na_genes, aes(x = Mean_Expression, y = NA_Count),
             color = "red", alpha = 0.8) +
  geom_text_repel(data = top_na_genes,
                  aes(x = Mean_Expression, y = NA_Count, label = Gene),
                  size = 3) +
  labs(title = "Top 10 Genes with Most Missing Values",
       x = "Mean Expression (NA removed)",
       y = "Number of Missing Values") +
  theme_minimal()

#comment
#From this result, we learn that most genes have relatively few missing values, while a small subset of genes show a disproportionately high number of missing entries. 
#By highlighting the top 10 genes with the most missing values, we can identify candidates that may need special attention, such as removal, 
#imputation, or further investigation to determine whether the missingness reflects biological variability or technical artifacts.
#This emphasizes the importance of checking missing data patterns


# ---task7-2-c---
library(ggplot2)
library(dplyr)

# 1) Calculate the proportion of missing values for each gene
na_ratio <- rowSums(is.na(expr_matrix)) / ncol(expr_matrix)
# 2) Define missing-value thresholds (e.g., 0%, 10%, 20%, 50%)
thresholds <- c(0, 0.1, 0.2, 0.5)
# 3) Summarize: number of genes exceeding each threshold
na_summary <- data.frame(
  Threshold  = thresholds,
  Gene_Count = sapply(thresholds, function(th) sum(na_ratio > th))
)
# 4) Visualize with ggplot2
ggplot(na_summary, aes(x = Threshold, y = Gene_Count)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(
    title = "Number of Genes Exceeding NA Thresholds",
    x = "NA Threshold",
    y = "Number of Genes"
  ) +
  theme_minimal() +
  theme(
    plot.title  = element_text(size = 10),  # Adjust title size
    axis.title.x = element_text(size = 10),
    axis.title.y = element_text(size = 10),
    axis.text.x  = element_text(size = 10),
    axis.text.y  = element_text(size = 10)
  )

#From this result, we learn how missing values are distributed across genes. 
#The conclusion is that threshold-based filtering provides a systematic way to decide which genes are reliable enough for downstream analyses. This 
#ensures that missing data does not bias clustering, differential expression, or other results.


# ---task7-2-d---
# 1) Calculate the proportion of missing values (NAs) for each gene (row)
na_ratio <- rowSums(is.na(expr_matrix)) / ncol(expr_matrix)
# 2) Identify genes with more than 50% missing values
high_na_genes <- which(na_ratio > 0.5)
# 3) Compute the row-wise mean expression (excluding NAs)
row_means <- rowMeans(expr_matrix, na.rm = TRUE)
# 4) Create a copy for imputation
expr_matrix_imputed <- expr_matrix
# 4-1) Replace all values of high-NA genes with 0
expr_matrix_imputed[high_na_genes, ] <- 0
# 4-2) Replace remaining NAs with the row mean
for (i in 1:nrow(expr_matrix_imputed)) {
  if (!(i %in% high_na_genes)) {
    na_idx <- is.na(expr_matrix_imputed[i, ])
    if (any(na_idx)) expr_matrix_imputed[i, na_idx] <- row_means[i]
  }
}
# 5) Check: Are there any NAs left?
anyNA(expr_matrix_imputed)
#[1] FALSE

#comment
#All missing values were successfully imputed (anyNA returned FALSE).
#Genes with >50% missing values were set to zero, others had NAs replaced with their row mean.
#The dataset is now complete (NA-free) and ready for downstream analyses.
#Caution: zeroing high-NA genes may bias results; alternatively, such genes could be removed.


# ---task7-3---
#Visualization for deeper understanding of the CO2 dataset
#(1) Relationship between CO2 concentration and uptake — scatter plot
library(ggplot2)
library(ggplot2)
data(CO2)

ggplot(CO2, aes(x = conc, y = uptake, color = Type)) +
  # Add jittered points to avoid overplotting
  geom_jitter(width = 2, height = 0, size = 1.5, alpha = 0.6) +
  # Add a linear regression line (no CI band, dashed style)
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  # Create separate panels for each plant Type (independent y-axes)
  facet_wrap(~ Type, ncol = 2, scales = "free_y") +
  # Add plot title and axis labels
  labs(
    title = "CO2 Uptake vs CO2 Concentration by Plant Type",
    x = "CO2 Concentration",
    y = "CO2 Uptake"
  ) +
  
  # Apply minimal theme with base font size
  theme_minimal(base_size = 12) +
  # Customize text and axis style
  theme(
    plot.title = element_text(size = 9, hjust = 0.5),  # Smaller, centered title
    strip.text  = element_text(size = 12),             # Font size of facet labels
    axis.title.x = element_text(size = 12),            # X-axis title size
    axis.title.y = element_text(size = 12),            # Y-axis title size
    axis.text.x  = element_text(size = 10, angle = 45, hjust = 1), # Rotate x labels
    axis.text.y  = element_text(size = 10)             # Y-axis text size
  )
#comment
#CO2 uptake generally increases with CO2 concentration (positive association).
#Quebec vs. Mississippi show different slopes/levels, indicating type-specific responses.
#Considerable spread at the same concentration suggests influence from other factors (e.g., Treatment, plant-to-plant variability).
#Include a Type × concentration interaction in modeling, since responses vary by type.


# ---task7-3---
library(ggplot2)
data(CO2)

ggplot(CO2, aes(x = Treatment, y = uptake, fill = Treatment)) +
  # Make the boxes slimmer
  geom_boxplot(width = 0.3, outlier.shape = NA) +
  # Add individual data points with jitter
  geom_jitter(width = 0.1, alpha = 0.5) +
  # Add plot title and axis labels
  labs(
    title = "CO2 Uptake by Treatment",
    x = "Treatment",
    y = "Uptake"
  ) +
  # Use a minimal theme with base font size 12
  theme_minimal(base_size = 12) +
  # Make the title smaller and centered
  theme(
    plot.title = element_text(size = 10, hjust = 0.5)
  )

#comment
#There is substantial within-treatment variability, suggesting individual or contextual effects.
#A few outliers/high clusters indicate that the treatment effect may not be uniform across observations.
#Preliminary evidence suggests that chilling may reduce CO₂ uptake on average.


# ---task7-3---
library(ggplot2)
data(CO2)

ggplot(CO2, aes(x = conc, y = uptake, color = Type)) +
  # Points with jitter to reduce overlap
  geom_jitter(width = 2, height = 0, size = 1.5, alpha = 0.6) +
  # Regression line for each type
  geom_smooth(method = "lm", se = FALSE, linetype = "dashed", linewidth = 0.7) +
  # Facet by plant type for clarity
  facet_wrap(~Type, ncol = 2, scales = "free_y") +
  # Labels
  labs(
    title = "CO2 Uptake vs CO2 Concentration by Plant Type",
    x = "CO2 Concentration",
    y = "CO2 Uptake"
  ) +
  # Theme and font sizes
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

#comment
#CO₂ uptake generally increases with CO₂ concentration (positive trend).
#Quebec vs. Mississippi show different slopes/intercepts, indicating type-specific responses.
#Substantial spread at the same concentration suggests individual variability or other factors (e.g., Treatment).
#Include a Type × concentration interaction in modeling.


# ---task8-1-a---
# Packages
library(tidybiology)  # contains the 'chromosome' dataset
library(dplyr)
library(tidyr)
library(knitr)

# Load data (if not already loaded)
data("chromosome")

# Summaries (mean/median/max) for selected columns in tidy format
summary_tbl <- chromosome %>%
  summarise(
    across(
      .cols = all_of(c("variations", "protein_codinggenes", "mi_rna")),
      .fns = list(
        mean   = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "statistic"),
    names_pattern = "(.+)_(mean|median|max)",
    values_to = "value"
  )
# Nicely formatted table
kable(
  summary_tbl,
  caption = "Summary statistics of chromosome data (tidy format)",
  digits = 2
)
#Table: Summary statistics of chromosome data (tidy format)

#|.       variable      |statistic |   value|
#  |:-------------------|:---------|-----------:|
#  |variations          |mean      |  6484571.50|
#  |variations          |median    |  6172346.00|
#  |variations          |max       | 12945965.00|
#  |protein_codinggenes |mean      |      849.96|
#  |protein_codinggenes |median    |      836.00|
#  |protein_codinggenes |max       |     2058.00|
#  |mi_rna              |mean      |       73.17|
#  |mi_rna              |median    |       75.00|
#  |mi_rna              |max       |      134.00|

#comment
#Variations occur on a much larger scale than protein_codinggenes and mi_rna.
#Where the max >> median, a few chromosomes show markedly high counts, indicating right-skewed distributions.
#Because the median is robust to outliers, it is a more stable summary for between-chromosome comparison.
#Use boxplots and/or log scales to visualize spread and outliers, and flag chromosomes driving the maxima.


# ---task8-1-b---
# Packages
library(tidybiology)  # for the 'chromosome' dataset
library(ggplot2)
library(patchwork)

# Load data (if not already loaded)
data("chromosome")

# ---- Histogram ----
p1 <- ggplot(chromosome, aes(x = basepairs)) +
  geom_histogram(binwidth = 5e7, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(
    title = "Chromosome Sizes (Histogram)",
    x = "Chromosome Size (basepairs)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),  # smaller, centered title
    axis.text  = element_text(size = 8),                # smaller tick labels
    axis.title = element_text(size = 9),                # smaller axis titles
    axis.text.x = element_text(angle = 45, hjust = 1)   # rotate x labels
  )
# ---- Density plot ----
p2 <- ggplot(chromosome, aes(x = basepairs)) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(
    title = "Chromosome Sizes (Density)",
    x = "Chromosome Size (basepairs)",
    y = "Density"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.text  = element_text(size = 8),
    axis.title = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# ---- Display side by side ----
p1 + p2

#comment
#Chromosome sizes span a wide range, with a long right tail indicating a few very large chromosomes.
#The density curve highlights the typical scale (around the main peak) and reveals larger-than-usual chromosomes beyond it.
#Identify and annotate the smallest/largest chromosomes to evaluate whether extremes reflect technical artifacts or biological signals.


# ---task8-c---
# Packages
library(tidybiology)  # for the 'chromosome' dataset
library(ggplot2)
library(patchwork)

# Load data
data("chromosome")

# Scatter plot: Chromosome length vs Protein-coding genes
p1 <- ggplot(chromosome, aes(x = basepairs, y = protein_codinggenes)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkblue", linetype = "dashed") +
  labs(
    title = "Chromosome Length vs Protein-Coding Genes",
    x = "Chromosome Length (basepairs)",
    y = "Number of Protein-Coding Genes"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8, hjust = 0.5),
    axis.text   = element_text(size = 8),
    axis.title  = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Scatter plot: Chromosome length vs miRNAs
p2 <- ggplot(chromosome, aes(x = basepairs, y = mi_rna)) +
  geom_point(color = "tomato", size = 2, alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Chromosome Length vs miRNAs",
    x = "Chromosome Length (basepairs)",
    y = "Number of miRNAs"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 8, hjust = 0.5),
    axis.text   = element_text(size = 8),
    axis.title  = element_text(size = 9),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
# Display side by side
p1 + p2

#comment
#Longer chromosomes tend to have more protein-coding genes (positive regression slope).
#miRNA counts also show a positive trend, but with greater scatter, suggesting a weaker association than for genes.
#Because totals scale with length, comparisons should use length-normalized densities (per Mb) for genes and miRNAs.


# ---task8-1-d---
# Packages
library(tidyverse)
# If your dataset comes from tidybiology, uncomment the next two lines:
# library(tidybiology)
# data("proteins")

# --- 1. Calculate summary statistics ---
protein_summary <- proteins %>%
  summarise(
    across(
      .cols = c(length, mass),
      .fns = list(
        mean   = ~ mean(.x, na.rm = TRUE),
        median = ~ median(.x, na.rm = TRUE),
        max    = ~ max(.x, na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  )
# Display the summary
print(protein_summary)
# --- 2. Scatter plot with dashed regression line (and CI) ---
ggplot(proteins, aes(x = length, y = mass)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Relationship between Protein Length and Mass",
    x = "Protein Length",
    y = "Protein Mass"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(size = 10, hjust = 0.5),
    axis.text  = element_text(size = 10),
    axis.title = element_text(size = 10)
  )

#comment
#Noticeable spread at a given length suggests effects of amino-acid composition, post-translational modifications, or annotation/measurement noise.
#Outliers are candidates for complexes, heavy modifications, or annotation issues and warrant follow-up.