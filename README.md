# Light-pollution-and-pond-soundscapes

```
#By Jack A. Greenhalgh, 30th June, 2025.
#Department of Biology, McGill University, 1205 Dr Penfield Ave, Montreal, Quebec, H3A 1B1, Canada.

# Load required packages
library(corrplot)
library(caret)
library(car)   
library(dplyr)
library(purrr)
library(FSA)     
library(tidyr)
library(ggplot2)
library(forcats)

#### Loading, cleaning, and scaling data ####

data <- read.csv("Light pollution full results.csv")
head(data)

# Remove any leading/trailing whitespace in Treatment
data$Treatment <- trimws(data$Treatment)

# Extract Site and Treatment into a separate object
site_treatment <- data[, c("Site", "Treatment")]

# Subset numeric columns from 4 to 63
numeric_data <- data[, 4:63]

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
help(cor)

# Plot correlation matrix
corrplot(cor_matrix, method = "color", type = "upper", 
         tl.cex = 0.7, tl.col = "black", addCoef.col = "black", number.cex = 0.5)

# Print the correlation matrix
print(cor_matrix)

# Find indices of highly correlated variables (threshold > 0.8)
high_corr_indices <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)

# Remove them from the dataset
filtered_data <- numeric_data[, !colnames(numeric_data) %in% high_corr_indices]

# Print the names of variables that were removed
print(high_corr_indices)

# Print the names of variables that have been kept
kept_variables <- colnames(filtered_data)
print(kept_variables)

# Z-transform the filtered_data
filtered_data_z <- as.data.frame(scale(filtered_data))
summary(filtered_data_z)

# Add Site and Treatment back to filtered_data
filtered_data_z <- cbind(site_treatment, filtered_data_z)
head(filtered_data_z)

##### Testing for normality #####

# Identify numeric variables
numeric_vars <- sapply(filtered_data_z, is.numeric)

# Apply Shapiro-Wilk test to each numeric variable
shapiro_results <- sapply(filtered_data_z[, numeric_vars], function(x) {
  if (length(unique(x)) >= 3) {
    shapiro.test(x)$p.value
  } else {
    NA  # Too few unique values for test
  }
})

# Format into a dataframe
shapiro_df <- data.frame(
  Variable = names(shapiro_results),
  Shapiro_p_value = shapiro_results,
  Normality = ifelse(shapiro_results > 0.05, "Yes", "No")
)

print(shapiro_df)

#### Testing for equal or unequal variance between treatment groups ####

# Ensure Treatment is a factor
filtered_data_z$Treatment <- as.factor(filtered_data_z$Treatment)

# Get only the numeric columns (exclude Site and Treatment)
numeric_vars <- filtered_data_z %>%
  select(where(is.numeric)) %>%
  colnames()

# Apply Levene's Test to each variable
levene_results <- lapply(numeric_vars, function(var) {
  formula <- as.formula(paste(var, "~ Treatment"))
  test <- leveneTest(formula, data = filtered_data_z)
  data.frame(
    Variable = var,
    F = test$`F value`[1],
    p_value = test$`Pr(>F)`[1]
  )
})

# Combine results into a single data frame
levene_results_df <- do.call(rbind, levene_results)

# View results
print(levene_results_df)

#### Kruskal-Wallis for Each Variable by Treatment within Site #####

# Normalize Treatment names completely (optional)
filtered_data_z <- filtered_data_z %>%
  mutate(Treatment = tolower(trimws(Treatment)))  # All lowercase, no whitespace

# Check for NA values in the Treatment column
sum(is.na(filtered_data_z$Treatment))

#Check treatment names are correct
unique(filtered_data_z$Treatment) %>% print()

# Variables to test
variables_to_test <- setdiff(names(filtered_data_z), c("Site", "Treatment"))

# Initialize results list
full_results <- list()

# Loop through each site
for (site_name in unique(filtered_data_z$Site)) {
  
  # Subset data for current site
  site_data <- filtered_data_z %>% filter(Site == site_name)
  
  # Loop through each variable
  for (var in variables_to_test) {
    
    # Run Kruskal-Wallis
    formula <- as.formula(paste(var, "~ Treatment"))
    kruskal <- kruskal.test(formula, data = site_data)
    
    # Store Kruskal result
    base_result <- tibble(
      Site = site_name,
      Variable = var,
      KW_statistic = kruskal$statistic,
      KW_p_value = kruskal$p.value,
      Significant = ifelse(kruskal$p.value < 0.05, "Yes", "No")
    )
    
    # If significant, run Dunn's test
    if (kruskal$p.value < 0.05) {
      dunn <- dunnTest(formula, data = site_data, method = "bonferroni")
      dunn_df <- as_tibble(dunn$res)
      
      # Extract treatment pairs, direction, and p-values
      dunn_df <- dunn_df %>%
        separate(Comparison, into = c("Group1", "Group2"), sep = " - ") %>%
        mutate(Direction = map2_chr(Group1, Group2, function(g1, g2) {
          med1 <- median(site_data[[var]][site_data$Treatment == g1])
          med2 <- median(site_data[[var]][site_data$Treatment == g2])
          if (med1 < med2) {
            paste(g2, ">", g1)
          } else if (med1 > med2) {
            paste(g1, ">", g2)
          } else {
            "No difference"
          }
        })) %>%
        select(Group1, Group2, Z = Z, P.adj = P.adj, Direction)
      
      # Join Kruskal and Dunn results
      combined <- base_result %>%
        crossing(dunn_df)  # one Kruskal result per posthoc row
      
    } else {
      combined <- base_result
    }
    
    # Store combined result
    full_results[[paste(site_name, var, sep = "_")]] <- combined
  }
}

# Combine everything
posthoc_results <- bind_rows(full_results)

# Preview
print(posthoc_results)

write.csv(posthoc_results, "posthoc_results.csv")

##### Heat map of key variables #####

# Prepare data
df <- posthoc_results %>%
  mutate(
    Comparison = paste(Group1, "vs", Group2),
    DirectionLabel = case_when(
      Z > 0 ~ "Group1 > Group2",
      Z < 0 ~ "Group2 > Group1",
      TRUE ~ "No difference"
    ),
    Significant = factor(Significant, levels = c("No", "Yes")),
    Comparison = as.factor(Comparison),
    Comparison = fct_relevel(Comparison, "NA vs NS", after = Inf)  # put this comparison last
  )

# Plot
ggplot(df, aes(x = Comparison, y = Variable, fill = Z)) +
  geom_tile(color = "white") +
  facet_wrap(~ Site, scales = "free_x") +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "orange",
    midpoint = 0,
    name = "Effect Size (Z)"
  ) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  ) +
  labs(
    fill = "Effect Size (Z)",
    x = "Group Comparison",
    y = "Variable"
  )
```

