# Light-pollution-and-pond-soundscapes

#### Calculation of acoustic indices (Python)

```
#By Jack A. Greenhalgh, June, 2025.
#Department of Biology, McGill University, 1205 Dr Penfield Ave, Montreal, Quebec, H3A 1B1, Canada.

import os
import pandas as pd
from maad import sound
import maad.features.alpha_indices as ai
from tqdm import tqdm  # Optional: shows progress bar

# =============================
# Configurable Parameters
# =============================

# Spectrogram parameters
NFFT = 1024            # Number of FFT points (nperseg)
noverlap = 512         # Overlap between segments
window = 'hann'        # Type of window ('hann', 'hamming', etc.)

# Temporal alpha indices parameters
temporal_threshold_db = -50  # dB threshold for signal detection

# Frequency range for spectral alpha indices
fmin = 1000     # Minimum frequency (Hz)
fmax = 24000    # Maximum frequency (Hz)

# Main directory containing multiple folders with audio files
main_directory = r"C:\Users\jgreenhalgh\Downloads\Light pollution\Light pollution Python"

# =============================
# Processing Loop
# =============================

# Loop through all subfolders in the main directory
for foldername in os.listdir(main_directory):
    folder_path = os.path.join(main_directory, foldername)
    
    # Proceed only if it's a directory
    if os.path.isdir(folder_path):
        print(f"\nProcessing folder: {foldername}")
        results = []
        
        # List all .wav files in this subfolder
        wav_files = [f for f in os.listdir(folder_path) if f.lower().endswith('.wav')]
        
        for filename in tqdm(wav_files, desc=f"Files in {foldername}"):
            filepath = os.path.join(folder_path, filename)
            try:
                # Load the audio file
                s, fs = sound.load(filepath)

                # Generate spectrogram with parameters
                Sxx_power, tn, fn, _ = sound.spectrogram(
                    s, fs, window=window, nperseg=NFFT, noverlap=noverlap
                )

                # Compute spectral alpha indices with frequency limits
                spectral = ai.all_spectral_alpha_indices(
                    Sxx_power, tn, fn, fmin=fmin, fmax=fmax
                )
                spectral_dict = spectral[0].iloc[0].to_dict()

                # Compute temporal alpha indices with threshold
                temporal = ai.all_temporal_alpha_indices(s, fs, threshold=temporal_threshold_db)
                temporal_dict = temporal.iloc[0].to_dict()

                # Merge both sets of indices and tag filename with folder
                combined = {**spectral_dict, **temporal_dict}
                combined['filename'] = f"{foldername}_{filename}"  # Add folder prefix to filename

                results.append(combined)

            except Exception as e:
                print(f"Error processing {filename} in {foldername}: {e}")

        # If results were gathered, save to CSV named after the folder
        if results:
            df = pd.DataFrame(results)
            cols = ['filename'] + [c for c in df.columns if c != 'filename']
            df = df[cols].sort_values(by='filename').reset_index(drop=True)

            output_csv = os.path.join(folder_path, f"{foldername}_alpha_acoustic_indices_results.csv")
            df.to_csv(output_csv, index=False)

            print(f"Results saved for folder '{foldername}' at:\n{output_csv}")
        else:
            print(f"No audio files processed in folder '{foldername}'.")
```

#### Analysis of acoustic indices data (R Studio) 

```
#By Jack A. Greenhalgh, June, 2025.
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

data <- read.csv("Light pollution full results (1 kHz, 10 kHz).csv")
head(data)

# Remove any leading/trailing whitespace in Treatment
data$Treatment <- trimws(data$Treatment)

# Extract Site and Treatment into a separate object
site_treatment <- data[, c("Site", "Treatment")]

# Subset numeric columns from 4 to 63
numeric_data <- data[, 4:63]

# Compute correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")

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

library(dplyr)
library(ggplot2)
library(forcats)

# Prepare and filter data for the specific comparison
df_light_vs_pre <- posthoc_results %>%
  filter(Group1 == "light treatment", Group2 == "pre-light treatment") %>%
  mutate(
    Comparison = "light vs pre-light",
    Missing = ifelse(is.na(Z), "NA", "Data"),  # flag NA
    Z = ifelse(is.na(Z), 0, Z),                # convert NA to 0 for plotting
    Variable = factor(Variable, levels = unique(posthoc_results$Variable))
  )

# Plot with gradient fill based on Z
ggplot(df_light_vs_pre, aes(x = Z, y = Variable, fill = Z, alpha = Missing)) +
  geom_col(width = 0.7, color = "black") +
  facet_wrap(~ Site, scales = "fixed") +
  scale_fill_gradient2(
    low = "red",
    mid = "white",
    high = "blue",
    midpoint = 0,
    name = "Effect size (Z)"
  ) +
  scale_alpha_manual(values = c("Data" = 1, "NA" = 0.3), guide = FALSE) +
  theme_bw(base_size = 12) +
  theme(
    axis.text.y = element_text(size = 9),
    strip.text = element_text(face = "bold", size = 13),
    legend.position = "bottom"
  ) +
  labs(
    x = "Effect size (Z)",
    y = "Acoustic index",
    fill = "Effect size (Z)"
  )

```

### Heat map of acoustic indices for light treatment vs pre-light treatment (1 kHz - 10 kHz) 

![Image](https://github.com/user-attachments/assets/f2104c4f-8cf8-408a-9339-2aacc04a422a) 

Darker red bars further to the left indicate _lower_ values of acoustic indices during the preiod when the light was on. Therefore, possibly _less_ stridulation but will need to check this against count data and what it is the acoustic index is measuring exactly. 

Darker blue bars furtger to the right indicate _higher_ values of acoustic indices during the preiod when the light was on. Therefore, possibly _more_ stridulation but will need to check this against count data and what it is the acoustic index is measuring exactly. 

Empty spaces / no bars indicated a non-signifcant result for that acoustic index. 
