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

### Light treatment vs pre-light treatment (1 kHz - 10 kHz) as shown by key acoustic indices

![Image](https://github.com/user-attachments/assets/f2104c4f-8cf8-408a-9339-2aacc04a422a) 

Darker red bars further to the left indicate _lower_ values of acoustic indices during the preiod when the light was on. Therefore, possibly _less_ stridulation but will need to check this against count data and what it is the acoustic index is measuring exactly. 

Darker blue bars further to the right indicate _higher_ values of acoustic indices during the preiod when the light was on. Therefore, possibly _more_ stridulation but will need to check this against count data and what it is the acoustic index is measuring exactly. 

Empty spaces / no bars indicated a non-signifcant result for that acoustic index. 

### False-colour spectrograms 


```
# === LIBRARIES ===
library(tuneR)
library(seewave)
library(signal)
library(entropy)
library(dplyr)
library(ggplot2)
library(grDevices)
library(patchwork) 

# === CONFIGURATION ===
# NOTE: User MUST change this path to their local directory
directory <- "C:/Users/Administrador/Downloads/Light pollution sound files/OSP_2_25.08.2025/OSP_2_25.08.2025"

cat("Checking directory:", directory, "\n")

if (!dir.exists(directory)) stop("The specified directory does not exist.")

file_list <- list.files(directory, pattern = "\\.WAV$", full.names = TRUE)

if (length(file_list) == 0) stop("No .WAV files found in the directory.")
cat("Found", length(file_list), "files.\n")

# === METRIC FUNCTIONS ===
calculate_entropy <- function(freq_bin) {
  total_energy <- sum(freq_bin)
  if (total_energy == 0) return(0)
  p <- freq_bin / total_energy
  -sum(p * log(p + 1e-10))
}

calculate_aci <- function(freq_bin) {
  local_maxima <- sum(diff(sign(diff(freq_bin))) == -2)
  local_maxima / length(freq_bin)
}

calculate_background_noise <- function(freq_bin) {
  median(freq_bin)
}

# === STORAGE ===
entropy_list <- list()
aci_list <- list()
background_noise_list <- list()

# === LOOP THROUGH FILES (Placeholder for user's local execution) ===
# This section assumes that combined_df is successfully created from the loop results.
# For a full execution, this entire block would need to run successfully.
# ... (File loop and initial processing steps remain as in previous versions) ...

# NOTE: Since the full data (combined_df) cannot be loaded here, 
# the subsequent code will assume combined_df, entropy_all, aci_all, 
# and background_noise_all have been successfully created.

# Placeholder functions for code completeness:

# === EXTRACT TIME FROM FILENAMES ===
extract_time <- function(df) {
  df %>%
    mutate(TimeString = substr(File, 10, 15)) %>%
    select(-File)
}

# === NEW: ADD FULL DATETIME TO HANDLE OVERNIGHT ===
add_datetime <- function(df) {
  df %>%
    mutate(
      Hour = as.numeric(substr(TimeString, 1, 2)),
      Date = ifelse(Hour < 12, "2025-08-26", "2025-08-25"), 
      TimeStr = paste0(substr(TimeString, 1, 2), ":",
                       substr(TimeString, 3, 4), ":",
                       substr(TimeString, 5, 6)),
      Time = as.POSIXct(paste(Date, TimeStr), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ) %>%
    select(-TimeString, -Hour, -Date, -TimeStr) 
}

# (The scaling, RGB combination, and background noise removal steps follow here
# to produce the combined_df which includes the Adjusted_Color column)

# For the plotting code to be runnable:
# === DEFINE EXPERIMENTAL PERIODS (Original - REQUIRED) ===
periods <- data.frame(
  Label = c("Natural darkness (Phase I)", "Light treatment (Phase II)", "Natural darkness (Phase III)"),
  Start = as.POSIXct(c("2025-08-25 21:11:00",
                       "2025-08-25 22:11:00", "2025-08-25 23:11:00"), tz = "UTC"),
  End = as.POSIXct(c("2025-08-25 22:11:00",
                     "2025-08-25 23:11:00", "2025-08-26 00:11:00"), tz = "UTC"), 
  Fill = c("gray60", "#FFC300", "gray60")
)


# === DEFINE PADDED Y-AXIS LIMITS (in kHz) ===
# This assumes combined_df has been created and its Frequency column is in kHz
# y_max <- max(combined_df$Frequency) 
# y_min_padded <- -0.5 
# y_max_padded <- y_max + 0.5 

# Placeholder values for demonstration (User must use the live values)
y_max_placeholder <- 50 
y_min_padded <- -0.5 
y_max_padded <- y_max_placeholder + 0.5 

# === PLOT MAIN SPECTROGRAM ===
# NOTE: This plot assumes 'combined_df' (with Frequency in kHz) and 
# 'Adjusted_Color' are available.
p_spec <- ggplot(combined_df, aes(x = Time, y = Frequency)) +
  geom_tile(aes(fill = Adjusted_Color)) +
  scale_fill_identity() +
  scale_x_datetime(limits = c(min(periods$Start), max(periods$End)), 
                   date_breaks = "15 min", 
                   date_labels = "%H:%M") + 
  # Uses padded limits in kHz
  scale_y_continuous(limits = c(y_min_padded, y_max_padded), expand = c(0, 0)) +
  labs(x = "Time", y = "Frequency (kHz)") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.ticks.x = element_line(),                    
    plot.margin = margin(0, 5, 0, 5)
  )

# === PERIOD BAR PLOT ===
p_periods <- ggplot(periods, aes(xmin = Start, xmax = End, ymin = 0, ymax = 1, fill = Fill)) +
  geom_rect(color = "white") +
  geom_text(aes(x = (Start + (End - Start) / 2),
                y = 0.5, label = Label),
            color = "black", size = 4, fontface = "bold") +
  scale_fill_identity() +
  scale_x_datetime(limits = c(min(periods$Start), max(periods$End))) +
  scale_y_continuous(expand = c(0, 0)) +
  theme_void() +
  # Hide x-axis elements for the top (now bottom) plot to align perfectly
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5, 5, 0, 5)
  )

# === COMBINE USING patchwork ===
# Swapped order to p_spec / p_periods to put spectrogram on top, period bar below.
final_plot <- p_spec / p_periods + plot_layout(heights = c(1, 0.075)) 

# === SAVE ===
ggsave("25.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.pdf", plot = final_plot, width = 10, height = 7, dpi = 300)

cat("Plot saved as 25.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.pdf\n")

```


