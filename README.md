# Light-pollution-and-pond-soundscapes

## Calculation of acoustic indices (Python)

```
#By Jack A. Greenhalgh, June, 2025.
#Department of Biology, McGill University, 1205 Dr Penfield Ave, Montreal, Quebec, H3A 1B1, Canada.
# =============================================
# Acoustic Indices Batch Processing Script
# Robust for Spyder and Command Line
# Supports optional spectrogram cropping AND index frequency parameters
# Prints summary of acoustic indices at the end
# =============================================

import os
import pandas as pd
from maad import sound
import maad.features.alpha_indices as ai
from tqdm import tqdm

# =============================
# Detect environment and choose executor
# =============================
def in_spyder():
    """Detect if running inside Spyder"""
    return 'SPYDER_ARGS' in os.environ or 'SPYDER_PID' in os.environ

if in_spyder():
    from concurrent.futures import ThreadPoolExecutor as Executor
    use_threads = True
else:
    from concurrent.futures import ProcessPoolExecutor as Executor
    use_threads = False

from concurrent.futures import as_completed

# =============================
# Configurable Parameters
# =============================
NFFT = 1024
noverlap = 512
window = 'hann'
temporal_threshold_db = -85

# Frequency range for spectral indices
# These parameters are passed directly to the spectral index functions
spectral_fmin = 2000
spectral_fmax = 4000

# Frequency range for optional spectrogram subset
# Only used if subset_spectrogram = True
spectrogram_fmin = 2000
spectrogram_fmax = 4000

# Option to manually subset spectrogram before index calculation
subset_spectrogram = True  # Set True to crop Sxx manually

# Main directory
main_directory = r"D:\Light pollution\Python"

# =============================
# File Processing Function
# =============================
def process_file(foldername, folder_path, filename):
    """Compute spectral and temporal alpha indices for a single .wav file"""
    filepath = os.path.join(folder_path, filename)
    try:
        s, fs = sound.load(filepath)
        if s is None or len(s) == 0:
            raise ValueError("Empty or invalid audio file")

        # Compute full spectrogram
        Sxx_power, tn, fn, _ = sound.spectrogram(
            s, fs, window=window, nperseg=NFFT, noverlap=noverlap
        )

        # Optional manual cropping of spectrogram
        if subset_spectrogram:
            freq_mask = (fn >= spectrogram_fmin) & (fn <= spectrogram_fmax)
            if freq_mask.sum() == 0:
                raise ValueError(f"No frequencies found in range {spectrogram_fmin}-{spectrogram_fmax} Hz")
            Sxx_power = Sxx_power[freq_mask, :]
            fn = fn[freq_mask]

        # Compute spectral indices using spectral_fmin/spectral_fmax as function parameters
        spectral = ai.all_spectral_alpha_indices(
            Sxx_power, tn, fn, fmin=spectral_fmin, fmax=spectral_fmax
        )
        spectral_dict = spectral[0].iloc[0].to_dict()

        # Compute temporal indices
        temporal = ai.all_temporal_alpha_indices(s, fs, threshold=temporal_threshold_db)
        temporal_dict = temporal.iloc[0].to_dict()

        # Combine results
        combined = {**spectral_dict, **temporal_dict}
        combined['filename'] = f"{foldername}_{filename}"
        return combined

    except Exception as e:
        print(f"⚠ Error processing {filename} in {foldername}: {e}")
        return None

# =============================
# Main Script
# =============================
def main():
    foldernames = [f for f in os.listdir(main_directory)
                   if os.path.isdir(os.path.join(main_directory, f))]
   
    if not foldernames:
        print(f"⚠ No sub-folders found in main directory:\n{main_directory}")
        return

    for foldername in foldernames:
        folder_path = os.path.join(main_directory, foldername)
        print(f"\n🎧 Processing folder: {foldername}")
        results = []

        wav_files = [f for f in os.listdir(folder_path) if f.lower().endswith('.wav')]
        if not wav_files:
            print(f"⚠ No .wav files found in {folder_path}")
            continue

        executor_type = "Threads" if use_threads else "Processes"
        print(f"Using {executor_type} for parallel processing")

        with Executor() as executor:
            futures = {executor.submit(process_file, foldername, folder_path, f): f for f in wav_files}

            for future in tqdm(as_completed(futures), total=len(futures), desc=f"Files in {foldername}"):
                result = future.result()
                if result:
                    results.append(result)

        if results:
            df = pd.DataFrame(results)

            # Reorder columns
            cols = ['filename'] + [c for c in df.columns if c != 'filename']
            df = df[cols].sort_values(by='filename').reset_index(drop=True)

            # Ensure numeric columns
            df = df.apply(pd.to_numeric, errors='ignore')

            # Save CSV
            output_csv = os.path.join(folder_path, f"{foldername}_boatman_alpha_acoustic_indices_results.csv")
            df.to_csv(output_csv, index=False, encoding='utf-8-sig')
            print(f"✔ Results saved for folder '{foldername}' at:\n{output_csv}")
        else:
            print(f"⚠ No audio files processed successfully in folder '{foldername}'.")

# =============================
# Acoustic indices summary table
# =============================
def print_acoustic_indices_summary():
    """
    Print a summary table of the acoustic indices used,
    their key parameters, and a brief description.
    """
    data = [
        ["VARf", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Variance of the frequency bins in the spectrogram"],
        ["KURTf", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Kurtosis of the frequency distribution of the spectrogram"],
        ["NBPEAKS", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Number of spectral peaks in the frequency range"],
        ["BGNf", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Background noise estimate of the frequency spectrum"],
        ["EAS", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Acoustic entropy across frequency bins"],
        ["ECV", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Coefficient of variation of the energy across frequency bins"],
        ["EPS", f"threshold={temporal_threshold_db} dB", "Entropy of the temporal amplitude signal"],
        ["EPS_KURT", f"threshold={temporal_threshold_db} dB", "Kurtosis of temporal entropy"],
        ["ACI", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Acoustic Complexity Index, measuring amplitude variation across time and frequency"],
        ["rBA", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Relative Bioacoustic Index, normalized energy in the frequency band"],
        ["BI", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Bioacoustic Index, measures total energy in the band"],
        ["ADI", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Acoustic Diversity Index, reflects the number of frequency bins with significant activity"],
        ["EVNspMean", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Mean of the Event-based Normalized Spectrogram"],
        ["TFSD", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Temporal Frequency Spectrum Density"],
        ["RAOQ", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Rao's Quadratic Entropy, diversity index in frequency domain"],
        ["AGI", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Acoustic Grouping Index, measures clustering of acoustic events"],
        ["aROI", f"fmin={spectral_fmin}, fmax={spectral_fmax}", "Acoustic Region of Interest index, energy in a specific band"],
        ["MEANt", f"threshold={temporal_threshold_db} dB", "Mean amplitude of temporal signal above threshold"],
        ["SKEWt", f"threshold={temporal_threshold_db} dB", "Skewness of temporal amplitude distribution"],
        ["KURTt", f"threshold={temporal_threshold_db} dB", "Kurtosis of temporal amplitude distribution"],
        ["Ht", f"threshold={temporal_threshold_db} dB", "Shannon entropy of the temporal signal"],
        ["EVNtMean", f"threshold={temporal_threshold_db} dB", "Mean of temporal event-based normalized signal"],
        ["EVNtCount", f"threshold={temporal_threshold_db} dB", "Count of temporal events above threshold"]
    ]

    df_summary = pd.DataFrame(data, columns=["Index", "Parameters", "Description"])
    print("\n🎶 Acoustic Indices Summary Table")
    print(df_summary.to_string(index=False))

    # Optional: save summary table to CSV in main directory
    try:
        summary_csv = os.path.join(main_directory, "boatman_acoustic_indices_summary.csv")
        df_summary.to_csv(summary_csv, index=False, encoding='utf-8-sig')
        print(f"\n✔ Acoustic indices summary saved as CSV at:\n{summary_csv}")
    except Exception as e:
        print(f"\n⚠ Could not save summary CSV to main directory: {e}")


# =============================
# SCRIPT EXECUTION
# =============================
if __name__ == "__main__":
    main()
    print_acoustic_indices_summary()
```

## Analysis of acoustic indices data (R Studio) 

```
# ============================
# 1. Load required packages
# ============================
library(dplyr)
library(stringr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(emmeans)
library(corrr)
library(caret)
library(purrr)

# ============================
# 2. Read and combine CSV files
# ============================
dir_path <- "D:/Light pollution/2.5 kHz - 4.0 kHz niche space"

csv_files <- list.files(path = dir_path, pattern = "\\.csv$", full.names = TRUE)
data_list <- lapply(csv_files, read.csv, stringsAsFactors = FALSE)
names(data_list) <- tools::file_path_sans_ext(basename(csv_files))

combined_df <- bind_rows(data_list, .id = "source_file")

# ============================
# 3. Clean and format metadata
# ============================
combined_df <- combined_df %>%
  mutate(
    Treatment = str_trim(Treatment),
    Treatment = factor(Treatment, levels = c("Phase I", "Phase II", "Phase III")),
    Site = factor(Site),
    Sampling.date = factor(Sampling.date)
  )

# ============================
# 4. Remove highly correlated indices (>0.8)
# ============================
numeric_vars <- combined_df %>% select(where(is.numeric))
cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")

high_cor <- findCorrelation(cor_matrix, cutoff = 0.8, verbose = TRUE, names = TRUE)
acoustic_df <- numeric_vars %>% select(-all_of(high_cor))

combined_clean <- bind_cols(
  combined_df %>% select(source_file, filename, Site, Sampling.date, Treatment),
  acoustic_df
)

# ============================
# 5. Z-standardize acoustic indices
# ============================
combined_clean <- combined_clean %>%
  mutate(across(where(is.numeric), ~ scale(.)[, 1]))

# ============================
# 6. Fit linear mixed-effects models
# ============================
acoustic_indices <- colnames(acoustic_df)
results_list <- list()

for (idx in acoustic_indices) {
  formula <- as.formula(paste(idx, "~ Treatment * Site + (1 | Sampling.date)"))
  mod <- lmer(formula, data = combined_clean)
  results_list[[idx]] <- list(model = mod)
}

# ============================
# 7. Extract effect sizes (BASELINE = Phase I)
# ============================
get_all_contrasts <- function(idx) {
  mod <- results_list[[idx]]$model
  emm <- emmeans(mod, ~ Treatment | Site)
  
  # Weights: Phase I, Phase II, Phase III
  # Baseline is Phase I (-1). 
  # Contrast 1: Phase II minus Phase I
  # Contrast 2: Phase III minus Phase I
  contr <- contrast(
    emm,
    method = list(
      "Impact (Light vs Natural Dark)"   = c(-1,  1,  0), 
      "Legacy (Recovery vs Natural Dark)" = c(-1,  0,  1)
    ),
    by = "Site",
    infer = c(TRUE, TRUE)
  )
  
  as.data.frame(contr) %>%
    mutate(Index = idx)
}

all_sites_effect_df <- map_dfr(acoustic_indices, get_all_contrasts)

# ============================
# 8. Categorize & Factor Formatting
# ============================
all_sites_effect_df <- all_sites_effect_df %>%
  mutate(
    Category = case_when(
      Index %in% c("ADI", "BI", "BioEnergy", "NDSI", "NBPEAKS", "rBA", "H_Havrda", "H_Renyi") ~ "Biophony & diversity",
      Index %in% c("EAS", "ECU", "EPS_KURT", "KURTf") ~ "Complexity & entropy",
      Index %in% c("AnthroEnergy", "LFC", "EVNspMean", "BGNf", "TFSD") ~ "Anthrophony & noise",
      TRUE ~ "Other"
    ),
    # Lock the order of the baseline comparisons
    contrast = factor(contrast, levels = c("Impact (Light vs Natural Dark)", "Legacy (Recovery vs Natural Dark)")),
    sig = ifelse(p.value < 0.05, "*", "")
  )

# ============================
# 9. Loop through Sites, Plot, and Export
# ============================
# Visual settings
contrast_colors <- c("Impact (Light vs Natural Dark)" = "#FFC300", "Legacy (Recovery vs Natural Dark)" = "gray60")
contrast_lines  <- c("Impact (Light vs Natural Dark)" = "solid",  "Legacy (Recovery vs Natural Dark)" = "dashed")
unique_sites    <- unique(all_sites_effect_df$Site)

for (current_site in unique_sites) {
  
  # 1. Filter and re-order data
  site_data <- all_sites_effect_df %>% filter(Site == current_site)
  site_index_order <- site_data %>%
    group_by(Index) %>%
    summarize(mean_est = mean(estimate)) %>%
    arrange(mean_est) %>%
    pull(Index)
  
  site_data$Index <- factor(site_data$Index, levels = site_index_order)
  
  # 2. Build Plot
  p <- ggplot(site_data, aes(x = estimate, y = Index, color = contrast, linetype = contrast)) +
    # Phase I baseline reference (0 line)
    geom_vline(xintercept = 0, linetype = "dotted", color = "black", linewidth = 0.6) +
    
    geom_errorbarh(
      aes(xmin = lower.CL, xmax = upper.CL),
      height = 0.5, linewidth = 1.1,
      position = position_dodge(width = 0.7)
    ) +
    
    geom_point(size = 3.5, position = position_dodge(width = 0.7)) +
    
    geom_text(
      aes(label = sig, x = upper.CL + 0.1), 
      position = position_dodge(width = 0.7),
      color = "#FFC300", size = 8, vjust = 0.7, show.legend = FALSE
    ) +
    
    facet_grid(Category ~ ., scales = "free_y", space = "free_y") +
    
    scale_color_manual(values = contrast_colors) + 
    scale_linetype_manual(values = contrast_lines) +
    
    theme_bw(base_size = 14) +
    theme(
      panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "gray95"),
      strip.text = element_text(face = "bold"),
      legend.position = "bottom",
      legend.title = element_blank()
    ) +
    labs(
      title = paste("Site:", current_site, "(Baseline = Phase I)"),
      x = "Standardized effect size (Relative to natural darkness)",
      y = "Acoustic index"
    )
  
  # 3. Export
  full_save_path <- file.path(dir_path, paste0(current_site, "_Phase1Baseline_Plot.jpg"))
  
  ggsave(
    filename = full_save_path,
    plot = p,
    device = "jpeg",
    dpi = 300,
    width = 10,
    height = 12,
    units = "in"
  )
}
```

### Light treatment vs pre-light treatment (1 kHz - 10 kHz) as shown by key acoustic indices

![Image](https://github.com/user-attachments/assets/f2104c4f-8cf8-408a-9339-2aacc04a422a) 

Darker red bars further to the left indicate _lower_ values of acoustic indices during the preiod when the light was on. Therefore, possibly _less_ stridulation but will need to check this against count data and what it is the acoustic index is measuring exactly. 

Darker blue bars further to the right indicate _higher_ values of acoustic indices during the preiod when the light was on. Therefore, possibly _more_ stridulation but will need to check this against count data and what it is the acoustic index is measuring exactly. 

Empty spaces / no bars indicated a non-signifcant result for that acoustic index. 

## False-colour spectrograms 

### Old Sneed Park - 25th August 2025

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

# === LOOP THROUGH FILES (REQUIRED FOR DATA GENERATION) ===
for (audio_file in file_list) {
  try({
    wave <- readWave(audio_file)
    y <- wave@left
    sr <- wave@samp.rate
    
    n_fft <- 16384
    hop_length <- n_fft / 2
    
    sg <- specgram(y, n = n_fft, Fs = sr, overlap = n_fft - hop_length)
    S <- abs(sg$S)
    S <- S / max(S)
    S_db <- 10 * log10(S + 1e-10)
    
    entropy_values <- apply(S_db, 1, calculate_entropy)
    aci_values <- apply(S_db, 1, calculate_aci)
    noise_values <- apply(S_db, 1, calculate_background_noise)
    
    freq_bins <- seq(0, sr / 2, length.out = n_fft / 2 + 1)[1:nrow(S_db)]
    
    entropy_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                       Frequency = freq_bins,
                                                       Entropy = entropy_values)
    aci_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                   Frequency = freq_bins,
                                                   ACI = aci_values)
    background_noise_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                                Frequency = freq_bins,
                                                                Background_Noise = noise_values)
  }, silent = TRUE)
}

# === COMBINE AND PROCESS METRICS ===
entropy_all <- do.call(rbind, entropy_list)
aci_all <- do.call(rbind, aci_list)
background_noise_all <- do.call(rbind, background_noise_list)

# === EXTRACT TIME FROM FILENAMES ===
extract_time <- function(df) {
  df %>%
    mutate(TimeString = substr(File, 10, 15)) %>%
    select(-File)
}

entropy_all <- extract_time(entropy_all)
aci_all <- extract_time(aci_all)
background_noise_all <- extract_time(background_noise_all)

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

entropy_all <- add_datetime(entropy_all)
aci_all <- add_datetime(aci_all)
background_noise_all <- add_datetime(background_noise_all)

# === SCALE INDICES ===
scale_vec <- function(x) (x - min(x)) / (max(x) - min(x))

entropy_all$Scaled_Entropy <- scale_vec(entropy_all$Entropy)
aci_all$Scaled_ACI <- scale_vec(aci_all$ACI)
background_noise_all$Scaled_Background_Noise <- scale_vec(background_noise_all$Background_Noise)

# === COMBINE RGB ===
combined_data <- data.frame(
  Scaled_Entropy = entropy_all$Scaled_Entropy,
  Scaled_ACI = aci_all$Scaled_ACI,
  Scaled_Background_Noise = background_noise_all$Scaled_Background_Noise
)

RGB_Data <- data.frame(Color = rgb(
  combined_data$Scaled_Entropy,
  combined_data$Scaled_ACI,
  combined_data$Scaled_Background_Noise
))

# === CREATE FINAL DATA FRAME (with kHz) ===
combined_df <- data.frame(
  Frequency = entropy_all$Frequency / 1000, 
  Time = entropy_all$Time,
  Color = RGB_Data$Color
)

# === REMOVE BACKGROUND NOISE ===
hex_table <- table(combined_df$Color)
hex_df <- as.data.frame(hex_table)
colnames(hex_df) <- c("HEX", "Count")
threshold <- quantile(hex_df$Count, 0.9)
dominant_hexes <- hex_df$HEX[hex_df$Count >= threshold]

replace_dominant_hex <- function(color) {
  if (color %in% dominant_hexes) "#000000" else color
}

combined_df$Adjusted_Color <- sapply(combined_df$Color, replace_dominant_hex)


# ====================================================================
# === PERIOD DEFINITION AND PLOTTING SETUP ===

# === DEFINE EXPERIMENTAL PERIODS (Corrected Date Alignment) ===
periods <- data.frame(
  Label = c("Natural darkness (Phase I)", "Light treatment (Phase II)", "Natural darkness (Phase III)"),
  Start = as.POSIXct(c("2025-08-25 21:11:00",
                       "2025-08-25 22:11:00", "2025-08-25 23:11:00"), tz = "UTC"),
  # CORRECTED: Phase I End adjusted to 22:07:00 for clean transition
  End = as.POSIXct(c("2025-08-25 22:11:00",
                     "2025-08-25 23:11:00", "2025-08-26 00:11:00"), tz = "UTC"), 
  Fill = c("gray60", "#FFC300", "gray60")
)


# === DEFINE PADDED Y-AXIS LIMITS (in kHz) ===
# This assumes combined_df is now populated.
y_max <- max(combined_df$Frequency) 
y_min_padded <- -0.5 
y_max_padded <- y_max + 0.5 

# === PLOT MAIN SPECTROGRAM ===
# NOTE: Using the full combined_df and letting scale_x_datetime define the visible range.
p_spec <- ggplot(combined_df, aes(x = Time, y = Frequency)) +
  geom_tile(aes(fill = Adjusted_Color)) +
  scale_fill_identity() +
  scale_x_datetime(limits = c(min(periods$Start), max(periods$End)), 
                   date_breaks = "15 min", 
                   date_labels = "%H:%M") + 
  scale_y_continuous(limits = c(y_min_padded, y_max_padded), expand = c(0, 0), 
                     breaks = seq(from = 0, to = y_max_padded, by = 5)) +
  labs(x = "Time", y = "Frequency (kHz)") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1), 
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
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5, 5, 0, 5)
  )

# === COMBINE USING patchwork ===
final_plot <- p_spec / p_periods + plot_layout(heights = c(1, 0.075)) 

# === SAVE ===
ggsave("25.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.pdf", plot = final_plot, width = 10, height = 7, dpi = 300)

cat("Plot saved as 25.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.pdf\n")

ggsave("25.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.jpeg", plot = final_plot, width = 10, height = 7, dpi = 300)

cat("Plot saved as 25.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.jpeg\n"

```

### Old Sneed Park 26th August 2024

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
directory <- "C:/Users/Administrador/Downloads/Light pollution sound files/OSP_26.08.2024/OSP_26.08.2024"

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

# === LOOP THROUGH FILES ===
for (audio_file in file_list) {
  try({
    wave <- readWave(audio_file)
    y <- wave@left
    sr <- wave@samp.rate
    
    n_fft <- 16384
    hop_length <- n_fft / 2
    
    sg <- specgram(y, n = n_fft, Fs = sr, overlap = n_fft - hop_length)
    S <- abs(sg$S)
    S <- S / max(S)
    S_db <- 10 * log10(S + 1e-10)
    
    entropy_values <- apply(S_db, 1, calculate_entropy)
    aci_values <- apply(S_db, 1, calculate_aci)
    noise_values <- apply(S_db, 1, calculate_background_noise)
    
    freq_bins <- seq(0, sr / 2, length.out = n_fft / 2 + 1)[1:nrow(S_db)]
    
    entropy_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                       Frequency = freq_bins,
                                                       Entropy = entropy_values)
    aci_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                   Frequency = freq_bins,
                                                   ACI = aci_values)
    background_noise_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                                Frequency = freq_bins,
                                                                Background_Noise = noise_values)
  }, silent = TRUE)
}

# === COMBINE AND PROCESS METRICS ===
entropy_all <- do.call(rbind, entropy_list)
aci_all <- do.call(rbind, aci_list)
background_noise_all <- do.call(rbind, background_noise_list)

# === EXTRACT TIME FROM FILENAMES ===
extract_time <- function(df) {
  df %>%
    mutate(TimeString = substr(File, 10, 15)) %>%
    select(-File)
}

entropy_all <- extract_time(entropy_all)
aci_all <- extract_time(aci_all)
background_noise_all <- extract_time(background_noise_all)

# === ADD FULL DATETIME (for overnight recording) ===
add_datetime <- function(df) {
  df %>%
    mutate(
      Hour = as.numeric(substr(TimeString, 1, 2)),
      Date = ifelse(Hour < 12, "2024-08-27", "2024-08-26"), 
      TimeStr = paste0(substr(TimeString, 1, 2), ":",
                       substr(TimeString, 3, 4), ":",
                       substr(TimeString, 5, 6)),
      Time = as.POSIXct(paste(Date, TimeStr), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ) %>%
    select(-TimeString, -Hour, -Date, -TimeStr)
}

entropy_all <- add_datetime(entropy_all)
aci_all <- add_datetime(aci_all)
background_noise_all <- add_datetime(background_noise_all)

# === SCALE INDICES ===
scale_vec <- function(x) (x - min(x)) / (max(x) - min(x))

entropy_all$Scaled_Entropy <- scale_vec(entropy_all$Entropy)
aci_all$Scaled_ACI <- scale_vec(aci_all$ACI)
background_noise_all$Scaled_Background_Noise <- scale_vec(background_noise_all$Background_Noise)

# === COMBINE RGB ===
combined_data <- data.frame(
  Scaled_Entropy = entropy_all$Scaled_Entropy,
  Scaled_ACI = aci_all$Scaled_ACI,
  Scaled_Background_Noise = background_noise_all$Scaled_Background_Noise
)

RGB_Data <- data.frame(Color = rgb(
  combined_data$Scaled_Entropy,
  combined_data$Scaled_ACI,
  combined_data$Scaled_Background_Noise
))

# === CREATE FINAL DATA FRAME (with kHz) ===
combined_df <- data.frame(
  Frequency = entropy_all$Frequency / 1000, 
  Time = entropy_all$Time,
  Color = RGB_Data$Color
)

# === REMOVE BACKGROUND NOISE ===
hex_table <- table(combined_df$Color)
hex_df <- as.data.frame(hex_table)
colnames(hex_df) <- c("HEX", "Count")
threshold <- quantile(hex_df$Count, 0.9)
dominant_hexes <- hex_df$HEX[hex_df$Count >= threshold]

replace_dominant_hex <- function(color) {
  if (color %in% dominant_hexes) "#000000" else color
}

combined_df$Adjusted_Color <- sapply(combined_df$Color, replace_dominant_hex)

# ====================================================================
# === PERIOD DEFINITION AND PLOTTING SETUP (UPDATED FOR OSP_26) ===
periods <- data.frame(
  Label = c("Natural darkness (Phase I)", 
            "Light treatment (Phase II)", 
            "Natural darkness (Phase III)"),
  Start = as.POSIXct(c("2024-08-26 21:08:00",
                       "2024-08-26 22:08:00",
                       "2024-08-26 23:08:00"), tz = "UTC"),
  End = as.POSIXct(c("2024-08-26 22:08:00",
                     "2024-08-26 23:08:00",
                     "2024-08-27 00:08:00"), tz = "UTC"),
  Fill = c("gray60", "#FFC300", "gray60")
)

# === DEFINE PADDED Y-AXIS LIMITS (in kHz) ===
y_max <- max(combined_df$Frequency) 
y_min_padded <- -0.5 
y_max_padded <- y_max + 0.5 

# === PLOT MAIN SPECTROGRAM ===
p_spec <- ggplot(combined_df, aes(x = Time, y = Frequency)) +
  geom_tile(aes(fill = Adjusted_Color)) +
  scale_fill_identity() +
  scale_x_datetime(limits = c(min(periods$Start), max(periods$End)), 
                   date_breaks = "15 min", 
                   date_labels = "%H:%M") + 
  scale_y_continuous(limits = c(y_min_padded, y_max_padded), expand = c(0, 0), 
                     breaks = seq(from = 0, to = y_max_padded, by = 5)) +
  labs(x = "Time", y = "Frequency (kHz)") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1),
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
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    plot.margin = margin(5, 5, 0, 5)
  )

# === COMBINE USING patchwork ===
final_plot <- p_spec / p_periods + plot_layout(heights = c(1, 0.075)) 

# === SAVE ===
ggsave("26.08.2024_fullfreq_periodbars_REVISED_RENAMED_KHZ.pdf", 
       plot = final_plot, width = 10, height = 7, dpi = 300)
cat("Plot saved as 26.08.2024_fullfreq_periodbars_REVISED_RENAMED_KHZ.pdf\n")

ggsave("26.08.2024_fullfreq_periodbars_REVISED_RENAMED_KHZ.jpeg", 
       plot = final_plot, width = 10, height = 7, dpi = 300)
cat("Plot saved as 26.08.2024_fullfreq_periodbars_REVISED_RENAMED_KHZ.jpeg\n")
```

### Old Sneed Park 27th August 2025

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
directory <- "C:/Users/Administrador/Downloads/Light pollution sound files/OSP_3_27.08.25/OSP_3_27.08.25"

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

for (audio_file in file_list) {
  try({
    wave <- readWave(audio_file)
    y <- wave@left
    sr <- wave@samp.rate
    
    n_fft <- 16384
    hop_length <- n_fft / 2
    
    sg <- specgram(y, n = n_fft, Fs = sr, overlap = n_fft - hop_length)
    S <- abs(sg$S)
    S <- S / max(S)
    S_db <- 10 * log10(S + 1e-10)
    
    entropy_values <- apply(S_db, 1, calculate_entropy)
    aci_values <- apply(S_db, 1, calculate_aci)
    noise_values <- apply(S_db, 1, calculate_background_noise)
    
    freq_bins <- seq(0, sr / 2, length.out = n_fft / 2 + 1)[1:nrow(S_db)]
    
    entropy_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                       Frequency = freq_bins,
                                                       Entropy = entropy_values)
    aci_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                   Frequency = freq_bins,
                                                   ACI = aci_values)
    background_noise_list[[basename(audio_file)]] <- data.frame(File = basename(audio_file),
                                                                Frequency = freq_bins,
                                                                Background_Noise = noise_values)
  }, silent = TRUE)
}

# === COMBINE AND PROCESS METRICS ===
entropy_all <- do.call(rbind, entropy_list)
aci_all <- do.call(rbind, aci_list)
background_noise_all <- do.call(rbind, background_noise_list)

# === EXTRACT TIME FROM FILENAMES ===
extract_time <- function(df) {
  df %>%
    mutate(TimeString = substr(File, 10, 15)) %>%
    select(-File)
}

entropy_all <- extract_time(entropy_all)
aci_all <- extract_time(aci_all)
background_noise_all <- extract_time(background_noise_all)

# === NEW: ADD FULL DATETIME TO HANDLE OVERNIGHT ===
add_datetime <- function(df) {
  df %>%
    mutate(
      Hour = as.numeric(substr(TimeString, 1, 2)),
      Date = ifelse(Hour < 12, "2025-08-28", "2025-08-27"), 
      TimeStr = paste0(substr(TimeString, 1, 2), ":",
                       substr(TimeString, 3, 4), ":",
                       substr(TimeString, 5, 6)),
      Time = as.POSIXct(paste(Date, TimeStr), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ) %>%
    select(-TimeString, -Hour, -Date, -TimeStr) 
}

entropy_all <- add_datetime(entropy_all)
aci_all <- add_datetime(aci_all)
background_noise_all <- add_datetime(background_noise_all)

# === SCALE INDICES ===
scale_vec <- function(x) (x - min(x)) / (max(x) - min(x))

entropy_all$Scaled_Entropy <- scale_vec(entropy_all$Entropy)
aci_all$Scaled_ACI <- scale_vec(aci_all$ACI)
background_noise_all$Scaled_Background_Noise <- scale_vec(background_noise_all$Background_Noise)

# === COMBINE RGB ===
combined_data <- data.frame(
  Scaled_Entropy = entropy_all$Scaled_Entropy,
  Scaled_ACI = aci_all$Scaled_ACI,
  Scaled_Background_Noise = background_noise_all$Scaled_Background_Noise
)

RGB_Data <- data.frame(Color = rgb(
  combined_data$Scaled_Entropy,
  combined_data$Scaled_ACI,
  combined_data$Scaled_Background_Noise
))

# === CREATE FINAL DATA FRAME (with kHz) ===
combined_df <- data.frame(
  Frequency = entropy_all$Frequency / 1000, 
  Time = entropy_all$Time,
  Color = RGB_Data$Color
)

# === REMOVE BACKGROUND NOISE ===
hex_table <- table(combined_df$Color)
hex_df <- as.data.frame(hex_table)
colnames(hex_df) <- c("HEX", "Count")
threshold <- quantile(hex_df$Count, 0.9)
dominant_hexes <- hex_df$HEX[hex_df$Count >= threshold]

replace_dominant_hex <- function(color) {
  if (color %in% dominant_hexes) "#000000" else color
}

combined_df$Adjusted_Color <- sapply(combined_df$Color, replace_dominant_hex)

# For the plotting code to be runnable:
# === DEFINE EXPERIMENTAL PERIODS (Original - REQUIRED) ===
periods <- data.frame(
  Label = c("Natural darkness (Phase I)", "Light treatment (Phase II)", "Natural darkness (Phase III)"),
  Start = as.POSIXct(c("2025-08-27 21:07:00",
                       "2025-08-27 22:07:00", "2025-08-27 23:07:00"), tz = "UTC"),
  End = as.POSIXct(c("2025-08-27 22:11:00",
                     "2025-08-27 23:07:00", "2025-08-28 00:07:00"), tz = "UTC"), 
  Fill = c("gray60", "#FFC300", "gray60")
)


# === DEFINE PADDED Y-AXIS LIMITS (in kHz) ===
# This assumes combined_df has been created and its Frequency column is in kHz
y_max <- max(combined_df$Frequency) 
y_min_padded <- -0.5 
y_max_padded <- y_max + 0.5 

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
  scale_y_continuous(limits = c(y_min_padded, y_max_padded), expand = c(0, 0),
                     breaks = seq(from = 0, to = y_max_padded, by = 5)) +
  labs(x = "Time", y = "Frequency (kHz)") +
  theme_bw() +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(angle = 0, hjust = 1), 
    axis.ticks.x = element_line(),                    
    plot.margin = margin(0, 5, 0, 5)
  )

p_spec

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
ggsave("27.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.pdf", plot = final_plot, width = 10, height = 7, dpi = 300)

cat("Plot saved as 27.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.pdf\n")

ggsave("27.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.jpeg", plot = final_plot, width = 10, height = 7, dpi = 300)

cat("Plot saved as 27.08.2025_fullfreq_periodbars_REVISED_RENAMED_KHZ.jpeg\n")
```

## PCAs for all Old Sneed Park dates

```
# ===============================
# 📦 Load Required Libraries
# ===============================
library(dplyr)
library(lubridate)
library(stringr)
library(corrplot)
library(caret)
library(ggplot2)

# ===============================
# 🧩 Data Loading
# ===============================
# Load data
Full_OSP_26_2024 <- read.csv("OSP_26_08_24_full_alpha_acoustic_indices_results.csv")
Full_OSP_25_2025 <- read.csv("OSP_25_08_25_full_alpha_acoustic_indices_results.csv")
Full_OSP_27_2025 <- read.csv("OSP_27_08_25_full_alpha_acoustic_indices_results.csv")

# Merge all data sets into one
merged_data <- rbind(Full_OSP_26_2024, Full_OSP_25_2025, Full_OSP_27_2025)
head(merged_data)

# ===============================
# ⚙️ Numeric Data Preparation for PCA
# ===============================
# Subset numeric columns
numeric_data <- merged_data[, 2:61]

# --- FIX #1: Remove Zero-Variance Columns FIRST ---
nzv_indices <- nearZeroVar(numeric_data, saveMetrics = FALSE)
if (length(nzv_indices) > 0) {
  cat("Removing", length(nzv_indices), "zero-variance columns:\n")
  cat(paste(colnames(numeric_data)[nzv_indices], collapse = ", "), "\n")
  numeric_data <- numeric_data[, -nzv_indices, drop = FALSE]
} else {
  cat("✅ No zero-variance columns found.\n")
}

# --- FIX #2: Remove Highly Correlated Features ---
if (ncol(numeric_data) > 1) {
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  high_corr_indices <- findCorrelation(cor_matrix, cutoff = 0.8, names = TRUE)
  
  if (length(high_corr_indices) > 0) {
    cat("Removing", length(high_corr_indices), "highly correlated columns:\n")
    cat(paste(high_corr_indices, collapse = ", "), "\n")
    numeric_data <- numeric_data[, !colnames(numeric_data) %in% high_corr_indices, drop = FALSE]
  } else {
    cat("✅ No highly correlated features to remove.\n")
  }
}

cat("✅ Final numeric columns for PCA:", ncol(numeric_data), "\n")
if (ncol(numeric_data) < 2) {
  stop("Not enough numeric columns with variation left for PCA. Check your data.")
}

# ===============================
# 🕒 Metadata Preparation
# ===============================
metadata <- data.frame(Filename = merged_data$filename)

metadata <- metadata %>%
  mutate(
    # FIX: Use a simple regex to extract just OSP_25 or OSP_27
    Site = str_extract(Filename, "OSP_25|OSP_27"),
    datetime_str = str_extract(Filename, "\\d{8}_\\d{6}"),
    Datetime = as.POSIXct(datetime_str, format = "%Y%m%d_%H%M%S", tz = "UTC")
  )

# ===============================
# # Add Treatment Periods (UPDATED SECTION)
# ===============================
metadata <- metadata %>%
  mutate(
    Treatment = case_when(
      # OSP_25 Treatments (2025-08-25 to 2025-08-26)
      (Datetime >= ymd_hms("2025-08-25 21:11:00") & Datetime < ymd_hms("2025-08-25 22:11:00")) ~ "Natural darkness (Phase I)",
      (Datetime >= ymd_hms("2025-08-25 22:11:00") & Datetime < ymd_hms("2025-08-25 23:11:00")) ~ "Light treatment (Phase II)",
      (Datetime >= ymd_hms("2025-08-25 23:11:00") & Datetime < ymd_hms("2025-08-26 00:11:00")) ~ "Natural darkness (Phase III)",
      
      # OSP_26 Treatments (2024-08-26 to 2024-08-27)
      (Datetime >= ymd_hms("2025-08-26 21:08:00") & Datetime < ymd_hms("2024-08-26 22:08:00")) ~ "Natural darkness (Phase I)",
      (Datetime >= ymd_hms("2025-08-26 22:08:00") & Datetime < ymd_hms("2024-08-26 23:08:00")) ~ "Light treatment (Phase II)",
      (Datetime >= ymd_hms("2025-08-26 23:08:00") & Datetime < ymd_hms("2024-08-27 00:08:00")) ~ "Natural darkness (Phase III)",
      
      # OSP_27 Treatments (2025-08-27 to 2025-08-28)
      (Datetime >= ymd_hms("2025-08-27 21:07:00") & Datetime < ymd_hms("2025-08-27 22:07:00")) ~ "Natural darkness (Phase I)",
      (Datetime >= ymd_hms("2025-08-27 22:07:00") & Datetime < ymd_hms("2025-08-27 23:07:00")) ~ "Light treatment (Phase II)",
      (Datetime >= ymd_hms("2025-08-27 23:07:00") & Datetime < ymd_hms("2025-08-28 00:07:00")) ~ "Natural darkness (Phase III)",
      
      TRUE ~ "Other"
    )
  )


# ===============================
# 📊 PCA Analysis
# ===============================
set.seed(123)
pca_res <- prcomp(numeric_data, center = TRUE, scale. = TRUE)
print(summary(pca_res))

# Variance explained
var_explained <- round(100 * (pca_res$sdev^2 / sum(pca_res$sdev^2)), 1)

# Calculate Eigenvalues
# Eigenvalues are the square of the standard deviations (sdev)
eigenvalues <- pca_res$sdev^2

# Name them for a clean output
names(eigenvalues) <- paste0("PC", 1:length(eigenvalues))

cat("--- Eigenvalues for each Component ---\n")
print(eigenvalues)
cat("\n")

# Combine PCA scores with metadata
pca_scores <- as.data.frame(pca_res$x)
pca_scores$Site <- metadata$Site
pca_scores$Treatment <- metadata$Treatment

# ===============================
# 🎨 PCA Plots with Treatment Colors & Site Shapes
# ===============================
# Define colors for Treatments
Treatment_colors <- c(
  "Natural darkness (Phase I)" = "gray60",
  "Light treatment (Phase II)" = "#FFC300",
  "Natural darkness (Phase III)" = "gray60"
)

# Define shapes for Sites
Site_shapes <- c(
  "OSP_25" = 16,  # circle
  "OSP_27" = 17   # triangle
)

# Filter data for plotting
pca_scores_filtered <- pca_scores %>%
  filter(Treatment != "Other") %>%
  mutate(
    Treatment = factor(Treatment,
                       levels = c("Natural darkness (Phase I)", 
                                  "Light treatment (Phase II)", 
                                  "Natural darkness (Phase III)")),
    Site = factor(Site)
  )

# Check that both sites are present in the filtered data
cat("\nSites included in the final plot data:\n")
print(table(pca_scores_filtered$Site))
cat("\n")

# PCA plot (color=Treatment, shape=Site)
p3 <- ggplot(pca_scores_filtered, aes(x = PC1, y = PC2, color = Treatment, shape = Site)) +
  geom_point(size = 2, alpha = 0.7) +
  # Ellipse is grouped by Treatment (the color variable)
  stat_ellipse(aes(group = Treatment), level = 0.95, linetype = 2, size = 1) + 
  scale_color_manual(values = Treatment_colors) + # Use Treatment colors
  scale_shape_manual(values = Site_shapes) +     # Use Site shapes
  theme_bw() +
  labs(
    x = paste0("PC1 (", sprintf("%.1f", var_explained[1]), "%)"),
    y = paste0("PC2 (", sprintf("%.1f", var_explained[2]), "%)"),
    color = "Treatment", 
    shape = "Site"       
  ) +
  facet_wrap(~ Treatment, ncol = 3)

print(p3)

ggsave("Full_OSP_25_and_27_with_sites.jpeg", plot = p3, width = 10, height = 3.5, dpi = 300)


#### Scree plot ####

# ===============================
# 📊 Scree Plot Visualization
# ===============================

# Make sure ggplot2 is loaded
library(ggplot2)

# 1. Create a data frame with the PCA variance information
# We use the 'pca_res' object from the previous script
pca_variance <- data.frame(
  Component = paste0("PC", 1:length(pca_res$sdev)),
  VarianceExplained = 100 * (pca_res$sdev^2 / sum(pca_res$sdev^2))
)

# Calculate cumulative variance
pca_variance$CumulativeVariance <- cumsum(pca_variance$VarianceExplained)

# Ensure components are in the correct order for plotting
pca_variance$Component <- factor(pca_variance$Component, 
                                 levels = pca_variance$Component)

# ===============================
# Plot 1: Classic Scree Plot (Bar Chart)
# ===============================
# This helps you find the "elbow"
scree_plot_classic <- ggplot(pca_variance, aes(x = Component, y = VarianceExplained, group = 1)) +
  geom_col(fill = "steelblue", alpha = 0.8) +
  geom_point(size = 2, color = "darkred") +
  geom_line(color = "darkred", linetype = "dashed") +
  theme_bw() +
  labs(
    title = "Scree Plot",
    x = "Principal Component",
    y = "Percentage of Variance Explained"
  ) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

print(scree_plot_classic)

# ===============================
# Plot 2: Scree Plot with Cumulative Variance (Pareto Plot)
# ===============================
# This helps you decide how many components to keep
scree_plot_cumulative <- ggplot(pca_variance, aes(x = Component)) +
  # Bar plot for individual variance
  geom_col(aes(y = VarianceExplained), fill = "steelblue", alpha = 0.8) +
  
  # Line and point plot for cumulative variance
  geom_point(aes(y = CumulativeVariance), size = 2, color = "darkred") +
  geom_line(aes(y = CumulativeVariance, group = 1), color = "darkred", linetype = "dashed") +
  
  # Add a horizontal line at 80% or 90% for reference
  geom_hline(yintercept = 80, linetype = "dotted", color = "black", size = 1) +
  
  # Use a secondary y-axis to show cumulative percentage
  scale_y_continuous(
    name = "Percentage of variance explained",
    sec.axis = sec_axis(~., name = "Cumulative variance (%)")
  ) +
  theme_bw() +
  labs(
    x = "Principal component",
    y = "Percentage of variance explained"
  ) +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1),
    axis.title.y.right = element_text(color = "darkred"),
    axis.text.y.right = element_text(color = "darkred")
  )

print(scree_plot_cumulative)

# Save your preferred plot
# ggsave("PCA_Scree_Plot.jpeg", plot = scree_plot_classic, width = 8, height = 5, dpi = 300)
ggsave("PCA_Scree_Plot_Cumulative.jpeg", plot = scree_plot_cumulative, width = 8, height = 5, dpi = 300)

#### PCA loadings ####

# ===============================
# 📊 PCA Loadings Extraction (FULL MATRIX)
# ===============================
library(dplyr)
# tidyr is no longer needed since the second section is removed.

# 1. Extract the raw loadings matrix
# This matrix contains the correlation of each original variable (row) 
# with each Principal Component (column).
loadings_matrix <- pca_res$rotation

# Convert to a data frame and add the variable names
loadings_df <- as.data.frame(loadings_matrix) %>%
  mutate(Acoustic_Index = rownames(.)) %>%
  relocate(Acoustic_Index)

# Display the full, un-filtered loadings matrix
loadings_full_matrix <- loadings_df

cat("--- Full PCA Loadings Matrix (All Indices vs. All Components) ---\n")
# Note: Since this is a wide table, R may truncate the output columns
print(loadings_full_matrix)
cat("\n")

head(loadings_full_matrix)

```
