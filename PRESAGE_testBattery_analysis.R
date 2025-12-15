# ==============================================================================
# PRESAGE cross-domain test battery -- analysis code for control group
# ==============================================================================
#
# Manuscript: "A cross-domain test battery for comprehensive hearing loss
# characterisation using functional, physiological, and vestibular measures"
#
# Code version: 1.0 (December, 2025)
# 
# Authors: 
# - Shiran Koifman (shiran.koifman@uol.de)
# - Mareike Buhl (mareike.buhl@pasteur.fr)

# The code: 
# - Loads a harmonised dataset (a single table integrating French and German data).
# - Performs pre-processing used in the manuscript (e.g., better-ear PTA definition, 
#   missing data checks, variables calculations, & harmonisation of data across centres).
# - Runs statistical models and produces manuscript figures/tables.
#   The code is divided into subparts for convenient browsing using the outline option in R.
#   Figures and table numbers are specified in the relevant subparts titles.

# Data availability:
# - The analysis dataset used for this manuscript/code is not included in this release.
# - The current data includes only participants from the control group (group=1).
# - Complete data are planned to be published in an open repository together with the hearing-impaired
#   data at a later stage of the project. 
# - Data are available upon motivated request. 
#
# Inputs: 
# - .CSV or .XLSX files located in ./input/
# - R functions located in ./functions/
#
# Outputs: 
# - Figures & tables written to ./output/
#
# Reproducibility: tested with R version 4.3.2.
# ==============================================================================

# clearing console, objects, & plots 
cat("\014")  
rm(list = ls())

# get the path where the script is saved
script_path = dirname(rstudioapi::getActiveDocumentContext()$path)

# set the working directory to the script's directory
setwd(script_path)
fileDir = getwd()

# --- Load packages ------------------------------------------------------------
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tidyr)){install.packages("tidyr")}
if(!require(ggpubr)){install.packages("ggpubr")}
if(!require(purrr)){install.packages("purrr")}
if(!require(stringr)){install.packages("stringr")}
if(!require(readxl)){install.packages("readxl")}
if(!require(data.table)){install.packages("data.table")}
if(!require(grid)){install.packages("grid")}
if(!require(patchwork)){install.packages("patchwork")}
if(!require(cowplot)){install.packages("cowplot")}
if(!require(ggtext)){install.packages("ggtext")}
if(!require(scales)){install.packages("scales")}
if(!require(MatchIt)){install.packages("MatchIt")}
if(!require(broom)){install.packages("broom")}
if(!require(readr)){install.packages("readr")}
if(!require(kableExtra)){install.packages("kableExtra")}

# --- Initialisations ----------------------------------------------------------

# --- Some buttons ---
# 1. remove uARHL (1/0, on/off)
dropGroup = 1

# 2. remove ids based on recruitment criteria (1/0, on/off)
dropCriteria = 1

# set correlation test for the plots 
corrMethod = "spearman" #"pearson"
date = Sys.Date()

dataFileName = 'PRESAGE_commonDataFormat_Example.xlsx'
filePath = file.path(fileDir,'input',dataFileName)
dataFramePath = file.path(fileDir,'input','df_w.RData')

# --- Load data from xls table -------------------------------------------------

# check if 'df_w.RData' exist, else, load data and save 'df_w.RData' in inputs
# if (file.exists(dataFramePath)) {
#   load(dataFramePath)
# } else {
#   
#   source(file.path(fileDir,"functions","getData.R"))
#   
#   df_w = getData(filePath)
#   
#   save(df_w, file = dataFramePath) 
# }

# source getData to load and preprocess the data
source(file.path(fileDir,"functions","getData.R"))

df_w = getData(filePath)

### --- Catch all NA cases -----------------------------------------------------

empty_cell_example = df_w$tin_2000_flag[38]

# replace specific empty strings and NA with NA
replace_empty_with_na <- function(x) {
  empty_strings = c("", " ", empty_cell_example)
  if (is.character(x)) {
    x = ifelse(trimws(x) %in% empty_strings, NA, x)
    return(x)
  } else {
    return(x)
  }
}

# apply function to each column using mutate(across(...))
df_w = df_w %>%
  mutate(across(everything(), ~ replace_empty_with_na(.)))

### --- Some filtering ---------------------------------------------------------
if (dropGroup==1){
df_w = df_w %>% filter(group=="1") %>% droplevels()
}

# Drop id's if not meeting recruitment criteria? 
if (dropCriteria==1){
  
  # Age > 39 years?
  df_w = df_w %>% filter(age>"39") %>% droplevels()   
  df_w = df_w %>% filter(id!="2021-062-C057-SS") %>% droplevels()   
}

### --- Age-corrected PTA4 -----------------------------------------------------

# Load PTA ISO norms 
# [based on extrapolated DIN EN ISO 7029 norms for NH male/female subjects aged 20 to 80 years]
ISO_norms = read.csv(file.path(fileDir,"input",'ISO_7029_norms.csv'), header=T) %>% lapply(., as.numeric) %>% as.data.frame() 

# Match normative PTA to subject's age- & sex-dependent norms [PTA_50_1 for 'male'(=1) or PTA_50_2 for 'female' (=2) or 'other'(=3)]:
normative_pta = mapply(
  function(age, sex) {
    column_name = if (sex == "1") "PTA_50_1" else "PTA_50_2"
    ISO_norms[match(age, ISO_norms$age), column_name]
  },
  age = df_w$age,
  sex = df_w$sex
)

# Define age groups from 40 to 90 years with 10 years steps
df_w$age_group = cut(df_w$age,breaks = seq(40, 100, by = 10),
  right = FALSE, include.lowest = TRUE,
  labels = c("40–49", "50–59", "60–69", "70–79", "80–89", "90+"))

### --- Check for missing data -------------------------------------------------

df_w_with999 = df_w # keep a copy to get frequency of not measurable cases (NM)

# Change 999 to NA for the analysis
df_w[df_w == 999] = NA

# change centre as factor
df_w$centre = factor(df_w$centre, levels = c("DE", "FR"), labels = c("DE", "FR"))

### --- Inspect criteria & descriptive -----------------------------------------

count(df_w, sex, centre)
count(df_w, sex, group)

# PTA4 side difference > 15 dB HL?
df_w$PTA4_diff = df_w$PTA4_L -  df_w$PTA4_R

df_w$id[which( abs(df_w$PTA4_diff) > 15 )]

# difference between ears - stats:
# summary(abs(df_w$PTA4_diff))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.250   2.500   2.614   4.000   8.000 

# by PTA4 (better-ear)
# psych::describeBy(PTA4_BE~centre, data=df_w, mat=TRUE, digits=1)

# D_est
# psych::describeBy(Dest~centre, data=df_w, mat=TRUE, digits=1)

df_w %>% psych::describeBy(age~centre, data=., mat=TRUE, digits=1)
df_w %>% psych::describeBy(age~group, data=., mat=TRUE, digits=1)
df_w %>% count(., sex)

# Age histogram
ggplot(df_w, aes(x = age, fill = centre)) +
  geom_histogram(binwidth = 5, alpha = 0.6, position = "identity") +
  labs(x = "Age (years)", y = "Count", title = "Age distribution by centre") +
  theme_minimal(base_size = 14)

## -- Match UOL/IDA participants by age & PTA  -------------------------------
# --> used at the end of script for centre effect analysis.
# --> for now, use it to plot the PTA and age by ISO (Fig 1) to identify between
# matched and unmatched IDs

# calculate propensity score and match patients from both groups based on that -> to have a fair comparison (first to learn about the correspondence of our two centers, but second as alternative sample choice for linear model below)
# Estimate propensity scores
columns_to_check <- c("age", "PTA4_BE")
df_complete <- df_w[complete.cases(df_w[, columns_to_check]), ]

# ps_model <- glm(centre ~ age + PTA4_BE, data = df_complete)
# df_w$ps_score <- predict(ps_model, type = "response")

# Match participants based on propensity scores 
# m.out <- matchit(centre ~ age + PTA4_BE, 
#               data = df_complete, method = "nearest", distance = "glm") # , m.order = "largest")  #  method = "nearest", distance = "glm")

# Match participants based on Mahalanobis distances 
m.out <- matchit(centre ~ age + PTA4_BE, 
                 data = df_complete,
                 method = "nearest",  # Nearest neighbor matching
                 distance = "mahalanobis",  # Better than propensity for small samples
                 ratio = 1,  # 1:1 matching (one French for each German)
                 replace = FALSE)  # Each French subject used only once

#summary(m.out)
#plot(summary(m.out))

# plot(m.out, type = "jitter", interactive = FALSE) # does not work for mahalanobis
# matching_density_plot <- plot(m.out, type = "density", interactive = FALSE, which.xs = ~age + PTA4_BE)


# Get matched dataset
matched_data <- match.data(m.out) # this data frame looks plausible 
# table(matched_data$centre)

id_matched <- matched_data$id
subclass_matched <- matched_data$subclass

# investigate matched_data 
# - number of datapoints per center
# - obtained age and PTA distributions, especially means 
# - balance 

# matched_data %>%
#   arrange(subclass) %>%
#   ggplot(aes(x = age, y = PTA4_BE, color = factor(centre))) +
#   geom_point() +
#   geom_line(aes(group = subclass), alpha = 0.5) +
#   labs(title = "Matched Pairs by Age and PTA",
#        color = "Center") +
#   theme_minimal()

## -- PTA4 ---------------------------------------------------------------------
# --- Get plot function ---
source(file.path(fileDir,"functions","plot_pta4.R"))

# Jitter is needed? find id's with the same age and PTA4
# df_w %>%
#   filter(id %in% id_matched, centre == "DE") %>%Ze
#   select(id, age, PTA4_BE) %>%
#   arrange(age, PTA4_BE) %>%
#   group_by(age, PTA4_BE) %>%
#   mutate(dup_count = n()) %>%
#   ungroup() %>%
#   filter(dup_count > 1)

# --> add a small jitter!
# id                   age PTA4_BE dup_count
# 1 PRESAGE_UOL_004    56    6.25         2
# 2 PRESAGE_UOL_013    56    6.25         2

PTA4 = plot_pta4(df_w, ISO_norms, id_matched = id_matched, subclass = subclass_matched)

# --- Save combined grid to PNG ---
ggsave(file.path(fileDir, "output", "PTA4_ISO.png"),
       plot = PTA4,
       width = 1500, height = 1000, dpi = 300, units = "px")

### --- Fig 1: Plot ISO PTA + density distributions ----------------------------
# Get matched data frame from MatchIt object m.out
matched_data <- match.data(m.out)

# Ensure centre is a factor with DE/FR
matched_data$centre <- factor(matched_data$centre, levels = c("DE", "FR"))

# Compute density ranges for ALL + MATCHED for AGE
d_all_age     <- density(df_w$age, na.rm = TRUE)
d_match_age   <- density(matched_data$age, na.rm = TRUE)
max_age_dens  <- 0.06 #max(d_all_age$y, d_match_age$y)

# Compute density ranges for ALL + MATCHED for PTA4
d_all_pta     <- density(df_w$PTA4_BE, na.rm = TRUE)
d_match_pta   <- density(matched_data$PTA4_BE, na.rm = TRUE)
max_pta_dens  <- 0.16 #max(d_all_pta$y, d_match_pta$y)

# --- Set axis limits ---
density_ylim_pta <- scale_y_continuous(limits = c(0, max_pta_dens), breaks = seq(0, max_pta_dens, by = 0.02))
density_ylim_age <- scale_y_continuous(limits = c(0, max_age_dens), breaks = seq(0, max_age_dens, by = 0.02))
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-10, 30), breaks = seq(-10, 30, 5))

# ALL by age
p_all_age <- ggplot(df_w, aes(x = age, colour = centre)) +
  geom_density(linewidth = 1.5, key_glyph = "path") + 
  scale_colour_manual(
    values = c("DE" = "black", "FR" = "grey60"),
    labels = c("DE" = "UOL", "FR" = "IDA")) +
  guides(colour = guide_legend(direction = "horizontal", override.aes = list(
    linetype = 1, linewidth = 1.5, fill = NA, shape = NA))) +
  labs(x = "Age [years]", y = "Kernel density estimate (KDE)", title = "All") +
  age_xlim +
  density_ylim_age +
  theme_bw() +
  theme(legend.position = c(0.97, 0.97),
        legend.justification = c(1, 1),
        legend.title = element_blank(),
        legend.key = element_rect(colour = NA, fill = NA),
        legend.margin = margin(0, 0, 0, 0),
        legend.box.spacing = unit(0, "pt"),
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 13, face = "bold", colour = "black"),
        axis.text = element_text(size = 11, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5))

# ALL by PTA
p_all_pta <- ggplot(df_w, aes(x = PTA4_BE, colour = centre)) +
  geom_density(linewidth = 1.5, key_glyph = "path") + 
  scale_colour_manual(
    values = c("DE" = "black", "FR" = "grey60"),
    labels = c("DE" = "UOL", "FR" = "IDA")) +
  guides(colour = guide_legend(direction = "horizontal", override.aes = list(
    linetype = 1, linewidth = 2, fill = NA, shape = NA))) +
  labs(x = "PTA [dB HL]", y = "Kernel density estimate (KDE)", title = NULL) +
  pta_xlim +
  density_ylim_pta +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 13, face = "bold", colour = "black"),
        axis.text = element_text(size = 11, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 11, face = "bold"),
        axis.title.y = element_text(size = 11, face = "bold"),
        plot.title = element_blank())

# MATCHED by age
p_matched_age <- ggplot(matched_data, aes(x = age, colour = centre)) +
  geom_density(linewidth = 1.5, key_glyph = "path") + 
  scale_colour_manual(
    values = c("DE" = "black", "FR" = "grey60"),
    labels = c("DE" = "UOL", "FR" = "IDA")) +
  guides(colour = guide_legend(direction = "horizontal", override.aes = list(
    linetype = 1, linewidth = 1.5, fill = NA, shape = NA))) +
  labs(x = "Age [years]", y = "Kernel density estimate (KDE)", title = "Matched") +
  age_xlim +
  density_ylim_age +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 13, face = "bold", colour = "black"),
        axis.text = element_text(size = 11, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 11, face = "bold"),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# MATCHED by PTA
p_matched_pta <- ggplot(matched_data, aes(x = PTA4_BE, colour = centre)) +
  geom_density(linewidth = 1.5, key_glyph = "path") + 
  scale_colour_manual(
    values = c("DE" = "black", "FR" = "grey60"),
    labels = c("DE" = "UOL", "FR" = "IDA")) +
  guides(colour = guide_legend(direction = "horizontal", override.aes = list(
    linetype = 1, linewidth = 1.5, fill = NA, shape = NA))) +
  labs(x = "PTA [dB HL]", y = "Kernel density estimate (KDE)", title = NULL) +
  pta_xlim +
  density_ylim_pta +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid.minor = element_blank(),
        legend.text = element_text(size = 13, face = "bold", colour = "black"),
        axis.text = element_text(size = 11, face = "bold", colour = "black"),
        axis.title.x = element_text(size = 11, face = "bold"),
        plot.title = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# combined_matching_density <-
#   PTA4 | ((p_all_age | p_matched_age) /
#   (p_all_pta | p_matched_pta)) +
#   plot_annotation(tag_levels = "A") &
#   theme(plot.tag = element_text(size = 24, face = "bold"))

# Manual addition of tags:
# A: PTA4
PTA4_tagged <- PTA4 +
  annotate("text", x = 40, y = -Inf, label = "A",
           hjust = 1, vjust = 1.5, fontface = "bold", size = 6)

# B: All block
p_all_age_tagged <- p_all_age +
  annotate("text", x = -Inf, y = Inf, label = "B",
           hjust = -0.5, vjust = 1.5, fontface = "bold", size = 6)

# C: matched block
p_matched_age_tagged <- p_matched_age + 
  annotate("text", x = -Inf, y = Inf, label = "C",
           hjust = -0.5, vjust = 1.5, fontface = "bold", size = 6)

combined_matching_density <- PTA4_tagged | ((p_all_age_tagged | p_matched_age_tagged) / (p_all_pta | p_matched_pta))

ggsave(file.path(fileDir, "output", "pta4_and_density.png"),
       plot = combined_matching_density,
       width = 4200, height = 1800, dpi = 300, units = "px")

## --- Matrix ------------------------------------------------------------------

### --- Adjust SRT & combine ---------------------------------------------------

# Reference values for stationary noise:
ref_SSN_DE = -7.1 # Wagener et al., 1999 --> list-specific SRT; slope = 17.1 %/dB
ref_SSN_FR = -6.0 # Jansen et al., 2012  --> list-specific SRT; slope = 14.0 %/dB

# Reference values for fluctuating noise:
ref_fluct_DE = -19.3 # Hochmuth et al., 2015 --> for ICRA5-250
ref_fluct_FR = -16.1 # Jansen et al., 2012  --> for ICRA4-250, but Hochmuth et al., (2015) 
                     # found that SRT differences between ICRA4 and ICRA5 are marginal)

# Calculate SRT difference from references:
df_w$matrix_mono_ssn_srt_norm = ifelse(df_w$centre=="DE", df_w$matrix_mono_ssn_srt - ref_SSN_DE,
                                      df_w$matrix_mono_ssn_srt - ref_SSN_FR)

df_w$matrix_mono_icra5_srt_norm = ifelse(df_w$centre=="DE", df_w$matrix_mono_icra5_srt - ref_fluct_DE,
                                      df_w$matrix_mono_icra5_srt - ref_fluct_FR)
# get stats: 
matrix_sum_table <- psych::describeBy(
  df_w[c("matrix_mono_ssn_srt", "matrix_mono_icra5_srt","matrix_mono_ssn_srt_norm", "matrix_mono_icra5_srt_norm")],
  group = df_w$centre, mat = TRUE, digits = 1)

# check limits:
psych::describeBy(df_w[c("matrix_mono_ssn_srt", "matrix_mono_icra5_srt", 
                         "matrix_mono_ssn_srt_norm", "matrix_mono_icra5_srt_norm")],
                  group = df_w$group, mat = TRUE, digits = 1)

## Inspect normalised SRT_norm:
# ggplot(df_w, aes(x = 1, y = matrix_mono_ssn_srt_norm, colour=centre)) +
#   geom_boxplot(alpha = 0.3) +
#   geom_jitter(width = 0.3, alpha = 0.4) +
#   theme_minimal() +
#   labs(title = "SSN", y = "SRT - reference [dB SNR]") +
#   scale_y_continuous(limits = c(-2,14),breaks=seq(-2,14,1)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_blank(),
#         axis.ticks.x = element_blank()) +
# 
# ggplot(df_w, aes(x = 1, y = matrix_mono_icra5_srt_norm, colour=centre)) +
#   geom_boxplot(alpha = 0.3) +
#   geom_jitter(width = 0.3, alpha = 0.4) +
#   theme_minimal() +
#   labs(title = "ICRA5-250", y = "SRT - reference [dB SNR]") +
#   scale_y_continuous(limits = c(-2,14),breaks=seq(-2,14,1)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_blank(),
#         axis.ticks.x = element_blank())
# 
# ggplot(df_w, aes(x = 1, y = matrix_mono_ssn_srt_norm)) +
#   geom_boxplot(alpha = 0.3) +
#   geom_jitter(width = 0.3, alpha = 0.4) +
#   theme_minimal() +
#   labs(title = "SSN", y = "SRT - reference [dB SNR]") +
#   scale_y_continuous(limits = c(-2,14),breaks=seq(-2,14,1)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_blank(),
#         axis.ticks.x = element_blank()) +
# 
#   ggplot(df_w, aes(x = 1, y = matrix_mono_icra5_srt_norm)) +
#   geom_boxplot(alpha = 0.3) +
#   geom_jitter(width = 0.3, alpha = 0.4) +
#   theme_minimal() +
#   labs(title = "ICRA5-250", y = "SRT - reference [dB SNR]") +
#   scale_y_continuous(limits = c(-2,14),breaks=seq(-2,14,1)) +
#   theme(axis.title.x = element_blank(),
#         axis.text.x  = element_blank(),
#         axis.ticks.x = element_blank())

### --- Plot - matrix (SRT) ----------------------------------------------------
# --- Get plot function ---
source(file.path(fileDir,"functions","scatterplot_matrix_1var.R"))
source(file.path(fileDir,"functions","scatterplot_matrix_2vars.R"))

# --- Set axis limits ---
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
srt_ylim <- scale_y_continuous(limits = c(-25,5),breaks=seq(-25,5,5))

#### --- Plot by language-specific SRT -----------------------------------------
# -- 2 centre x 2 noise type ---

matrix_mono_srt_age_DE =
  scatterplot_matrix_2vars(df_w, "age", "matrix_mono_ssn_srt", "matrix_mono_icra5_srt", "centre", "DE",
                           "left", "top", corrMethod, labelSpacing = 0.05, show_text = 1) +
  age_xlim +
  srt_ylim +
  xlab("Age [years]") +
  ylab("SRT [dB SNR]") 

matrix_mono_srt_age_FR =
  scatterplot_matrix_2vars(df_w, "age", "matrix_mono_ssn_srt", "matrix_mono_icra5_srt", "centre", "FR",
                           "left", "top", corrMethod, labelSpacing = 0.05, show_text = 1) +
  age_xlim +
  srt_ylim +
  xlab("Age [years]") +
  ylab("SRT [dB SNR]") +
  guides(fill = "none", shape = "none", linetype = "none") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

matrix_mono_srt_pta_DE =
  scatterplot_matrix_2vars(df_w, "PTA4_BE", "matrix_mono_ssn_srt", "matrix_mono_icra5_srt", "centre", "DE",
                           "left", "top", corrMethod, labelSpacing = 0.05, show_text = 1) +
  pta_xlim +
  srt_ylim +
  xlab("PTA [dB HL]") +
  ylab("SRT [dB SNR]") 

matrix_mono_srt_pta_FR =
  scatterplot_matrix_2vars(df_w, "PTA4_BE", "matrix_mono_ssn_srt", "matrix_mono_icra5_srt", "centre", "FR",
                           "left", "top", corrMethod, labelSpacing = 0.05, show_text = 1) +
  pta_xlim +
  srt_ylim +
  xlab("PTA [dB HL]") +
  ylab("SRT [dB SNR]") +
  guides(fill = "none", shape = "none", linetype = "none") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# --- Combine plots into 2x2 grid: columns = centre (DE, FR), rows = Age/PTA ---
matrix_combined_grid <- (matrix_mono_srt_age_DE | matrix_mono_srt_age_FR) /
  (matrix_mono_srt_pta_DE | matrix_mono_srt_pta_FR) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right",         
    legend.box = "vertical",           
    plot.margin = margin(5, 5, 5, 5),  
    panel.spacing = unit(0.3, "lines"))

# --- Save combined grid to PNG ---
ggsave(file.path(fileDir, "output", "matrix_srt_separated.png"),
       plot = matrix_combined_grid,
       width = 3800, height = 3000, dpi = 300, units = "px")

#### --- Fig. 4: Plot by SRT_norm ----------------------------------------------

srt_ylim <- scale_y_continuous(limits = c(-2,16),breaks=seq(-2,16,2))
matrix_mono_srt_ssn_norm_age =
  scatterplot_matrix_1var(df_w, "age","matrix_mono_ssn_srt_norm", "group", "1",
                              "left", "top", corrMethod, labelSpacing = 0.05, show_text = 1) +
  age_xlim +
  srt_ylim +
  xlab("Age [years]") +
  ylab("**Reference-normalised SRT**<sub>norm</sub> [dB SNR]") +
  theme(axis.title.x = ggtext::element_markdown(size = 16, face = "bold"),
    axis.title.y = ggtext::element_markdown(size = 16, face = "bold"))

matrix_mono_srt_ssn_norm_pta =
  scatterplot_matrix_1var(df_w, "PTA4_BE", "matrix_mono_ssn_srt_norm", "group", "1",
                              "left", "top", corrMethod, labelSpacing = 0.05, show_text = 1) +
  pta_xlim +
  srt_ylim +
  xlab("PTA [dB HL]") +
  ylab("**Reference-normalised SRT**<sub>norm</sub> [dB SNR]") +
  guides(fill = "none", shape = "none", linetype = "none") +
  theme(axis.title.x = ggtext::element_markdown(size = 16, face = "bold"),
        axis.title.y = ggtext::element_markdown(size = 16, face = "bold"))

matrix_mono_srt_icra5_norm_age =
  scatterplot_matrix_1var(df_w, "age","matrix_mono_icra5_srt_norm", "group", "1",
                          "left", "top", corrMethod, labelSpacing = 0.05, show_text = 1) +
  age_xlim +
  srt_ylim +
  xlab("Age [years]") +
  ylab("**Reference-normalised SRT**<sub>norm</sub> [dB SNR]") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

matrix_mono_srt_icra5_norm_pta =
  scatterplot_matrix_1var(df_w, "PTA4_BE", "matrix_mono_icra5_srt_norm", "group", "1",
                              "left", "top", corrMethod, labelSpacing = 0.05, show_text = 1) +
  pta_xlim +
  srt_ylim +
  xlab("PTA [dB HL]") +
  ylab("**Reference-normalised SRT**<sub>norm</sub> [dB SNR]") +
  guides(fill = "none", shape = "none", linetype = "none") + 
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# --- Combine plots ---
matrix_combined_grid_norm <- (matrix_mono_srt_ssn_norm_age | matrix_mono_srt_icra5_norm_age) /
  (matrix_mono_srt_ssn_norm_pta | matrix_mono_srt_icra5_norm_pta) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right",         
        legend.box = "vertical",           
        plot.margin = margin(5, 5, 5, 5),  
        panel.spacing = unit(0.3, "lines"))

# --- Save combined grid to PNG ---
ggsave(file.path(fileDir, "output", "matrix_srt_norm_combined.png"),
       plot = matrix_combined_grid_norm,
       width = 3800, height = 3000, dpi = 300, units = "px")  
  
## -- TIN ----------------------------------------------------------------------
# get dB SNR: threshold relative to the average masker levels per equivalent 
# rectangular bandwidth (ERB) at 500, 1000, 2000 Hz. 
# (This approach is similar to that in Hülsmeier & Kollmeier, 2022)
# --> i.e., threshold - mean_mskr_lev_ERB_<FREQUENCY>

# get masker level per ERB
TIN_tone_freq = c(0.5, 2)
ERB_frequencies <- sapply(TIN_tone_freq, function(f) 24.7 * (4.37 * f + 1))

df_w$tin_mskr_lev_ERB_500 = df_w$tin_mskr_spec_lev + 20 * log10(sqrt(ERB_frequencies[1]))
df_w$tin_mskr_lev_ERB_2000 = df_w$tin_mskr_spec_lev + 20 * log10(sqrt(ERB_frequencies[2]))

# get SNR 
df_w$tin_snr_ERB_500  = df_w$tin_thres_500 - df_w$tin_mskr_lev_ERB_500
df_w$tin_snr_ERB_2000 = df_w$tin_thres_2000 - df_w$tin_mskr_lev_ERB_2000

tin_sum_table = psych::describeBy(df_w[c("tin_snr_ERB_500", "tin_snr_ERB_2000")],
                                  group = df_w$centre, mat = TRUE, digits = 1)

knitr::kable(tin_sum_table, format = "pandoc", caption = "Summary Statistics by Centre")

# get limits:
# psych::describeBy(df_w[c("tin_snr_ERB_500", "tin_snr_ERB_2000")],group = df_w$group, mat = TRUE, digits = 1)

### --- Fig. 3: Plot - TIN ERB (dB SNR) ----------------------------------------
# --- Get plot function ---
source(file.path(fileDir,"functions","scatterplot_tin_combined.R"))

# --- Set axis limits ---
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
snr_ylim <- scale_y_continuous(limits = c(-10,6),breaks=seq(-10,6,2))

tin_snr_ERB_500_age = 
  scatterplot_tin_combined(df_w, "age", "tin_snr_ERB_500", "left", "top",
                           corrMethod, "centre", labelSpacing = 0.05, show_legend=1, show_text = 1) +
  age_xlim +
  snr_ylim +
  xlab("Age [years]") +
  ylab("Detection threshold [dB SNR]")

tin_snr_ERB_2000_age = 
  scatterplot_tin_combined(df_w, "age", "tin_snr_ERB_2000", "left", "top",
                           corrMethod, "centre", labelSpacing = 0.05, show_legend=1, show_text = 1) +
  age_xlim +
  snr_ylim +
  xlab("Age [years]") +
  ylab("Detection threshold [dB SNR]") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

tin_snr_ERB_500_pta = 
  scatterplot_tin_combined(df_w, "PTA4_BE", "tin_snr_ERB_500", "left", "top",
                           corrMethod, "centre", labelSpacing = 0.05, show_legend=1, show_text = 1) +
  pta_xlim +
  snr_ylim +
  xlab("PTA [dB HL]") + 
  ylab("Detection threshold [dB SNR]")

tin_snr_ERB_2000_pta = 
  scatterplot_tin_combined(df_w, "PTA4_BE", "tin_snr_ERB_2000", "left", "top",
                           corrMethod, "centre", labelSpacing = 0.05, show_legend=1, show_text = 1) +
  pta_xlim +
  snr_ylim +
  xlab("PTA [dB HL]") + 
  ylab("Detection threshold [dB SNR]") +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

# --- Combine plots
tin_combined_grid <- (tin_snr_ERB_500_age | tin_snr_ERB_2000_age) /
  (tin_snr_ERB_500_pta | tin_snr_ERB_2000_pta) +
  plot_layout(guides = "collect") & 
  theme(legend.position = "right",         
    legend.box = "vertical",           
    plot.margin = margin(5, 5, 5, 5),  
    panel.spacing = unit(0.3, "lines"))

# --- Save combined grid to PNG ---
ggsave(file.path(fileDir, "output", "tin_both_freqs.png"),
  plot = tin_combined_grid,
  width = 3800, height = 3000, dpi = 300, units = "px")

## --- ACALOS ------------------------------------------------------------------

# check = df_w %>% select (id, group, centre, grep("^ls_", names(df_w), value = TRUE))

### --- Get better-ear data ----------------------------------------------------
# ACLOS variable names
vars = c("ls_cu_15","ls_cu_25", "ls_cu_35", "ls_cu_35", "ls_flag")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R",
                           df_w[[paste0(i, "_r")]],
                           df_w[[paste0(i, "_l")]])
}

### --- Check if noise level for TIN and Matrix test audible -------------------
# Calculate deviation from norms for L25 = 70.2 dB SPL (IFNoise, monaural; Oetting et al., 2018)
df_w$ls_cu_25_BE_diff = df_w$ls_cu_25_BE - 70.2

check = df_w %>% 
  select(id, centre, PTA4_BE, matrix_mono_noise_lev, ls_cu_25_BE, ls_cu_25_BE_diff) %>%
  filter(ls_cu_25_BE_diff >= 10)

### get additional ACALOS variables --------------------------------------------
# get loudness scaling difference diff_35-15 (cu_35 - cu_15) [dB SPL]
df_w$ls_diff_35_15_BE = df_w$ls_cu_35_BE - df_w$ls_cu_15_BE # better-ear
df_w$ls_diff_35_15_b = df_w$ls_cu_35_b - df_w$ls_cu_15_b # binaural

# get binaural loudness summation: describes gain benefit for binaural stimuli
df_w$ls_cu_15_bin_summation = rowMeans(df_w[, c("ls_cu_15_l", "ls_cu_15_r")], na.rm = TRUE)  - df_w$ls_cu_15_b 
df_w$ls_cu_35_bin_summation = rowMeans(df_w[, c("ls_cu_35_l", "ls_cu_35_r")], na.rm = TRUE) - df_w$ls_cu_35_b 

# check x-y range for plots:
acalos_sum_table = psych::describeBy(
  df_w[c("ls_cu_15_BE", "ls_cu_35_BE", "ls_cu_15_bin_summation", "ls_diff_35_15_BE", "ls_cu_35_bin_summation")],
  group = df_w$centre, mat = TRUE, digits = 1)

knitr::kable(acalos_sum_table, format = "pandoc", caption = "Summary Statistics by Centre")

### --- Check BLS comparability between L15 & L35 ------------------------------
# ---> check if binaural summation is comparable between 15 and 35 CU

# Convert ACALOS level data to long format for analysis
ACALOS_l = df_w %>% 
  select (id, age, matrix_testear, PTA4_BE, centre, grep("^ls_", names(df_w), value = TRUE)) %>%
  pivot_longer(
    cols = c("ls_cu_15_bin_summation", "ls_cu_35_bin_summation"),
    names_to = "CondCode", 
    values_to = "bin_sum") %>%
  # Clean CondCode: keep only "15_cu" and "35_cu"
  mutate(
    CondCode = str_replace(CondCode, "ls_cu_(\\d+)_bin_summation", "\\1_cu"),
    CondCode = factor(CondCode, levels = c("15_cu", "35_cu"))  # Set order
  ) %>% 
  filter(!is.na(PTA4_BE))


#### --- Test assumptions for LME ----------------------------------------------

# Test of homogeneity of variance - Levene's test

# By interaction: Order x Condition
car::leveneTest(bin_sum ~ CondCode, data=ACALOS_l,center=median) 
# Results: p value = 0.5472; is not sig., i.e. we reject the hypothesis that the variance is not equal.
# ==> Assumption of homogeneity is met!

## perform linear regression model

w1 <- lm(bin_sum~ CondCode * age * PTA4_BE, data=ACALOS_l)

# normality tests: Shapiro-Wilk test

shapiro.test(residuals(w1))
# p-value = 0.393

# ==> Assumption of normal distribution is met

# step-wise analysis from fully saturated model.
library(lme4)
library(lmerTest)
library(sjPlot)
library(emmeans)

#### --- Mixed-effects model (LME) ---------------------------------------------
# --- run step-wise model testing ---
# Find model with the best fit
# If p > 0.05 → keep simpler model.
# If p < 0.05 → keep saturated model.

# Model comparison using MLE - for model comparisons use REML = FALSE
# Saturated model
m1 = lme4::lmer(bin_sum ~ CondCode + age + PTA4_BE + (1 | id), data = ACALOS_l, REML = FALSE)
m2 = lme4::lmer(bin_sum ~ CondCode + age + (1 | id), data = ACALOS_l, REML = FALSE)
anova(m1, m2)
# m2: bin_sum ~ CondCode + age + (1 | id)
# m1: bin_sum ~ CondCode + age + PTA4_BE + (1 | id)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)  
# m2    5 691.55 704.96 -340.77   681.55                       
# m1    6 689.69 705.78 -338.84   677.69 3.8563  1    0.04956 *
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# --> Keep PTA4_BE

m3 <- update(m1, . ~ . - age)
anova(m1, m3)
# Data: ACALOS_l
# Models:
#   m3: bin_sum ~ CondCode + PTA4_BE + (1 | id)
# m1: bin_sum ~ CondCode + age + PTA4_BE + (1 | id)
# npar    AIC    BIC  logLik deviance  Chisq Df Pr(>Chisq)
# m3    5 689.07 702.48 -339.54   679.07                     
# m1    6 689.69 705.78 -338.84   677.69 1.3849  1     0.2393

# --> age can be removed! 

# final_model - m3: bin_sum ~ CondCode + PTA4_BE + (1 | id)

# Refit final model using REML = TRUE (!!!) for reporting:
final_model <- lme4::lmer(bin_sum ~ CondCode + PTA4_BE + (1 | id), data = ACALOS_l, REML = TRUE)
summary(final_model)
tab_model(final_model)

# tab_model(fit_all)
emmeans(final_model, ~ CondCode) %>%
  contrast("revpairwise") %>%
  summary(infer = TRUE)
# --> 
# contrast        estimate        SE    df t.ratio p.value
# 15_cu - 35_cu -0.9520435 0.8671407 55.02  -1.098  0.2770

# ggplot(ACALOS_l, aes(x = age, y = bin_sum, color = CondCode)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE)

# --- Averaging binaural summation across 15 and 35 CUs: ---
df_w$ls_cu_mean_bin_summation = rowMeans(df_w[, c("ls_cu_15_bin_summation", "ls_cu_35_bin_summation")], na.rm = TRUE)

# get limits:
psych::describeBy(df_w[c("ls_cu_15_BE", "ls_cu_35_BE", "ls_diff_35_15_BE", "ls_cu_mean_bin_summation")],
                  group = df_w$group, mat = TRUE, digits = 1)

### --- Plot - ACALOS (level, DR & BLS) ----------------------------------------
# --- Get plot function ---
source(file.path(fileDir,"functions","scatterplot_acalos_2vars.R"))
source(file.path(fileDir,"functions","scatterplot_acalos_combined.R"))

# --- Set axis limits ---
age_xlim    <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim    <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
level_ylim  <- scale_y_continuous(limits = c(0,110),breaks=seq(0,110,10)) 
binSum_ylim <- scale_y_continuous(limits = c(-5,25),breaks=seq(-5,25,5))

# Age
ls_cu_BE_age = 
  scatterplot_acalos_2vars(df_w, "age", "ls_cu_15_BE", "ls_cu_35_BE", "centre",
                           "left", "bottom", corrMethod, labelSpacing = 0.07, show_legend = 1) +
  age_xlim +
  level_ylim +
  xlab("Age [years]") +
  ylab("Level [dB SPL]")

ls_cu_DR_BE_age = 
  scatterplot_acalos_combined(df_w, "age", "ls_diff_35_15_BE", "left", "top",
                              corrMethod, "centre", labelSpacing = 0.07,  show_legend = 1, show_text = 1) +
  age_xlim +
  level_ylim +
  xlab("Age [years]") +
  ylab("Dynamic range [dB]")

ls_cu_mean_bin_summation_age =
  scatterplot_acalos_combined(df_w, "age", "ls_cu_mean_bin_summation", "left", "top",
                              corrMethod, "centre", labelSpacing = 0.07, show_legend = 1, show_text = 1) +
  age_xlim +
  binSum_ylim +
  xlab("Age [years]") +
  ylab("Binaural loudness summation [dB]")

# PTA
ls_cu_BE_pta = 
  scatterplot_acalos_2vars(df_w, "PTA4_BE", "ls_cu_15_BE", "ls_cu_35_BE", "centre",
                           "left", "bottom", corrMethod, labelSpacing = 0.07, show_legend = 1) +
  pta_xlim +
  level_ylim +
  xlab("PTA [dB HL]") +
  ylab("Level [dB SPL]")

ls_cu_DR_BE_pta = 
  scatterplot_acalos_combined(df_w, "PTA4_BE", "ls_diff_35_15_BE", "left", "top",
                              corrMethod, "centre", labelSpacing = 0.07,  show_legend = 1, show_text = 1) +
  pta_xlim +
  level_ylim +
  xlab("PTA [dB HL]") +
  ylab("Dynamic range [dB]")

ls_cu_mean_bin_summation_pta =
  scatterplot_acalos_combined(df_w, "PTA4_BE", "ls_cu_mean_bin_summation", "left", "top",
                              corrMethod, "centre", labelSpacing = 0.07, show_legend = 1, show_text = 1) +
  pta_xlim +
  binSum_ylim +
  xlab("PTA [dB HL]") +
  ylab("Binaural loudness summation [dB]")

# --- Combine L15 and L35 plots ---
acalos_L15_L35_combined_grid <- (ls_cu_BE_age / ls_cu_BE_pta) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
    legend.box = "vertical",
    plot.margin = margin(5, 5, 5, 5),  
    panel.spacing = unit(0.3, "lines"))

# --- Save L15 & L35 combined grid ---
ggsave(file.path(fileDir, "output", "acalos_L15_L35.png"),
  plot = acalos_L15_L35_combined_grid,
  width = 2600, height = 2600, dpi = 300, units = "px")

# --- Combine DR plots ---
acalos_DR_combined_grid <- (ls_cu_DR_BE_age / ls_cu_DR_BE_pta) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
    legend.box = "vertical",
    plot.margin = margin(5, 5, 5, 5),
    panel.spacing = unit(0.3, "lines"))

# --- Save DR combined grid ---
ggsave(file.path(fileDir, "output", "acalos_DR.png"),
       plot = acalos_DR_combined_grid,
       width = 2600, height = 2600, dpi = 300, units = "px")

# --- Combine binaural summation ---
acalos_bin_sum_combined_grid <- (ls_cu_mean_bin_summation_age / ls_cu_mean_bin_summation_pta) +
  plot_layout(guides = "collect") & 
  theme(
    legend.position = "right",         
    legend.box = "vertical",           
    plot.margin = margin(5, 5, 5, 5),  
    panel.spacing = unit(0.3, "lines") 
  )

# --- Save combined grid to PNG ---
ggsave(
  file.path(fileDir, "output", "acalos_bin_sum.png"),
  plot = acalos_bin_sum_combined_grid,
  width = 2600, height = 2600, dpi = 300, units = "px"
)

### --- Fig. 2: Combine ACALOS plots -------------------------------------------

# 1) Left block: Level
p1_age_left <- ls_cu_BE_age +
  guides(colour   = "none",
         fill     = "none",
         shape    = guide_legend(ncol = 1, byrow = TRUE),
         linetype = guide_legend(ncol = 1, byrow = TRUE)) +
  theme(legend.direction = "vertical",
        legend.position = c(0.77, 20/110),
        legend.justification = c("left", "bottom"),
        legend.background = element_rect(fill   = scales::alpha("white", 0.65), colour = "black"),
        legend.key.size = unit(0.35, "lines"),
        legend.spacing.x = unit(0.1, "lines"),
        legend.spacing.y = unit(0.1, "lines"),
        legend.text = ggtext::element_markdown(size = 10))

# Lower panel (PTA): remove all legends
p1_pta_left <- ls_cu_BE_pta +
  guides(colour = "none", fill = "none", shape = "none", linetype = "none")

# Assemble left block
left_block <- (p1_age_left / p1_pta_left) +
  plot_layout(heights = c(1, 1), guides = "keep") &
  theme(plot.margin   = margin(5, 5, 5, 5),
    panel.spacing = unit(0.3, "lines"))

# 2) middle block: DR
p2_age_mid <- ls_cu_DR_BE_age +
  guides(shape = "none", linetype = "none")

p2_pta_mid <- ls_cu_DR_BE_pta +
  guides(shape = "none", linetype = "none")

# 2) Right block: BLS
p3_age_right <- ls_cu_mean_bin_summation_age +
  guides(shape = "none", linetype = "none")

p3_pta_right <- ls_cu_mean_bin_summation_pta +
  guides(shape = "none", linetype = "none")

# Combine blocks:
right_block <- (p2_age_mid | p3_age_right) /
  (p2_pta_mid | p3_pta_right) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
    legend.box      = "vertical",
    plot.title      = element_text(size = 11, face = "bold"),
    plot.margin     = margin(5, 5, 5, 5),
    panel.spacing   = unit(0.3, "lines"))

# 3) Combine into one figure
fig_acalos <- cowplot::plot_grid(left_block, right_block,
  nrow = 1, rel_widths = c(0.85, 2), align = "h", axis  = "tb")

# 4) Add A | B | C tags above blocks using cowplot
fig_acalos_tagged <- ggdraw() +
  # draw the combined figure slightly shorter to leave room for labels
  draw_plot(fig_acalos, x = 0, y = 0, width = 1, height = 0.95) +
  
  # A over the left block (Level)
  draw_label("A",
             x = 0.005, y = 0.99,
             hjust = 0, vjust = 1,
             fontface = "bold", size = 26) +
  
  # B over the mid block (DR)
  draw_label("B",
             x = 0.30, y = 0.99,
             hjust = 0, vjust = 1,
             fontface = "bold", size = 26) +
  
  # C over the right block (BLS)
  draw_label("C",
             x = 0.605, y = 0.99,
             hjust = 0, vjust = 1,
             fontface = "bold", size = 26)

# 5) Save tagged figure
ggsave(file.path(fileDir, "output", "acalos_combined.png"),
       fig_acalos_tagged, width = 5800, height = 3000,
       dpi = 300,units = "px")

## -- DPOAE --------------------------------------------------------------------
# dpoae = df_w %>% select (grep("^dpoae_", names(df_w), value = TRUE))

### --- Get better-ear data ----------------------------------------------------
# DPOAEs variable names
vars = c("dpoae_dp_lev_1khz","dpoae_dp_lev_2khz", "dpoae_dp_lev_4khz",
         "dpoae_snr_1khz", "dpoae_snr_2khz", "dpoae_snr_4khz",
         "dpoae_flag", "dpoae_tar_lev")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R",
                           df_w[[paste0(i, "_r")]],
                           df_w[[paste0(i, "_l")]])
}

# count how many IDs measured in each level:
df_w %>%
  filter(!is.na(dpoae_tar_lev_BE)) %>%
  distinct(id, dpoae_tar_lev_BE) %>%
  count(dpoae_tar_lev_BE) %>%
  mutate(proportion = round(100 * n / sum(n), 1))

# How many distinct IDs with a non-missing dpoae_tar_lev_BE?
df_w %>%
  filter(!is.na(dpoae_tar_lev_BE)) %>%
  summarise(n_ids = n_distinct(id))

# List all IDs (to compare to your expected 55)
ids_in_oae <- df_w %>%
  filter(!is.na(dpoae_tar_lev_BE)) %>%
  distinct(id)

ids_in_oae

### --- Get present/absent responses -------------------------------------------
# (based on Elios, ECHODIA manual recommendations)
# Absent response if min DP-level < –6 dB SPL & SNR < 6 dB
min_dp_level = -6.0
min_snr = 6.0

# Evaluate presence (1) or absence (2) per frequency (1, 2, & 4 kHz)
df_w$dpoae_res_1khz_BE = ifelse(df_w$dpoae_dp_lev_1khz_BE >= min_dp_level & df_w$dpoae_snr_1khz_BE >= min_snr, 1, 2)
df_w$dpoae_res_2khz_BE = ifelse(df_w$dpoae_dp_lev_2khz_BE >= min_dp_level & df_w$dpoae_snr_2khz_BE >= min_snr, 1, 2)
df_w$dpoae_res_4khz_BE = ifelse(df_w$dpoae_dp_lev_4khz_BE >= min_dp_level & df_w$dpoae_snr_4khz_BE >= min_snr, 1, 2)

# Count number of absent responses (dpoae_res_ = 2)
df_w$dpoae_absent_count = apply(
  df_w[, c("dpoae_res_1khz_BE", "dpoae_res_2khz_BE", "dpoae_res_4khz_BE")],
  1,
  function(x) {
    if (any(is.na(x))) {
      return(NA)
    } else {
      return(sum(x == 2))
    }
  }
)

# Count number of present responses (dpoae_res_ = 1)
df_w$dpoae_response_count = apply(
  df_w[, c("dpoae_res_1khz_BE", "dpoae_res_2khz_BE", "dpoae_res_4khz_BE")],
  1,
  function(x) {
    if (any(is.na(x))) {
      return(NA)
    } else {
      return(sum(x != 2))
    }
  }
)

# How many IDs had absent response > 1 and per level (65 dB and 68 dB)?
df_w %>%
  group_by(dpoae_tar_lev_BE, centre) %>%
  dplyr::count(., dpoae_absent_count)

df_w %>% 
  filter(dpoae_absent_count > 1) %>% 
  select(id, matrix_testear, dpoae_tar_lev_BE, dpoae_absent_count, dpoae_res_1khz_BE, dpoae_res_2khz_BE, dpoae_res_4khz_BE)

# Global pass criterion: (i.e., minimum 2 out of 3 valid responses)
df_w %>%
  filter(!is.na(dpoae_tar_lev_l)) %>%
  summarise(total = n(),
            n_below_3 = sum(dpoae_response_count < 2),
            proportion = round(n_below_3 / total,1)*100)
# --> 50 out of 55 subjects (90%) fulfilled the global pass criterion
# total n_below_3 proportion
# 1    55         5         10

# check limits:
psych::describeBy(df_w[c("dpoae_snr_1khz_BE", "dpoae_snr_2khz_BE","dpoae_snr_4khz_BE")], group = df_w$group, mat = TRUE, digits = 1)


# --- Get not-measurable cases (=999) from df_w_with999 ---

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w_with999[[new_var]] = ifelse(df_w_with999$matrix_testear == "R", df_w_with999[[paste0(i, "_r")]], df_w_with999[[paste0(i, "_l")]])
}

dpoae_999_cases <- df_w_with999 %>%
  select(id, age, PTA4_BE, centre, matches("^dpoae_snr_\\d+khz_BE$")) %>%
  pivot_longer(cols = matches("^dpoae_snr_\\d+khz_BE$"), names_to = "condition", values_to = "snr") %>%
  filter(snr == 999)

### Plot - DPOAE ---------------------------------------------------------------
# --- Get plot function ---
# source(file.path(fileDir,"functions","scatterplot_oae_combined.R"))
# 
# # --- Set axis limits ---
# age_xlim = scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
# pta_xlim = scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
# # snr_ylim = scale_y_continuous(limits = c(-14, 45), breaks = seq(-10, 45, 5))
# snr_y_limits = c(-10, 45)
# snr_y_breaks = seq(-10, 45, 5)
# 
# # --- Helper function for plotting ---
# make_oae_plot <- function(x_var, y_var, res_var, xlab_txt) {
#   scatterplot_oae_combined(df = df_w, param_x = x_var, param_y = y_var, absent_col = res_var,
#                            groupBy = "centre", corrMethod = corrMethod, 
#                            statsLocation_x = "left", statsLocation_y = "bottom",
#                            show_legend = 1,level_param = "dpoae_tar_lev_l",
#                            absent_lim = NULL, not_measurable_df = NULL,
#                            y_limits = snr_y_limits, y_breaks = snr_y_breaks) +
#     (if (x_var == "age") age_xlim else pta_xlim) +
#     xlab(xlab_txt) +
#     ylab("SNR [dB]")
# }
# 
# # --- Define frequencies and variables ---
# freqs <- c("1khz", "2khz", "4khz")
# y_vars_age <- paste0("dpoae_snr_", freqs, "_BE")
# res_vars <- paste0("dpoae_res_", freqs, "_BE")
# 
# # --- Create age and PTA plots ---
# plots_age <- map2(y_vars_age, res_vars, ~ make_oae_plot("age", .x, .y, "Age [years]"))
# plots_pta <- map2(y_vars_age, res_vars, ~ make_oae_plot("PTA4_BE", .x, .y, "PTA4 [dB HL]"))
# 
# # --- Remove y labels and ticks from middle/right plots ---
# strip_y <- function(p) {
#   p + theme(axis.title.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks.y = element_blank())
# }
# plots_age[2:3] <- map(plots_age[2:3], strip_y)
# plots_pta[2:3] <- map(plots_pta[2:3], strip_y)
# 
# # --- Remove x ticks from top row ---
# plots_age <- map(plots_age, ~ .x + theme(axis.ticks.x = element_blank()))
# 
# # --- Combine plots in grid ---
# combined_grid <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 3) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "right",
#         plot.margin = margin(5, 5, 5, 5),
#         panel.spacing = unit(0.4, "lines"))
# 
# # --- Save to PNG ---
# ggsave(file.path(fileDir, "output", "dpoae.png"),
#        plot = combined_grid,
#        width = 6000, height = 3500, dpi = 300, units = "px")

# --- Plot DPOAE hist (present/absent per target level) ---

# Variables
vars_dpoae <- c("dpoae_res_1khz_BE", "dpoae_res_2khz_BE", "dpoae_res_4khz_BE")

# Prepare frequency-specific summary
df_summary_freq <- df_w %>%
  select(all_of(vars_dpoae), dpoae_tar_lev_l) %>%
  pivot_longer(cols = all_of(vars_dpoae),
               names_to = "frequency",
               values_to = "status") %>%
  filter(status %in% c(1, 2)) %>%
  mutate(frequency = dplyr::recode(
    frequency,
    dpoae_res_1khz_BE = "1 kHz",
    dpoae_res_2khz_BE = "2 kHz",
    dpoae_res_4khz_BE = "4 kHz"),
    frequency = forcats::fct_relevel(frequency, "1 kHz", "2 kHz", "4 kHz")) %>%
  group_by(frequency) %>%
  summarise(n_present = sum(status == 1),
            total = n(),
            pct = round(100 * n_present / total),
            label = paste0(pct, "%\n(", n_present, "/", total, ")"),
            .groups = "drop")

#### --- Compute pass proportion -----------------------------------------------
df_summary_pass <- df_w %>%
  filter(!is.na(dpoae_absent_count)) %>%
  summarise(frequency = "global \npass criterion",
            n_present = sum(dpoae_absent_count < 2),
            total = n(),
            pct = round(100 * n_present / total),
            label = paste0(pct, "%\n(", n_present, "/", total, ")"))

#### --- Combine ---------------------------------------------------------------
df_summary_dpoae <- bind_rows(df_summary_freq, df_summary_pass) %>%
  mutate(frequency = factor(frequency,
                            levels = c("global \npass criterion", "1 kHz", "2 kHz", "4 kHz")))

#### --- Barplot ---------------------------------------------------------------
dpoae_hist <- df_summary_dpoae %>%
  ggplot(aes(x = frequency, y = pct)) +
  geom_col(fill = "grey", colour = "grey") +
  geom_text(aes(label = label), colour = "black", vjust = 1.2,
            size = 4.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  labs(x = NULL, y = "Proportion present [%]") +
  annotate("text", x = Inf, y = Inf, label = "DPOAE",
           hjust = 1.1, vjust = 1.3, size = 5, fontface = "bold") +
  theme_bw() +
  theme(axis.text   = element_text(size = 14, face = "bold", colour = "black"),
        axis.title  = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13),
        panel.grid  = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16, face = "bold", colour = "black"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "dpoae_hist.png"), plot = dpoae_hist, 
       width =  1333.333, height = 1500, dpi = 300, units = "px")

## -- TEOAE --------------------------------------------------------------------
# teoae = df_w %>% select (grep("^teoae_", names(df_w), value = TRUE))

# TO DO: define absent responses based on SNR and Reproducibility
# Reliability: split repetitions into A / B blocks at random  Averaging each blocks, resulting in waveforms for block A and block B  cross correlation
# Step-wise rules in ELIOS:
# SNR ≥ 9 dB and Reproducibility ≥ 50%
# SNR ≥ 6 dB and Reproducibility ≥ 60%
# SNR ≥ 3 dB and Reproducibility ≥ 75%

### --- Get better-ear data ----------------------------------------------------

# for plot, add measured level (85 dB SPL)
df_w$teoae_tar_lev = 85

# TEOAEs variable names
vars = c("teoae_snr_1k", "teoae_snr_2k", "teoae_snr_3k", "teoae_snr_4k", "teoae_snr_5k",
         "teoae_res_1k", "teoae_res_2k", "teoae_res_3k", "teoae_res_4k", "teoae_res_5k",
         "teoae_flag")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R",
                           df_w[[paste0(i, "_r")]],
                           df_w[[paste0(i, "_l")]])
}

### --- Get present/absent responses -------------------------------------------

# Count number of absent responses across frequencies
# absent (res=2)
df_w$teoae_absent_count = apply(
  df_w[, c("teoae_res_1k_BE", "teoae_res_2k_BE", "teoae_res_3k_BE", "teoae_res_4k_BE", "teoae_res_5k_BE")],
  1,
  function(x) {
    if (any(is.na(x))) {
      return(NA)
    } else {
      return(sum(x == 2))
    }
  }
)

# present (res=1)
df_w$teoae_response_count = apply(
  df_w[, c("teoae_res_1k_BE", "teoae_res_2k_BE", "teoae_res_3k_BE", "teoae_res_4k_BE", "teoae_res_5k_BE")],
  1,
  function(x) {
    if (any(is.na(x))) {
      return(NA)
    } else {
      return(sum(x != 2))
    }
  }
)

# How many IDs had absent response > 1?
df_w %>% 
  group_by(group, centre) %>%
  dplyr::count(., teoae_absent_count)

# Who is it & at what freq? (present=1; absent=2)
df_w %>% 
  filter(teoae_absent_count > 3) %>% 
  select(id, centre, teoae_absent_count, teoae_res_1k_BE, teoae_res_2k_BE, teoae_res_3k_BE, teoae_res_4k_BE, teoae_res_5k_BE)

# Global pass criterion (i.e., minimum 3 out of 5 valid responses)
df_w %>%
  filter(!is.na(teoae_absent_count)) %>%
  summarise(total = n(),
            n_below_4 = sum(teoae_absent_count <= 3),
            proportion = round(n_below_4 / total,3)*100)
# --> 45 out of 53 subjects (85.9%) fulfilled the global pass criterion
# total n_below_4 proportion
# 1    53        45       84.9

# check limits:
psych::describeBy(df_w[c("teoae_snr_1k_BE", "teoae_snr_2k_BE","teoae_snr_3k_BE",
                         "teoae_snr_4k_BE","teoae_snr_5k_BE")], group = df_w$group, mat = TRUE, digits = 1)

## --- Get not-measurable cases (=999) from df_w_with999 ---

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w_with999[[new_var]] = ifelse(df_w_with999$matrix_testear == "R", df_w_with999[[paste0(i, "_r")]], df_w_with999[[paste0(i, "_l")]])
}

teoae_999_cases <- df_w_with999 %>%
  select(id, age, PTA4_BE, centre, matches("^teoae_snr_\\d+k_BE$")) %>%
  pivot_longer(cols = matches("^teoae_snr_\\d+k_BE$"), names_to = "condition", values_to = "snr") %>%
  filter(snr == 999)

### Plot - TEOAE ---------------------------------------------------------------
# # --- Get plot function ---
# source(file.path(fileDir,"functions","scatterplot_oae_combined.R"))
# 
# # --- Set axis limits ---
# age_xlim = scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
# pta_xlim = scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
# # snr_ylim = scale_y_continuous(limits = c(-15, 35), breaks = seq(-15, 35, 5))
# snr_y_limits <- c(-15, 30)
# snr_y_breaks <- seq(-15, 30, 5)
# 
# # --- Helper function for plotting ---
# make_oae_plot <- function(x_var, y_var, res_var, xlab_txt) {
#   scatterplot_oae_combined(df = df_w, param_x = x_var, param_y = y_var,
#     absent_col = res_var, groupBy = "centre", corrMethod = "spearman",
#     statsLocation_x = "left", statsLocation_y = "bottom",
#     show_legend = 1, level_param = "teoae_tar_lev", 
#     absent_lim = 30, not_measurable_df = teoae_999_cases,
#     xlim = NULL, y_limits = snr_y_limits, y_breaks = snr_y_breaks) +
#     (if (x_var == "age") age_xlim else pta_xlim) +
#     xlab(xlab_txt) +
#     ylab("SNR [dB]")
# }
# 
# # --- Define frequencies and variables ---
# freqs <- c("1k", "2k", "3k", "4k", "5k")
# y_vars_age <- paste0("teoae_snr_", freqs, "_BE")
# res_vars <- paste0("teoae_res_", freqs, "_BE")
# 
# # --- Create age and PTA plots ---
# plots_age <- map2(y_vars_age, res_vars, ~ make_oae_plot("age", .x, .y, "Age [years]"))
# plots_pta <- map2(y_vars_age, res_vars, ~ make_oae_plot("PTA4_BE", .x, .y, "PTA4 [dB HL]"))
# 
# # --- Remove y labels/ticks from middle and right columns ---
# strip_y <- function(p) {
#   p + theme(axis.title.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks.y = element_blank())
# }
# plots_age[2:5] <- map(plots_age[2:5], strip_y)
# plots_pta[2:5] <- map(plots_pta[2:5], strip_y)
# 
# # --- Remove x-axis ticks from top row ---
# plots_age <- map(plots_age, ~ .x + theme(axis.ticks.x = element_blank()))
# 
# # --- Combine plots ---
# combined_grid <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 5) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "right",
#         plot.margin = margin(t = 5, r = 5, b = 5, l = 5),
#         panel.spacing = unit(0.4, "lines"))
# 
# # --- Save to PNG ---
# ggsave(file.path(fileDir, "output", "teoae.png"),
#        plot = combined_grid,
#        width = 9000, height = 3000, dpi = 300, units = "px")


# --- Plot TEOAE hist (present/absent per target level) ---
# Variables
vars_teoae <- c("teoae_res_1k_BE", "teoae_res_2k_BE", "teoae_res_3k_BE", 
                "teoae_res_4k_BE", "teoae_res_5k_BE")

# Prepare frequency-specific summary
df_summary_freq <- df_w %>%
  select(all_of(vars_teoae), teoae_tar_lev) %>%
  pivot_longer(cols = all_of(vars_teoae),
               names_to = "frequency",
               values_to = "status") %>%
  filter(status %in% c(1, 2)) %>%
  mutate(frequency = dplyr::recode(frequency,
                                   teoae_res_1k_BE = "1 kHz",
                                   teoae_res_2k_BE = "2 kHz",
                                   teoae_res_3k_BE = "3 kHz",
                                   teoae_res_4k_BE = "4 kHz",
                                   teoae_res_5k_BE = "5 kHz"),
         frequency = forcats::fct_relevel(frequency, "1 kHz", "2 kHz", "3 kHz","4 kHz", "5 kHz")) %>%
  group_by(frequency) %>%
  summarise(n_present = sum(status == 1),
            total = n(),
            pct = round(100 * n_present / total),
            label = paste0(pct, "%\n(", n_present, "/", total, ")"),
            .groups = "drop")

#### --- Compute pass proportion -----------------------------------------------
df_summary_pass <- df_w %>%
  filter(!is.na(teoae_absent_count)) %>%
  summarise(frequency = "global \npass criterion",
            n_present = sum(teoae_absent_count <= 3),
            total = n(),
            pct = round(100 * n_present / total),
            label = paste0(pct, "%\n(", n_present, "/", total, ")"))

#### --- Combine ---------------------------------------------------------------
df_summary_teoae <- bind_rows(df_summary_freq, df_summary_pass) %>%
  mutate(frequency = factor(frequency,
                            levels = c("global \npass criterion", "1 kHz", "2 kHz", "3 kHz", "4 kHz", "5 kHz")))

#### --- Barplot ---------------------------------------------------------------
teoae_hist <- df_summary_teoae %>%
  ggplot(aes(x = frequency, y = pct)) +
  geom_col(fill = "grey", colour = "grey") +
  geom_text(aes(label = label), colour = "black",
            vjust = 1.2, size = 4.5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10)) +
  labs(x = NULL, y = "Proportion present [%]") +
  annotate("text", x = Inf, y = Inf, label = "TEOAE",
           hjust = 1.1, vjust = 1.3, size = 5, fontface = "bold") +
  theme_bw() +
  theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
        axis.title  = element_text(size = 16, face = "bold"),
        legend.text = element_text(size = 13),
        panel.grid  = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(size = 16, face = "bold", colour = "black"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "teoae_hist.png"),
       plot = teoae_hist,
       width = 2000, height = 1500, dpi = 300, units = "px")

## --- Fig. 6: Combine OAE plots -----------------------------------------------

# Get all levels from the teoae plot
all_levels <- levels(teoae_hist$data$bin)

# Force dpoae to use same number of bins
dpoae_hist_fixed <- dpoae_hist +
  scale_x_discrete(drop = FALSE, limits = all_levels)

# Combine side by side with two tags
combined_oae <- (teoae_hist | dpoae_hist) +
  plot_layout(widths = c(1, 0.7)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 32, face = "bold"))

ggsave(file.path(fileDir, "output", "oae_combined.png"), plot = combined_oae,
       width = 4000, height = 1500, dpi = 300, units  = "px")

## --- MEMR (Reflex) -----------------------------------------------------------
# reflex = df_w %>% select (grep("^reflex_", names(df_w), value = TRUE))

### --- Get better-ear data ----------------------------------------------------

# reflex variable names
vars = c("reflex_500", "reflex_1k", "reflex_2k", "reflex_flag")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R",
                           df_w[[paste0(i, "_r")]],
                           df_w[[paste0(i, "_l")]])
}

## check limits:
#psych::describeBy(reflex_500~centre, data=df_w, mat=TRUE, digits=1)

### --- Get present/absent responses -------------------------------------------

# --> look for missing data (=999) in df_w_with999
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w_with999[[new_var]] = ifelse(df_w_with999$matrix_testear == "R", df_w_with999[[paste0(i, "_r")]], df_w_with999[[paste0(i, "_l")]])
}

# Find events where reflex was not triggered (=999) and label with '1', 
# else 0 if triggered or NA if not measured
df_w$reflex_500_absent_BE = ifelse(is.na(df_w_with999$reflex_500_BE),NA,
                                   ifelse(df_w_with999$reflex_500_BE == 999, 1, 0))

df_w$reflex_1k_absent_BE = ifelse(is.na(df_w_with999$reflex_1k_BE),NA,
                                   ifelse(df_w_with999$reflex_1k_BE == 999, 1, 0))

df_w$reflex_2k_absent_BE = ifelse(is.na(df_w_with999$reflex_2k_BE),NA,
                                   ifelse(df_w_with999$reflex_2k_BE == 999, 1, 0))

# Count number of absent responses (= 1)
df_w$reflex_absent_count = rowSums(df_w[, c("reflex_500_absent_BE", "reflex_1k_absent_BE", "reflex_2k_absent_BE")], na.rm = TRUE)

df_w = df_w %>%
  mutate(reflex_absent_count = rowSums(across(c(reflex_500_absent_BE, reflex_1k_absent_BE, reflex_2k_absent_BE)), na.rm = TRUE),
         reflex_absent_count = if_else(if_all(c(reflex_500_absent_BE, reflex_1k_absent_BE, reflex_2k_absent_BE), is.na),
                                       NA_integer_,reflex_absent_count))

# How many IDs had absent response > 1?

df_w %>% 
  filter(reflex_absent_count > 1) %>% 
  select(id, age, PTA4_BE,centre, reflex_absent_count, reflex_500_absent_BE, reflex_1k_absent_BE, reflex_2k_absent_BE)

# How many subjects were measured and did not have a triggered response in any frequency? (i.e. reflex_absent_count > 2)
df_w %>%
  filter(!is.na(reflex_absent_count)) %>%
  summarise(total = n(),
            n_above_2 = sum(reflex_absent_count > 2),
            proportion = round(n_above_2 / total,2)*100,
            n_500 = sum(reflex_500_absent_BE),
            n_1k = sum(reflex_1k_absent_BE),
            n_2k = sum(reflex_2k_absent_BE),
            pct_500hz = round(n_500 / total,2)*100,
            pct_1khz = round(n_1k / total,2)*100,
            pct_2khz = round(n_2k / total,2)*100)

### --- Get average threshold level (reflex_mean across freqs) -----------------
# --> only take the mean if at least 2 responses where measurable.
# --> if only 1 triggered response, use that level instead.
df_w$reflex_mean_BE = apply(
  df_w[, c("reflex_500_BE", "reflex_1k_BE", "reflex_2k_BE")], 1,
  function(x) {
    n_valid <- sum(!is.na(x))
    if (n_valid >= 1) round(mean(x, na.rm = TRUE), 2) else NA
  }
)

# check limits:
psych::describeBy(reflex_mean_BE~group, data=df_w, mat=TRUE, digits=1)

### --- Fig. 5: Plot - MEMR ----------------------------------------------------
# --- Get plot function ---
source(file.path(fileDir,"functions","scatterplot_reflex_combined.R"))

# --- Set axis limits ---
age_xlim = scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim = scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))

reflex_mean_ylim = scale_y_continuous(limits = c(75, 107), 
                                      breaks = c(seq(75, 100, 5), 105),
                                      labels = c(paste0(seq(75, 100, 5)), "NT"))

# By age
reflex_mean_BE_age  =
  scatterplot_reflex_combined(df_w, "age", "reflex_mean_BE", "left", "top",
                              corrMethod, "centre", labelSpacing = 0.05,  show_legend = 1, 
                              absent_lim = 105) +
  age_xlim +
  reflex_mean_ylim +
  xlab("Age [years]") +
  ylab("Averaged threshold [dB SPL]")

# By pta
reflex_mean_BE_pta  =
  scatterplot_reflex_combined(df_w, "PTA4_BE", "reflex_mean_BE", "left", "top",
                              corrMethod, "centre", labelSpacing = 0.05,  show_legend = 1,
                              absent_lim = 105) +
  pta_xlim +
  reflex_mean_ylim +
  xlab("PTA [dB HL]") +
  ylab("Averaged threshold [dB SPL]")

# --- Combine plots vertically (Age on top, PTA below) ---
reflex_combined_grid <- wrap_plots(reflex_mean_BE_age, reflex_mean_BE_pta, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.4, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "reflex.png"),
       plot = reflex_combined_grid,
       width = 2500, height = 3000, dpi = 300, units = "px")

## --- Histogram: proportion of triggered responses by frequency ---

# Get long data (exclude NA = not measured)
# --> triggered equals 0
reflex_long = df_w %>%
  select(reflex_500_absent_BE, reflex_1k_absent_BE, reflex_2k_absent_BE) %>%
  pivot_longer(cols = everything(), names_to = "frequency", values_to = "absent") %>%
  mutate(frequency = dplyr::recode(frequency,
                       "reflex_500_absent_BE" = "0.5 kHz",
                       "reflex_1k_absent_BE" = "1 kHz",
                       "reflex_2k_absent_BE" = "2 kHz"),
         response = ifelse(absent == 1, "Not triggered", "Triggered")) %>%
  filter(!is.na(absent)) %>%
  group_by(frequency, response) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(frequency) %>%
  mutate(total = sum(n),
         proportion = n / total,
         label_color = ifelse(response == "Triggered", "white", "black"))

reflex_res_pct = ggplot(reflex_long, aes(x = frequency, y = proportion, fill = response)) +
  geom_col(position = "stack", width = 0.6, color = "black") +
  geom_text(aes(label = scales::percent(proportion, accuracy = 1), color = label_color),
            position = position_stack(vjust = 0.5),
            fontface = "bold", size = 4, show.legend = FALSE) +
  scale_fill_manual(values = c("Triggered" = "black", "Not triggered" = "white"),
                    name = NULL) +
  scale_color_identity() +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(x = "Frequency", y = "Proportion of responses") +
  theme_minimal(base_size = 14) +
  theme_bw() +
  theme(axis.text = element_text(face = "bold", color = "black", size = 12),
        axis.title = element_text(face = "bold", size = 14),
        legend.position = "bottom",
        legend.text = element_text(size = 12, face = "bold"))

ggsave(file.path(fileDir, "output", "reflex_response_pct.png"), plot = reflex_res_pct,
       width = 3000, height = 3000, dpi = 300, units = "px")

## --- vHIT --------------------------------------------------------------------
# vHIT = df_w %>% select (grep("^vhit_", names(df_w), value = TRUE))

### --- Get better-ear data ----------------------------------------------------

# vHIT variable names
vars = c("vhit_gain_anter", "vhit_gain_later","vhit_gain_poster",
         "vhit_sd_anter", "vhit_sd_later", "vhit_sd_poster")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R", df_w[[paste0(i, "_r")]], df_w[[paste0(i, "_l")]])
}

### --- Recompute Asymmetry ratio (AR) -----------------------------------------

## --- Check equation per device with actual value from table ---
get_vhit_asym <- function(r, l)  ( (r - l) / (r + l) ) * 100

# --- Calculate better ear AR per canal ---
# Adjust asymmetry sign relative to better (no sign change) vs. worse ear (flipped sign)
df_w <- df_w %>%
  mutate(
    # anterior
    vhit_asym_anter_calc = get_vhit_asym(vhit_gain_anter_r,  vhit_gain_anter_l),
    vhit_asym_anter_calc_pta = ifelse(matrix_testear=="R", vhit_asym_anter_calc, 
                                      vhit_asym_anter_calc * -1), 
    # lateral
    vhit_asym_later_calc = get_vhit_asym(vhit_gain_later_r,  vhit_gain_later_l),
    vhit_asym_later_calc_pta = ifelse(matrix_testear=="R", vhit_asym_later_calc, 
                                      vhit_asym_later_calc * -1), 
    # posterior
    vhit_asym_poster_calc = get_vhit_asym(vhit_gain_poster_r, vhit_gain_poster_l),
    vhit_asym_poster_calc_pta = ifelse(matrix_testear=="R", vhit_asym_poster_calc, 
                                       vhit_asym_poster_calc * -1)
    )

# 
# # check limits:
psych::describeBy(df_w[c("vhit_gain_anter_BE", "vhit_gain_poster_BE","vhit_gain_later_BE")], group = df_w$centre, mat = TRUE, digits = 1)
psych::describeBy(df_w[c("vhit_asym_anter", "vhit_asym_poster","vhit_asym_later")], group = df_w$centre, mat = TRUE, digits = 1)
psych::describeBy(df_w[c("vhit_asym_anter_calc_pta", "vhit_asym_later_calc_pta","vhit_asym_poster_calc_pta")], group = df_w$centre, mat = TRUE, digits = 1)

# inspect IQR:
vars_gain <- c("vhit_gain_anter_BE", "vhit_gain_poster_BE", "vhit_gain_later_BE")
 
df_w %>%
  select(all_of(vars_gain)) %>%
  pivot_longer(cols = all_of(vars_gain), names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  summarise(n = sum(!is.na(value)),
            mean = mean(value, na.rm = TRUE),
            sd = sd(value, na.rm = TRUE),
            median = median(value, na.rm = TRUE),
            IQR = IQR(value, na.rm = TRUE),
            Q1 = quantile(value, 0.25, na.rm = TRUE),
            Q3 = quantile(value, 0.75, na.rm = TRUE),
            min = min(value, na.rm = TRUE),
            max = max(value, na.rm = TRUE),
            .groups = "drop")

### --- Get not-measurable cases (=999) from df_w_with999 ----------------------

vars = c("vhit_gain_anter", "vhit_gain_later","vhit_gain_poster")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w_with999[[new_var]] = ifelse(df_w_with999$matrix_testear == "R",
                                   df_w_with999[[paste0(i, "_r")]],
                                   df_w_with999[[paste0(i, "_l")]])
}

# collect 999 cases
vhit_999_cases <- df_w_with999 %>%
  select(id, age, PTA4_BE, centre, matches("^vhit_gain_.*_BE$")) %>%
  pivot_longer(cols = matches("^vhit_gain_.*_BE$"),
               names_to = "condition",
               values_to = "gain") %>%
  filter(gain == 999)

### --- Plot - vHIT (gain) -----------------------------------------------------
# --- Get plot function ---
source(file.path(fileDir, "functions", "scatterplot_vhit.R"))

# --- Set axis limits ---
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
# y-axis for gain
gain_y_limits <- c(0, 1.9)
gain_y_breaks <- seq(0, 1.75, 0.25)

# --- Helper function for plotting ---
make_vhit_plot <- function(x_var, y_var, xlab_txt, asym = FALSE) {
  # norms line: 0.8 for lateral, 0.7 for anterior/posterior
  # hline_val <- if (grepl("later", y_var, ignore.case = TRUE)) 0.8 else 0.7
  
  scatterplot_vhit(df = df_w, param_x = x_var, param_y = y_var,
                   statsLocation_x = "left", statsLocation_y = "bottom",
                   corrMethod = corrMethod, groupBy = "centre",
                   absent_lim = 1.75,
                   not_measurable_df = vhit_999_cases,
                   y_limits = gain_y_limits,
                   y_breaks = gain_y_breaks,
                   labelSpacing = 0.07, show_legend = 1, show_text = 1,
                   hline = NULL) +
    (if (x_var == "age") age_xlim else pta_xlim) +
    xlab(xlab_txt) +
    ylab("Gain")
    # ylab(expression(bold("Gain [" ~ bar(x) ~ "]")))
}
     
# --- Define intensity levels and variables ---
canal <- c("anter", "later", "poster")
y_vars_age <- paste0("vhit_gain_", canal, "_BE")

# --- Create age and PTA plots ---
plots_age <- purrr::map(y_vars_age, ~ make_vhit_plot("age", .x, "Age [years]"))
plots_pta <- purrr::map(y_vars_age, ~ make_vhit_plot("PTA4_BE", .x, "PTA [dB HL]"))

# --- Remove y labels and ticks from middle/right columns ---
strip_y <- function(p) {
  p + theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
}
plots_age[2:3] <- purrr::map(plots_age[2:3], strip_y)
plots_pta[2:3] <- purrr::map(plots_pta[2:3], strip_y)

# --- Remove x-axis ticks from top row ---
plots_age <- purrr::map(plots_age, ~ .x + theme(axis.ticks.x = element_blank()))

# --- Combine plots in grid ---
combined_grid_gain <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 3) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 6),  
    panel.spacing = unit(0.3, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "vhit_gain.png"), plot = combined_grid_gain,
       width = 5600, height = 3000, dpi = 300, units = "px")

### --- Plot - vHIT (asym) -----------------------------------------------------

# --- Set axis limits ---
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
# y-axis for asymmetry
asym_y_limits <- c(-30, 35)
asym_y_breaks <- seq(-30, 30, 5)

# --- Helper function for plotting ---
make_vhit_plot <- function(x_var, y_var, xlab_txt) {
  scatterplot_vhit(df_w, x_var, y_var,
                            "left", "bottom", corrMethod, groupBy = "centre",
                            absent_lim = 30,
                            not_measurable_df = vhit_999_cases,
                            y_limits = asym_y_limits,
                            y_breaks = asym_y_breaks,
                            labelSpacing = 0.07, show_legend = 1, show_text = 1) +
    (if (x_var == "age") age_xlim else pta_xlim) +
    xlab(xlab_txt) +
    ylab(expression(bold("Asymmetry ratio [%]")))
}

# --- Define intensity levels and variables ---
canal <- c("anter", "later", "poster")
y_vars <- paste0("vhit_asym_", canal, "_calc_pta")

# --- Create age and PTA plots ---
plots_age <- purrr::map(y_vars, ~ make_vhit_plot("age", .x, "Age [years]"))
plots_pta <- purrr::map(y_vars, ~ make_vhit_plot("PTA4_BE", .x, "PTA [dB HL]"))

# --- Remove y labels and ticks from middle/right columns ---
strip_y <- function(p) {
  p + theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
}
plots_age[2:3] <- purrr::map(plots_age[2:3], strip_y)
plots_pta[2:3] <- purrr::map(plots_pta[2:3], strip_y)

# --- Remove x-axis ticks from top row ---
plots_age <- purrr::map(plots_age, ~ .x + theme(axis.ticks.x = element_blank()))

# --- Add better-/worse-ear (BE/WE) arrows inside the plot ---
# for age plot
plots_age[[1]] <- plots_age[[1]] +
  # upper text + arrow
  annotate("text", x = 88.2, y = 8, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment",
           x = 90, xend = 90, y = 12, yend = 20,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 88.2, y = -8, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment",
           x = 90, xend = 90, y = -12, yend = -20,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)
# for pta plot
plots_pta[[1]] <- plots_pta[[1]] +
  # upper text + arrow
  annotate("text", x = 28.7, y = 8, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment",
           x = 30, xend = 30, y = 12, yend = 20,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 28.7, y = -8, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment",
           x = 30, xend = 30, y = -12, yend = -20,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

# --- Combine plots in grid ---
combined_grid_asym <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 3) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 6),  
    panel.spacing = unit(0.3, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", paste0("vhit_asym", ".png")), plot = combined_grid_asym,
       width = 5600, height = 3000, dpi = 300, units = "px")

### --- Fig. 8: Combine vHIT plots + tags --------------------------------------

# first remove legends from second plot (asymmetry)
# --- Combine plots in grid ---
combined_grid_asym <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 3) + 
  theme(plot.margin   = margin(t = 5, r = 5, b = 5, l = 6),  
    panel.spacing = unit(0.3, "lines"))

# Kill ALL legends inside the asymmetry grid
combined_grid_asym <- combined_grid_asym & theme(legend.position = "none")

# gain: label A
# asymmetry: label B

## 1) Take one legend from the gain grid
legend_gain <- cowplot::get_legend(combined_grid_gain + theme(legend.position = "right"))

## 2) Remove legends from both grids (so they have identical widths)
combined_grid_gain_noleg <- combined_grid_gain & theme(legend.position = "none")
combined_grid_asym_noleg <- combined_grid_asym & theme(legend.position = "none")

## 3) Stack the two grids with tags A (top) and B (bottom)
left_plot <- (wrap_elements(combined_grid_gain_noleg) /
                wrap_elements(combined_grid_asym_noleg)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 32, face = "bold"))

## 4) Put the single legend to the right, with equal widths
combined_gain_vhit <- cowplot::plot_grid(left_plot, legend_gain,
                                         ncol = 2, rel_widths = c(1, 0.20))

## 5) Save
ggsave(file.path(fileDir, "output", "vhit_gain_asym_combined.png"), 
       plot = combined_gain_vhit, width = 5600, height = 6000, dpi = 300, units  = "px")

## --- Caloric test (CT) -------------------------------------------------------

### --- get absolute SPV for OL data -------------------------------------------
# to allow for comparison with FR data which is absolute SPV by default
# * FR (Ulmer VNG): RW, RC, LW, LC = all POSITIVE, i.e, | SPV |
# * DE (Otometrics):  RW = POSITIVE (right beating) -> abs()
#                     RC = NEGATIVE (left beating)  -> abs()
#                     LW = NEGATIVE (left beating)  -> abs()
#                     LC = POSITIVE (right beating) -> abs()

df_w = df_w %>%
  mutate(ct_velcty_nystgms_cold_abs_r  = abs(ct_velcty_nystgms_cold_r),
         ct_velcty_nystgms_cold_abs_l  = abs(ct_velcty_nystgms_cold_l),
         ct_velcty_nystgms_warm_abs_r  = abs(ct_velcty_nystgms_warm_r),
         ct_velcty_nystgms_warm_abs_l  = abs(ct_velcty_nystgms_warm_l))

### --- Calculate UW and DP for absolute SPVs ----------------------------------
# UW = ((TRE - TLE) / RC + RW + LC + LW ) * 100 ; TRE=total right ear (RW+RC) ; TLE=total left ear (LW+LC)
# DP = ((TRB - TLB) / RC + RW + LC + LW ) * 100 ; TRB=total right beating (RW+LC) ; TLB=total left beating (LW+RC)

get_uw <- function(RC, RW, LC, LW) {
  TRE = RW + RC
  TLE = LW + LC
  uw = ( (TRE - TLE) / (TRE + TLE) ) * 100
  return(uw)
}

get_dp <- function(RC, RW, LC, LW) {
  TRB = RW + LC
  TLB = LW + RC
  dp = ( (TRB - TLB) / (TRB + TLB) ) * 100
  return(dp)
}

# --- UW function for monothermal caloric asymmetry (MCA) ---
# see Lightfoot et al., 2009; Ear & Hearing
get_monothermal_uw <- function(r, l) {
  ifelse(is.na(r) | is.na(l),
         NA_real_,
         ((r - l) / (r + l)) * 100)
}

# --- get UW & DP, check for monothermal data and flip sign by better-ear PTA ---
df_w <- df_w %>%
  mutate(
    # --- First try bithermal UW ---
    ct_uw_calc = get_uw(ct_velcty_nystgms_cold_abs_r, ct_velcty_nystgms_warm_abs_r,
                        ct_velcty_nystgms_cold_abs_l, ct_velcty_nystgms_warm_abs_l),
    
    # assume bithermal measurement is fulfilled 
    ct_monothermal = 0,
    
    # --- If NA, check if monothermal ---
    ct_uw_calc = case_when(
      !is.na(ct_uw_calc) ~ ct_uw_calc,  # keep bithermal result
      
      # Warm only
      is.na(ct_velcty_nystgms_cold_r) & is.na(ct_velcty_nystgms_cold_l) &
        !is.na(ct_velcty_nystgms_warm_r) & !is.na(ct_velcty_nystgms_warm_l) ~
        get_monothermal_uw(ct_velcty_nystgms_warm_r, ct_velcty_nystgms_warm_l),
      
      # Cold only
      is.na(ct_velcty_nystgms_warm_r) & is.na(ct_velcty_nystgms_warm_l) &
        !is.na(ct_velcty_nystgms_cold_r) & !is.na(ct_velcty_nystgms_cold_l) ~
        get_monothermal_uw(ct_velcty_nystgms_cold_r, ct_velcty_nystgms_cold_l),
      
      TRUE ~ NA_real_
    ),
    
    # update monothermal if monothermal UW was calculated
    ct_monothermal = case_when(
      is.na(ct_velcty_nystgms_cold_r) & is.na(ct_velcty_nystgms_cold_l) &
        !is.na(ct_velcty_nystgms_warm_r) & !is.na(ct_velcty_nystgms_warm_l) ~ 1,
      is.na(ct_velcty_nystgms_warm_r) & is.na(ct_velcty_nystgms_warm_l) &
        !is.na(ct_velcty_nystgms_cold_r) & !is.na(ct_velcty_nystgms_cold_l) ~ 1,
      TRUE ~ ct_monothermal
    ),
    
    # --- apply sign for better/worse ear by PTA (left = *-1) ---
    ct_uw_calc_pta = ifelse(matrix_testear == "R", ct_uw_calc,
                            ct_uw_calc * -1),
    
    # --- DP ---
    ct_dp_calc = get_dp(ct_velcty_nystgms_cold_abs_r, ct_velcty_nystgms_warm_abs_r,
                        ct_velcty_nystgms_cold_abs_l, ct_velcty_nystgms_warm_abs_l),
    ct_dp_calc_pta = ifelse(matrix_testear == "R", ct_dp_calc,
                            ct_dp_calc * -1)
  )

### --- Get better-ear data ----------------------------------------------------

# ct variable names
vars = c("ct_velcty_nystgms_warm_abs", "ct_velcty_nystgms_cold_abs")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R",
                           df_w[[paste0(i, "_r")]],
                           df_w[[paste0(i, "_l")]])
}

# # check limits:
psych::describeBy(df_w[c("ct_velcty_nystgms_warm_abs_BE", "ct_velcty_nystgms_cold_abs_BE","ct_uw_calc_pta", "ct_dp_calc_pta")],
                  group = df_w$group, mat = TRUE, digits = 1, na.rm = TRUE)

### --- Plot CT hist -----------------------------------------------------------
# --- Count abnormal responses: SPV ---
# SPV < 21: normal (ct_spv_abnormal = 0)
# SPV > 20: abnormal (ct_spv_abnormal = 1)
# 
# df_w$ct_spv_abnormal <- apply(
#   df_w[, c("ct_velcty_nystgms_warm_abs_BE", "ct_velcty_nystgms_cold_abs_BE")],
#   1,
#   function(x) {
#     if (all(is.na(x))) {
#       return(NA)
#     } else if (any(x > 20, na.rm = TRUE)) {
#       return(1)
#     } else {
#       return(0)
#     }
#   }
# )

# by centre:
# df_abnormal_summary <- df_w %>%
#   pivot_longer(cols = c("ct_velcty_nystgms_warm_abs_BE", "ct_velcty_nystgms_cold_abs_BE"),
#                names_to = "condition",
#                values_to = "SPV") %>%
#   mutate(abnormal = ifelse(!is.na(SPV) & SPV > 20, 1, 0)) %>%
#   group_by(centre, condition) %>%
#   summarise(n_abnormal = sum(abnormal, na.rm = TRUE),
#             n_total = sum(!is.na(SPV)),
#             .groups = "drop") %>%
#   mutate(perc_abnormal = round(100 * n_abnormal / n_total, 1))

# combined data
df_abn_summary <- df_w %>% 
  pivot_longer(cols = c("ct_velcty_nystgms_warm_abs_BE", "ct_velcty_nystgms_cold_abs_BE"),
               names_to = "condition",
               values_to = "SPV") %>%
  filter(!is.na(SPV)) %>%
  mutate(status = ifelse(SPV > 20, "Abnormal", "Normal"),
         condition = case_when(
           condition == "ct_velcty_nystgms_warm_abs_BE" ~ "Warm irrigation",
           condition == "ct_velcty_nystgms_cold_abs_BE" ~ "Cold irrigation",
           TRUE ~ condition)) %>%
  group_by(condition, status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(prop = n / sum(n),
         label = paste0(round(100 * prop, 1), "%\n(", n, "/", sum(n), ")"))

# --- Plot stacked bars Normal (white) vs Abnormal (black) ---
p_abn <- ggplot(df_abn_summary, aes(x = condition, y = prop, fill = status)) +
  geom_col(position = "stack", width = 0.7, color = "black") +
  geom_text(aes(label = label, colour = status),
            position = position_stack(vjust = 0.5),
            size = 4, fontface = "bold", lineheight = 0.9) +
  scale_fill_manual(values = c("Normal" = "black", "Abnormal" = "white"),
                    name = NULL) +
  scale_colour_manual(values = c("Normal" = "white", "Abnormal" = "black"),
                      guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x = NULL, y = "Proportion of SPV") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text = element_text(size = 14, face = "bold", colour = "black"),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 13, face = "bold", margin = margin(b = 6)),
        legend.text = element_text(size = 13, face = "bold"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "ct_spv_abnormal.png"),
       plot = p_abn,
       width = 1700, height = 1500, dpi = 300, units = "px")

# --- Count abnormal responses: UW / DP ---
## | UW | > 25% = abnormal
## | DP | > 30% = abnormal

# --- Summary: abnormal UW/DP ---
df_abn_ud <- df_w %>%
  # include only bithermal data!
  filter(ct_monothermal == 0) %>%
  select(ct_uw_calc, ct_dp_calc) %>%
  pivot_longer(everything(), names_to = "measure", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(condition = dplyr::recode(measure,
                                   ct_uw_calc = "Unilateral weakness (UW)",
                                   ct_dp_calc = "Directional preponderance (DP)"),
         condition = factor(condition,
                            levels = c("Unilateral weakness (UW)",
                                       "Directional preponderance (DP)")),
         threshold = ifelse(measure == "ct_uw_calc", 25, 30),
         status = ifelse(abs(value) > threshold, "Abnormal", "Normal")) %>%
  group_by(condition, status) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(condition) %>%
  mutate(prop = n / sum(n),
         label = paste0(round(100 * prop, 1), "%\n(", n, "/", sum(n), ")"))

# --- Plot: stacked bars Normal (white) vs Abnormal (black) ---
p_abn_ud <- ggplot(df_abn_ud, aes(x = condition, y = prop, fill = status)) +
  geom_col(position = "stack", width = 0.7, color = "black") +
  geom_text(aes(label = label, colour = status),
            position = position_stack(vjust = 0.58),
            size = 4, fontface = "bold", lineheight = 0.9) +
  scale_fill_manual(values = c(Normal = "black", Abnormal = "white"),
                    breaks = c("Normal", "Abnormal"),
                    name = NULL) +
  scale_colour_manual(values = c(Normal = "white", Abnormal = "black"),
                      guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), 
                     limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
  labs(x = NULL, y = "Proportion of cases") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text  = element_text(size = 14, face = "bold", colour = "black"),
        axis.title = element_text(size = 16, face = "bold"),
        plot.title = element_text(size = 16, face = "bold"),
        legend.title = element_text(size = 13, face = "bold", margin = margin(b = 6)),
        legend.text  = element_text(size = 13, face = "bold"))

# --- Save ---
ggsave(file.path(fileDir, "output", "ct_uw_dp_abnormal.png"),
       plot = p_abn_ud, width = 2600, height = 1800, dpi = 300, units = "px")

### --- Plot - CT (SPV) --------------------------------------------------------
# --- Get plot function ---
source(file.path(fileDir, "functions", "scatterplot_ct_combined.R"))

# --- Set axis limits ---
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
ampl_ylim <- scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 10))

# --- Helper function for plotting ---
make_ct_plot <- function(x_var, y_var, xlab_txt) {
  scatterplot_ct_combined(df_w, x_var, y_var,
                            "left", "top", corrMethod, "centre", hline = NULL,
                            labelSpacing = 0.07, show_legend = 1, show_text = 1) +
    (if (x_var == "age") age_xlim else pta_xlim) +
    ampl_ylim +
    xlab(xlab_txt) +
    ylab(expression(bold("Slow-phase-velocity (SPV) [") ~ bold(degree) ~ bold("/s]")))
}

# --- Define variables ---
temp <- c("cold", "warm")
y_vars <- paste0("ct_velcty_nystgms_", temp, "_abs_BE")

# --- Create age and PTA plots ---
plots_age <- purrr::map(y_vars, ~ make_ct_plot("age", .x, "Age [years]"))
plots_pta <- purrr::map(y_vars, ~ make_ct_plot("PTA4_BE", .x, "PTA [dB HL]"))

# --- Remove y labels and ticks from middle/right columns ---
strip_y <- function(p) {
  p + theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
}
plots_age[[2]] <- strip_y(plots_age[[2]])
plots_pta[[2]] <- strip_y(plots_pta[[2]])

# --- Remove x-axis ticks from top row ---
plots_age <- purrr::map(plots_age, ~ .x + theme(axis.ticks.x = element_blank()))

# --- Combine plots in grid ---
ct_combined_grid_spv <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 2) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 6),  
    panel.spacing = unit(0.3, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "ct_spv.png"),
       plot = ct_combined_grid_spv,
       width = 3700, height = 3000, dpi = 300, units = "px")

### --- Plot - CT (UW, DP) -----------------------------------------------------

# --- Set axis limits ---
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
ct_uw_dp_ylim <- scale_y_continuous(limits = c(-50, 55), breaks = seq(-50, 50, 10))

# --- Helper function for plotting ---
make_ct_plot <- function(x_var, y_var, xlab_txt) {
  # choose hline depending on variable
  # hline_val <- case_when(grepl("uw", y_var, ignore.case = TRUE) ~ 25,
  #                        grepl("dp", y_var, ignore.case = TRUE) ~ 30,
  #                        TRUE ~ 0)
  
  scatterplot_ct_combined(df_w, x_var, y_var,
                          "left", "top", corrMethod, "centre", 
                          hline = NULL,
                          labelSpacing = 0.07, show_legend = 1, show_text = 1) +
    (if (x_var == "age") age_xlim else pta_xlim) +
    ct_uw_dp_ylim +
    xlab(xlab_txt) +
    ylab("Percentage [%]")
}

# --- Define variables ---
ct_var <- c("uw", "dp")
y_vars <- paste0("ct_", ct_var, "_calc_pta")

# --- Create age and PTA plots ---
plots_age <- purrr::map(y_vars, ~ make_ct_plot("age", .x, "Age [years]"))
plots_pta <- purrr::map(y_vars, ~ make_ct_plot("PTA4_BE", .x, "PTA [dB HL]"))

# --- Remove y labels and ticks from middle/right columns ---
strip_y <- function(p) {
  p + theme(axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank())
}
plots_age[[2]] <- strip_y(plots_age[[2]])
plots_pta[[2]]  <- strip_y(plots_pta[[2]])

# --- Remove x-axis ticks from top row ---
plots_age <- purrr::map(plots_age, ~ .x + theme(axis.ticks.x = element_blank()))

#### --- UW arrows -------------------------------------------------------------
plots_age[[1]] <- plots_age[[1]] +
  # upper text + arrow
  annotate("text", x = 88.6, y = 15, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 90, xend = 90, y = 20, yend = 35,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 88.6, y = -15, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 90, xend = 90, y = -20, yend = -35,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

plots_pta[[1]] <- plots_pta[[1]] +
  # upper text + arrow
  annotate("text", x = 29, y = 15, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 30, xend = 30, y = 20, yend = 35,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 29, y = -15, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 30, xend = 30, y = -20, yend = -35,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

#### --- DP arrows -------------------------------------------------------------
plots_age[[2]] <- plots_age[[2]] +
  # upper text + arrow
  annotate("text", x = 88.6, y = 15, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 90, xend = 90, y = 20, yend = 35,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 88.6, y = -15, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 90, xend = 90, y = -20, yend = -35,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

plots_pta[[2]] <- plots_pta[[2]] +
  # upper text + arrow
  annotate("text", x = 29, y = 15, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 30, xend = 30, y = 20, yend = 35,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 29, y = -15, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 30, xend = 30, y = -20, yend = -35,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

# --- Combine plots in grid ---
ct_combined_grid_ar <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 2) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
    plot.margin = margin(t = 5, r = 5, b = 5, l = 6),
    panel.spacing = unit(0.3, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "ct_uw_dp.png"),
       plot = ct_combined_grid_ar,
       width = 3700, height = 3200, dpi = 300, units = "px")

### --- Fig. 11: Combine CT plots + tags ---------------------------------------

# first remove legends from second plot (asymmetry)
# --- Combine plots in grid ---
ct_combined_grid_ar <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 2) + 
  theme(plot.margin = margin(t = 5, r = 5, b = 5, l = 6),  
    panel.spacing = unit(0.3, "lines"))

# Kill ALL legends inside the asymmetry grid
ct_combined_grid_ar <- ct_combined_grid_ar & theme(legend.position = "none")

# SPV: label A
# asymmetry: label B

# library(patchwork)
# library(cowplot)

## 1) Take one legend from the gain grid
legend_gain <- cowplot::get_legend(ct_combined_grid_spv + theme(legend.position = "right"))

## 2) Remove legends from both grids (so they have identical widths)
ct_combined_grid_spv_noleg <- ct_combined_grid_spv & theme(legend.position = "none")
ct_combined_grid_ar_noleg <- ct_combined_grid_ar & theme(legend.position = "none")

## 3) Stack the two grids with tags A (top) and B (bottom)
left_plot <- (wrap_elements(ct_combined_grid_spv_noleg) |
                wrap_elements(ct_combined_grid_ar_noleg)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 32, face = "bold"))

## 4) Put the single legend to the right, with equal widths
ct_combined_spv_ar <- cowplot::plot_grid(left_plot, legend_gain, ncol = 2, rel_widths = c(1, 0.08))

## 5) Save
ggsave(file.path(fileDir, "output", "ct_spv_ar_combined.png"), 
       plot = ct_combined_spv_ar, width = 7400, height = 3200, dpi = 300, units  = "px")

## -- cVEMPs -------------------------------------------------------------------
# cVEMP = df_w %>% select (grep("^cvemp_", names(df_w), value = TRUE))

### --- Harmonise cVEMPs data --------------------------------------------------

# Peak-to-peak amplitute [μV]
# --- Ensure P-N normalised amplitude calculation is comparable --
# --> Neurosoft formula:  |P-N| / EMG
# --> Eclipse formula:    (P-N) / EMG
# ==> use the same formula as used in Neurosoft |P-N| / EMG
# ==> Because P and N are always positive, both equations should provide the same results.

# - Eclipse EMG:    mean of EMG during testing (stimulus presentation + response).
# - Neurosoft EMG:  pre-stimulus baseline measurement.

# If centre = DE, re-calculate EMG scaled P-N (cvemp_norm_ampl_p_n_r, cvemp_norm_ampl_p_n_l)
# as done by Neurosoft (|P-N| / EMG)

get_p_n_normed <- function(P, N, EMG) (p_n_normed = abs(P-N) / EMG)

# re-normalise DE data to make sure it is done in the same way as FR.
# --> For FR: normalised p-n amplitude is not re-calculated as raw p and n amplitudes are not provided!
df_w <- df_w %>%
  mutate(
    cvemp_norm_ampl_p_n_r = ifelse(cvemp_device==1,
                                   get_p_n_normed(cvemp_ampl_p_r, cvemp_ampl_n_r, cvemp_averg_emg_ampl_r),
                                   cvemp_norm_ampl_p_n_r),
    cvemp_norm_ampl_p_n_l = ifelse(cvemp_device==1, 
                                   get_p_n_normed(cvemp_ampl_p_l, cvemp_ampl_n_l, cvemp_averg_emg_ampl_l),
                                   cvemp_norm_ampl_p_n_l))

## --- Asymmetry ratio (AR) formula ---
vemps_asym_asym <- function(r, l)  ( (r - l) / (r + l) ) * 100

# --- Compute AR ---
# --> adjust asymmetry sign relative to better (no sign change) vs. worse ear (flipped sign)
df_w <- df_w %>%
  mutate(cvemp_asym_calc = vemps_asym_asym(cvemp_norm_ampl_p_n_r, cvemp_norm_ampl_p_n_l),
         cvemp_asym_calc_pta = ifelse(matrix_testear=="R", cvemp_asym_calc, 
                                      cvemp_asym_calc * -1))

### --- Get better-ear data ----------------------------------------------------
# cVEMPs variable names
vars = c("cvemp_norm_ampl_p_n")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R",
                           df_w[[paste0(i, "_r")]],
                           df_w[[paste0(i, "_l")]])
}

### --- Count selected intensity and cvemp_results  ----------------------------
# --> cvemp_results is either: present (=1), absent (unilateral=2), or absent (bilateral=3)

# count how many present (=1), absent (unilateral=2), & absent (bilateral=3)
df_w %>%
  group_by(cvemp_results) %>%
  dplyr::count(., centre)

# selected intensity, cvemp_norm_ampl_p_n_BE
df_w %>% 
  filter(!is.na(cvemp_norm_ampl_p_n_BE)) %>%
  group_by(cvemp_dbnhl_intensity_lev) %>%
  dplyr::count(., centre)

# selected intensity, asymmetry
df_w %>% 
  filter(!is.na(cvemp_asym_calc)) %>%
  group_by(cvemp_dbnhl_intensity_lev) %>%
  dplyr::count(., centre)

# absent response (1=Normal; 2=Unilateral absent; 3=Bilateral absent)
df_w %>%
  group_by(cvemp_results) %>%
  dplyr::count(., centre)

### --- Split normed amplitude by level ----------------------------------------
cvemp_levels = unique(na.omit(df_w$cvemp_dbnhl_intensity_lev))
for (lvl in cvemp_levels) {
  colname <- paste0("cvemp_norm_ampl_p_n_BE_", lvl)
  df_w[[colname]] <- ifelse(df_w$cvemp_dbnhl_intensity_lev == lvl, df_w$cvemp_norm_ampl_p_n_BE, NA)
}

# set cvemp_dbnhl_intensity_lev as a factor and ensure ascending order
df_w <- df_w %>%
  mutate(cvemp_dbnhl_intensity_lev = factor(cvemp_dbnhl_intensity_lev,
                                            levels = sort(unique(na.omit(cvemp_dbnhl_intensity_lev)))))

# check limits:
psych::describeBy(df_w[c("cvemp_norm_ampl_p_n_BE_95",
                         "cvemp_norm_ampl_p_n_BE_100",
                         "cvemp_norm_ampl_p_n_BE_105")],
                  group = df_w$group, mat = TRUE, digits = 1)

psych::describeBy(df_w[c("cvemp_asym", "cvemp_asym_calc_pta")],
                  group = df_w$group, mat = TRUE, digits = 1)

### --- Plot - cVEMPs (P-N ampl) -----------------------------------------------
# --- Get plot function ---
source(file.path(fileDir, "functions", "scatterplot_vemp.R"))

#### --- cvemp_norm_ampl_p_n_BE by levels --------------------------------------
# # --- Set axis limits ---
# age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
# pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
# ampl_ylim <- scale_y_continuous(limits = c(0, 3), breaks = seq(0, 3, 0.5))
# asym_ylim <- scale_y_continuous(limits = c(0, 60), breaks = seq(0, 70, 10))
# 
# # --- Helper function for plotting ---
# make_cvemp_plot <- function(x_var, y_var, xlab_txt) {
#   scatterplot_vemp_combined(df_w, x_var, y_var,
#                             "left", "top", corrMethod, "centre",
#                             labelSpacing = 0.07, show_legend = 1, show_text = 1) +
#     (if (x_var == "age") age_xlim else pta_xlim) +
#     ampl_ylim +
#     xlab(xlab_txt) +
#     ylab("Normalised ampl.\n(|P1 -N1| / Avg EMG)")
# }
# 
# # --- Define intensity levels and variables ---
# levs <- c("95", "100", "105")
# y_vars_age <- paste0("cvemp_norm_ampl_p_n_BE_", levs)
# 
# # --- Create age and PTA plots ---
# plots_age <- purrr::map(y_vars_age, ~ make_cvemp_plot("age", .x, "Age [years]"))
# plots_pta <- purrr::map(y_vars_age, ~ make_cvemp_plot("PTA4_BE", .x, "PTA4 [dB HL]"))
# 
# # --- Remove y labels and ticks from middle/right columns ---
# strip_y <- function(p) {
#   p + theme(axis.title.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks.y = element_blank())
# }
# 
# # strip y on middle & right panels (indices 2:3, not 2:4)
# plots_age[2:3] <- purrr::map(plots_age[2:3], strip_y)
# plots_pta[2:3] <- purrr::map(plots_pta[2:3], strip_y)
# 
# # remove x ticks only on the TOP row plots
# plots_age <- purrr::map(plots_age, ~ .x + theme(axis.ticks.x = element_blank(),
#                                                 axis.text.x  = element_blank()))
# 
# # combine: use plotlist= and set ncol = 3 (since you have 3 plots per row)
# combined_grid <- wrap_plots(plotlist = c(plots_age, plots_pta),
#                             nrow = 2, ncol = 3) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "right",
#         plot.margin = margin(5, 5, 5, 5),
#         panel.spacing = unit(0.3, "lines"))
# 
# # --- Save to PNG ---
# ggsave(file.path(fileDir, "output", "cvemp_byLevel.png"),
#        plot = combined_grid,
#        width = 5200, height = 2500, dpi = 300, units = "px")

#### --- cvemp_norm_ampl_p_n_BE for 100 +/-5 dB --------------------------------

# Get cVEMP limits & break points
age_x_limits = c(40, 90)
age_x_breaks = seq(40, 90, 5)

pta_x_limits = c(-5, 30)
pta_x_breaks = seq(-5, 30, 5)

ampl_y_limits = c(0, 2.6)
ampl_y_breaks = seq(0, 2, 0.2)

# --- by age ---
cvemps_age = scatterplot_vemp(df_w, "age", "cvemp_norm_ampl_p_n_BE",
                           "left", "top", corrMethod, "centre", "cvemp_dbnhl_intensity_lev",
                           show_legend = 1, show_text = 0, 
                           age_x_limits, age_x_breaks, ampl_y_limits, ampl_y_breaks,
                           absent_lim = c(NA, 2.3)) +
  ylab("EMG normalised peak-to-peak amplitude [µV]") +
  xlab("Age [years]")

# --- by pta ---
cvemps_pta = scatterplot_vemp(df_w, "PTA4_BE", "cvemp_norm_ampl_p_n_BE",
                           "left", "top", corrMethod, "centre", "cvemp_dbnhl_intensity_lev", 
                           show_legend = 1, show_text = 0, 
                           pta_x_limits, pta_x_breaks, ampl_y_limits, ampl_y_breaks,
                           absent_lim = c(NA, 2.3)) +
  ylab("EMG normalised peak-to-peak amplitude [µV]") +
  xlab("PTA [dB HL]")

# --- Combine plots vertically (Age on top, PTA below) ---
cvemps_combined_grid_ampl <- wrap_plots(cvemps_age, cvemps_pta, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.4, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "cvemp_ampl.png"),
       plot = cvemps_combined_grid_ampl,
       width = 3200, height = 3500, dpi = 300, units = "px")

### --- Plot - cVEMPs (asym) ---------------------------------------------------
asym_y_limits = c(-55, 95)
asym_y_breaks = seq(-60, 60, 10)

# --- by age ---
cvemp_asym_age = scatterplot_vemp(df_w, "age", "cvemp_asym_calc_pta",
                                        "left", "top", corrMethod, "centre", "cvemp_dbnhl_intensity_lev",
                                        show_legend = 1, show_text = 0, 
                                        age_x_limits, age_x_breaks, asym_y_limits, asym_y_breaks,
                                        absent_lim = c(72, 86)) +
  ylab("Asymmetry ratio [%]") +
  xlab("Age [years]")

# --- by pta ---
cvemp_asym_pta = scatterplot_vemp(df_w, "PTA4_BE", "cvemp_asym_calc_pta",
                                        "left", "top", corrMethod, "centre", "cvemp_dbnhl_intensity_lev",
                                        show_legend = 1, show_text = 0, 
                                        pta_x_limits, pta_x_breaks, asym_y_limits, asym_y_breaks,
                                        absent_lim = c(72, 86)) +
  ylab("Asymmetry ratio [%]") +
  xlab("PTA [dB HL]")

# --- Add better-/worse-ear (BE/WE) arrows inside the plot ---
cvemp_asym_age <- cvemp_asym_age +
  # upper text + arrow
  annotate("text", x = 88.6, y = 20, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 90, xend = 90, y = 27, yend = 45,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 88.6, y = -20, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 90, xend = 90, y = -27, yend = -45,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

cvemp_asym_pta <- cvemp_asym_pta +
  # upper text + arrow
  annotate("text", x = 29, y = 20, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 30, xend = 30, y = 27, yend = 45,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 29, y = -20, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 30, xend = 30, y = -27, yend = -45,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

# --- Combine plots vertically (Age on top, PTA below) ---
cvemps_combined_grid_asym <- wrap_plots(cvemp_asym_age, cvemp_asym_pta, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.4, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "cvemp_asym.png"),
       plot = cvemps_combined_grid_asym,
       width = 3200, height = 3500, dpi = 300, units = "px")

### --- Fig. 9: Combine cVEMPs plots -------------------------------------------

# For tags control: wrap each block so patchwork treats them as ONE plot each
ampl_block <- wrap_elements(full = cvemps_combined_grid_ampl)
asym_block <- wrap_elements(full = cvemps_combined_grid_asym)

# Combine side by side with two tags
combined_cvemp <- (ampl_block | asym_block) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 32, face = "bold"),
        plot.tag.position = c(0.05, 0.999))

ggsave(file.path(fileDir, "output", "cvemp_ampl_asym_combined.png"),
       plot = combined_cvemp, width = 6400, height = 3500, dpi = 300, units = "px")

## -- oVEMPs -------------------------------------------------------------------
# oVEMP = df_w %>% select (grep("^ovemp_", names(df_w), value = TRUE))

### Harmonise oVEMP data -------------------------------------------------------

# Peak-to-peak amplitute [μV]
# --- Ensure P-N normalised amplitude calculation is comparable --
# --> Neurosoft formula:  |P-N| / EMG
# --> Eclipse formula:    (P-N) / EMG
# ==> use the same formula as used in Neurosoft |P-N| / EMG
# ==> Because P and N are always positive, both equations should provide the same results.

# - Eclipse EMG:    mean of EMG during testing (stimulus presentation + response).
# - Neurosoft EMG:  pre-stimulus baseline measurement.

# If centre = DE, re-calculate EMG scaled P-N (ovemp_norm_ampl_p_n_r, ovemp_norm_ampl_p_n_l)
# as done by Neurosoft (|P-N| / EMG)

get_p_n_normed <- function(P, N, EMG) (p_n_normed = abs(P-N) / EMG)

# re-normalise DE data to make sure it is done in the same way as FR.
# --> For FR: normalised p-n amplitude is not re-calculated as raw p and n amplitudes are not provided!
df_w <- df_w %>%
  mutate(
    ovemp_norm_ampl_p_n_r = ifelse(ovemp_device==1,
                                   get_p_n_normed(ovemp_ampl_p_r, ovemp_ampl_n_r, ovemp_averg_emg_ampl_r),
                                   ovemp_norm_ampl_p_n_r),
    ovemp_norm_ampl_p_n_l = ifelse(ovemp_device==1, 
                                   get_p_n_normed(ovemp_ampl_p_l, ovemp_ampl_n_l, ovemp_averg_emg_ampl_l),
                                   ovemp_norm_ampl_p_n_l))

## --- Asymmetry ratio (AR) formula ---
vemps_asym_asym <- function(r, l) ( (r - l) / (r + l) ) * 100

# --- Compute AR ---
# --> adjust asymmetry sign relative to better (no sign change) vs. worse ear (flipped sign)
df_w <- df_w %>%
  mutate(ovemp_asym_calc = vemps_asym_asym(ovemp_norm_ampl_p_n_r,  ovemp_norm_ampl_p_n_l),
         ovemp_asym_calc_pta = ifelse(matrix_testear=="R", ovemp_asym_calc, 
                                      ovemp_asym_calc * -1))

### Get better-ear data --------------------------------------------------------
# oVEMPs variable names
vars = c("ovemp_norm_ampl_p_n")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R",
                           df_w[[paste0(i, "_r")]],
                           df_w[[paste0(i, "_l")]])
}

### --- Count selected intensity and ovemp_results  ----------------------------
# --> ovemp_results is either: present (=1), absent (unilateral=2), or absent (bilateral=3)

df_w %>%
  filter(!is.na(ovemp_norm_ampl_p_n_BE)) %>%
  group_by(ovemp_results) %>%
  dplyr::count(., centre)

# selected intensity, ovemp_norm_ampl_p_n_BE
df_w %>%
  filter(!is.na(ovemp_norm_ampl_p_n_BE)) %>%
  group_by(ovemp_dbnhl_intensity_lev) %>%
  dplyr::count(., centre)

# selected intensity, ovemp_asym
df_w %>%
  filter(!is.na(ovemp_asym_calc_pta)) %>%
  group_by(ovemp_dbnhl_intensity_lev) %>%
  dplyr::count(., centre)

# absent response (1=Normal; 2=Unilateral absent; 3=Bilateral absent)
df_w %>%
  group_by(ovemp_results) %>%
  dplyr::count(., centre)

### --- Split normed amplitude by level ----------------------------------------
ovemp_levels = unique(na.omit(df_w$ovemp_dbnhl_intensity_lev))
for (lvl in ovemp_levels) {
  colname <- paste0("ovemp_norm_ampl_p_n_BE_", lvl)
  df_w[[colname]] <- ifelse(df_w$ovemp_dbnhl_intensity_lev == lvl, df_w$ovemp_norm_ampl_p_n_BE, NA)
}
# unique(df_w$ovemp_dbnhl_intensity_lev)
# [1] 100 105  NA 110
# --> 95 does not exist!

# check limits:
psych::describeBy(df_w[c("ovemp_norm_ampl_p_n_BE_100",
                         "ovemp_norm_ampl_p_n_BE_105",
                         "ovemp_norm_ampl_p_n_BE_110")],
                  group = df_w$group, mat = TRUE, digits = 1)

psych::describeBy(df_w[c("ovemp_asym", "ovemp_asym_calc_pta")],
                  group = df_w$group, mat = TRUE, digits = 1)

### --- Plot - oVEMPs (P-N ampl) -----------------------------------------------
# --- Get plot function ---
source(file.path(fileDir, "functions", "scatterplot_vemp.R"))

#### --- ovemp_norm_ampl_p_n_BE by levels --------------------------------------
# # --- Set axis limits ---
# age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
# pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
# ampl_ylim <- scale_y_continuous(limits = c(0, 2), breaks = seq(0, 2, 0.5))
# asym_ylim <- scale_y_continuous(limits = c(-0.5, 1), breaks = seq(-0.5, 1, 0.2))
# 
# # --- Helper function for plotting ---
# make_ovemp_plot <- function(x_var, y_var, xlab_txt) {
#   scatterplot_vemp_combined(df_w, x_var, y_var,
#                             "left", "top", corrMethod, "centre",
#                             labelSpacing = 0.07, show_legend = 1, show_text = 1) +
#     (if (x_var == "age") age_xlim else pta_xlim) +
#     ampl_ylim +
#     xlab(xlab_txt) +
#     ylab("Normalised ampl.\n(|P1-N1| / Avg EMG)")
# }
# 
# # --- Define intensity levels and variables ---
# levs <- c("100", "105", "110")
# y_vars_age <- paste0("ovemp_norm_ampl_p_n_BE_", levs)
# 
# # --- Create age and PTA plots ---
# plots_age <- purrr::map(y_vars_age, ~ make_cvemp_plot("age", .x, "Age [years]"))
# plots_pta <- purrr::map(y_vars_age, ~ make_cvemp_plot("PTA4_BE", .x, "PTA4 [dB HL]"))
# 
# # --- Remove y labels and ticks from middle/right columns ---
# strip_y <- function(p) {
#   p + theme(axis.title.y = element_blank(),
#             axis.text.y = element_blank(),
#             axis.ticks.y = element_blank())
# }
# plots_age[2:3] <- purrr::map(plots_age[2:3], strip_y)
# plots_pta[2:3] <- purrr::map(plots_pta[2:3], strip_y)
# 
# # --- Remove x-axis ticks from top row ---
# plots_age <- purrr::map(plots_age, ~ .x + theme(axis.ticks.x = element_blank()))
# 
# # --- Combine plots in grid ---
# combined_grid <- wrap_plots(c(plots_age, plots_pta), nrow = 2, ncol = 3) +
#   plot_layout(guides = "collect") &
#   theme(legend.position = "right",             # Legend to the right
#         plot.margin = margin(5, 5, 5),
#         panel.spacing = unit(0.3, "lines"))
# 
# # --- Save to PNG ---
# ggsave(file.path(fileDir, "output", "ovemp.png"),
#        plot = combined_grid,
#        width = 4000, height = 2500, dpi = 300, units = "px")

#### --- ovemp_norm_ampl_p_n_BE for 100 +/-5 dB ---------------------------------

# Get oVEMP limits & break points
age_x_limits = c(40, 90)
age_x_breaks = seq(40, 90, 5)

pta_x_limits = c(-5, 30)
pta_x_breaks = seq(-5, 30, 5)

ampl_y_limits = c(0, 2.6)
ampl_y_breaks = seq(0, 2, 0.2)

# --- by age ---
ovemp_age = scatterplot_vemp(df_w, "age", "ovemp_norm_ampl_p_n_BE",
                                        "left", "top", corrMethod, "centre", "ovemp_dbnhl_intensity_lev",
                                        show_legend = 1, show_text = 0, 
                                        age_x_limits, age_x_breaks, ampl_y_limits, ampl_y_breaks,
                                        absent_lim = c(NA, 2.3)) +
  ylab("EMG normalised peak-to-peak amplitude [µV]") +
  xlab("Age [years]")

# --- by pta ---
ovemp_pta = scatterplot_vemp(df_w, "PTA4_BE", "ovemp_norm_ampl_p_n_BE",
                                        "left", "top", corrMethod, "centre", "ovemp_dbnhl_intensity_lev", 
                                        show_legend = 1, show_text = 0, 
                                        pta_x_limits, pta_x_breaks, ampl_y_limits, ampl_y_breaks,
                             absent_lim = c(NA, 2.3)) +
  ylab("EMG normalised peak-to-peak amplitude [µV]") +
  xlab("PTA [dB HL]")

# --- Combine plots vertically (Age on top, PTA below) ---
ovemps_combined_grid_ampl <- wrap_plots(ovemp_age, ovemp_pta, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.4, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "ovemp_ampl.png"),
       plot = ovemps_combined_grid_ampl,
       width = 3000, height = 3000, dpi = 300, units = "px")

#### --- Plot - oVEMPs (asym) --------------------------------------------------
asym_y_limits = c(-80, 95)
asym_y_breaks = seq(-80, 50, 10)

# --- by age ---
ovemp_asym_age = scatterplot_vemp(df_w, "age", "ovemp_asym_calc_pta",
                                       "left", "top", corrMethod, "centre", "ovemp_dbnhl_intensity_lev",
                                       show_legend = 1, show_text = 0, 
                                       age_x_limits, age_x_breaks, asym_y_limits, asym_y_breaks,
                                       absent_lim = c(65, 83)) +
  ylab("Asymmetry ratio [%]") +
  xlab("Age [years]")

# --- by pta ---
ovemp_asym_pta = scatterplot_vemp(df_w, "PTA4_BE", "ovemp_asym_calc_pta",
                                       "left", "top", corrMethod, "centre", "ovemp_dbnhl_intensity_lev", 
                                       show_legend = 1, show_text = 0, 
                                       pta_x_limits, pta_x_breaks, asym_y_limits, asym_y_breaks,
                                       absent_lim = c(65, 83)) +
  ylab("Asymmetry ratio [%]") +
  xlab("PTA [dB HL]")

# --- Add better-/worse-ear (BE/WE) arrows inside the plot ---
ovemp_asym_age <- ovemp_asym_age +
  # upper text + arrow
  annotate("text", x = 88.6, y = 20, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 90, xend = 90, y = 27, yend = 45,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 88.6, y = -20, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 90, xend = 90, y = -27, yend = -45,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

ovemp_asym_pta <- ovemp_asym_pta +
  # upper text + arrow
  annotate("text", x = 29, y = 20, label = "BE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 30, xend = 30, y = 27, yend = 45,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1) +
  
  # lower text + arrow
  annotate("text", x = 29, y = -20, label = "WE",
           hjust = 0, vjust = 0.5, fontface = "bold",
           size = 5, color = "black", parse = TRUE) +
  annotate("segment", x = 30, xend = 30, y = -27, yend = -45,
           arrow = arrow(length = unit(0.2, "cm"), ends = "last", type = "closed"),
           colour = "black", linewidth = 1)

# --- Combine plots vertically (Age on top, PTA below) ---
ovemps_combined_grid_asym <- wrap_plots(ovemp_asym_age, ovemp_asym_pta, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.4, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "ovemp_asym.png"),
       plot = ovemps_combined_grid_asym,
       width = 3000, height = 3500, dpi = 300, units = "px")

#### --- Fig. 10: Combine oVEMPs plots -----------------------------------------

# For tags control: wrap each block so patchwork treats them as ONE plot each
ampl_block <- wrap_elements(full = ovemps_combined_grid_ampl)
asym_block <- wrap_elements(full = ovemps_combined_grid_asym)

# Combine side by side with two tags only
combined_ovemp <-(ampl_block | asym_block) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 32, face = "bold"),
        plot.tag.position = c(0.05, 0.999))

ggsave(file.path(fileDir, "output", "ovemp_ampl_asym_combined.png"),
       plot = combined_ovemp, 
       width = 6400, height = 3500, dpi = 300, units = "px")

## --- ECochG ------------------------------------------------------------------
# df_w %>% select (grep("^ecochg", names(df_w), value = TRUE)) %>% names()

#### --- Get inter-peak latency ------------------------------------------------

# III_V (L/R):
df_w$ecochg_III_V_ms_r = df_w$ecochg_v_ms_r - df_w$ecochg_III_ms_r
df_w$ecochg_III_V_ms_l = df_w$ecochg_v_ms_l - df_w$ecochg_III_ms_l

# AP_V (L/R):
df_w$ecochg_AP_V_ms_r = df_w$ecochg_v_ms_r - df_w$ecochg_ap_ms_r
df_w$ecochg_AP_V_ms_l = df_w$ecochg_v_ms_l - df_w$ecochg_ap_ms_l

#### --- Get better-ear data ---------------------------------------------------

# ABR variable names
vars = c("ecochg_ap_ms", "ecochg_ap_uv", "ecochg_III_ms", "ecochg_III_uv",
         "ecochg_v_ms", "ecochg_v_uv", "ecochg_III_V_ms", "ecochg_AP_V_ms", "ecochg_level")

# select columns based on matrix_testear and save better-ear data with the suffix '_BE'
for (i in vars) {
  new_var = paste0(i, "_BE")
  df_w[[new_var]] = ifelse(df_w$matrix_testear == "R",
                           df_w[[paste0(i, "_r")]],
                           df_w[[paste0(i, "_l")]])
}

# check limits:
psych::describeBy(df_w[c("ecochg_ap_ms_BE", "ecochg_III_V_ms_BE", "ecochg_AP_V_ms_BE")],
                  group = df_w$centre, mat = TRUE, digits = 1)


#### --- Get present/absent responses ------------------------------------------

# --- get inter-peak latency ---

# --- Interpeak III–V (L/R) ---
df_w_with999 <- df_w_with999 %>%
  mutate(ecochg_III_V_ms_r = case_when(
    ecochg_v_ms_r   == 999 ~ 999,
    ecochg_III_ms_r == 999 ~ 999,
    TRUE ~ ecochg_v_ms_r - ecochg_III_ms_r),
    ecochg_III_V_ms_l = case_when(
      ecochg_v_ms_l   == 999 ~ 999,
      ecochg_III_ms_l == 999 ~ 999,
      TRUE ~ ecochg_v_ms_l - ecochg_III_ms_l))

# --- Interpeak AP–V (L/R) ---
df_w_with999 <- df_w_with999 %>%
  mutate(ecochg_AP_V_ms_r = case_when(
    ecochg_v_ms_r   == 999 ~ 999,
    ecochg_ap_ms_r  == 999 ~ 999,
    TRUE ~ ecochg_v_ms_r - ecochg_ap_ms_r),
    ecochg_AP_V_ms_l = case_when(
      ecochg_v_ms_l   == 999 ~ 999,
      ecochg_ap_ms_l  == 999 ~ 999,
      TRUE ~ ecochg_v_ms_l - ecochg_ap_ms_l))

vars = c("ecochg_ap_ms", "ecochg_ap_uv", "ecochg_III_ms", "ecochg_III_uv",
         "ecochg_v_ms", "ecochg_v_uv", "ecochg_III_V_ms", "ecochg_AP_V_ms")

# --> look for missing data (=999) in df_w_with999
# collapse left/right into *_BE
for (i in vars) {
  new_var <- paste0(i, "_BE")
  df_w_with999[[new_var]] <-
    ifelse(df_w_with999$matrix_testear == "R",
           df_w_with999[[paste0(i, "_r")]],
           df_w_with999[[paste0(i, "_l")]])
}

# flag absent/present into *_BE_absent ---
df_w_with999 <- df_w_with999 %>%
  mutate(
    across(all_of(paste0(vars, "_BE")),
           ~ case_when(
             is.na(.x) ~ NA_integer_,  # not measured
             .x == 999 ~ 1L,           # measured but absent
             TRUE      ~ 0L            # measured and present
           ),
           .names = "{.col}_absent"
    )
  )

# bind absent vars into df_w
df_w <- bind_cols(df_w,
                  df_w_with999 %>% select(matches("^ecochg_.*_BE_absent$")))

# --- Count selected level and absent  ---
# count how many present (=0) or absent (=1)
# - level per ear
df_w %>%
  filter(!is.na(ecochg_level_l) | !is.na(ecochg_level_r)) %>%
  group_by(centre) %>%
  dplyr::count(., ecochg_level_l, ecochg_level_r)

# - AP:
df_w %>%
  filter(!is.na(ecochg_level_l) | !is.na(ecochg_level_r)) %>%
  group_by(ecochg_AP_V_ms_BE_absent) %>%
  dplyr::count(., centre, ecochg_level_l, ecochg_level_r)

# - III:
df_w %>%
  filter(!is.na(ecochg_level_l) | !is.na(ecochg_level_r)) %>%
  group_by(ecochg_III_ms_BE_absent) %>%
  dplyr::count(., centre, ecochg_level_l, ecochg_level_r)

# - V:
df_w %>%
  filter(!is.na(ecochg_level_l) | !is.na(ecochg_level_r)) %>%
  group_by(ecochg_v_ms_BE_absent) %>%
  dplyr::count(., centre, ecochg_level_l, ecochg_level_r)

#### Plot - ECochG (latency, ms) -----------------------------------------------
# --- get plot function ---
source(file.path(fileDir, "functions", "scatterplot_abr.R"))

# --- Set axis limits ---
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
ms_ylim_no_abs = scale_y_continuous(limits = c(0, 6.5),
                                    breaks = seq(0, 5, 0.5),
                                    labels = seq(0, 5, 0.5))
ms_ylim = scale_y_continuous(limits = c(0, 6.5), 
                             breaks = c(seq(0, 5, 0.5), 5.5), 
                             labels = c(paste0(seq(0, 5, 0.5)), "AP Absent"))

##### --- AP -------------------------------------------------------------------

# --- by age ---
abr_age = scatterplot_abr(df = df_w, "age" ,"ecochg_ap_ms_BE",
                          statsLocation_x = "left", statsLocation_y = "top", 
                          corrMethod = corrMethod,
                          groupBy = "centre", labelSpacing = 0.07,
                          xlim = NULL, ylim = NULL,
                          show_legend = 1, show_text = 1,
                          absent_var = "ecochg_ap_ms_BE_absent", absent_lim = 5.5) +
  ms_ylim +
  age_xlim +
  ylab("Latency [ms]") +
  xlab("Age [years]")

# --- by pta ---
abr_pta = scatterplot_abr(df = df_w, "PTA4_BE" ,"ecochg_ap_ms_BE",
                          statsLocation_x = "left", statsLocation_y = "top", 
                          corrMethod = corrMethod,
                          groupBy = "centre", labelSpacing = 0.07,
                          xlim = NULL, ylim = NULL,
                          show_legend = 1, show_text = 0,
                          absent_var = "ecochg_ap_ms_BE_absent", absent_lim = 5.5) +
  ms_ylim +
  pta_xlim +
  ylab("Latency [ms]") +
  xlab("PTA [dB HL]")

# --- Combine plots vertically (Age on top, PTA below) ---
abr_combined_grid_AP <- wrap_plots(abr_age, abr_pta, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.4, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "abr_ap_ms.png"),
       plot = abr_combined_grid_AP,
       width = 3000, height = 3000, dpi = 300, units = "px")

##### --- Interpeak: III - V --------------------------------------------------- 

# --- by age ---
abr_age = scatterplot_abr(df = df_w, "age" ,"ecochg_III_V_ms_BE",
                          statsLocation_x = "left", statsLocation_y = "top", 
                          corrMethod = corrMethod,
                          groupBy = "centre", labelSpacing = 0.07, 
                          xlim = NULL, ylim = NULL,
                          show_legend = 1, show_text = 1,
                          absent_var = NULL, absent_lim = NULL) +
  ms_ylim_no_abs +
  age_xlim +
  ylab("Latency [ms]") +
  xlab("Age [years]")

# --- by pta ---
abr_pta = scatterplot_abr(df = df_w, "PTA4_BE" ,"ecochg_III_V_ms_BE",
                          statsLocation_x = "left", statsLocation_y = "top", 
                          corrMethod = corrMethod,
                          groupBy = "centre", labelSpacing = 0.07, 
                          xlim = NULL, ylim = NULL,
                          show_legend = 1, show_text = 1,
                          absent_var = NULL, absent_lim = NULL) +
  ms_ylim +
  pta_xlim +
  ylab("Latency [ms]") +
  xlab("PTA [dB HL]")

# --- Combine plots vertically (Age on top, PTA below) ---
abr_combined_grid_III_V <- wrap_plots(abr_age, abr_pta, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.4, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "abr_III_V_ms.png"),
       plot = abr_combined_grid_III_V,
       width = 3000, height = 3000, dpi = 300, units = "px")

##### --- Interpeak: AP - V ---------------------------------------------------- 

# --- by age ---
abr_age = scatterplot_abr(df = df_w, "age" ,"ecochg_AP_V_ms_BE",
                          statsLocation_x = "left", statsLocation_y = "top", 
                          corrMethod = corrMethod,
                          groupBy = "centre", labelSpacing = 0.07, 
                          xlim = NULL, ylim = NULL,
                          show_legend = 1, show_text = 1,
                          absent_var = "ecochg_AP_V_ms_BE_absent", absent_lim = 5.5) +
  ms_ylim +
  age_xlim +
  ylab("Latency [ms]") +
  xlab("Age [years]")

# --- by pta ---
abr_pta = scatterplot_abr(df = df_w, "PTA4_BE" ,"ecochg_AP_V_ms_BE",
                          statsLocation_x = "left", statsLocation_y = "top", 
                          corrMethod = corrMethod,
                          groupBy = "centre", labelSpacing = 0.07, 
                          xlim = NULL, ylim = NULL,
                          show_legend = 1, show_text = 1,
                          absent_var = "ecochg_AP_V_ms_BE_absent", absent_lim = 5.5) +
  ms_ylim +
  pta_xlim +
  ylab("Latency [ms]") +
  xlab("PTA4 [dB HL]")

# --- Combine plots vertically (Age on top, PTA below) ---
abr_combined_grid_AP_V <- wrap_plots(abr_age, abr_pta, ncol = 1) +
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.4, "lines"))

# --- Save to PNG ---
ggsave(file.path(fileDir, "output", "abr_AP_V_ms.png"),
       plot = abr_combined_grid_AP_V,
       width = 3000, height = 3000, dpi = 300, units = "px")

##### --- Fig. 7: Plot multiple vars all at once -------------------------------

# --- Set axis limits ---
age_xlim <- scale_x_continuous(limits = c(40, 90), breaks = seq(40, 90, 5))
pta_xlim <- scale_x_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5))
ms_ylim = scale_y_continuous(limits = c(0, 6), 
                             breaks = c(seq(0, 5, 0.5), 5.5), 
                             labels = c(paste0(seq(0, 5, 0.5)), "Wave I \nabsent"))

# set variables
vars <- c("ecochg_ap_ms_BE", "ecochg_AP_V_ms_BE", "ecochg_III_V_ms_BE")
absent_vars <- c("ecochg_ap_ms_BE_absent", "ecochg_AP_V_ms_BE_absent","ecochg_III_V_ms_BE_absent")

# helper
make_abr_plot <- function(x_var, y_var, xlab_txt, absent_var, show_leg = 0) {
  scatterplot_abr(df = df_w, param_x = x_var, param_y = y_var,
                  statsLocation_x = "left", statsLocation_y = "bottom",
                  corrMethod = corrMethod, groupBy = "centre",
                  show_legend = show_leg, show_text = 1,
                  absent_var = absent_var, absent_lim = 5.7) +
    (if (x_var == "age") age_xlim else pta_xlim) +
    ms_ylim +
    xlab(xlab_txt) + 
    ylab("Latency [ms]")}

# build plots (only the very first panel emits a legend)
legend_flags <- c(1, rep(0, length(vars) - 1))

plots_age <- Map(function(y, a, leg) make_abr_plot("age", y, "Age [years]",  a, show_leg = leg),
                 vars, absent_vars, legend_flags)

plots_pta <- Map(function(y, a) make_abr_plot("PTA4_BE", y, "PTA [dB HL]", a, show_leg = 0),
                 vars, absent_vars)

# avoid extra legends: remove legend from all but the first plot
if (length(plots_age) > 1) { 
  
  for (i in 2:length(plots_age)) plots_age[[i]] <- plots_age[[i]] + theme(legend.position = "none")

  }

for (i in seq_along(plots_pta)) plots_pta[[i]] <- plots_pta[[i]] + theme(legend.position = "none")

# optional: tidy axes
strip_y <- function(p) p + theme(axis.title.y = element_blank(),
                                 axis.text.y  = element_blank(),
                                 axis.ticks.y = element_blank())
if (length(plots_age) > 1) {
  idx <- 2:length(plots_age)
  plots_age[idx] <- lapply(plots_age[idx], strip_y)
  plots_pta[idx] <- lapply(plots_pta[idx], strip_y)
}

plots_age <- lapply(plots_age, function(p) p + theme(axis.ticks.x = element_blank()))

# combine
combined_grid <- wrap_plots(plotlist = c(plots_age, plots_pta), ncol = length(vars)) + 
  plot_layout(guides = "collect") &
  theme(legend.position = "right",
        plot.margin = margin(5, 5, 5, 5),
        panel.spacing = unit(0.3, "lines"))

# save
ggsave(file.path(fileDir, "output", "abr_multi_intrpk.png"),
       plot = combined_grid, 
       width = 6000, height = 3000, dpi = 300, units = "px")

## --- Tab. A2: (vars Median by age groups) ------------------------------------

### --- Clean data frame for centre analysis and tables generation -------------

# keep ABR (ECochG) measures only at level = 115
abr_vars <- c("ecochg_ap_ms_BE", "ecochg_AP_V_ms_BE", "ecochg_III_V_ms_BE")

df_w_cleaned <- df_w %>%
  mutate(abr_keep = !is.na(ecochg_level_BE) & ecochg_level_BE %in% c(115, "115")) %>%
  mutate(across(all_of(abr_vars), ~ ifelse(abr_keep, ., NA_real_))) %>%
  select(-abr_keep)

# keep only present OAE responses (DPOAE and TEOAE)
df_w_cleaned <- df_w_cleaned %>%
  mutate(dpoae_snr_1khz_BE = if_else(dpoae_res_1khz_BE == 1, dpoae_snr_1khz_BE, NA_real_),
         dpoae_snr_2khz_BE = if_else(dpoae_res_2khz_BE == 1, dpoae_snr_2khz_BE, NA_real_),
         dpoae_snr_4khz_BE = if_else(dpoae_res_4khz_BE == 1, dpoae_snr_4khz_BE, NA_real_), 
         teoae_snr_1k_BE   = if_else(teoae_res_1k_BE == 1, teoae_snr_1k_BE, NA_real_),
         teoae_snr_2k_BE   = if_else(teoae_res_2k_BE == 1, teoae_snr_2k_BE, NA_real_),
         teoae_snr_3k_BE   = if_else(teoae_res_3k_BE == 1, teoae_snr_3k_BE, NA_real_),
         teoae_snr_4k_BE   = if_else(teoae_res_4k_BE == 1, teoae_snr_4k_BE, NA_real_),
         teoae_snr_5k_BE   = if_else(teoae_res_5k_BE == 1, teoae_snr_5k_BE, NA_real_))

# keep only bithermal CT UW
df_w_cleaned <- df_w_cleaned %>%
  mutate(ct_uw_calc_pta = if_else(ct_monothermal == 0, ct_uw_calc_pta, NA_real_))

### Get Median table -----------------------------------------------------------

# --- Normalise age_group labels ---
df_w_cleaned <- df_w_cleaned %>%
  mutate(age_group = as.character(age_group) %>% str_squish(),
         age_group = str_replace_all(age_group, "[\u2012\u2013\u2014\u2212–—]", "-"),
         age_group = str_replace_all(age_group, "\\s*-\\s*", "-"),
         age_group = if_else(age_group %in% c("40-49","50-59","60-69","70-79","80-89"),
                             age_group, NA_character_),
         age_group = factor(age_group, levels = c("40-49","50-59","60-69","70-79","80-89"))) %>%
  # ---- create centre-split Matrix variables ---
mutate(matrix_mono_ssn_srt_DE   = if_else(centre == "DE", matrix_mono_ssn_srt,   NA_real_),
       matrix_mono_ssn_srt_FR   = if_else(centre == "FR", matrix_mono_ssn_srt,   NA_real_),
       matrix_mono_icra5_srt_DE = if_else(centre == "DE", matrix_mono_icra5_srt, NA_real_),
       matrix_mono_icra5_srt_FR = if_else(centre == "FR", matrix_mono_icra5_srt, NA_real_))

# --- selected variables ---
vars <- c("age", "PTA4_BE",
          "ls_cu_15_BE", "ls_cu_35_BE", "ls_diff_35_15_BE", "ls_cu_mean_bin_summation",
          "tin_snr_ERB_500","tin_snr_ERB_2000",
          "matrix_mono_ssn_srt_DE", "matrix_mono_ssn_srt_FR",
          "matrix_mono_icra5_srt_DE", "matrix_mono_icra5_srt_FR",
          "matrix_mono_ssn_srt_norm", "matrix_mono_icra5_srt_norm",
          "reflex_mean_BE", "dpoae_snr_1khz_BE", "dpoae_snr_2khz_BE", "dpoae_snr_4khz_BE", 
          "teoae_snr_1k_BE", "teoae_snr_2k_BE", "teoae_snr_3k_BE", "teoae_snr_4k_BE", 
          "teoae_snr_5k_BE","ecochg_ap_ms_BE", "ecochg_AP_V_ms_BE", "ecochg_III_V_ms_BE", 
          "ct_velcty_nystgms_cold_abs_BE", "ct_velcty_nystgms_warm_abs_BE", "ct_uw_calc_pta",
          "ct_dp_calc_pta", "vhit_gain_anter_BE", "vhit_gain_later_BE", "vhit_gain_poster_BE", 
          "vhit_asym_anter_calc_pta", "vhit_asym_later_calc_pta", "vhit_asym_poster_calc_pta", 
          "cvemp_norm_ampl_p_n_BE", "cvemp_asym_calc_pta", 
          "ovemp_norm_ampl_p_n_BE", "ovemp_asym_calc_pta")

age_levels <- c("40-49","50-59","60-69","70-79","80-89")

# --- helper: "Median [Q1, Q3], n=" ---
fmt_med_iqr_n <- function(med, q1, q3, n, digits = 1) {
  sprintf(paste0("%.", digits, "f [", "%.", digits, "f, %.", digits, "f], n=%d"),
          med, q1, q3, n)
}

# --- summary by age-group ---
by_group_long <- df_w_cleaned %>%
  filter(!is.na(age_group)) %>%
  select(age_group, all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "measure", values_to = "value") %>%
  group_by(age_group, measure) %>%
  summarise(n = sum(!is.na(value)),
            med = median(value, na.rm = TRUE),
            q1 = quantile(value, 0.25, na.rm = TRUE, type = 7),
            q3 = quantile(value, 0.75, na.rm = TRUE, type = 7),
            .groups = "drop") %>%
  mutate(med_iqr_n = fmt_med_iqr_n(med, q1, q3, n))

# --- overall summary ---
overall_long <- df_w_cleaned %>%
  select(all_of(vars)) %>%
  pivot_longer(cols = all_of(vars), names_to = "measure", values_to = "value") %>%
  group_by(measure) %>%
  summarise(n = sum(!is.na(value)),
            med = median(value, na.rm = TRUE),
            q1 = quantile(value, 0.25, na.rm = TRUE, type = 7),
            q3 = quantile(value, 0.75, na.rm = TRUE, type = 7),
            .groups = "drop") %>%
  mutate(age_group = factor("Combined", levels = c("Combined", age_levels)),
         med_iqr_n = fmt_med_iqr_n(med, q1, q3, n)) %>%
  select(age_group, measure, n, med, q1, q3, med_iqr_n)

# --- combine + re-order ---
measure_order = vars

summary_long <- bind_rows(overall_long, by_group_long) %>%
  mutate(age_group = factor(age_group, levels = c("Combined", age_levels)),
         measure = stringr::str_squish(as.character(measure)),
         measure = factor(measure, levels = measure_order)) %>%
  arrange(measure)

# --- column headers with group n ---
if ("id" %in% names(df_w_cleaned)) {
  n_by_group <- df_w_cleaned %>%
    filter(!is.na(age_group)) %>%
    distinct(age_group, id) %>%
    count(age_group, name = "n_group")
  overall_N <- df_w_cleaned %>% distinct(id) %>% nrow()
} else {
  n_by_group <- df_w_cleaned %>%
    filter(!is.na(age_group)) %>%
    count(age_group, name = "n_group")
  overall_N <- nrow(df_w_cleaned)
}

n_by_group <- bind_rows(
  tibble(age_group = factor("Combined", levels = levels(summary_long$age_group)),
         n_group = overall_N),
  n_by_group) %>%
  mutate(col_lab = ifelse(age_group == "Combined",
                          paste0("Combined (N=", n_group, ")"),
                          paste0(as.character(age_group), " (n=", n_group, ")")))

# --- wide table with "Median [IQR] (n=)" ---
summary_wide <- summary_long %>%
  select(age_group, measure, med_iqr_n) %>%
  mutate(measure = stringr::str_squish(as.character(measure))) %>%
  pivot_wider(names_from = age_group,
              values_from = med_iqr_n,
              values_fn = dplyr::first) %>%
  mutate(Measure = dplyr::recode(measure,
                                 age = "Age (years)",
                                 PTA4_BE = "PTA4_BE (dB HL)",
                                 ls_cu_15_BE = "ACALOS L15 (dB SPL)",
                                 ls_cu_35_BE = "ACALOS L35 (dB SPL)",
                                 ls_diff_35_15_BE = "ACALOS DR (dB)",
                                 ls_cu_mean_bin_summation = "ACALOS BLS (dB)",
                                 tin_snr_ERB_500 = "TIN ERB_500Hz (dB SNR)",
                                 tin_snr_ERB_2000 = "TIN ERB_2000Hz (dB SNR)",
                                 matrix_mono_ssn_srt_DE = "Matrix SRT_UOL SSN (dB SNR)",
                                 matrix_mono_ssn_srt_FR = "Matrix SRT_IDA SSN (dB SNR)",
                                 matrix_mono_icra5_srt_DE = "Matrix SRT_UOL ICRA5-250 (dB SNR)",
                                 matrix_mono_icra5_srt_FR = "Matrix SRT_IDA ICRA5-250 (dB SNR)",
                                 matrix_mono_ssn_srt_norm = "Matrix SRT_norm SSN (dB SNR)",
                                 matrix_mono_icra5_srt_norm = "Matrix SRT_norm ICRA5-250 (dB SNR)",
                                 reflex_mean_BE = "Acoustic reflex level (dB SPL)", 
                                 dpoae_snr_1khz_BE = "DPOAE 1kHz (dB SNR)",
                                 dpoae_snr_2khz_BE = "DPOAE 2kHz (dB SNR)",
                                 dpoae_snr_4khz_BE = "DPOAE 4kHz (dB SNR)",
                                 teoae_snr_1k_BE = "TEOAE 1kHz (dB SNR)", 
                                 teoae_snr_2k_BE = "TEOAE 2kHz (dB SNR)",
                                 teoae_snr_3k_BE = "TEOAE 3kHz (dB SNR)",
                                 teoae_snr_4k_BE = "TEOAE 4kHz (dB SNR)", 
                                 teoae_snr_5k_BE = "TEOAE 5kHz (dB SNR)",
                                 ecochg_ap_ms_BE = "ECochG wave 1 (ms)", 
                                 ecochg_AP_V_ms_BE = "ECochG interpeak I-V (ms)",
                                 ecochg_III_V_ms_BE = "ECochG interpeak III-V (ms)", 
                                 ct_velcty_nystgms_cold_abs_BE = "CT SPV cold (°/s)",
                                 ct_velcty_nystgms_warm_abs_BE = "CT SPV warm (°/s)",
                                 ct_uw_calc_pta = "CT UW (%)",
                                 ct_dp_calc_pta = "CT DP (%)",
                                 vhit_gain_anter_BE = "vHIT gain_anterior",
                                 vhit_gain_later_BE = "vHIT gain_lateral",
                                 vhit_gain_poster_BE = "vHIT gain_posterior",
                                 vhit_asym_anter_calc_pta = "vHIT AR_anterior (%)", 
                                 vhit_asym_later_calc_pta = "vHIT AR_lateral (%)",
                                 vhit_asym_poster_calc_pta = "vHIT AR_posterior (%)",
                                 cvemp_norm_ampl_p_n_BE = "cVEMP EMG-normalised P-N amplitude (µV)", 
                                 cvemp_asym_calc_pta = "cVEMP AR (%)", 
                                 ovemp_norm_ampl_p_n_BE = "oVEMP EMG-normalised P-N amplitude (µV)", 
                                 ovemp_asym_calc_pta = "oVEMP AR (%)", 
                                 .default = measure)) %>%
  select(Measure, everything(), -measure) %>%
  rename_with(~ paste0(.x, " years"), .cols = matches("^(40-49|50-59|60-69|70-79|80-89)$"))

# rename columns
col_map <- setNames(n_by_group$col_lab, as.character(n_by_group$age_group))
summary_wide <- summary_wide %>% rename(any_of(col_map))

# --- SAVE CSV ---
readr::write_excel_csv(summary_wide, file.path(fileDir, "output", "median_by_age_summary.csv"))


### --- Tab 1 (correlations & regressions) -------------------------------------

# --- Helpers ---
get_lm_coef <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(c(intercept = NA_real_, slope = NA_real_))
  co <- coef(lm(y ~ x, model = FALSE))
  c(intercept = unname(co[1]), slope = unname(co[2]))
}

get_spearman <- function(x, y) {
  ok <- complete.cases(x, y)
  if (sum(ok) < 3) return(c(rho = NA_real_, p = NA_real_))
  ct <- suppressWarnings(cor.test(x[ok], y[ok], method = "spearman"))
  c(rho = unname(ct$estimate), p = ct$p.value)
}

summarise_reg <- function(df, vars, x) {
  tmp <- df_w_cleaned %>%
    select(all_of(c(x, vars))) %>%
    pivot_longer(all_of(vars), names_to = "measure", values_to = "y") |>
    group_by(measure) %>%
    summarise(coef = list(get_lm_coef(.data[[x]], y)),
              cor = list(get_spearman(.data[[x]], y)),
              .groups = "drop")
  
  b0 <- vapply(tmp$coef, `[[`, numeric(1), 1)
  b1 <- vapply(tmp$coef, `[[`, numeric(1), 2)
  r <- vapply(tmp$cor,  `[[`, numeric(1), 1)
  p <- vapply(tmp$cor,  `[[`, numeric(1), 2)
  
  tmp %>% mutate(!!paste0("\u03b20_", x) := b0,
                 !!paste0("\u03b21_", x) := b1,
                 !!paste0("\u03c1_",  x) := r,
                 !!paste0("p_",  x) := p) %>%
    select(-coef, -cor)
}

fmt_med_iqr_n <- function(med, q1, q3, n, digits = 1) {
  sprintf(paste0("%.", digits, "f [", "%.", digits, "f, %.", digits, "f], n=%d"), med, q1, q3, n)
}

# --- Variables to include and their order ---
vars <- c("ls_cu_15_BE","ls_cu_35_BE","ls_diff_35_15_BE","ls_cu_mean_bin_summation",
          "tin_snr_ERB_500","tin_snr_ERB_2000",
          "matrix_mono_ssn_srt_DE","matrix_mono_ssn_srt_FR",
          "matrix_mono_icra5_srt_DE","matrix_mono_icra5_srt_FR",
          "matrix_mono_ssn_srt_norm", "matrix_mono_icra5_srt_norm",
          "reflex_mean_BE",
          "dpoae_snr_1khz_BE","dpoae_snr_2khz_BE","dpoae_snr_4khz_BE",
          "teoae_snr_1k_BE","teoae_snr_2k_BE","teoae_snr_3k_BE","teoae_snr_4k_BE","teoae_snr_5k_BE",
          "ecochg_ap_ms_BE","ecochg_AP_V_ms_BE","ecochg_III_V_ms_BE",
          "ct_velcty_nystgms_cold_abs_BE","ct_velcty_nystgms_warm_abs_BE","ct_uw_calc_pta","ct_dp_calc_pta",
          "vhit_gain_anter_BE","vhit_gain_later_BE","vhit_gain_poster_BE",
          "vhit_asym_anter_calc_pta","vhit_asym_later_calc_pta","vhit_asym_poster_calc_pta",
          "cvemp_norm_ampl_p_n_BE","cvemp_asym_calc_pta",
          "ovemp_norm_ampl_p_n_BE","ovemp_asym_calc_pta")

# --- Combined summary ---
long_df <- df_w_cleaned %>%
  select(age, PTA4_BE, all_of(vars)) |>
  pivot_longer(all_of(vars), names_to = "measure", values_to = "value")

combined_summ <- long_df %>%
  group_by(measure) %>%
  summarise(n_age = sum(complete.cases(age, value)),
            med = median(value, na.rm = TRUE),
            q1 = quantile(value, 0.25, na.rm = TRUE, type = 7),
            q3 = quantile(value, 0.75, na.rm = TRUE, type = 7),
            .groups = "drop") %>%
  mutate(combined = fmt_med_iqr_n(med, q1, q3, n_age)) |>
  select(measure, combined)

# --- Regression summary ---
reg_age <- summarise_reg(df_w_cleaned, vars, "age")
reg_pta <- summarise_reg(df_w_cleaned, vars, "PTA4_BE")

# --- Label map ---
measure_labels <- setNames(c("ACALOS L15 (dB SPL)", "ACALOS L35 (dB SPL)", "ACALOS DR (dB)", "ACALOS BLS (dB)",
                             "TIN ERB_500Hz (dB SNR)","TIN ERB_2000Hz (dB SNR)",
                             "Matrix SRT_UOL SSN (dB SNR)", "Matrix SRT_IDA SSN (dB SNR)",
                             "Matrix SRT_UOL ICRA5-250 (dB SNR)", "Matrix SRT_IDA ICRA5-250 (dB SNR)",
                             "Matrix SRT_norm SSN (dB SNR)", "Matrix SRT_norm ICRA5-250 (dB SNR)",
                             "Acoustic reflex level (dB SPL)",
                             "DPOAE 1kHz (dB SNR)", "DPOAE 2kHz (dB SNR)", "DPOAE 4kHz (dB SNR)",
                             "TEOAE 1kHz (dB SNR)", "TEOAE 2kHz (dB SNR)", "TEOAE 3kHz (dB SNR)",
                             "TEOAE 4kHz (dB SNR)", "TEOAE 5kHz (dB SNR)",
                             "ECochG wave I (ms)", "ECochG interpeak I-V (ms)", "ECochG interpeak III-V (ms)",
                             "CT SPV cold (°/s)", "CT SPV warm (°/s)", "CT UW (%)", "CT DP (%)",
                             "vHIT gain_anterior", "vHIT gain_lateral", "vHIT gain_posterior",
                             "vHIT AR_anterior (%)", "vHIT AR_lateral (%)", "vHIT AR_posterior (%)",
                             "cVEMP EMG-normalised P-N amplitude (µV)", "cVEMP AR (%)",
                             "oVEMP EMG-normalised P-N amplitude (µV)", "oVEMP AR (%)"),
                           vars)

# --- Final table ---
reg_table <- combined_summ %>%
  left_join(reg_age, by = "measure") %>%
  left_join(reg_pta, by = "measure") %>%
  rename(Measure = measure) %>%
  relocate(combined, .after = Measure) %>%
  mutate(Measure = dplyr::recode(Measure, !!!measure_labels),
         Measure = factor(Measure, levels = unname(measure_labels))) %>%
  arrange(Measure)

# --- Export without rounding (raw) ---
readr::write_excel_csv(reg_table,
                       file.path(fileDir, "output",
                                 paste0("regression_summary_", format(Sys.Date(), "%d-%m-%Y"), ".csv")))

#### --- format table for paper ------------------------------------------------

# p formatting function
fmt_p <- function(p) {
  out <- rep(NA_character_, length(p))
  na  <- is.na(p)
  out[!na & p < 0.001]                  <- "**<0.001**"
  out[!na & p >= 0.001 & p < 0.01]      <- "**<0.01**"
  out[!na & p >= 0.01  & p < 0.05]      <- "**<0.05**"
  keep <- !na & p >= 0.05
  out[keep] <- formatC(p[keep], format = "f", digits = 3)
  out
}

# rho formatting function
fmt_rho <- function(x) {
  out <- rep(NA_character_, length(x))
  idx <- !is.na(x)
  out[idx] <- formatC(x[idx], format = "f", digits = 2)
  out
}

# coefficients formatting function
fmt_coef <- function(x) {
  sup_digit <- c('0'='⁰','1'='¹','2'='²','3'='³','4'='⁴','5'='⁵','6'='⁶','7'='⁷','8'='⁸','9'='⁹')
  sup_sign  <- c('+'='⁺','-'='⁻')
  to_sup <- function(s) {
    if (is.na(s)) return("")
    ch <- strsplit(s, "", fixed = TRUE)[[1]]
    mapped <- ifelse(ch %in% names(sup_digit), sup_digit[ch],
                     ifelse(ch %in% names(sup_sign),  sup_sign[ch], ""))
    paste0(mapped, collapse = "")
  }
  sci_one <- function(v) {
    fx <- sprintf("%.2e", v)              # e.g., "2.30e-04"
    parts <- strsplit(fx, "e", fixed = TRUE)[[1]]
    mant  <- parts[1]
    expn  <- parts[2]                     # e.g., "-04" or "+12"
    sign  <- substr(expn, 1, 1)
    digits<- gsub("^0+", "", substr(expn, 2, nchar(expn)))
    if (digits == "") digits <- "0"
    paste0(mant, "×10", to_sup(paste0(sign, digits)))
  }
  out <- rep(NA_character_, length(x))
  na <- is.na(x)
  nz <- !na
  z <- x[nz]
  out[nz] <- ifelse(z == 0, "0.00",
                    ifelse(abs(z) < 1e-2 | abs(z) >= 1e3,
                           vapply(z, sci_one, character(1)),
                           formatC(z, format = "f", digits = 2)))
  out
}

# Create a formatted display version
reg_table_formatted <- reg_table %>%
  dplyr::mutate(`β0_age (disp)` = fmt_coef(`β0_age`),
                `β1_age (disp)` = fmt_coef(`β1_age`),
                `ρ_age (disp)` = fmt_rho(`ρ_age`),
                `p_age (disp)` = fmt_p(`p_age`),
                
                `β0_PTA (disp)` = fmt_coef(`β0_PTA4_BE`),
                `β1_PTA (disp)` = fmt_coef(`β1_PTA4_BE`),
                `ρ_PTA (disp)` = fmt_rho(`ρ_PTA4_BE`),
                `p_PTA (disp)` = fmt_p(`p_PTA4_BE`)) %>%
  
  dplyr::select(Measure, combined,
                `β0_age (disp)`, `β1_age (disp)`, `ρ_age (disp)`, `p_age (disp)`,
                `β0_PTA (disp)`, `β1_PTA (disp)`, `ρ_PTA (disp)`, `p_PTA (disp)`,
                # keep raw numeric columns for reference
                `β0_age`, `β1_age`, `ρ_age`, `p_age`,
                `β0_PTA4_BE`, `β1_PTA4_BE`, `ρ_PTA4_BE`, `p_PTA4_BE`)

# Export the formatted (display) table
readr::write_excel_csv(reg_table_formatted,
                       file.path(fileDir, "output",
                                 paste0("regression_summary_formatted_", format(Sys.Date(), "%d-%m-%Y"), ".csv")))

## -- Centre effect analysis ---------------------------------------------------
if(!require(kableExtra)){install.packages("kableExtra")}
if(!require(ggtext)){install.packages("car")}
if(!require(ggtext)){install.packages("MatchIt")}
if(!require(ggtext)){install.packages("broom")}
if(!require(ggtext)){install.packages("knitr")}


### --- Clean data frame for analysis & tables generation ----------------------

# keep ABR (ECochG) measures only at level = 115
abr_vars <- c("ecochg_ap_ms_BE", "ecochg_AP_V_ms_BE", "ecochg_III_V_ms_BE")

df_w_cleaned <- df_w %>%
  mutate(abr_keep = !is.na(ecochg_level_BE) & ecochg_level_BE %in% c(115, "115")) %>%
  mutate(across(all_of(abr_vars), ~ ifelse(abr_keep, ., NA_real_))) %>%
  select(-abr_keep)

# keep only present OAE responses (DPOAE and TEOAE)
df_w_cleaned <- df_w_cleaned %>%
  mutate(dpoae_snr_1khz_BE = if_else(dpoae_res_1khz_BE == 1, dpoae_snr_1khz_BE, NA_real_),
         dpoae_snr_2khz_BE = if_else(dpoae_res_2khz_BE == 1, dpoae_snr_2khz_BE, NA_real_),
         dpoae_snr_4khz_BE = if_else(dpoae_res_4khz_BE == 1, dpoae_snr_4khz_BE, NA_real_), 
         teoae_snr_1k_BE = if_else(teoae_res_1k_BE == 1, teoae_snr_1k_BE, NA_real_),
         teoae_snr_2k_BE = if_else(teoae_res_2k_BE == 1, teoae_snr_2k_BE, NA_real_),
         teoae_snr_3k_BE = if_else(teoae_res_3k_BE == 1, teoae_snr_3k_BE, NA_real_),
         teoae_snr_4k_BE = if_else(teoae_res_4k_BE == 1, teoae_snr_4k_BE, NA_real_),
         teoae_snr_5k_BE = if_else(teoae_res_5k_BE == 1, teoae_snr_5k_BE, NA_real_))

# keep only bithermal CT UW
df_w_cleaned <- df_w_cleaned %>%
  mutate(ct_uw_calc_pta = if_else(ct_monothermal == 0, ct_uw_calc_pta, NA_real_))

## --- Re-run matchit with final data frame  ------------------------------------

# calculate propensity score and match patients from both groups based on that -> to have a fair comparison (first to learn about the correspondence of our two centers, but second as alternative sample choice for linear model below)
# Estimate propensity scores (needs complete data, here in the control group it is only C057-SS who is anyway not included in the analysis)
columns_to_check <- c("age", "PTA4_BE")
df_complete <- df_w_cleaned[complete.cases(df_w_cleaned[, columns_to_check]), ]

#### --- OPTION 1 --------------------------------------------------------------
# Match participants based on propensity scores 
# m.out <- matchit(centre ~ age + PTA4_BE,
#               data = df_complete, method = "nearest", distance = "glm") # , m.order = "largest")  #  method = "nearest", distance = "glm")

#### --- OPTION 2 (final) ------------------------------------------------------
# Match participants based on Mahalanobis distances 
m.out <- matchit(centre ~ age + PTA4_BE, 
                 data = df_complete,
                 method = "nearest",  # Nearest neighbor matching
                 distance = "mahalanobis",  # Better than propensity for small samples
                 ratio = 1,  # 1:1 matching (one French for each German)
                 replace = FALSE)  # Each French subject used only once

#summary(m.out)
#plot(summary(m.out))

# plot(m.out, type = "jitter", interactive = FALSE) # does not work for mahalanobis
matching_density_plot <- plot(m.out, type = "density", interactive = FALSE, which.xs = ~age + PTA4_BE)

# Get matched dataset
matched_data <- match.data(m.out) # this data frame looks plausible 
table(matched_data$centre)

# investigate matched_data 
# - number of datapoints per center
# - obtained age and PTA distributions, especially means 
# - balance 

matched_data %>%
  arrange(subclass) %>%
  ggplot(aes(x = age, y = PTA4_BE, color = factor(centre))) +
  geom_point() +
  geom_line(aes(group = subclass), alpha = 0.5) +
  labs(title = "Matched Pairs by Age and PTA",
       color = "Center") +
  theme_minimal()

# Get matched data frame from MatchIt object m.out
matched_data <- match.data(m.out)

# Ensure centre is a factor with DE/FR
matched_data$centre <- factor(matched_data$centre, levels = c("DE", "FR"))

#### --- Boxplots to investigate two centers before and after matching ---------
df_plot <- df_w_cleaned  # matched_data # df_w

pta_plot <- ggplot(df_plot, aes(x = centre, y = PTA4_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "PTA by centre", y = "PTA [dB HL]")

age_plot <- ggplot(df_plot, aes(x = centre, y = age, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Age by centre", y = "Age [years]")

tin_plot_500 <- ggplot(df_plot, aes(x = centre, y = tin_snr_ERB_500, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "TIN SNR by centre", y = "TIN SNR at 500 Hz")

tin_plot_2000 <- ggplot(df_plot, aes(x = centre, y = tin_snr_ERB_2000, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "TIN SNR by centre", y = "TIN SNR at 2000 Hz")

ls_cu_15_BE_plot <- ggplot(df_plot, aes(x = centre, y = ls_cu_15_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "L15 BE by centre", y = "Level [dB SPL]")

ls_cu_35_BE_plot <- ggplot(df_plot, aes(x = centre, y = ls_cu_35_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "L35 BE by centre", y = "Level [dB SPL]")

ls_dr_plot <- ggplot(df_plot, aes(x = centre, y = ls_diff_35_15_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Dynamic range by centre", y = "Level [dB]")

ls_cu_15_bin_sum_plot <- ggplot(df_plot, aes(x = centre, y = ls_cu_15_bin_summation, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "L15 binaural loudness summation by centre", y = "Binaural loudness summation [dB]")

ls_cu_35_bin_sum_plot <- ggplot(df_plot, aes(x = centre, y = ls_cu_mean_bin_summation, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "L35 binaural loudness summation by centre", y = "Binaural loudness summation [dB]")

matrix_ssn_plot <- ggplot(df_plot, aes(x = centre, y = matrix_mono_ssn_srt, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Matrix test SSN by centre", y = "SRT [dB SNR]")

matrix_icra_plot <- ggplot(df_plot, aes(x = centre, y = matrix_mono_icra5_srt, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Matrix test ICRA5 by centre", y = "SRT [dB SNR]")

matrix_ssn_plot_norm <- ggplot(df_plot, aes(x = centre, y = matrix_mono_ssn_srt_norm, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Matrix test SSN normalised SRT pooled across centre", y = "reference-normalised SRT_norm [dB SNR]")

matrix_icra_plot_norm <- ggplot(df_plot, aes(x = centre, y = matrix_mono_icra5_srt_norm, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "Matrix test ICRA5 normalised SRT pooled across centre", y = "reference-normalised SRT_norm [dB SNR]")

teoae_snr_1k_plot <- ggplot(df_plot, aes(x = centre, y = dpoae_snr_1khz_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "TEOAE 1 kHz by centre", y = "SNR [dB]")

ct_spv_cold_plot <- ggplot(df_plot, aes(x = centre, y = ct_velcty_nystgms_cold_abs_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "CT cold SPV by centre", y = "SPV [deg/s]")

ct_spv_warm_plot <- ggplot(df_plot, aes(x = centre, y = ct_velcty_nystgms_warm_abs_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "CT warm SPV by centre", y = "SPV [deg/s]")

vhit_gain_later_plot <- ggplot(df_plot, aes(x = centre, y = vhit_gain_later_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "vHIT lateral gain by centre", y = "gain")

cvemp_p_n_ampl_plot <- ggplot(df_plot, aes(x = centre, y = cvemp_norm_ampl_p_n_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "cVEMPs normalised p-n peak amplitude by centre", y = "amplitude [μV]")

ovemp_p_n_ampl_plot <- ggplot(df_plot, aes(x = centre, y = ovemp_norm_ampl_p_n_BE, fill = centre)) +
  geom_boxplot(alpha = 0.7) +
  geom_jitter(width = 0.2, alpha = 0.3) +
  theme_minimal() +
  labs(title = "oVEMPs normalised p-n peak amplitude by centre", y = "amplitude [μV]")

### --- Define outcome variables -----------------------------------------------
# --> "teoae_snr_5k_BE" was dropped out due to too many missing present responses 
# (which resulted in errors)

outcomes <- c("ls_cu_15_BE", "ls_cu_35_BE", "ls_diff_35_15_BE", "ls_cu_mean_bin_summation",
              "tin_snr_ERB_500","tin_snr_ERB_2000",
              "matrix_mono_ssn_srt","matrix_mono_icra5_srt",
              "matrix_mono_ssn_srt_norm", "matrix_mono_icra5_srt_norm",
              "reflex_mean_BE", "dpoae_snr_1khz_BE", "dpoae_snr_2khz_BE", "dpoae_snr_4khz_BE", 
              "teoae_snr_1k_BE", "teoae_snr_2k_BE", "teoae_snr_3k_BE", "teoae_snr_4k_BE",
              "ecochg_ap_ms_BE", "ecochg_AP_V_ms_BE", "ecochg_III_V_ms_BE", 
              "ct_velcty_nystgms_cold_abs_BE", "ct_velcty_nystgms_warm_abs_BE", "ct_uw_calc_pta",
              "ct_dp_calc_pta", "vhit_gain_anter_BE", "vhit_gain_later_BE", "vhit_gain_poster_BE", 
              "vhit_asym_anter_calc_pta", "vhit_asym_later_calc_pta", "vhit_asym_poster_calc_pta", 
              "cvemp_norm_ampl_p_n_BE", "cvemp_asym_calc_pta", 
              "ovemp_norm_ampl_p_n_BE", "ovemp_asym_calc_pta")

### --- Levene's test ----------------------------------------------------------

# Examples:
# Levene's test to estimate if variances are equal (examples)
car::leveneTest(tin_snr_ERB_2000 ~ centre, data = df_w) # p = 0.095 
car::leveneTest(tin_snr_ERB_2000 ~ centre, data = matched_data) 

#### --- levene's test: all IDs ------------------------------------------------
# Run Levene's test on each outcome and collect results
levene_results_all <- map_df(outcomes, function(y) {
  # Create formula
  formula <- as.formula(paste(y, "~ centre"))
  
  # Run Levene's test
  test_result <- car::leveneTest(formula, data = df_w_cleaned)
  
  # Extract F value and p-value (first row of results)
  tibble(outcome = y,
         F_value = test_result[1, "F value"],
         p_value = test_result[1, "Pr(>F)"],
         significant = p_value < 0.05,
         interpretation = ifelse(p_value < 0.05, 
                                 "Unequal variances", 
                                 "Equal variances"))
})

# Create nice table with formatting
levene_table_all <- levene_results_all %>%
  mutate(F_value = round(F_value, 3),
         p_value = round(p_value, 4),
         p_value_formatted = case_when(
           p_value < 0.001 ~ paste0(format(p_value, scientific = FALSE), "***"),
           p_value < 0.01 ~ paste0(format(p_value, scientific = FALSE), "**"),
           p_value < 0.05 ~ paste0(format(p_value, scientific = FALSE), "*"),
           TRUE ~ as.character(p_value)))

# Print the formatted table
kable(levene_table_all %>%
        select(outcome, F_value, p_value, interpretation),
      caption = "Levene's Test for homogeneity of variance across centres (all IDs)",
      col.names = c("Outcome", "F Value", "p-value", "Interpretation"))

#### --- Tab. A1, levene's test: matched IDs -----------------------------------
# Run Levene's test on each outcome and collect results
levene_results_matched <- map_df(outcomes, function(y) {
  # Create formula
  formula <- as.formula(paste(y, "~ centre"))
  
  # Run Levene's test
  test_result <- car::leveneTest(formula, data = matched_data)
  
  # Extract F value and p-value (first row of results)
  tibble(outcome = y,
         F_value = test_result[1, "F value"],
         p_value = test_result[1, "Pr(>F)"],
         significant = p_value < 0.05,
         interpretation = ifelse(p_value < 0.05, 
                                 "Unequal variances", 
                                 "Equal variances"))
})

# Create nice table with formatting
levene_table_matched <- levene_results_matched %>%
  mutate(
    F_value = round(F_value, 3),
    p_value = round(p_value, 4),
    p_value_formatted = case_when(
      p_value < 0.001 ~ paste0(format(p_value, scientific = FALSE), "***"),
      p_value < 0.01 ~ paste0(format(p_value, scientific = FALSE), "**"),
      p_value < 0.05 ~ paste0(format(p_value, scientific = FALSE), "*"),
      TRUE ~ as.character(p_value)
    )
  )

# Print the formatted table
kableExtra::kable(levene_table_matched %>% 
                    select(outcome, F_value, p_value, interpretation),
                  caption = "Levene's Test for homogeneity of variance across centres (matched IDs)",
                  col.names = c("Outcome", "F Value", "p-value", "Interpretation"))

# --- Save CSV file ---
readr::write_excel_csv(levene_table_matched,
                       file.path(fileDir, "output",
                                 paste0("centre_effect_LevenesTest_", format(Sys.Date(), "%d-%m-%Y"), ".csv")))

### --- Center effect GLM ------------------------------------------------------

# --- Examples: ---
# linear model to test for center effect 
# tin_center_pta <- lm(tin_snr_ERB_2000 ~ centre + PTA4_BE, data = df_w)
tin_center_pta_age <- lm(tin_snr_ERB_2000 ~ centre + PTA4_BE + age, data = df_w)
tin_center_pta_age_matched <- lm(tin_snr_ERB_2000 ~ centre + PTA4_BE + age, data = matched_data)
summary(tin_center_pta_age_matched)

ls_cu_15_center_pta_age <- lm(ls_cu_15_BE ~ centre + PTA4_BE + age, data = df_w)
summary(ls_cu_15_center_pta_age)

##### --- All IDs --------------------------------------------------------------
results_all <- map_df(outcomes, function(y) {
  formula <- as.formula(paste(y, "~ centre + PTA4_BE + age"))
  model <- lm(formula, data = df_w_cleaned)
  
  # Extract key results
  coefs <- broom::tidy(model)
  fit <- broom::glance(model)
  
  # Return the center effect row with model stats
  centre_row <- coefs %>% 
    filter(term == "centreFR") %>%
    mutate(outcome = y,
           r.squared = fit$r.squared,
           adj.r.squared = fit$adj.r.squared,
           model_p = fit$p.value)
  
  return(centre_row)
})

# Create nice table
kable(results_all %>%
        select(outcome, estimate, std.error, p.value, r.squared, adj.r.squared) %>%
        mutate(across(where(is.numeric), round, 3)),
      caption = "Centre effect across PRESAGE outcomes - all IDs")

##### --- Matched IDs ----------------------------------------------------------
# repeat for matched participants 
results_matched <- map_df(outcomes, function(y) {
  formula <- as.formula(paste(y, "~ centre + PTA4_BE + age"))
  model <- lm(formula, data = matched_data)
  
  # Extract key results
  coefs <- broom::tidy(model)
  fit <- broom::glance(model)
  
  # Return the center effect row with model stats
  centre_row <- coefs %>% 
    filter(term == "centreFR") %>%
    mutate(outcome = y,
           r.squared = fit$r.squared,
           adj.r.squared = fit$adj.r.squared,
           model_p = fit$p.value)
  
  return(centre_row)
})

# Create nice table
kableExtra::kable(results_matched %>% 
                    select(outcome, estimate, std.error, p.value, r.squared, adj.r.squared) %>%
                    mutate(across(where(is.numeric), round, 3)),
                  caption = "Centre effect across PRESAGE outcomes - matched IDs")


##### --- Tab. A1: Save centre effect table ---------------------------------------------
# simplified version:
# print_results_matched = results_matched %>% 
#   select(outcome, estimate, std.error, p.value, r.squared, adj.r.squared) %>%
#   mutate(across(where(is.numeric), round, 3))
# readr::write_excel_csv(print_results_matched,
#                        file.path(fileDir, "output",
#                                  paste0("centre_effect_", format(Sys.Date(), "%d-%m-%Y"), ".csv")))


# library(broom)
# library(readr)
# library(kableExtra)

# Helper function to compute pooled SD
pooled_sd <- function(x, g) {
  group_stats <- split(x, g) %>% 
    lapply(function(v) c(n = sum(!is.na(v)), sd = sd(v, na.rm = TRUE)))
  n1 <- group_stats[[1]]["n"]; n2 <- group_stats[[2]]["n"]
  sd1 <- group_stats[[1]]["sd"]; sd2 <- group_stats[[2]]["sd"]
  sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
}

# Main loop through all outcome variables
results_matched <- map_df(outcomes, function(y) {
  formula <- as.formula(paste(y, "~ centre + PTA4_BE + age"))
  model <- lm(formula, data = matched_data)
  
  coefs <- broom::tidy(model)
  fit <- broom::glance(model)
  
  # Prepare data for this outcome only (non-missing values)
  df <- matched_data %>% select(centre, !!sym(y)) %>% filter(!is.na(.data[[y]]))
  n_de <- sum(df$centre == "DE")
  n_fr <- sum(df$centre == "FR")
  
  # Calculate group means and pooled SD
  mean_de <- mean(df[[y]][df$centre == "DE"], na.rm = TRUE)
  mean_fr <- mean(df[[y]][df$centre == "FR"], na.rm = TRUE)
  sd_pooled <- pooled_sd(df[[y]], df$centre)
  
  # Calculate Cohen's d (consistent with β: FR relative to DE)
  d <- (mean_fr - mean_de) / sd_pooled
  
  # Interpret d
  d_interpretation <- case_when(
    abs(d) < 0.2 ~ "negligible",
    abs(d) < 0.5 ~ "small",
    abs(d) < 0.8 ~ "medium",
    TRUE ~ "large")
  
  # Extract and return centre effect row
  centre_row <- coefs %>%
    filter(term == "centreFR") %>%
    mutate(outcome = y,
           r.squared = fit$r.squared,
           adj.r.squared = fit$adj.r.squared,
           model_p = fit$p.value,
           n_DE = n_de,
           n_FR = n_fr,
           cohens_d = d,
           d_interpretation = d_interpretation)
  
  return(centre_row)
})

# Format and print result table
print_results_matched <- results_matched %>%
  select(outcome, n_DE, n_FR, estimate, std.error, cohens_d, d_interpretation, p.value, r.squared, adj.r.squared) %>%
  mutate(across(where(is.numeric), round, 3))

kableExtra::kable(print_results_matched,
  caption = "Centre effect across PRESAGE outcomes – matched IDs",
  booktabs = TRUE)

# --- Export to CSV ---
readr::write_excel_csv(print_results_matched,
                       file.path(fileDir, "output", 
                                 paste0("centre_effect_", format(Sys.Date(), "%d-%m-%Y"), ".csv")))
