scatterplot_ct_combined <- function(df, param_x, param_y,
                                    statsLocation_x = "left", statsLocation_y = "top",
                                    corrMethod = "spearman", groupBy,
                                    labelSpacing = 0.07,
                                    xlim = NULL, ylim = NULL,
                                    hline = NULL,
                                    show_legend = 1, show_text = 1) {
  
  # --- Clean data ---
  df_clean <- df %>%
    filter(!is.na(.data[[param_x]]), !is.na(.data[[param_y]]),
           !is.na(.data[[groupBy]]), !is.na(age_group)) %>%
    mutate(
      stroke_group = .data[[groupBy]] == levels(factor(df[[groupBy]]))[1],
      age_group = factor(age_group, levels = c("40–49","50–59","60–69","70–79","80–89"))
    )
  
  # --- Exclude monothermal cases ---
  df_stats <- df_clean %>%
    filter(is.na(ct_monothermal) | ct_monothermal == 0)
  
  # --- Correlation label ---
  cor_test    <- suppressWarnings(cor.test(df_stats[[param_x]], df_stats[[param_y]], method = corrMethod))
  method_name <- tools::toTitleCase(corrMethod)
  r_symbol    <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  n_samples   <- nrow(df_stats)
  n_de        <- sum(df_stats[[groupBy]] == "DE", na.rm = TRUE)
  cor_label   <- paste0("n = ", n_samples,
                        " (UOL: ", n_de, ")",
                        ", ", r_symbol, " = ", round(cor_test$estimate, 2),
                        ", p = ", signif(cor_test$p.value, 2))
  
  # --- Condition label ---
  CondCode_label <- case_when(
    grepl("warm", param_y, TRUE) ~ "Warm irrigation",
    grepl("cold", param_y, TRUE) ~ "Cold irrigation",
    grepl("uw",   param_y, TRUE) ~ "Unilateral weakness (UW)",
    grepl("dp",   param_y, TRUE) ~ "Directional preponderance (DP)",
    TRUE ~ "Unknown")
  
  # --- Age colours ---
  age_colors <- c("40–49"="#E69F00","50–59"="#56B4E9","60–69"="#009E73",
                  "70–79"="#CC79A7","80–89"="#F0E442")
  age_labels_colored <- paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels_colored) <- names(age_colors)
  
  # --- Plot ---
  p <- ggplot(df_clean, aes(x = .data[[param_x]], y = .data[[param_y]])) 
  
  # Add hlines first so they go to the back
  if (!is.null(hline)) {
    if (grepl("uw|dp", param_y, ignore.case = TRUE)) {
      p <- p + 
        geom_hline(yintercept = c(-hline, hline), linetype = "dashed",
                   colour = "black", linewidth = 0.8, show.legend = FALSE)
    } else {
      p <- p + 
        geom_hline(yintercept = hline, linetype = "dashed",
                   colour = "black", linewidth = 0.8, show.legend = FALSE)
    }
  }
  
  # place 'M' in the middle
  p <- p + geom_point(aes(fill = age_group,
                          alpha = ifelse(ct_monothermal == 1, 1, 1)),
                      shape = 21, colour = "black", size = 5,
                      stroke = ifelse(df_clean$stroke_group, 0.8, 0),
                      na.rm = TRUE, show.legend = (show_legend == 1)) +
    scale_alpha_identity() +
    geom_text(data = subset(df_clean, ct_monothermal == 1),
              aes(label = "M"), vjust = 0.5, hjust = 0.5, size = 3,
              fontface = "bold", colour = "black")
  
  
  # Regression line (only bithermal)
  p <- p + geom_smooth(data = df_stats,
                       aes(x = .data[[param_x]], y = .data[[param_y]]),
                       method = "lm", se = FALSE, colour = "black",
                       linewidth = 1.3, na.rm = TRUE)
  
  # Legend
  p <- p + scale_fill_manual(values = age_colors, labels = age_labels_colored,
                             name = "Age group (years)", drop = FALSE)
  
  if (show_legend == 1) {
    p <- p + guides(fill = guide_legend(
      title = "Age group (years)",
      override.aes = list(shape = 15, size = 0.001, fill = NA, colour = NA),
      order = 1))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # --- Styling ---
  p <- p + theme_bw() +
    theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 13, face = "bold", margin = margin(b = 6)),
          legend.text = ggtext::element_markdown(size = 13, face = "bold"),
          legend.box = "vertical",
          legend.position = "inside",
          legend.justification = c("right","bottom"),
          legend.position.inside = c(0.95, 0.05),
          legend.direction = "vertical",
          legend.spacing.y = unit(0.1, "cm"),
          legend.key = element_rect(fill = NA, colour = NA),
          legend.key.height = unit(0.8, "cm"),
          legend.key.width  = unit(1.2, "cm"),
          legend.box.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
          legend.background = element_blank()) +
    xlab("") + ylab("")
  
  if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + scale_y_continuous(limits = ylim)
  
  # Correlation text
  x_npc <- ifelse(statsLocation_x == "left", 0.01, ifelse(statsLocation_x == "right", 0.98, 0.5))
  y_npc <- ifelse(statsLocation_y == "top", 0.98, ifelse(statsLocation_y == "bottom", 0.1, 0.5))
  p <- p + annotation_custom(
    grob = textGrob(label = cor_label,
                    x = unit(x_npc, "npc"), y = unit(y_npc, "npc"),
                    hjust = 0, vjust = 1,
                    gp = gpar(col = "black", fontsize = 16, fontface = "plain")))
  
  if (show_text == 1) {
    p <- p + annotation_custom(
      grob = textGrob(label = CondCode_label,
                      x = unit(0.98, "npc"), y = unit(0.92, "npc"),
                      hjust = 1, vjust = 1,
                      gp = gpar(col = "black", fontsize = 15, fontface = "bold")))
  }
  
  p
}