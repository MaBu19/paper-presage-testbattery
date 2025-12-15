# acalos plot with one y param & stroke for reference groupBy level (no stroke for second level):

scatterplot_acalos_combined <- function(df, param_x, param_y,
                                        statsLocation_x = "left", statsLocation_y = "top",
                                        corrMethod = "spearman", groupBy,
                                        labelSpacing = 0.07,
                                        xlim = NULL, ylim = NULL,
                                        show_legend = 1, show_text = 1) {
  # Clean and filter data
  df_clean <- df[!is.na(df[[param_x]]) & !is.na(df[[param_y]]) &
                   !is.na(df[[groupBy]]) & !is.na(df$age_group), ]
  
  # Identify the first level of groupBy
  group_levels <- levels(factor(df_clean[[groupBy]]))
  first_level <- group_levels[1]
  
  # Add stroke indicator (TRUE for first level, FALSE for second level)
  df_clean$stroke_group <- df_clean[[groupBy]] == first_level
  
  # Determine complete cases
  complete_cases <- complete.cases(df_clean[[param_x]], df_clean[[param_y]])
  
  # Correlation (only uses complete cases)
  cor_result <- suppressWarnings(cor.test(df_clean[[param_x]], df_clean[[param_y]], method = corrMethod))
  
  # Prepare correlation label
  method_name <- tools::toTitleCase(corrMethod)
  r_symbol <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  
  # Corrected sample sizes
  n_total <- sum(complete_cases)
  n_de <- sum(df_clean[[groupBy]] == "DE" & complete_cases, na.rm = TRUE)
  
  # Correlation label with sample info
  cor_label <- paste0("n = ", n_total, " (UOL: ", n_de, ")",
                      ", ", r_symbol, " = ", round(cor_result$estimate, 2),
                      ", p = ", signif(cor_result$p.value, 2)) 
  
  # Define condition label
  condition_label <- case_when(grepl("diff_35_15", param_y, ignore.case = TRUE) ~ "DR",
                               grepl("bin_summation", param_y, ignore.case = TRUE) ~ "BLS",
                               TRUE ~ "Unknown")
  
  # Colour map for age groups
  age_colors <- c("40–49" = "#E69F00", "50–59" = "#56B4E9", "60–69" = "#009E73",
                  "70–79" = "#CC79A7", "80–89" = "#F0E442")
  age_labels_colored <- paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels_colored) <- names(age_colors)
  
  # Plot
  p <- ggplot(df_clean, aes(x = .data[[param_x]], y = .data[[param_y]])) +
    geom_point(aes(fill = age_group),
               shape = 21, size = 5,
               stroke = ifelse(df_clean$stroke_group, 0.8, 0),
               colour = "black", na.rm = TRUE, show.legend = (show_legend == 1)) +
    geom_smooth(method = "lm", se = FALSE,
                colour = "black", linewidth = 1.3, linetype = "solid",
                show.legend = FALSE) +
    scale_fill_manual(values = age_colors,
                      labels = age_labels_colored, name = "Age group (years)")
  
  # Guides: coloured text only, no symbol
  if (show_legend == 1) {
    p <- p + guides(
      fill = guide_legend(title = "Age group (years)",
                          override.aes = list(shape = NA, size = 0, fill = NA, colour = NA), order = 1))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # Styling
  p <- p + theme_bw() +
    theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 13, face = "bold", margin = margin(b = 6)),
          legend.text = ggtext::element_markdown(size = 13, face = "bold"),
          legend.text.align = 0.5,
          legend.box = "vertical",
          legend.position = "inside",
          legend.justification = c("right", "top"),
          legend.position.inside = c(0.95, 0.6),
          legend.direction = "vertical",
          legend.spacing.y = unit(0.1, "cm"),
          legend.key = element_rect(fill = NA, colour = NA),
          legend.key.height = unit(0.8, "cm"),
          legend.key.width = unit(1.2, "cm"),
          legend.box.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
          legend.background = element_blank()) +
    xlab("") + ylab("")
  
  # Axis limits
  if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + scale_y_continuous(limits = ylim)
  
  # Add correlation label
  x_npc <- if (statsLocation_x == "left") 0.01 else if (statsLocation_x == "right") 0.98 else 0.5
  y_npc <- if (statsLocation_y == "top") 0.98 else if (statsLocation_y == "bottom") 0.1 else 0.5
  
  p <- p + annotation_custom(
    grob = textGrob(label = cor_label,
                    x = unit(x_npc, "npc"), y = unit(y_npc, "npc"),
                    hjust = 0, vjust = 1,
                    gp = gpar(col = "black", fontsize = 16, fontface = "plain")))
  
  # Add condition label (if show_text == 1)
  if (show_text == 1) {
    p <- p + annotation_custom(
      grob = textGrob(label = condition_label,
                      x = unit(0.98, "npc"), y = unit(0.98, "npc"),
                      hjust = 1, vjust = 1,
                      gp = gpar(col = "black", fontsize = 18, fontface = "bold"))
    )
  }
  
  return(p)
}
