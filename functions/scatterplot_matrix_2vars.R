scatterplot_matrix_2vars <- function(df, param_x, param_y1, param_y2,
                                     groupBy, groupLevel,
                                     statsLocation_x = "left", statsLocation_y = "top",
                                     corrMethod = "spearman",
                                     labelSpacing = 0.07,
                                     xlim = NULL, ylim = NULL, show_text = 1) {
  
  # Pivot to long format using id (and set condition levels explicitly)
  df_long <- df %>%
    select(id, all_of(c(param_x, param_y1, param_y2, groupBy, "age_group"))) %>%
    pivot_longer(cols = c(all_of(param_y1), all_of(param_y2)),
                 names_to = "condition", values_to = "srt") %>%
    mutate(
      condition = factor(condition, levels = c(param_y1, param_y2))
    ) %>%
    filter(.data[[groupBy]] == groupLevel) %>%  # filter for selected group level
    drop_na(.data[[param_x]], srt, age_group)
  
  # Correlation method formatting
  method_name <- tools::toTitleCase(corrMethod)
  r_symbol <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  
  # Correlation labels with clean condition names per condition by method:
  cor_labels <- df_long %>%
    group_by(condition) %>%
    summarise(cor = suppressWarnings(cor(.data[[param_x]], srt, method = corrMethod, use = "complete.obs")),
              p = suppressWarnings(cor.test(.data[[param_x]], srt, method = corrMethod)$p.value),
              n = sum(!is.na(.data[[param_x]]) & !is.na(srt)),
              .groups = "drop") %>%
    mutate(condition_clean = case_when(
      grepl("ssn", condition, ignore.case = TRUE) ~ "Stationary",
      grepl("icra5", condition, ignore.case = TRUE) ~ "Fluctuating",
      TRUE ~ as.character(condition)),
      label = paste0(condition_clean, ": n = ", n,
                     ", ", r_symbol, " = ", round(cor, 2),
                     ", p = ", signif(p, 2)))
  
  # Colour map for age group
  age_colors <- c("40–49" = "#E69F00", "50–59" = "#56B4E9", "60–69" = "#009E73",
                  "70–79" = "#CC79A7", "80–89" = "#F0E442")
  age_labels_colored <- paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels_colored) <- names(age_colors)
  
  # Line types and shapes per condition
  condition_shapes <- setNames(c(21, 22), c(param_y1, param_y2))
  condition_linetypes <- setNames(c("solid", "dashed"), c(param_y1, param_y2))
  condition_labels <- setNames(c("Stationary", "Fluctuating"), c(param_y1, param_y2))
  
  # Stroke: if DE = 0.8, if FR = 0
  shape_stroke <- if (groupLevel == "DE") 0.8 else 0
  
  # Plot
  p <- ggplot(df_long, aes(x = .data[[param_x]], y = srt)) +
    geom_point(aes(shape = condition, fill = age_group),
               colour = "black", size = 5, stroke = shape_stroke, na.rm = TRUE) +
    geom_smooth(aes(linetype = condition),
                method = "lm", se = FALSE, colour = "black", linewidth = 1.3) +
    scale_shape_manual(values = condition_shapes, labels = condition_labels, name = "Noise type") +
    scale_linetype_manual(values = condition_linetypes, labels = condition_labels, name = "Noise type") +
    scale_fill_manual(values = age_colors, labels = age_labels_colored, name = "Age group (years)") +
    guides(shape = guide_legend(title = "Noise type", override.aes = list(fill = "white"), order = 1),
           linetype = guide_legend(title = "Noise type", order = 1),
           fill = guide_legend(title = "Age group (years)",
                               override.aes = list(shape = NA, colour = NA,
                                                   fill = NA, size = 0),order = 2)) +
    theme_bw() +
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
  
  # Position correlation labels
  npc_to_numeric <- function(pos, axis = "x") {
    switch(pos,"left" = if (axis == "x") 0.01 else NULL, 
           "center" = 0.5,
           "right" = if (axis == "x") 0.98 else NULL,
           "bottom" = if (axis == "y") 0.1 else NULL,
           "top" = if (axis == "y") 0.98 else NULL,
           0.5)
  }
  
  x_npc <- npc_to_numeric(statsLocation_x, "x")
  y_npc <- npc_to_numeric(statsLocation_y, "y")
  
  for (i in seq_len(nrow(cor_labels))) {
    y_npc_i <- y_npc - (i - 1) * labelSpacing
    p <- p + annotation_custom(
      grob = textGrob(label = cor_labels$label[i],
                      x = unit(x_npc, "npc"), y = unit(y_npc_i, "npc"),
                      hjust = 0, vjust = 1,
                      gp = gpar(col = "black", fontsize = 16, fontface = "plain")))
  }
  
  # Add centre label (if show_text == 1)
  if (show_text == 1) {
    groupLevel_paper = ifelse(groupLevel=="DE", "UOL", "IDA")
    p <- p + annotation_custom(
      grob = textGrob(label = groupLevel_paper,
                      x = unit(0.98, "npc"), y = unit(0.98, "npc"),
                      hjust = 1, vjust = 1,
                      gp = gpar(col = "black", fontsize = 18, fontface = "bold")))
  }
  
  return(p)
}
