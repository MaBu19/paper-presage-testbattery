# acalos plot with two y params & stroke for first level in groupBy (second level without a stroke):
scatterplot_acalos_2vars <- function(df, param_x, param_y1, param_y2,
                                     groupBy,
                                     statsLocation_x = "left", statsLocation_y = "top",
                                     corrMethod = "spearman",
                                     labelSpacing = 0.07,
                                     xlim = NULL, ylim = NULL,
                                     show_legend = 1) {
  
  # Pivot to long format using id (set condition levels explicitly)
  df_long <- df %>%
    select(id, all_of(c(param_x, param_y1, param_y2, groupBy, "age_group"))) %>%
    pivot_longer(cols = c(all_of(param_y1), all_of(param_y2)),
                 names_to = "condition", values_to = "srt") %>%
    mutate(
      condition = factor(condition, levels = c(param_y1, param_y2)),
      stroke_group = .data[[groupBy]] == levels(factor(df[[groupBy]]))[1]) %>%
    drop_na(.data[[param_x]], srt, age_group)
  
  # Correlation labels
  method_name <- tools::toTitleCase(corrMethod)
  r_symbol <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  
  cor_labels <- df_long %>%
    group_by(condition) %>%
    summarise(cor = cor(.data[[param_x]], srt, method = corrMethod, use = "complete.obs"),
              p = suppressWarnings(cor.test(.data[[param_x]], srt, method = corrMethod)$p.value),
              n_total = sum(!is.na(.data[[param_x]]) & !is.na(srt)),
              n_de = sum(!is.na(.data[[groupBy]]) & .data[[groupBy]] == "DE" & !is.na(.data[[param_x]]) & !is.na(srt)),
              .groups = "drop") %>%
    mutate(condition_label = case_when(grepl("_cu_15", condition) ~ "L15",
                                       grepl("_cu_35", condition) ~ "L35",
                                       TRUE ~ as.character(condition)),
           label = paste0(condition_label, ": ", "n = ", n_total, 
                          " (UOL: ", n_de, ")",
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
  condition_labels <- setNames(c("L15", "L35"), c(param_y1, param_y2))
  
  # Base plot
  p <- ggplot(df_long, aes(x = .data[[param_x]], y = srt)) +
    geom_point(aes(shape = condition, fill = age_group),
               colour = "black", size = 5,
               stroke = ifelse(df_long$stroke_group, 0.8, 0),  # Frame only for first group level
               na.rm = TRUE, show.legend = (show_legend == 1)) +
    geom_smooth(aes(linetype = condition),
                method = "lm", se = FALSE, colour = "black", linewidth = 1.3,
                show.legend = (show_legend == 1)) +
    scale_shape_manual(values = condition_shapes, labels = condition_labels, name = "Level") +
    scale_linetype_manual(values = condition_linetypes, labels = condition_labels, name = "Level") +
    scale_fill_manual(
      values = age_colors,
      labels = age_labels_colored,
      name = "Age group (years)")
  
  # Apply guides only if legend is ON
  if (show_legend == 1) {
    p <- p + guides(
      shape = guide_legend(title = "Level", override.aes = list(fill = "white"), order = 1),
      linetype = guide_legend(title = "Level", order = 1),
      fill = guide_legend(title = "Age group (years)",
                          override.aes = list(shape = NA, colour = NA, fill = NA, size = 0),
                          order = 2))
  } else {
    p <- p + theme(legend.position = "none")  # Remove all legends
  }
  
  # Final styling
  p <- p +
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
    switch(pos,
           "left" = if (axis == "x") 0.01 else NULL,
           "center" = 0.5,
           "right" = if (axis == "x") 0.98 else NULL,
           "bottom" = if (axis == "y") 0.15 else NULL,
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
                      gp = gpar(col = "black", fontsize = 16, fontface = "plain"))
    )
  }
  
  return(p)
}
