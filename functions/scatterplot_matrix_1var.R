scatterplot_matrix_1var <- function(df, param_x, param_y,
                                    groupBy, groupLevel,
                                    statsLocation_x = "left", statsLocation_y = "top",
                                    corrMethod = "spearman",
                                    labelSpacing = 0.07,
                                    xlim = NULL, ylim = NULL, show_text = 1) {
  
  # --- prepare data ---
  df_long <- df %>%
    select(id, centre, all_of(c(param_x, param_y, groupBy, "age_group"))) %>%
    filter(.data[[groupBy]] == groupLevel) %>%
    mutate(stroke = ifelse(centre == "DE", 0.8, 0),
           srt    = .data[[param_y]]) %>%
    drop_na(.data[[param_x]], srt, age_group)
  
  # --- correlation info ---
  r_symbol  <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  cor_test  <- suppressWarnings(cor.test(df_long[[param_x]], df_long$srt, method = corrMethod))
  n_total   <- sum(!is.na(df_long[[param_x]]) & !is.na(df_long$srt))
  n_de      <- sum(!is.na(df_long[[param_x]]) & !is.na(df_long$srt) & df_long$centre == "DE")
  
  cor_label <- paste0(
    "n = ", n_total, " (UOL: ", n_de, ")",
    ", ", r_symbol, " = ", round(cor_test$estimate, 2),
    ", p = ", signif(cor_test$p.value, 2))
  
  cor_labels <- data.frame(label = cor_label, stringsAsFactors = FALSE)
  
  # --- colors for age group ---
  age_colors <- c("40–49" = "#E69F00",
                  "50–59" = "#56B4E9",
                  "60–69" = "#009E73",
                  "70–79" = "#CC79A7",
                  "80–89" = "#F0E442")
  
  age_labels_colored <- paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels_colored) <- names(age_colors)
  
  # --- plot ---
  p <- ggplot(df_long, aes(x = .data[[param_x]], y = srt)) +
    geom_point(aes(fill = age_group, stroke = stroke),
               shape = 21, colour = "black", size = 5,
               na.rm = TRUE) +
    geom_smooth(method = "lm", se = FALSE,
                colour = "black", linewidth = 1.3) +
    scale_fill_manual(values = age_colors,
                      labels = age_labels_colored,
                      name   = "Age group (years)") +
    guides(fill = guide_legend(
      title = "Age group (years)",
      override.aes = list(shape = NA),
      order = 1)) +
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
  
  # --- axis limits ---
  if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + scale_y_continuous(limits = ylim)
  
  # --- helper for correlation label position ---
  npc_to_numeric <- function(pos, axis = "x") {
    switch(
      pos,
      "left"   = if (axis == "x") 0.01 else NULL,
      "center" = 0.5,
      "right"  = if (axis == "x") 0.98 else NULL,
      "bottom" = if (axis == "y") 0.1 else NULL,
      "top"    = if (axis == "y") 0.98 else NULL,
      0.5
    )
  }
  
  x_npc <- npc_to_numeric(statsLocation_x, "x")
  y_npc <- npc_to_numeric(statsLocation_y, "y")
  
  # correlation annotation
  for (i in seq_len(nrow(cor_labels))) {
    y_npc_i <- y_npc - (i - 1) * labelSpacing
    p <- p + annotation_custom(
      grob = textGrob(label = cor_labels$label[i],
                      x = unit(x_npc, "npc"),
                      y = unit(y_npc_i, "npc"),
                      hjust = 0, vjust = 1,
                      gp = gpar(col = "black", fontsize = 16, fontface = "plain")))
  }
  
  # --- noise-type label instead of centre label ---
  if (show_text == 1) {
    noise_label <- if (grepl("_ssn", param_y, ignore.case = TRUE)) "SSN" else "ICRA5-250"
    
    p <- p + annotation_custom(
      grob = textGrob(label = noise_label,
                      x = unit(0.98, "npc"), y = unit(0.93, "npc"),
                      hjust = 1, vjust = 1,
                      gp = gpar(col = "black", fontsize = 18, fontface = "bold")))
  }
  
  return(p)
}