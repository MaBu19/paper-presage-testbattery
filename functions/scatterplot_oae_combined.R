scatterplot_oae_combined <- function(df, param_x, param_y,
                                     absent_col,
                                     statsLocation_x = "left", statsLocation_y = "top",
                                     corrMethod = "spearman", groupBy,
                                     level_param = NULL,
                                     labelSpacing = 0.07,
                                     xlim = NULL,
                                     y_limits = NULL, y_breaks = NULL,
                                     show_legend = 1,
                                     absent_lim = 25,
                                     not_measurable_df = NULL) {
  
  # --- data ---
  df_clean <- df %>%
    filter(!is.na(.data[[param_x]]),
           !is.na(.data[[param_y]]),
           !is.na(.data[[groupBy]]),
           !is.na(age_group)) %>%
    mutate(stroke_group = .data[[groupBy]] == levels(factor(.data[[groupBy]]))[1])
  
  # --- correlation + n (only present responses, absent_col == 1) ---
  df_corr <- df_clean %>%
    filter(.data[[absent_col]] == 1 & !is.na(.data[[absent_col]]))
  
  cor_result <- suppressWarnings(cor.test(df_corr[[param_x]], df_corr[[param_y]], method = corrMethod))
  r_symbol    <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  n_total     <- nrow(df_corr)   # n only for present responses
  n_de        <- sum(df_corr[[groupBy]] == "DE", na.rm = TRUE)
  cor_label   <- paste0("n = ", n_total, " (UOL: ", n_de, ")",
                        ", ", r_symbol, " = ", round(cor_result$estimate, 2),
                        ", p = ", signif(cor_result$p.value, 2))
  
  # --- frequency label ---
  freq_label <- gsub(".*snr_(\\d+)k.*", "\\1 kHz", param_y)
  
  # --- legend colours & labels ---
  age_colors <- c("40–49"="#E69F00","50–59"="#56B4E9","60–69"="#009E73",
                  "70–79"="#CC79A7","80–89"="#F0E442")
  age_labels_colored <- paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels_colored) <- names(age_colors)
  
  # --- not-measurable subset ---
  show_nm <- FALSE
  not_measurable_sub <- NULL
  if (!is.null(not_measurable_df) && nrow(not_measurable_df) > 0) {
    not_measurable_sub <- not_measurable_df %>%
      filter(condition == param_y, !is.na(.data[[param_x]])) %>%
      mutate(stroke_nm = ifelse(centre == "DE", 0.8, 0), y_pseudo = absent_lim)
    show_nm <- nrow(not_measurable_sub) > 0
  }
  
  # --- shape legend (for DPOAEs, skip if TEOAE) ---
  show_shape_legend <- FALSE
  shape_values <- NULL
  shape_labels <- NULL
  if (!is.null(level_param) && !grepl("teoae_", param_y, ignore.case = TRUE)) {
    df_clean[[level_param]] <- factor(df_clean[[level_param]])
    shape_levels <- levels(df_clean[[level_param]])
    level_counts <- table(df_clean[[level_param]])
    shape_labels <- paste0(shape_levels, " (n = ", level_counts[shape_levels], ")")
    names(shape_labels) <- shape_levels
    shape_values <- setNames(c(21,22,23)[seq_along(shape_levels)], shape_levels)
    show_shape_legend <- length(shape_levels) > 0
  }
  
  # --- base plot ---
  p <- ggplot(df_clean, aes(x = .data[[param_x]], y = .data[[param_y]]))
  
  if (!is.null(level_param) && !grepl("teoae_", param_y, ignore.case = TRUE)) {
    p <- p +
      geom_point(aes(shape = .data[[level_param]], fill = age_group,
                     stroke = ifelse(stroke_group, 0.8, 0)),
                 colour = "black", size = 5,
                 na.rm = TRUE, show.legend = (show_legend == 1)) +
      scale_shape_manual(name = "Target level (dB SPL)",
                         values = shape_values, labels = shape_labels)
  } else {
    p <- p +
      geom_point(aes(fill = age_group, stroke = ifelse(stroke_group, 0.8, 0)),
                 shape = 21, colour = "black", size = 5,
                 na.rm = TRUE, show.legend = (show_legend == 1))
  }
  
  # --- absent responses (x mark) ---
  p <- p + geom_point(data = df_clean[df_clean[[absent_col]] == 2, ],
                      aes(x = .data[[param_x]], y = .data[[param_y]]),
                      shape = 4, size = 3, stroke = 1.2, colour = "black",
                      inherit.aes = FALSE)
  
  # --- not measurable ---
  if (show_nm) {
    p <- p + geom_point(data = not_measurable_sub,
                        aes(x = .data[[param_x]], y = y_pseudo, fill = "Not measurable", stroke = stroke_nm),
                        shape = 21, colour = "black", size = 5,
                        inherit.aes = FALSE)
  }
  
  # --- regression line (only present responses) ---
  p <- p + geom_smooth(data = df_clean[df_clean[[absent_col]] == 1, ],
                       aes(x = .data[[param_x]], y = .data[[param_y]]),
                       method = "lm", se = FALSE, colour = "black", linewidth = 1.3,
                       inherit.aes = FALSE)
  
  # --- fill scale ---
  if (show_nm) {
    fill_values <- c(age_colors, "Not measurable" = "#B0B0B0")
    fill_breaks <- c(names(age_colors), "Not measurable")
    fill_labels <- c(age_labels_colored, "Not measurable" = "<span style='color:#808080;'>Not measurable</span>")
  } else {
    fill_values <- age_colors
    fill_breaks <- names(age_colors)
    fill_labels <- age_labels_colored
  }
  
  p <- p + scale_fill_manual(values = fill_values, breaks = fill_breaks, labels = fill_labels,
                             limits = fill_breaks, drop = FALSE, name = "Age group (years)")
  
  # --- guides ---
  if (show_legend == 1) {
    p <- p + guides(fill  = guide_legend(
      title = "Age group (years)", title.position = "top",
      override.aes = list(shape = NA, size = 0, fill = NA, colour = NA),
      order = 1),
      shape = if (show_shape_legend) guide_legend(override.aes = list(fill = "white"), order = 2) else "none")
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # --- axes ---
  if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
  if (!is.null(y_limits) && !is.null(y_breaks)) {
    full_breaks <- sort(unique(c(y_breaks, if (show_nm) absent_lim else NULL)))
    full_labels <- sapply(full_breaks, function(b) if (show_nm && b == absent_lim) "Not measurable" else b)
    p <- p + scale_y_continuous(limits = y_limits, breaks = full_breaks, labels = full_labels)
  }
  
  # --- theme ---
  p <- p + theme_bw() +
    theme(panel.grid = element_blank(),
          axis.text = element_text(size = 14, face = "bold", colour = "black"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 13, face = "bold", margin = margin(b = 6)),
          legend.text = ggtext::element_markdown(size = 13, face = "bold"),
          legend.title.align = 0.5,
          legend.box = "vertical",
          legend.spacing.y = unit(0, "pt"),
          legend.margin = margin(0,0,0,0),
          legend.box.margin= margin(0,0,0,0),
          legend.position = "inside",
          legend.justification = c("right","top"),
          legend.position.inside = c(0.95, 0.60),
          legend.key = element_rect(fill = NA, colour = NA),
          legend.box.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
          legend.background = element_blank()) +
    xlab("") + ylab("")
  
  # --- annotations ---
  x_npc <- ifelse(statsLocation_x == "left", 0.01, ifelse(statsLocation_x == "right", 0.98, 0.5))
  y_npc <- ifelse(statsLocation_y == "top", 0.98, ifelse(statsLocation_y == "bottom", 0.07, 0.5))
  
  p <- p + annotation_custom(
    grob = grid::textGrob(label = cor_label,
                          x = grid::unit(x_npc, "npc"), y = grid::unit(y_npc, "npc"),
                          hjust = 0, vjust = 1,
                          gp = grid::gpar(col = "black", fontsize = 16, fontface = "plain"))) +
    annotation_custom(
      grob = grid::textGrob(label = freq_label,
                            x = grid::unit(0.95, "npc"), y = grid::unit(0.98, "npc"),
                            hjust = 1, vjust = 1,
                            gp = grid::gpar(col = "black", fontsize = 18, fontface = "bold")))
  
  p
}