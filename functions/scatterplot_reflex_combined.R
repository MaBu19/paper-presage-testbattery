scatterplot_reflex_combined <- function(df, param_x, param_y,
                                        statsLocation_x = "left", statsLocation_y = "top",
                                        corrMethod = "spearman", groupBy,
                                        labelSpacing = 0.07,
                                        xlim = NULL, ylim = NULL,
                                        absent_lim = 102,
                                        show_legend = 1) {
  
  # ---- add absent-count ------------------------------------------------------
  df_aug <- df %>%
    mutate(reflex_absent_count = rowSums(select(.,
                                                reflex_500_absent_BE,
                                                reflex_1k_absent_BE,
                                                reflex_2k_absent_BE),
                                         na.rm = TRUE))
  
  # ---- filter/clean ----------------------------------------------------------
  df_clean <- df_aug %>%
    filter(!is.na(.data[[param_x]]),
           !is.na(.data[[param_y]]),
           !is.na(.data[[groupBy]]),
           !is.na(age_group)) %>%
    mutate(stroke_group = .data[[groupBy]] == levels(factor(.data[[groupBy]]))[1])
  
  # ---- correlation label -----------------------------------------------------
  cor_test <- suppressWarnings(cor.test(df_clean[[param_x]], df_clean[[param_y]], method = corrMethod))
  r_symbol <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  n_samples <- nrow(df_clean)
  n_de <- sum(df_clean[[groupBy]] == "DE", na.rm = TRUE)
  cor_label <- paste0("n = ", n_samples, " (UOL: ", n_de, ")",
                      ", ", r_symbol, " = ", round(cor_test$estimate, 2),
                      ", p = ", signif(cor_test$p.value, 2))
  
  # ---- colours & legend labels ----------------------------------------------
  age_colors <- c("40–49"="#E69F00","50–59"="#56B4E9","60–69"="#009E73","70–79"="#CC79A7","80–89"="#F0E442")
  not_trig_fill <- "#B0B0B0"
  
  age_labels_colored <- paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels_colored) <- names(age_colors)
  
  fill_values <- c(age_colors, "Not triggered" = not_trig_fill)
  fill_breaks <- c(names(age_colors), "Not triggered")
  fill_labels <- c(age_labels_colored,
                   "Not triggered" = "<span style='color:#808080;'>Not triggered</span>")
  
  # ---- absent/not-triggered markers (all three absent) -----------------------
  absent_points <- df_aug %>%
    filter(!is.na(.data[[param_x]]), reflex_absent_count == 3) %>%
    mutate(y = absent_lim, is_de = .data[[groupBy]] == "DE")
  
  absent_de  <- filter(absent_points,  is_de)
  absent_non_de <- filter(absent_points, !is_de)
  
  # ---- base plot -------------------------------------------------------------
  p <- ggplot(df_clean, aes(x = .data[[param_x]], y = .data[[param_y]])) +
    # main measured points (no alpha mapping)
    geom_point(aes(fill = age_group),
               shape = 21, size = 5,
               stroke = ifelse(df_clean$stroke_group, 0.8, 0),
               colour = "black",
               na.rm = TRUE,
               show.legend = (show_legend == 1)) +
    geom_smooth(method = "lm", se = FALSE,
                colour = "black", linewidth = 1.3, linetype = "solid")
  
  # ---- absent/not-triggered markers (stroke only for DE) ---------------------
  if (nrow(absent_de) > 0) {
    p <- p + geom_point(data = absent_de,
                        aes(x = .data[[param_x]], y = y, fill = "Not triggered"),
                        shape = 21, size = 5, stroke = 0.8, colour = "black",
                        inherit.aes = FALSE,
                        show.legend = TRUE)
  }
  if (nrow(absent_non_de) > 0) {
    p <- p + geom_point(data = absent_non_de,
                        aes(x = .data[[param_x]], y = y, fill = "Not triggered"),
                        shape = 21, size = 5, stroke = 0, colour = "black",
                        inherit.aes = FALSE,
                        show.legend = FALSE)
  }
  
  # ---- scales & guides -------------------------------------------------------
  p <- p +
    scale_fill_manual(values = fill_values, breaks = fill_breaks, labels = fill_labels,
                      name = "Age group (years)") +
    {
      if (show_legend == 1) guides(fill = guide_legend(title = "Age group (years)",
                                                       override.aes = list(shape = NA, size = 0, fill = NA, colour = NA),
                                                       order = 1))
      else theme(legend.position = "none")
    }
  
  # ---- axes ------------------------------------------------------------------
  if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
  if (!is.null(ylim)) p <- p + scale_y_continuous(limits = ylim)
  
  # ---- theme -----------------------------------------------------------------
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
          legend.key = element_rect(fill = NA, colour = NA),
          legend.box.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
          legend.background = element_blank()) +
    xlab("") + ylab("")
  
  # ---- correlation label placement ------------------------------------------
  x_npc <- ifelse(statsLocation_x == "left", 0.01, ifelse(statsLocation_x == "right", 0.98, 0.5))
  y_npc <- ifelse(statsLocation_y == "top", 0.98, ifelse(statsLocation_y == "bottom", 0.1, 0.5))
  
  p <- p + annotation_custom(
    grob = textGrob(label = cor_label, x = unit(x_npc, "npc"), y = unit(y_npc, "npc"),
                    hjust = 0, vjust = 1, gp = gpar(col = "black", fontsize = 16, fontface = "plain")))
  
  return(p)
}
