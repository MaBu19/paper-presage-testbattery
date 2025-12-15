scatterplot_vhit <- function(df, param_x, param_y,
                             statsLocation_x = "left", statsLocation_y = "top",
                             corrMethod = "spearman", groupBy,
                             labelSpacing = 0.07,
                             xlim = NULL, y_limits = NULL, y_breaks = NULL,
                             show_legend = 1, show_text = 1,
                             not_measurable_df = NULL,
                             absent_lim = NULL,
                             hline = NULL) {
  # --- data (drop NA) ---
  df_clean <- df %>%
    filter(!is.na(.data[[param_x]]),
           !is.na(.data[[param_y]]),
           !is.na(.data[[groupBy]]),
           !is.na(age_group)) %>%
    mutate(stroke_group = .data[[groupBy]] == levels(factor(.data[[groupBy]]))[1],
           age_group = factor(age_group, levels = c("40–49","50–59","60–69","70–79","80–89")))
  
  # --- correlation label (VEMP-style) ---
  cor_test <- suppressWarnings(cor.test(df_clean[[param_x]], df_clean[[param_y]], method = corrMethod))
  method_name <- tools::toTitleCase(corrMethod)
  r_symbol <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  n_samples <- nrow(df_clean)
  n_de <- sum(df_clean[[groupBy]] == "DE", na.rm = TRUE)
  cor_label <- paste0("n = ", n_samples,
                        " (UOL: ", n_de, ")",
                        ", ", r_symbol, " = ", round(cor_test$estimate, 2),
                        ", p = ", signif(cor_test$p.value, 2))
  
  # --- canal token & label ---
  canal_token <- if (grepl("later",  param_y, TRUE)) "later"
  else if      (grepl("anter",  param_y, TRUE)) "anter"
  else if      (grepl("poster", param_y, TRUE)) "poster" else NA_character_
  
  canal_label <- if (identical(canal_token,"later")) "Lateral"
  else if      (identical(canal_token,"anter")) "Anterior"
  else if      (identical(canal_token,"poster")) "Posterior" else "Unknown"
  
  # --- colours & labels ---
  age_colors <- c("40–49"="#E69F00","50–59"="#56B4E9","60–69"="#009E73","70–79"="#CC79A7","80–89"="#F0E442")
  age_labels_colored <- paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels_colored) <- names(age_colors)
  
  # --- not measurable (999) selection: exact match with canal ---
  show_nm <- FALSE
  nm_sub  <- NULL
  if (!is.null(not_measurable_df) && !is.null(absent_lim) && !is.na(canal_token)) {
    cond_needed <- paste0("vhit_gain_", canal_token, "_BE")  # later/anter/poster
    nm_sub <- not_measurable_df %>%
      filter(condition == cond_needed) %>%
      mutate(stroke_nm = ifelse(centre == "DE", 0.8, 0),
             y_pseudo  = absent_lim)
    show_nm <- nrow(nm_sub) > 0
  }
  
  # --- base plot ---
  p <- ggplot(df_clean, aes(x = .data[[param_x]], y = .data[[param_y]]))
  
  # optional reference hline (add first so it stays behind)
  if (!is.null(hline)) {
    p <- p + geom_hline(yintercept = hline, linetype = "dashed",
                        colour = "black", linewidth = 0.8, show.legend = FALSE)
  }
  
  p <- p +
    geom_point(aes(fill = age_group),
               shape = 21, colour = "black", size = 5,
               stroke = ifelse(df_clean$stroke_group, 0.8, 0),
               na.rm = TRUE, show.legend = (show_legend == 1)) +
    geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 1.3,
                na.rm = TRUE, show.legend = FALSE)
  
  # --- 999 markers (like OAE; show fill as "Not measurable") ---
  if (show_nm) {
    p <- p + geom_point(data = nm_sub,
                        aes(x = .data[[param_x]], y = y_pseudo, fill = "Not measurable", stroke = stroke_nm),
                        shape = 21, colour = "black", size = 5, inherit.aes = FALSE)
  }
  
  # --- fill scale (include "Not measurable" if present) ---
  if (show_nm) {
    fill_values <- c(age_colors, "Not measurable" = "#B0B0B0")
    fill_breaks <- c(names(age_colors), "Not measurable")
    fill_labels <- c(age_labels_colored,
                     "Not measurable" = "<span style='color:#808080;'>Not measurable</span>")
  } else {
    fill_values <- age_colors
    fill_breaks <- names(age_colors)
    fill_labels <- age_labels_colored
  }
  p <- p + scale_fill_manual(values = fill_values, breaks = fill_breaks,
                             labels = fill_labels, limits = fill_breaks,
                             drop = FALSE, name = "Age group (years)")
  
  if (show_legend == 1) {
    p <- p + guides(fill = guide_legend(
      title = "Age group (years)", title.position = "top",
      override.aes = list(shape = NA, size = 0, fill = NA, colour = NA),
      order = 1))
  } else {
    p <- p + theme(legend.position = "none")
  }
  
  # --- axes (add absent tick/label if needed) ---
  if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
  
  if (!is.null(y_limits) && !is.null(y_breaks)) {
    full_breaks <- if (show_nm && !is.null(absent_lim)) sort(unique(c(y_breaks, absent_lim))) else y_breaks
    full_labels <- if (show_nm && !is.null(absent_lim)) {
      sapply(full_breaks, function(b) if (b == absent_lim) "NM" else b)
    } else full_breaks
    p <- p + scale_y_continuous(limits = y_limits, breaks = full_breaks, labels = full_labels)
  } else if (!is.null(ylim)) {
    p <- p + scale_y_continuous(limits = ylim)
  }
  
  # --- theme ---
  p <- p + theme_bw() +
    theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title   = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 13, face = "bold", margin = margin(b = 6)),
          legend.text  = ggtext::element_markdown(size = 13, face = "bold"),
          legend.title.align = 0.5,
          legend.box = "vertical",
          legend.spacing.y = unit(0, "pt"),
          legend.margin    = margin(0,0,0,0),
          legend.box.margin= margin(0,0,0,0),
          legend.position  = "inside",
          legend.justification = c("right","top"),
          legend.position.inside = c(0.95, 0.60),
          legend.key = element_rect(fill = NA, colour = NA),
          legend.box.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
          legend.background = element_blank()) +
    xlab("") + ylab("")
  
  # --- correlation text ---
  x_npc <- ifelse(statsLocation_x == "left", 0.01, ifelse(statsLocation_x == "right", 0.98, 0.5))
  y_npc <- ifelse(statsLocation_y == "top", 0.98, ifelse(statsLocation_y == "bottom", 0.07, 0.5))
  p <- p + annotation_custom(grob = textGrob(
    label = cor_label,
    x = unit(x_npc, "npc"), y = unit(y_npc, "npc"),
    hjust = 0, vjust = 1,
    gp = gpar(col = "black", fontsize = 16, fontface = "plain")))
  
  # --- canal label (optional) ---
  if (show_text == 1) {
    p <- p + annotation_custom(grob = textGrob(
      label = canal_label,
      x = unit(0.97, "npc"), y = unit(0.98, "npc"),
      hjust = 1, vjust = 1,
      gp = gpar(col = "black", fontsize = 18, fontface = "bold")))
  }
  
  p
}