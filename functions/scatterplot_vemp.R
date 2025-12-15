scatterplot_vemp <- function(df, param_x, param_y,
                             statsLocation_x = "left", statsLocation_y = "top",
                             corrMethod = "spearman", groupBy, level_param,
                             x_limits = NULL, x_breaks = NULL,
                             y_limits = NULL, y_breaks = NULL,
                             show_legend = 1, show_text = 1,
                             absent_lim = c(2.4, 2.8)) {
  
  absent_jitter_x = 0.2 #0.5       # horizontal jitter for absent points
  absent_jitter_y = 0 #0.1         # vertical jitter for absent points
  absent_fill <- "grey70"
  absent_size <- 5
  
  vemp_type   <- sub("_.*", "", level_param)
  results_col <- paste0(vemp_type, "_results")
  
  lvl_fac      <- if (is.factor(df[[level_param]])) df[[level_param]] else factor(df[[level_param]])
  shape_levels <- levels(lvl_fac)
  first_group  <- levels(factor(df[[groupBy]]))[1]
  
  df <- df %>%
    mutate(
      !!level_param := factor(as.character(.data[[level_param]]), levels = shape_levels),
      y_pseudo = case_when(
        .data[[results_col]] == 2 & !is.na(absent_lim[1]) ~ absent_lim[1],   # unilateral absent
        .data[[results_col]] == 3 & !is.na(absent_lim[2]) ~ absent_lim[2],   # bilateral absent
        TRUE ~ NA_real_
      ),
      stroke_absent = ifelse(.data[[groupBy]] == first_group, 0.8, 0)
    )
  
  df_clean <- df %>%
    filter(!is.na(.data[[param_y]]),
           !is.na(.data[[groupBy]]), !is.na(age_group),
           !is.na(.data[[level_param]])) %>%
    mutate(stroke_group = .data[[groupBy]] == first_group,
           age_group = factor(age_group, levels = c("40–49", "50–59", "60–69", "70–79", "80–89")))
  
  shape_codes  <- c(21, 22, 23)[seq_along(shape_levels)]
  shape_values <- setNames(shape_codes, shape_levels)
  
  counts_tbl <- df %>%
    filter(!is.na(.data[[param_y]]),
           .data[[results_col]] != 3,
           !is.na(.data[[level_param]])) %>%
    mutate(lvl = factor(as.character(.data[[level_param]]), levels = shape_levels)) %>%
    count(lvl, name = "n")
  
  counts_full <- setNames(rep(0L, length(shape_levels)), shape_levels)
  if (nrow(counts_tbl) > 0) counts_full[as.character(counts_tbl$lvl)] <- counts_tbl$n
  
  shape_labels <- setNames(
    paste0(shape_levels, " (n = ", counts_full[shape_levels], ")"),
    shape_levels
  )
  
  max_level_chr <- tail(shape_levels, 1)
  absent_df <- df %>%
    filter(!is.na(y_pseudo), !is.na(.data[[param_x]])) %>%
    mutate(.lvl_chr = ifelse(is.na(as.character(.data[[level_param]])), max_level_chr,
                             as.character(.data[[level_param]])),
           !!level_param := factor(.lvl_chr, levels = shape_levels))
  
  cor_test <- suppressWarnings(cor.test(df_clean[[param_x]], df_clean[[param_y]], method = corrMethod))
  r_symbol <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  n_samples <- nrow(df_clean)
  n_de <- sum(df_clean[[groupBy]] == "DE", na.rm = TRUE)
  cor_label <- paste0("n = ", n_samples,
                      " (UOL: ", n_de, ")",
                      ", ", r_symbol, " = ", round(cor_test$estimate, 2),
                      ", p = ", signif(cor_test$p.value, 2))
  
  level_label <- gsub(".*BE_(\\d+)$", "\\1 dB SPL", param_y)
  
  age_colors <- c("40–49" = "#E69F00", "50–59" = "#56B4E9", "60–69" = "#009E73",
                  "70–79" = "#CC79A7", "80–89" = "#F0E442")
  age_labels <- paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels) <- names(age_colors)
  fill_values <- c(age_colors, "Absent" = absent_fill)
  fill_labels <- c(age_labels, "Absent" = "<span style='color:grey70;'>Absent</span>")
  
  p <- ggplot(df_clean, aes(x = .data[[param_x]], y = .data[[param_y]])) +
    geom_point(data = absent_df, 
               aes(x = .data[[param_x]], y = y_pseudo, fill = "Absent",
                   shape = .data[[level_param]], stroke = stroke_absent),
               colour = "black", size = absent_size,
               inherit.aes = FALSE, show.legend = TRUE,
               position = position_jitter(width = absent_jitter_x, height = absent_jitter_y)) +
    geom_point(aes(fill = age_group, 
                   shape = .data[[level_param]]),
               colour = "black", size = 5, 
               stroke = ifelse(df_clean$stroke_group, 0.8, 0),
               na.rm = TRUE, show.legend = (show_legend == 1)) +
    geom_smooth(method = "lm", se = FALSE, colour = "black", linewidth = 1.3,
                na.rm = TRUE, show.legend = FALSE) +
    scale_fill_manual(values = fill_values, labels = fill_labels, name = "Age group (years)", drop = FALSE) +
    scale_shape_manual(values = shape_values, labels = shape_labels,
                       name   = "Intensity level (dB nHL)",
                       breaks = shape_levels, limits = shape_levels, drop = FALSE) +
    guides(fill  = guide_legend(override.aes = list(shape = NA, size = 5, colour = "black"), order = 1),
           shape = if (show_legend == 1) guide_legend(override.aes = list(fill = NA)) else "none") +
    theme_bw() +
    theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title  = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 13, face = "bold", margin = margin(b = 6)),
          legend.text  = ggtext::element_markdown(size = 13, face = "bold"),
          legend.box = "vertical",
          legend.position = "inside",
          legend.justification = c("right", "bottom"),
          legend.position.inside = c(0.95, 0.05),
          legend.key = element_rect(fill = NA, colour = NA),
          legend.box.background = element_rect(colour = "black", fill = "white", linewidth = 0.8),
          legend.background = element_blank()) +
    xlab("") + ylab("")
  
  if (!is.null(x_limits) && !is.null(x_breaks)) {
    p <- p + scale_x_continuous(limits = x_limits, breaks = x_breaks)
  }
  if (!is.null(y_limits) && !is.null(y_breaks)) {
    full_breaks <- unique(c(y_breaks, absent_lim[!is.na(absent_lim)]))
    full_labels <- sapply(full_breaks, function(b) {
      if (!is.na(absent_lim[1]) && b == absent_lim[1]) return("Absent\n(unilat.)")
      if (!is.na(absent_lim[2]) && b == absent_lim[2]) return("Absent\n(bilat.)")
      b
    })
    p <- p + scale_y_continuous(limits = y_limits, breaks = full_breaks, labels = full_labels)
  }
  
  x_npc <- ifelse(statsLocation_x == "left", 0.01, ifelse(statsLocation_x == "right", 0.98, 0.5))
  y_npc <- ifelse(statsLocation_y == "top", 0.98, ifelse(statsLocation_y == "bottom", 0.1, 0.5))
  p <- p + annotation_custom(
    grob = grid::textGrob(label = cor_label,
                          x = grid::unit(x_npc, "npc"), y = grid::unit(y_npc, "npc"),
                          hjust = 0, vjust = 1.2,
                          gp = grid::gpar(col = "black", fontsize = 16, fontface = "plain"))
  )
  
  if (show_text == 1) {
    p <- p + annotation_custom(
      grob = grid::textGrob(label = level_label,
                            x = grid::unit(0.95, "npc"), y = grid::unit(0.98, "npc"),
                            hjust = 1, vjust = 1,
                            gp = grid::gpar(col = "black", fontsize = 18, fontface = "bold"))
    )
  }
  
  p
}