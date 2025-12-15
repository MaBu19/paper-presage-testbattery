plot_pta4 <- function(df_w, ISO_norms, id_matched, subclass_matched,
                      norm_col = "PTA_50_2", age_col = "age", # PTA_50_2 = ISO norms for female
                      pta_col = "PTA4_BE", centre_col = "centre",
                      xlim = c(40, 90), ylim = c(50, -10),
                      show_legend = TRUE, jitter_width = 0.4,
                      jitter_height = 0.4) {
  
  # --- Data prep ---
  norms <- ISO_norms %>%
    filter(!is.na(.data[[age_col]]), !is.na(.data[[norm_col]]))
  
  pts <- df_w %>%
    filter(!is.na(.data[[age_col]]), !is.na(.data[[pta_col]])) %>%
    mutate(age_group = cut(.data[[age_col]],
                           breaks = c(40, 50, 60, 70, 80, 90),
                           right = FALSE,
                           labels = c("40–49 years", "50–59 years",
                                      "60–69 years", "70–79 years", "80–89 years")),
           stroke_w = ifelse(.data[[centre_col]] == "DE", 0.8, 0),
           alpha_pt = ifelse(id %in% id_matched, 1, 0.5))
  
  matched_df <- df_w %>%
    filter(id %in% id_matched) %>%
    mutate(subclass = subclass_matched) %>%
    select(id, !!age_col, !!pta_col, !!centre_col, subclass)
  
  # Age-group colours
  age_levels_present <- levels(pts$age_group)[!is.na(levels(pts$age_group))]
  age_colors <- c("40–49 years"="#E69F00", "50–59 years"="#56B4E9", "60–69 years"="#009E73",
                  "70–79 years"="#CC79A7", "80–89 years"="#F0E442")[age_levels_present]
  
  age_labels_colored <- setNames(
    paste0("<span style='color:", age_colors, ";'>", names(age_colors), "</span>"),
    names(age_colors))
  
  ggplot() +
    
    # ISO median line
    geom_line(data = norms, aes(x = .data[[age_col]], y = .data[[norm_col]], 
                                linetype = "ISO median"), linewidth = 1.2, colour = "#1f78b4") +
    
    # Matched line segments
    geom_line(data = matched_df,
              aes(x = .data[[age_col]], y = .data[[pta_col]], group = subclass),
              colour = "grey60", linewidth = 0.7) +
    
    # Participant points with jitter
    geom_point(data = pts,
               aes(x = .data[[age_col]], y = .data[[pta_col]],
                   fill = age_group, stroke = stroke_w, alpha = alpha_pt),
               shape = 21, size = 3, colour = "black",
               position = position_jitter(width = jitter_width, height = jitter_height),
               show.legend = show_legend) +
    
    scale_alpha_identity(guide = "none") +
    
    # Axes
    scale_x_continuous(breaks = seq(xlim[1], xlim[2], 10)) +
    scale_y_reverse(breaks = seq(-10, 120, 10)) +
    coord_cartesian(xlim = xlim, ylim = c(ylim[1], ylim[2])) +
    labs(x = "Age [years]", y = "PTA [dB HL]") +
    
    # Legends
    scale_fill_manual(values = age_colors, labels = age_labels_colored, name = NULL) +
    scale_linetype_manual(values = c("ISO median" = 1), name = NULL) +
    
    guides(linetype = guide_legend(order = 1, override.aes = 
                                     list(colour = "#1f78b4", linewidth = 1.5, shape = NA)),
           fill = guide_legend(order = 2, 
                               override.aes = 
                                 list(shape = NA, size = 3, colour = "black", stroke = 0)), stroke = "none") +
    theme_bw(base_size = 12) +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.ticks = element_line(colour = "black"),
          axis.text = element_text(colour = "black", size = 13, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold"),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = c(0.04, 0.05),
          legend.justification  = c("left", "bottom"),
          legend.box = "vertical",
          legend.box.background = element_rect(fill = "white", colour = "black", linewidth = 0.6),
          legend.background = element_blank(),
          legend.key = element_blank(),
          legend.key.height = unit(0.22, "cm"),
          legend.key.width = unit(0.8, "cm"),
          legend.spacing.y = unit(-0.08, "cm"),
          legend.spacing.x = unit(0.1, "cm"),
          legend.box.margin = margin(1, 1, 1, 1),
          legend.text = ggtext::element_markdown(size = 13, face = "bold"),
          legend.title = element_blank()) +
    {
      if (!show_legend) theme(legend.position = "none") else NULL
    }
}