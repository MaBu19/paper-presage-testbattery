# NOTE: Reg line and corr is calculated only for level = 115!!
scatterplot_abr <- function(df, param_x, param_y,
                            statsLocation_x = "left", statsLocation_y = "top",
                            corrMethod = "spearman", groupBy = NULL,
                            labelSpacing = 0.07,
                            xlim = NULL, ylim = NULL, y_breaks = NULL,
                            show_legend = 1, show_text = 1,
                            absent_var = NULL, absent_lim = NULL,
                            absent_jitter_x = 0, absent_jitter_y = 0
) {
  # --- data prep --------------------------------------------------------------
  df_clean <- df %>%
    filter(!is.na(.data[[param_x]]),
           !is.na(.data[[param_y]])) %>%
    { if (!is.null(groupBy)) filter(., !is.na(.data[[groupBy]])) else . }
  
  if (!is.null(groupBy)) {
    first_group <- levels(factor(df[[groupBy]]))[1]
    df_clean <- df_clean %>%
      mutate(
        stroke_group = .data[[groupBy]] == first_group,
        stroke_width = ifelse(stroke_group, 1, 0)
      )
  } else {
    df_clean <- df_clean %>%
      mutate(stroke_group = FALSE,
             stroke_width = 0)
  }
  
  # --- correlation text (ONLY level = 115) ------------------------------------
  # subset to 115 dB
  df115 <- df_clean %>% filter(ecochg_level_BE == "115")
  
  # exclude absent==2 from corr if absent_var is present
  df_corr <- if (!is.null(absent_var) && absent_var %in% names(df115)) {
    df115 %>% filter(.data[[absent_var]] != 2 & !is.na(.data[[absent_var]]))
  } else df115
  
  cor_test    <- suppressWarnings(cor.test(df_corr[[param_x]], df_corr[[param_y]], method = corrMethod))
  method_name <- tools::toTitleCase(corrMethod)
  r_symbol    <- ifelse(tolower(corrMethod) == "spearman", "ρ", "R")
  n_total     <- nrow(df115)
  n_de        <- if (!is.null(groupBy)) sum(df115[[groupBy]] == "DE", na.rm = TRUE) else NA_integer_
  
  cor_label <- if (!is.null(groupBy)) {
    paste0("n = ", n_total,
           " (UOL: ", n_de, ")",
           ", ", r_symbol, " = ", round(cor_test$estimate, 2),
           ", p = ", signif(cor_test$p.value, 2))
  } else {
    paste0("n = ", n_total,
           ", ", r_symbol, " = ", round(cor_test$estimate, 2),
           ", p = ", signif(cor_test$p.value, 2))
  }
  
  # --- level counts text ----------------------------------------------------
  level_text <- NULL
  if ("ecochg_level_BE" %in% names(df_clean)) {
    level_counts <- df_clean %>%
      filter(!is.na(ecochg_level_BE)) %>%
      count(ecochg_level_BE) %>%
      arrange(desc(ecochg_level_BE))
    if (nrow(level_counts) > 0) {
      level_text <- paste0(
        "Levels: ",
        paste0(level_counts$ecochg_level_BE, " dB (n=", level_counts$n, ")",
               collapse = ", ")
      )
    }
  }
  
  # --- metric label -----------------------------------------------------------
  if (grepl("III_V", param_y, ignore.case = TRUE)) {
    level_label <- "Interpeak: III–V"
  } else if (grepl("AP_V", param_y, ignore.case = TRUE)) {
    level_label <- "Interpeak: I–V"
  } else if (grepl("AP_ms|AP_uv", param_y, ignore.case = TRUE)) {
    level_label <- "Wave I"
  } else if (grepl("III_ms|III_uv", param_y, ignore.case = TRUE)) {
    level_label <- "Wave III"
  } else if (grepl("V_ms|V_uv", param_y, ignore.case = TRUE)) {
    level_label <- "Wave V"
  } else {
    level_label <- param_y
  }
  
  # --- fill colors & labels ---------------------------------------------------
  age_colors <- c("40–49" = "#E69F00", "50–59" = "#56B4E9",
                  "60–69" = "#009E73", "70–79" = "#CC79A7",
                  "80–89" = "#F0E442")
  age_labels_colored <- paste0(
    "<span style='color:", age_colors, ";'>", names(age_colors), "</span>")
  names(age_labels_colored) <- names(age_colors)
  fill_values <- c(age_colors, "Absent" = "grey70")
  fill_labels <- c(age_labels_colored,
                   "Absent" = "<span style='color:grey70;'>Absent</span>")
  
  # --- shapes for ecochg_level_BE ---------------------------------------------
  use_shapes <- "ecochg_level_BE" %in% names(df_clean)
  if (use_shapes) {
    df_clean <- df_clean %>%
      mutate(ecochg_level_BE = factor(ecochg_level_BE))
    sh_levels <- levels(df_clean$ecochg_level_BE)
    sh_values <- setNames(rep(23, length(sh_levels)), sh_levels)
    if ("115" %in% sh_levels) sh_values["115"] <- 21
    if ("105" %in% sh_levels) sh_values["105"] <- 22
  }
  
  # --- base ggplot ------------------------------------------------------------
  p <- ggplot(df_clean, aes(x = .data[[param_x]], y = .data[[param_y]]))
  
  if (use_shapes) {
    p <- p +
      geom_point(aes(shape = ecochg_level_BE,
                     fill = age_group, stroke= stroke_width), colour = "black",
                 size = 5, na.rm = TRUE, show.legend= (show_legend == 1)) +
      scale_shape_manual(name = "Level (dB SPL)", values = sh_values,
                         breaks = names(sh_values), labels = names(sh_values))
  } else {
    p <- p +
      geom_point(aes(fill = age_group, stroke= stroke_width),
                 shape = 21, colour = "black", size = 5, na.rm = TRUE, show.legend= (show_legend == 1))
  }
  
  # --- regression line (ONLY level = 115) -------------------------------------
  if (nrow(df115) > 1) {
    p <- p +
      geom_smooth(data = df115, aes(x = .data[[param_x]], y = .data[[param_y]]),
                  method = "lm", se = FALSE, colour = "black", linewidth = 1.3,
                  na.rm = TRUE, show.legend = FALSE)
  }
  
  # --- absent points layer ----------------------------------------------------
  if (!is.null(absent_var) &&
      !is.null(absent_lim) &&
      absent_var %in% names(df)) {
    
    abs_df <- df %>%
      filter(.data[[absent_var]] == 1,
             !is.na(.data[[param_x]]))
    
    if (!is.null(groupBy)) {
      first_group <- levels(factor(df[[groupBy]]))[1]
      abs_df <- abs_df %>%
        mutate(stroke_absent = ifelse(.data[[groupBy]] == first_group, 0.8, 0))
    } else {
      abs_df <- abs_df %>% mutate(stroke_absent = 0)
    }
    
    if (nrow(abs_df) > 0) {
      p <- p +
        geom_point(data = abs_df, aes(x = .data[[param_x]], y = absent_lim,
                                      fill = "Absent", stroke = stroke_absent), shape = 21,
                   colour = "black", size = 5, inherit.aes = FALSE,
                   position = position_jitter(width = absent_jitter_x, height = absent_jitter_y),
                   show.legend = c(fill = (show_legend == 1), shape = FALSE))
    }
  }
  
  # --- scales & legends -------------------------------------------------------
  if ("age_group" %in% names(df_clean)) {
    p <- p +
      scale_fill_manual(values = fill_values, labels = fill_labels,
                        name = "Age group (years)",drop = FALSE)
    
    if (show_legend == 1) {
      p <- p +
        guides(
          fill = guide_legend(
            override.aes = list(shape = NA, fill = NA, colour = NA), 
            order = 2),
          shape = if (use_shapes) guide_legend(
            override.aes = list(fill = "white"),
            order = 1
          ) else "none"
        )
    } else {
      p <- p + theme(legend.position = "none")
    }
  } else {
    p <- p + scale_fill_manual(values = c("steelblue"), guide = FALSE)
  }
  
  # --- theme & axes tweaks ----------------------------------------------------
  p <- p +
    theme_bw() +
    theme(axis.text = element_text(size = 14, face = "bold", colour = "black"),
          axis.title.x = element_text(size = 16, face = "bold"),
          axis.title.y = element_text(size = 16, face = "bold"),
          plot.title = element_text(size = 16, face = "bold"),
          legend.title = element_text(size = 13, face = "bold", margin = margin(b = 6)),
          legend.text = ggtext::element_markdown(size = 13, face = "bold"),
          legend.box = "vertical",
          legend.position = if (show_legend == 1) "inside" else "none",
          legend.justification  = c("right", "bottom"),
          legend.position.inside= c(0.95, 0.05)) +
    xlab("") + ylab("")
  
  if (!is.null(xlim)) p <- p + scale_x_continuous(limits = xlim)
  
  if (!is.null(ylim) || !is.null(y_breaks) || !is.null(absent_lim)) {
    if (!is.null(y_breaks)) {
      full_breaks <- sort(unique(c(y_breaks, if (!is.null(absent_lim)) absent_lim)))
      full_labels <- vapply(
        full_breaks,
        function(b) if (!is.null(absent_lim) && b == absent_lim) "Absent" else as.character(b),
        character(1)
      )
      p <- p + scale_y_continuous(limits = ylim, breaks = full_breaks, labels = full_labels)
    } else if (!is.null(ylim)) {
      p <- p + scale_y_continuous(limits = ylim)
    }
  }
  
  # --- annotations: correlation (115 only) + counts ---------------------------
  x_npc <- ifelse(statsLocation_x == "left", 0.01, ifelse(statsLocation_x == "right", 0.98, 0.5))
  y_npc <- ifelse(statsLocation_y == "top", 0.98, ifelse(statsLocation_y == "bottom", 0.15, 0.5))
  
  p <- p +
    annotation_custom(
      grob = grid::textGrob(
        label = cor_label,
        x = grid::unit(x_npc, "npc"),
        y = grid::unit(y_npc, "npc"),
        hjust = 0, vjust = 1,
        gp = grid::gpar(col = "black", fontsize = 16)
      )
    )
  
  if (!is.null(level_text)) {
    p <- p +
      annotation_custom(
        grob = grid::textGrob(
          label = level_text,
          x = grid::unit(x_npc, "npc"),
          y = grid::unit(y_npc - labelSpacing, "npc"),
          hjust = 0, vjust = 1,
          gp = grid::gpar(col = "black", fontsize = 16)
        )
      )
  }
  
  if (show_text == 1) {
    p <- p +
      annotation_custom(
        grob = grid::textGrob(
          label = level_label,
          x = grid::unit(0.98, "npc"),
          y = grid::unit(0.98, "npc"),
          hjust = 1, vjust = 1,
          gp = grid::gpar(col = "black", fontsize = 18, fontface = "bold")
        )
      )
  }
  
  p
}