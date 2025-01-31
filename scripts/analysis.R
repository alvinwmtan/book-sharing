apply_and_merge <- function(all_data, FUN) {
  sources <- all_data |> pull(source) |> unique()
  languages <- all_data |> pull(language) |> unique()
  df_list <- list()
  for (cur_lang in languages) {
    for (cur_source in sources) {
      df <- all_data |> 
        filter(language == cur_lang,
               source == cur_source) |> 
        FUN() |> 
        mutate(language = cur_lang,
               source = cur_source)
      df_list <- df_list |> 
        append(list(df))
    }
  }
  
  bind_rows(df_list) |> 
    mutate(source = factor(source, levels = sources),
           family_id = str_match(file_name, ("([0-9]+)(?:_context)"))[,2])
}

agg_and_widen <- function(data_df) {
  data_agg <- data_df["value"] |>
    aggregate(by = list(family_id = data_df$family_id, source = data_df$source), 
              FUN = \(x) {mean(x[is.finite(x)])})
  data_wide <- data_agg |>
    pivot_wider(id_cols = family_id,
                names_from = source,
                values_from = value)
  return(data_wide)
}

separate_contrasts <- function(cons, by_language = FALSE) {
  cons_df <- cons |>
    as_tibble() |>
    separate(contrast, c("group1", "group2"), " - ") |> 
    mutate(
      group1 = gsub("[)(]", "", group1),
      group2 = gsub("[)(]", "", group2),
      p.signif = cut(p.value,
                     breaks = c(-Inf, .0001, .001, .01, .05, .1, 1),
                     labels = c("****", "***", "**", "*", "ns", "ns")))
  # if (by_language) {
  #   cons_df <- cons_df |> 
  #     mutate(group1 = ifelse(language == ".", source, group1),
  #            group2 = ifelse(language == ".", source, group2),
  #            type = ifelse(language == ".", "Within", language) |> 
  #              factor(levels = c("Within", "Spanish", "English"))) |> 
  #     arrange(type)
  # }
  
  cons_df
}

run_analysis <- function(metric_vals, ylab, by_language = FALSE) {
  mod_vals <- metric_vals |> 
    mutate(is.extreme = is_extreme(value)) |> 
    filter(!is.extreme) |> 
    mutate(source = `contrasts<-`(source |> as.factor() |> fct_shift(1), 
                                  value = source |> unique() |> 
                                    length() |> contr.sum() * 0.5) |> fct_shift(-1))
  
  
  if (by_language) {
    mod_vals <- mod_vals |> 
      mutate(language = `contrasts<-`(language |> as.factor(), 
                                      value = language |> unique() |> 
                                        length() |> contr.sum() * 0.5))
    model <- lmer(value ~ source * language + 
                    (1 | family_id) + (1 | family_id:file_name), 
                  data = mod_vals)
    emms <- emmeans(model, ~ source * language)
    cons <- contrast(emms, method = "pairwise", 
                     adjust = "bonferroni",
                     simple = "source",
                     #simple = list("source", "language"), 
                     combine = T)
  } else {
    model <- lmer(value ~ source + 
                    (1 | family_id) + (1 | family_id:file_name), 
                  data = mod_vals)
    emms <- emmeans(model, ~ source)
    cons <- contrast(emms, method = "pairwise",
                     adjust = "bonferroni")
  }
  
  cons <- cons |> 
    separate_contrasts(by_language = by_language)
  cons_plot <- cons |>
    filter(p.signif != "ns")
  # cons <- contrast(emms, method = "pairwise") |>
  #   as_tibble() |>
  #   separate(contrast, c("group1", "group2"), " - ") |> 
  #   mutate(
  #     group1 = gsub("[)(]", "", group1),
  #     group2 = gsub("[)(]", "", group2),
  #     p.signif = cut(p.value,
  #                    breaks = c(-Inf, .0001, .001, .01, .05, .1, 1),
  #                    labels = c("****", "***", "**", "*", "ns", "ns")))
  # 
  # if (by_language) {
  #   cons <- cons |> 
  #     separate(group1, c("group1src", "group1lang"), " (?=[ES])") |> 
  #     separate(group2, c("group2src", "group2lang"), " (?=[ES])")
  #   cons_plot <- cons |>
  #     filter(group1src == group2src | group1lang == group2lang) |>
  #     mutate(group1 = group1src,
  #            group2 = group2src,
  #            type = factor(ifelse(group1src == group2src, "Within", group1lang),
  #                          levels = c("Within", "English", "Spanish"))) |>
  #     arrange(type) |>
  #     filter(p.signif != "ns")
  # } else {
  #   cons_plot <- cons
  # }
  
  plot <- ggplot(data = mod_vals, aes(y = value, x = source)) + 
    geom_violin(aes(fill = language)) +
    geom_boxplot(aes(fill = language), width = 0.15, 
                 position = position_dodge(width = 0.9)) +
    stat_pvalue_manual(cons_plot,
                       y.position = 1.1 * max(mod_vals$value),
                       step.increase = 0.1,
                       color = if (by_language) "language" else "black",
                       tip.length = 0) +
    labs(x = "Source", y = ylab, fill = "Language") +
    theme_classic() +
    theme(legend.position = "bottom")
  
  if (by_language) plot <- plot + TYP_SCALE
  
  return(list("data" = mod_vals, 
              "model" = model,
              "emmeans" = emms, 
              "pairwise" = cons, 
              "plot" = plot))
}

make_line_plot <- function(sl_analysis, s_analysis, ylab) {
  emm <- sl_analysis[["emmeans"]] |> as_tibble()
  cons <- s_analysis[["pairwise"]]
  ggplot(data = emm, aes(y = emmean, 
                         x = source, 
                         group = language)) + 
    geom_line(aes(col = language)) +
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, col = language),
                  width = .1) +
    stat_pvalue_manual(cons,
                       y.position = 1.2 * max(emm$emmean) - 0.2 * min(emm$emmean),
                       step.increase = 0.1,
                       # color = "black",
                       tip.length = 0) +
    # TYP_SCALE +
    labs(x = "Source", y = ylab, col = "Language") +
    theme_classic()
  # theme(legend.position = "none")
}

make_int_line_plot <- function(sl_analysis, s_analysis, ylab) {
  emm <- sl_analysis[["emmeans"]] |> as_tibble()
  cons <- sl_analysis[["pairwise"]] |>
    filter(p.signif != "ns")
  ggplot(data = emm, aes(y = emmean, 
                         x = source, 
                         group = language)) + 
    geom_line(aes(col = language)) +
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE, col = language),
                  width = .1) +
    stat_pvalue_manual(cons,
                       y.position = 1.2 * max(emm$emmean) - 0.2 * min(emm$emmean),
                       step.increase = 0.1,
                       color = "language",
                       tip.length = 0) +
    TYP_SCALE +
    labs(x = "Source", y = ylab, col = "Language") +
    theme_classic()
  # theme(legend.position = "none")
}

make_ls_line_plot <- function(sl_analysis, l_analysis, ylab) {
  lang <- l_analysis$data$language[1]
  emm <- l_analysis[["emmeans"]] |> as_tibble()
  cons <- sl_analysis[["pairwise"]] |> filter(language == lang)
  lang_col <- if (lang == "English") "#F8766D" else "#00BFC4"
  ggplot(data = emm, aes(y = emmean, 
                         x = source)) + 
    geom_line(aes(group = 1), col = lang_col) +
    geom_errorbar(aes(ymin = emmean - SE, ymax = emmean + SE),
                  col = lang_col, width = .1) +
    stat_pvalue_manual(cons,
                       y.position = 1.2 * max(emm$emmean) - 0.2 * min(emm$emmean),
                       step.increase = 0.1,
                       # color = "black",
                       tip.length = 0) +
    # TYP_SCALE +
    labs(x = "Source", y = ylab, col = "Language") +
    theme_classic()
  # theme(legend.position = "none")
}

get_zscore <- function(all_vals, metric) {
  metric_name <- metric
  all_vals |> 
    filter(is.finite(value),
           metric == metric_name) |> 
    group_by(source, family_id) |> 
    summarise(mean_val = mean(value, na.rm = TRUE)) |> 
    mutate(mean_val = 0.6745 * (mean_val - median(mean_val)) / 
             median(abs(mean_val - median(mean_val)))) |> 
    rename(!!metric_name := "mean_val")
}

get_composite_index <- function(all_vals_list, metric_list) {
  zscores <- map2(all_vals_list, metric_list, get_zscore) |> 
    reduce(left_join, by = c("source", "family_id"))
  zscores |> 
    pivot_longer(cols = -c("source", "family_id")) |> 
    group_by(source, family_id) |> 
    summarise(index = mean(value, na.rm = TRUE)) |> 
    pivot_wider(names_from = "source",
                values_from = "index")
}

get_cor <- function(index_vals) {
  vals_cor <- cor(index_vals |> select(-family_id), 
                  use = "na.or.complete",
                  method = "spearman")
  vals_cor
}

print_res <- function(pairwise, lang, gp1, gp2) {
  if (is.null(lang)) {
    p_string = pairwise |> 
      filter(group1 == gp1,
             group2 == gp2) |> 
      pull(p.value) |> 
      apa_p(add_equals = TRUE)
    out <- glue("$p <<p_string>>$",
                .open = "<<", .close = ">>")
  } else {
    lang_string = if (lang == "English") "EN" else "ES"
    p_string = pairwise |> 
      filter(language == lang,
             group1 == gp1,
             group2 == gp2) |> 
      pull(p.value) |> 
      apa_p(add_equals = TRUE)
    out <- glue("$p_\\textup{<<lang_string>>} <<p_string>>$",
                .open = "<<", .close = ">>")
  }
  out
}

print_main <- function(model, pred) {
  mod_tidy <- model |> tidy(conf.int = TRUE) |> 
    filter(term == pred)
  glue("$b$ = {apa_num(mod_tidy$estimate)} [{apa_num(mod_tidy$conf.low)}, {apa_num(mod_tidy$conf.high)}], $p$ {apa_p(mod_tidy$p.value, add_equals = TRUE)}")
}
