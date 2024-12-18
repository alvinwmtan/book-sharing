COL_SCALE <- 
  scale_colour_manual("Source",
                      values = c("ReadOxford" = "darkorchid3", 
                                 "Book text" = "darkorchid1", 
                                 "Read-aloud" = "steelblue1", 
                                 "Spont-book" = "lightgreen", 
                                 "Spont-other" = "salmon1", 
                                 "CHILDES" = "salmon3"))
LTY_SCALE <- 
  scale_linetype_manual("Source",
                        values = c("ReadOxford" = 3, "Book text" = 1, 
                                   "Read-aloud" = 1, "Spont-book" = 1, 
                                   "Spont-other" = 1, "CHILDES" = 3))

COL_SCALE_2 <- 
  scale_colour_manual("Source",
                      values = c("Book text" = "darkorchid1", 
                                 "Read-aloud" = "steelblue1", 
                                 "Spont-book" = "lightgreen", 
                                 "Spont-other" = "salmon1"))

TYP_SCALE <- scale_colour_manual("Language",
                                 values = c("Spanish" = "#00bfc4", 
                                            "English" = "#f8766d", 
                                            "Within" = "black"),
                                 guide = "none")

COR_SCALE <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))

make_scatter_smooth <- function(data, mapping, ...) {
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = .4, color = I("black")) +
    geom_smooth(method = "lm", color = I("blue"), ...)
}

make_cor_fill <- function(data, mapping, method = "spearman", ...) {
  cor_val <- cor(eval_data_col(data, mapping$x),
                 eval_data_col(data, mapping$y),
                 method = method,
                 use = "pairwise.complete.obs")
  fill <- COR_SCALE(200)[findInterval(cor_val, seq(-1, 1, length = 200))]
  
  ggally_cor(data = data, 
             mapping = mapping, 
             title = "ρ",
             method = method,
             color = I("black"),
             ...) +
    theme(panel.background = element_rect(fill = fill))
}
