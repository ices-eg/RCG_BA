
# 1) Aggregation function: returns a list with merged data.frame and scaleFactor
aggregate_two_measures <- function(count_formula, count_data, count_fun = length,
                                   sum_formula, sum_data, sum_fun = sum,
                                   by_count = NULL, by_sum = NULL,
                                   merge_by = NULL,
                                   na_to_zero = TRUE) {
  
  # Helper: extract LHS and RHS from a formula
  f_to_names <- function(f) {
    f <- as.formula(f)
    lhs <- all.vars(f[[2]])
    rhs_terms <- attr(terms(f), "term.labels")
    list(lhs = lhs, rhs = rhs_terms)
  }
  
  # parse formulas
  cf <- f_to_names(count_formula)
  sf <- f_to_names(sum_formula)
  if (is.null(by_count)) by_count <- cf$rhs
  if (is.null(by_sum))   by_sum   <- sf$rhs
  
  # build aggregate formulas and run aggregate()
  count_lhs <- cf$lhs
  count_agg_formula <- as.formula(paste0("cbind(countVal = ", count_lhs, ") ~ ", paste(by_count, collapse = " + ")))
  countAgg <- aggregate(count_agg_formula, data = count_data, FUN = count_fun)
  
  sum_lhs <- sf$lhs
  sum_agg_formula <- as.formula(paste0("cbind(sumVal = ", sum_lhs, ") ~ ", paste(by_sum, collapse = " + ")))
  sumAgg <- aggregate(sum_agg_formula, data = sum_data, FUN = sum_fun)
  
  # determine merge keys
  if (is.null(merge_by)) {
    common <- intersect(names(countAgg), names(sumAgg))
    if (length(common) == 0) stop("No common columns to merge by. Provide merge_by or rename columns so they share a key.")
    merge_by <- common
  }
  
  merged <- merge(sumAgg, countAgg, by.y = names(merge_by), by.x =merge_by , all = TRUE)
  
  if (na_to_zero) {
    merged$sumVal[is.na(merged$sumVal)] <- 0
    merged$countVal[is.na(merged$countVal)] <- 0
  }
  
  # compute scale factor and scaled sum for plotting (scale sum down to count range)
  max_sum   <- ifelse(all(merged$sumVal == 0), 1, max(merged$sumVal, na.rm = TRUE))
  max_count <- ifelse(all(merged$countVal == 0), 1, max(merged$countVal, na.rm = TRUE))
  scaleFactor <- max_sum / max_count 
  if (!is.finite(scaleFactor) || scaleFactor == 0) scaleFactor <- 1
  
  merged$sum_scaled <- merged$sumVal / scaleFactor
  merged$scaleFactor <- scaleFactor
  
  
  return(merged)
}


# 2) Plot function: consumes output of aggregate_two_measures()
plot_from_aggregated <- function(merged,
                                 x,
                                 sum_scaled_col = "sum_scaled",
                                 count_col = "countVal",
                                 sum_label = "Sum (original units)",
                                 count_label = "Count / Measurements",
                                 sum_fill = "lightblue",
                                 count_color = "red",
                                 facet = NULL) {
  
  scaleFactor <- unique(merged$scaleFactor)
  
  if (!x %in% names(merged)) stop("x must be a column name in the aggregated data.frame")
  
  p <- ggplot(merged, aes(x = .data[[x]])) +
    geom_bar(aes(y = .data[[sum_scaled_col]]),
             stat = "identity", fill = sum_fill, alpha = 0.7) +
    geom_point(aes(y = .data[[count_col]]),
               colour = count_color, size = 1.8) +
    scale_y_continuous(
      name = count_label,                        # left axis shows counts (plotted directly)
      sec.axis = sec_axis(~ . * scaleFactor, name = sum_label) # right axis shows sum (inverse transform)
    ) +
    labs(x = x) +
    theme_bw() +
    theme(
      axis.title.y.left  = element_text(colour = count_color),
      axis.title.y.right = element_text(colour = sum_fill)
    )
  
  if (!is.null(facet)) p <- p + facet_grid(as.formula(facet))
  
  # attach useful metadata
  attr(p, "scaleFactor") <- scaleFactor
  attr(p, "aggregated_df") <- merged
  return(p)
}


