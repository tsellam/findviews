######################################
# Zig-Dissimilarity for numeric data #
######################################
#---------------------------------------------#
# Difference between the means with Cohen's D #
#---------------------------------------------#
zig_means <- function(view, in_data, out_data) {
   stopifnot(is.data.frame(in_data))
   stopifnot(is.data.frame(out_data))
   stopifnot(is.character(view))
   stopifnot(length(view)>0)

   # Computes the difference between the means for each column
   diffs <- sapply(view, function(col){

      # Scrubs missing values
      col_in <- na.omit(in_data[[col]])
      col_out <- na.omit(out_data[[col]])

      # Gets basic stats
      in_count <- length(col_in)
      out_count<- length(col_out)
      in_mean  <- mean(col_in)
      out_mean <- mean(col_out)
      in_var   <- var(col_in)
      out_var  <- var(col_out)

      if (in_count == 0 | out_count == 0){
         delta <- NA
         t_test_out <- NA

      } else if (in_count == 1 & out_count == 1){
         delta <- abs((in_mean - out_mean))
         t_test_out <- NA

      } else if (in_count > 1 & out_count == 1){
         delta <- abs((out_mean - in_mean) / in_var)
         t_test_out <- NA

      } else if (in_count == 1 & out_count > 1){
         delta <- abs((in_mean - out_mean) / out_var)
         t_test_out <- NA

      } else if (in_count > 1 & out_count > 1){
         # Gets Cohen's d
         agg_var    <- (in_count - 1) * in_var + (out_count - 1) * out_var
         pooled_var <- sqrt(agg_var / (in_count + out_count - 2))
         delta <- abs((in_mean - out_mean) / pooled_var)

         # Performs t.test
         t_test_out <- tryCatch(
            t.test(col_in, col_out)$p.value,
            error=function(e) return(NA)
         )
      }

      # Returns
      c(size = delta, pvalue = t_test_out)
   })
   diffs <- as.matrix(diffs)
   colnames(diffs) <- view

   # Aggregates them
   agg_diffs <- mean(diffs['size',], na.rm = T)
   if(!is.finite(agg_diffs)) agg_diffs <- NA

   # Generates description
   is_significant <- (diffs['pvalue',] < P_VALUE_ZIG)
   large_effects  <- colnames(diffs[,is_significant,drop=F])
   tip <- if (length(large_effects) == 0){
      ""
   } else if (length(large_effects) >= 1){
      if (length(view) == 1){
         paste0('the difference between the means')
      } else {
         col_enum <- enumerate_char(large_effects)
         paste0('the difference between the means on ', col_enum)
      }
   }

   list(
      score  = agg_diffs,
      detail = list(diffs = diffs),
      tip    = tip
   )

}


#-------------------------------------------------------------------#
# Difference between the variances, with the ratio of the variances #
#-------------------------------------------------------------------#
zig_sds <- function(view, in_data, out_data) {
   stopifnot(is.data.frame(in_data))
   stopifnot(is.data.frame(out_data))
   stopifnot(is.character(view))
   stopifnot(length(view)>0)

   # Computes the ratio between the variances for each column
   diffs <- sapply(view, function(col){

      # Scrubs missing values
      col_in <- na.omit(in_data[[col]])
      col_out <- na.omit(out_data[[col]])

      # Computes difference between SDs
      s_in  <- sd(col_in)
      s_out <- sd(col_out)
      sd_diss <- abs((s_in - s_out) / max(s_in, s_out))

      # Performs F test
      pvalue <- tryCatch(var.test(col_in, col_out)$p.value,
                         error=function(e) return(NA))

      c(
         'sd_diss'= sd_diss,
         'pvalue' = pvalue
      )

   })
   diffs <- as.matrix(diffs)
   colnames(diffs) <- view

   # Aggregates them
   agg_diffs <- mean(diffs['sd_diss',], na.rm = T)
   if(!is.finite(agg_diffs)) agg_diffs <- NA

   # Generates description
   is_significant <- (diffs['pvalue',] < P_VALUE_ZIG)
   large_effects  <- colnames(diffs[,is_significant,drop=F])
   tip <- if (length(large_effects) == 0){
      ""
   } else if (length(large_effects) >= 1){
      if (length(view) == 1){
         paste0('the difference between the variances')
      } else {
         col_enum <- enumerate_char(large_effects)
         paste0('the difference between the variances on ', col_enum)
      }
   }

   list(
      score  = agg_diffs,
      detail = list(diffs=diffs),
      tip    = tip
   )
}


#--------------------------------------------------------------------#
# Difference between the correlations with the Fisher transformation #
#--------------------------------------------------------------------#
# Tests the difference between correlations with Fisher transformation
fisher_transform <- function(r) {
   if (!is.finite(r)) return(NA)
   if (r < -1 | r > 1) return(NA)
   0.5*log((1+r)/(1-r))
}

corr_diff_test <- function(r1, r2, n1, n2){
   if (is.na(r1) | is.na(r2)) return(NA)
   if (n1 < 4 | n2 < 4) return(NA)

   z1 <- fisher_transform(r1)
   z2 <- fisher_transform(r2)

   var1 <- 1 / (n1 - 3)
   var2 <- 1 / (n2 - 3)
   sd_diff <- sqrt(var1 + var2)

   z_diff <- (z1 - z2) / sd_diff
   p_value <- 2 * pnorm(-abs(z_diff))

   return(p_value)
}

# Main function for the Zig-Component
zig_corr <- function(view, in_data, out_data) {
   stopifnot(is.data.frame(in_data))
   stopifnot(is.data.frame(out_data))
   stopifnot(is.character(view))

   # Eliminates missing values
   view_in <- na.omit(in_data[,view, drop=F])
   view_out <- na.omit(out_data[,view, drop=F])

   if (length(attr(view_in, 'na.action')) > 0.3 * nrow(in_data) |
       length(attr(view_out, 'na.action')) > 0.3 * nrow(out_data))
      warning('Lots of rows with NAs removed in view', view)

   if (nrow(view_in) == 0 | nrow(view_out) == 0)
      return(list(
         score  = NA,
         detail = list(),
         tip = NA
      ))

   # Computes correlation matrices
   cor_in  <- suppressWarnings(cor(view_in))
   cor_out <- suppressWarnings(cor(view_out))

   # Gets differences and aggregates into one score
   f_in  <- apply(cor_in, c(1,2), fisher_transform)
   f_out <- apply(cor_out, c(1,2), fisher_transform)
   cor_diff <- abs(f_in - f_out)
   score <- mean(cor_diff, na.rm = TRUE)
   if (!is.finite(score)) score <- NA

   # Gets p-values
   pvalues <- apply_2_matrices(cor_in, cor_out, corr_diff_test,
                               nrow(in_data), nrow(out_data))

   # Generates comments
   is_significant <- ( pvalues < P_VALUE_ZIG)
   tip <- if (!any(is_significant, na.rm = T)) {
      ""
   } else {
      sig_columns <- which_true_elements(is_significant)
      sig_columns_str <- sapply(sig_columns, paste, collapse = "/")
      sig_columns_str <- enumerate_char(sig_columns_str)
      tip <- paste0('the difference in correlations on these columns : ',
                    sig_columns_str)
   }

   list(
      score  = score,
      detail = list(cor_in  = cor_in,
                    cor_out = cor_out,
                    pvalues = pvalues),
      tip = tip
   )
}


######################################
# Zig-Dissimilarity for nominal data #
######################################

hist_diss_score <- function(table_in, table_out){
   # First, aligns tables
   in_minus_out <- setdiff(names(table_in), names(table_out))
   table_out[in_minus_out] <- 0

   out_minus_in <- setdiff(names(table_out), names(table_in))
   table_in[out_minus_in] <- 0

   table_out <- table_out[sort(names(table_out))]
   table_in  <- table_in[sort(names(table_in))]

   stopifnot(names(table_in) == names(table_out))

   # Then, computes the Euclidean distance between the two histograms
   hist_in  <- table_in / sum(table_in)
   hist_out <- table_out/ sum(table_out)

   diss_score <- sqrt(sum((hist_out - hist_in)^2))

   return(diss_score)
}

# Performs Chi-2 test
# WARNING! Ignores all the levels in table_in which are not in table_out
wrap_chi_squared <- function(table_in, table_out){

   # Detects what type of test to use
   small_sample <- (any(table_out < 5) | any(table_in < 5))

   # Aligns the tables
   out_minus_in <- setdiff(names(table_out), names(table_in))
   table_in[out_minus_in] <- 0

   in_minus_out <- setdiff(names(table_in), names(table_out))
   table_in <- table_in[!names(table_in) %in% in_minus_out]

   table_out <- table_out[sort(names(table_out))]
   table_in  <- table_in[sort(names(table_in))]

   stopifnot(names(table_in) == names(table_out))

   # Runs the test
   chisq_out <- if (small_sample){
      chisq.test(x=table_in, p=table_out,
                 simulate.p.value = TRUE, rescale.p = TRUE, B = 100)
   } else {
      chisq.test(x=table_in, p=table_out, rescale.p = TRUE)
   }
   if (!grepl("given probabilities", chisq_out$method))
       stop("Something went wrong with Chi-Squared calculations")

   # Extracts residuals
   residual_pvalues <- 2*pnorm(-abs(chisq_out$stdres))
   names(residual_pvalues) <- names(table_in)

   return(list(
      chi2             = as.numeric(chisq_out$statistic),
      pvalue           = chisq_out$p.value,
      residual_pvalues = residual_pvalues
   ))

}

comment_chi_squared_analysis <- function(chisq_results, max_levels = 3){
   stopifnot(is.list(chisq_results))

   # comments_per_variable <- sapply(names(chisq_results), function(col){
   #    # Gets the results of Chi-Squared test
   #    results <- chisq_results[[col]]
   #    # Discards if non significant
   #    if (!is.finite(results$pvalue)) return(NULL)
   #    if (results$pvalue > P_VALUE_ZIG) return(NULL)
   #
   #    # Builds sentence 'colname (in particular values v1, v2 and v3)'
   #    # Gets values associated with high Pearson residuals
   #    is_exceptional <- results$residual_pvalues < P_VALUE_PEARSON_RESIDUALS
   #    exceptional_levels <- names(results$residual_pvalues)[is_exceptional]
   #    if (length(exceptional_levels) > max_levels)
   #       exceptional_levels <- exceptional_levels[1:3]
   #
   #    # Concatenates
   #    parenthesis <- if (length(exceptional_levels) > 0){
   #       str <- enumerate_char(exceptional_levels)
   #       str <- paste0(' (values ', str, ')')
   #    } else {
   #       ""
   #    }
   #
   #    descr <- paste0(col, parenthesis)
   #
   #    return(descr)
   # })

   comments_per_variable <- sapply(names(chisq_results), function(col){
      # Gets the results of Chi-Squared test
      results <- chisq_results[[col]]
      # Discards if non significant
      if (!is.finite(results$pvalue)) return(NULL)
      if (results$pvalue > P_VALUE_ZIG) return(NULL)

      return(col)
   })

   is_null <- sapply(comments_per_variable, is.null)
   comments_per_variable <- comments_per_variable[!is_null]
   comments_per_variable <- as.character(comments_per_variable)

   comment_trunk     <- 'the differences in distribution on the '
   comment_trunk <- paste0(comment_trunk,
                           if (length(comments_per_variable) ==1) 'variable '
                           else 'variables ')
   comment_variables <- enumerate_char(comments_per_variable)
   full_comment      <- paste0(comment_trunk, comment_variables)

   return(full_comment)
}

zig_histogram <- function(view, in_data, out_data) {
   stopifnot(is.data.frame(in_data))
   stopifnot(is.data.frame(out_data))
   stopifnot(is.character(view))
   stopifnot(length(view)>0)

   # Computes and compares the histograms
   chisq_analysis <- lapply(view, function(v){
      hist_in  <- table(in_data[[v]],  useNA = "no")
      hist_out <- table(out_data[[v]], useNA = "no")

      diss_score  <- hist_diss_score(hist_in, hist_out)
      chi_squared <- wrap_chi_squared(hist_in, hist_out)
      c(diss_score=diss_score, chi_squared)
   })
   names(chisq_analysis) <- view

   # Aggregates scores
   scores <- sapply(chisq_analysis, function(x) x$diss_score)
   score <- mean(scores, na.rm = T)
   if (!is.finite(score)) score <- NA

   # Generates the comments
   tip <- comment_chi_squared_analysis(chisq_analysis)

   list(
      score  = score,
      detail = chisq_analysis,
      tip    = tip
   )

}

##################################
# Generic View Analysis Function #
##################################
zig_aggregate <- function(table_score, weights){
   stopifnot(is.data.frame(table_score))
   stopifnot(is.numeric(weights))

   score_names <- names(table_score)
   if (!setequal(score_names, names(weights)))
      stop("Weights do not match Zig-Components")

   if (nrow(table_score) == 0) return(numeric(0))
   if (nrow(table_score) == 1) return(c(1))

   # Extracts the scores and places them in a matrix
   score_lists <- sapply(score_names, function(comp){
      scores <- sapply(table_score[[comp]], function(view_comp) view_comp$score)
      as.numeric(scores)
   })
   scores_mat <- matrix(score_lists,
                        nrow = nrow(table_score),
                        ncol = ncol(table_score),
                        byrow = FALSE)
   colnames(scores_mat) <- score_names

   # Normalizes and replaces missing values by column averages
   scores_mat <- scale(scores_mat, center = T, scale = T)
   scores_mat[is.na(scores_mat)] <- 0

   # Aggregates
   weights_mat <- as.matrix(weights[score_names])
   agg_scores  <- as.numeric(scores_mat %*% weights_mat)

   # Re-normalizes to have scores between 0 (no difference) and 1 (largest diff)
   lowest_score  <- min(agg_scores, na.rm = T)
   highest_score <- max(agg_scores, na.rm = T)

   if (lowest_score == highest_score)
      return(rep(1, length(agg_scores)))

   old_means <- attr(scores_mat,"scaled:center")
   old_sds   <- attr(scores_mat,"scaled:scale")
   low_bounds <- ifelse(is.finite(old_sds), - old_means / old_sds, 0)
   theory_lowest_score <- sum(weights[score_names] * low_bounds[score_names])

   agg_scores <- (agg_scores - theory_lowest_score) /
                  (highest_score - theory_lowest_score)

   if (any(agg_scores < 0 | agg_scores > 1))
      stop("Something went wrong with the scoring!")

   return(agg_scores)
}


score_views <- function(views, group1, group2, data, zig_components){
   stopifnot(is.list(views))
   stopifnot(is.data.frame(data), nrow(data) >= 2)
   stopifnot(is.logical(group1), length(group1) == nrow(data), sum(group1) > 0)
   stopifnot(is.logical(group2), length(group2) == nrow(data), sum(group2) > 0)
   stopifnot(is.character(zig_components))

   # Checks that all columns exist
   col_exists <- unlist(views) %in% names(data)
   if (!all(col_exists)) stop('Column missing in dataset')

   # Checks and retrieves the zig_components
   zig_components_fun <- lapply(zig_components, function(zig_name){
      if (!exists(zig_name)) stop('Zig-Component ', zig_name,' not found')
      fun <- get(zig_name)
      if (!is.function(fun)) stop(zig_name, ' is not a function')
      return(fun)
   })

   # Separates the two populations
   in_sel  <- data[group1,,drop=F]
   out_sel <- data[group2,,drop=F]

   # Does the heavy lifting
   zig_structure <- lapply(zig_components_fun, function(zig_fun){
       lapply(views, zig_fun, in_sel, out_sel)
   })

   # Converts into a data frame
   names(zig_structure) <- names(zig_components)
   zig_structure_I <- lapply(zig_structure, I)
   zig_scores <- do.call(data.frame, zig_structure_I)

   return(zig_scores)
}

#################
# Main function #
#################
#' @export
findviews_to_compare <- function(group1, group2, data, max_cols=NULL){

   # Input checks
   if (is.matrix(data)) data <- data.frame(data)
   else if (!is.data.frame(data)) stop("Input data is not a data frame")
   if (nrow(data) < 2) stop("Data set is too small for Ziggy")

   stopifnot(is.logical(group1) & is.logical(group2))
   if (nrow(data) != length(group1) | nrow(data) != length(group2))
      stop("The size of the group description does not match the size of the data")
   if (sum(group1) == 0 | sum(group2) == 0)
      stop("The groups should contain at least 1 row")

   # Sets max_cols = log2(ncol(data)) if no value specified
   if (is.null(max_cols)) max_cols <- max(1, log2(ncol(data)))
   stopifnot(is.numeric(max_cols), max_cols >= 1)
   max_cols <- as.integer(max_cols)

   # Type detection and conversions
   # Flat columns = pimary keys, or columns with only 1 distinct value
   #cat('Processing the data.... ')
   preprocessed <- preprocess(data)
   data_num <- preprocessed$data_num
   data_cat <- preprocessed$data_cat
   excluded <- preprocessed$excluded

   # Creates views
   #cat('Creating the views.... ')
   views_num <- cluster_columns(data_num, max_cols, DEP_FUNC_NUM)
   views_cat <- cluster_columns(data_cat, max_cols, DEP_FUNC_CAT)

   # Dissimilarity analysis of each view
   #cat('Scoring the views.... ')
   zig_components_num <- score_views(views_num, group1, group2,
                                     data_num, ZIG_COMPONENTS_NUM)
   zig_components_cat <- score_views(views_cat, group1, group2,
                                     data_cat, ZIG_COMPONENTS_CAT)

   # Aggregates all the Zig-Components into one score
   zig_scores_num <- zig_aggregate(zig_components_num, WEIGHT_COMPONENTS_NUM)
   zig_scores_cat <- zig_aggregate(zig_components_cat, WEIGHT_COMPONENTS_CAT)

   # Ranks the views accordingly
   order_num <- order(zig_scores_num, decreasing = T)
   order_cat <- order(zig_scores_cat, decreasing = T)

   #cat('View creation done.\n')

   return(list(
      views_cat   = views_cat[order_cat],
      scores_cat  = zig_scores_cat[order_cat],
      details_cat = zig_components_cat[order_cat,,drop=FALSE],
      views_num   = views_num[order_num],
      scores_num  = zig_scores_num[order_num],
      details_num = zig_components_num[order_num,,drop=FALSE],
      excluded    = excluded
   ))
}

#' @export
ziggy_web <- function(group1, group2, data, max_cols=NULL, ...){
   ziggy_out    <- findviews_to_compare(group1, group2, data, max_cols)

   #cat('Starting server...\n')
   ziggy_app    <- create_ziggy_app(ziggy_out, group1, group2, data)
   shiny::runApp(ziggy_app, display.mode = "normal", ...)
}
