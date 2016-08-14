###############################
# Dissimilarity calculations  #
###############################
#---------------------------------------------#
# Difference between the means with Cohen's D #
#---------------------------------------------#
diff_means <- function(view, g1_data, g2_data) {
   stopifnot(is.data.frame(g1_data))
   stopifnot(is.data.frame(g2_data))
   stopifnot(is.character(view))
   stopifnot(length(view)>0)

   # Computes the difference between the means for each column
   diffs <- sapply(view, function(col){

      # Scrubs missing values
      col_g1 <- na.omit(g1_data[[col]])
      col_g2 <- na.omit(g2_data[[col]])

      # Gets basic stats
      g1_count <- length(col_g1)
      g2_count<- length(col_g2)
      g1_mean  <- mean(col_g1)
      g2_mean <- mean(col_g2)
      g1_var   <- var(col_g1)
      g2_var  <- var(col_g2)

      if (g1_count == 0 | g2_count == 0){
         delta <- NA
         t_test_g2 <- NA

      } else if (g1_count == 1 & g2_count == 1){
         delta <- abs((g1_mean - g2_mean))
         t_test_g2 <- NA

      } else if (g1_count > 1 & g2_count == 1){
         delta <- abs((g2_mean - g1_mean) / g1_var)
         t_test_g2 <- NA

      } else if (g1_count == 1 & g2_count > 1){
         delta <- abs((g1_mean - g2_mean) / g2_var)
         t_test_g2 <- NA

      } else if (g1_count > 1 & g2_count > 1){
         # Gets Cohen's d
         agg_var    <- (g1_count - 1) * g1_var + (g2_count - 1) * g2_var
         pooled_var <- sqrt(agg_var / (g1_count + g2_count - 2))
         delta <- abs((g1_mean - g2_mean) / pooled_var)

         # Performs t.test
         t_test_g2 <- tryCatch(
            t.test(col_g1, col_g2)$p.value,
            error=function(e) return(NA)
         )
      }

      # Returns
      c(size = delta, pvalue = t_test_g2)
   })
   diffs <- as.matrix(diffs)
   colnames(diffs) <- view

   # Aggregates them
   agg_diffs <- mean(diffs['size',], na.rm = T)
   if(!is.finite(agg_diffs)) agg_diffs <- NA

   # Generates description
   is_significant <- (diffs['pvalue',] < P_VALUE_DIFF)
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
diff_sds <- function(view, g1_data, g2_data) {
   stopifnot(is.data.frame(g1_data))
   stopifnot(is.data.frame(g2_data))
   stopifnot(is.character(view))
   stopifnot(length(view)>0)

   # Computes the ratio between the variances for each column
   diffs <- sapply(view, function(col){

      # Scrubs missing values
      col_g1 <- na.omit(g1_data[[col]])
      col_g2 <- na.omit(g2_data[[col]])

      # Computes difference between SDs
      s_g1  <- sd(col_g1)
      s_g2 <- sd(col_g2)
      sd_diss <- abs((s_g1 - s_g2) / max(s_g1, s_g2))

      # Performs F test
      pvalue <- tryCatch(var.test(col_g1, col_g2)$p.value,
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
   is_significant <- (diffs['pvalue',] < P_VALUE_DIFF)
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

diff_corr <- function(view, g1_data, g2_data) {
   stopifnot(is.data.frame(g1_data))
   stopifnot(is.data.frame(g2_data))
   stopifnot(is.character(view))

   # Eliminates missing values
   view_g1 <- na.omit(g1_data[,view, drop=F])
   view_g2 <- na.omit(g2_data[,view, drop=F])

   if (length(attr(view_g1, 'na.action')) > 0.3 * nrow(g1_data) |
       length(attr(view_g2, 'na.action')) > 0.3 * nrow(g2_data))
      warning('Lots of rows with NAs removed in view', view)

   if (nrow(view_g1) == 0 | nrow(view_g2) == 0)
      return(list(
         score  = NA,
         detail = list(),
         tip = NA
      ))

   # Computes correlation matrices
   cor_g1  <- suppressWarnings(cor(view_g1))
   cor_g2 <- suppressWarnings(cor(view_g2))

   # Gets differences and aggregates into one score
   f_g1  <- apply(cor_g1, c(1,2), fisher_transform)
   f_g2 <- apply(cor_g2, c(1,2), fisher_transform)
   cor_diff <- abs(f_g1 - f_g2)
   score <- mean(cor_diff, na.rm = TRUE)
   if (!is.finite(score)) score <- NA

   # Gets p-values
   pvalues <- apply_2_matrices(cor_g1, cor_g2, corr_diff_test,
                               nrow(g1_data), nrow(g2_data))

   # Generates comments
   is_significant <- ( pvalues < P_VALUE_DIFF)
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
      detail = list(cor_g1  = cor_g1,
                    cor_g2 = cor_g2,
                    pvalues = pvalues),
      tip = tip
   )
}


#----------------------------------#
# Dissimilarities for nominal data #
#----------------------------------#
hist_diss_score <- function(table_g1, table_g2){
   # First, aligns tables
   g1_minus_g2 <- setdiff(names(table_g1), names(table_g2))
   table_g2[g1_minus_g2] <- 0

   g2_minus_g1 <- setdiff(names(table_g2), names(table_g1))
   table_g1[g2_minus_g1] <- 0

   table_g2 <- table_g2[sort(names(table_g2))]
   table_g1 <- table_g1[sort(names(table_g1))]

   stopifnot(names(table_g1) == names(table_g2))

   # Then, computes the Euclidean distance between the two histograms
   hist_g1 <- table_g1 / sum(table_g1)
   hist_g2 <- table_g2/ sum(table_g2)

   diss_score <- sqrt(sum((hist_g2 - hist_g1)^2))

   return(diss_score)
}

# Performs Chi-2 test
# WARNING! this test is not symetric
# because it ignores all the levels in table_g1 which are not in table_g2
wrap_chi_squared <- function(table_g1, table_g2){

   # Detects what type of test to use
   small_sample <- (any(table_g2 < 5) | any(table_g1 < 5))

   # Aligns the tables
   g2_minus_g1 <- setdiff(names(table_g2), names(table_g1))
   table_g1[g2_minus_g1] <- 0

   g1_minus_g2 <- setdiff(names(table_g1), names(table_g2))
   table_g1 <- table_g1[!names(table_g1) %in% g1_minus_g2]

   table_g2 <- table_g2[sort(names(table_g2))]
   table_g1  <- table_g1[sort(names(table_g1))]

   stopifnot(names(table_g1) == names(table_g2))

   # Runs the test
   chisq_g2 <- if (small_sample){
      chisq.test(x=table_g1, p=table_g2,
                 simulate.p.value = TRUE, rescale.p = TRUE, B = 100)
   } else {
      chisq.test(x=table_g1, p=table_g2, rescale.p = TRUE)
   }
   if (!grepl("given probabilities", chisq_g2$method))
       stop("Something went wrong with Chi-Squared calculations")

   # Extracts residuals
   residual_pvalues <- 2*pnorm(-abs(chisq_g2$stdres))
   names(residual_pvalues) <- names(table_g1)

   return(list(
      chi2             = as.numeric(chisq_g2$statistic),
      pvalue           = chisq_g2$p.value,
      residual_pvalues = residual_pvalues
   ))

}

comment_chi_squared_analysis <- function(chisq_results, max_levels = 3){
   stopifnot(is.list(chisq_results))

   comments_per_variable <- sapply(names(chisq_results), function(col){
      # Gets the results of Chi-Squared test
      results <- chisq_results[[col]]
      # Discards if non significant
      if (!is.finite(results$pvalue)) return(NULL)
      if (results$pvalue > P_VALUE_DIFF) return(NULL)

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

diff_histogram <- function(view, g1_data, g2_data) {
   stopifnot(is.data.frame(g1_data))
   stopifnot(is.data.frame(g2_data))
   stopifnot(is.character(view))
   stopifnot(length(view)>0)

   # Computes and compares the histograms
   chisq_analysis <- lapply(view, function(v){
      hist_g1  <- table(g1_data[[v]],  useNA = "no")
      hist_g2 <- table(g2_data[[v]], useNA = "no")

      diss_score  <- hist_diss_score(hist_g1, hist_g2)

      # Caution: the chi-squared test is not symmetric
      # Runs the test both direction and keeps the worst p-value
      chi_squared_1to2 <- wrap_chi_squared(hist_g1, hist_g2)
      chi_squared_2to1 <- wrap_chi_squared(hist_g2, hist_g1)
      test_results <- list(chi_squared_1to2, chi_squared_2to1)

      pvalues <- sapply(test_results, function(r) r$pvalue)
      to_keep <- max(which.max(pvalues), 1, na.rm = T)
      chi_squared <- test_results[[to_keep]]

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

##################
# Main functions #
##################
aggregate_differences <- function(table_score, weights){
   stopifnot(is.data.frame(table_score))
   stopifnot(is.numeric(weights))

   score_names <- names(table_score)
   if (!setequal(score_names, names(weights)))
      stop("Weights do not match Diff-Components")

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


score_views <- function(views, group1, group2, data, diff_components){
   stopifnot(is.list(views))
   stopifnot(is.data.frame(data), nrow(data) >= 2)
   stopifnot(is.logical(group1), length(group1) == nrow(data), sum(group1) > 0)
   stopifnot(is.logical(group2), length(group2) == nrow(data), sum(group2) > 0)
   stopifnot(is.character(diff_components))

   # Checks that all columns exist
   col_exists <- unlist(views) %in% names(data)
   if (!all(col_exists)) stop('Column missing in dataset')

   # Checks and retrieves the diff_components
   diff_components_fun <- lapply(diff_components, function(diff_name){
      if (!exists(diff_name)) stop('Diff-Component ', diff_name,' not found')
      fun <- get(diff_name)
      if (!is.function(fun)) stop(diff_name, ' is not a function')
      return(fun)
   })

   # Separates the two populations
   g1_sel  <- data[group1,,drop=F]
   g2_sel <- data[group2,,drop=F]

   # Does the heavy lifting
   diff_structure <- lapply(diff_components_fun, function(diff_fun){
       lapply(views, diff_fun, g1_sel, g2_sel)
   })

   # Converts into a data frame
   names(diff_structure) <- names(diff_components)
   diff_structure_I <- lapply(diff_structure, I)
   diff_scores <- do.call(data.frame, diff_structure_I)

   return(diff_scores)
}


#' @export
findviews_to_compare_core <- function(group1, group2, data, view_size_max=NULL){

   if (!(is.logical(group1) & is.logical(group2)))
      stop('The input variables group1 and group2 must be vectors of booleans')
   if (nrow(data) != length(group1) | nrow(data) != length(group2))
      stop("The size of the group description does not match the size of the data")
   if (sum(group1) == 0 | sum(group2) == 0)
      stop("The groups should contain at least 1 row")
   if (all(group1 == group2))
      stop("The groups to be compared are strictly identical.")

   # Checks all the other parameters and creates the views
   data_and_views <- findviews_trunk(data, view_size_max)
   data_num  <- data_and_views$data_num
   views_num <- data_and_views$views_num
   data_cat  <- data_and_views$data_cat
   views_cat <- data_and_views$views_cat
   excluded  <- data_and_views$excluded

   # Dissimilarity analysis of each view
   #cat('Scoring the views.... ')
   diff_components_num <- score_views(views_num, group1, group2,
                                     data_num, DIFF_COMPONENTS_NUM)
   diff_components_cat <- score_views(views_cat, group1, group2,
                                     data_cat, DIFF_COMPONENTS_CAT)

   # Aggregates all the Diff-Components into one score
   diff_scores_num <- aggregate_differences(diff_components_num,
                                           WEIGHT_COMPONENTS_NUM)
   diff_scores_cat <- aggregate_differences(diff_components_cat,
                                           WEIGHT_COMPONENTS_CAT)

   # Ranks the views accordingly
   order_num <- order(diff_scores_num, decreasing = T)
   order_cat <- order(diff_scores_cat, decreasing = T)

   return(list(
      views_cat   = views_cat[order_cat],
      scores_cat  = diff_scores_cat[order_cat],
      details_cat = diff_components_cat[order_cat,,drop=FALSE],
      views_num   = views_num[order_num],
      scores_num  = diff_scores_num[order_num],
      details_num = diff_components_num[order_num,,drop=FALSE],
      excluded    = excluded
   ))
}

