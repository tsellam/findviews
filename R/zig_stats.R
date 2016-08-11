#########
# Utils #
#########
enumerate_char <- function(v){
   stopifnot(is.character(v))
   if (length(v) == 0) {
      return("")
   } else if (length(v) == 1){
      return(v)
   } else if (length(v) == 2){
      return(paste0(v[1], ' and ', v[2]))
   } else if (length(v) > 2){
      out <- paste0(v[1:length(v)-1], collapse = ', ')
      out <- paste0(out, ' and ', v[length(v)])
      return(out)
   }
}

# Applies fun to each couple (M1[i,j] M2[i,j])
apply_2_matrices <- function(M1, M2, fun, ...){
   stopifnot(is.matrix(M1) & is.matrix(M2))
   stopifnot(nrow(M1) == nrow(M2) & ncol(M1) == ncol(M2))
   stopifnot(is.function(fun))

   M_out <- matrix(NA, nrow=nrow(M1), ncol = ncol(M1))
   rownames(M_out) <- rownames(M1)
   colnames(M_out) <- colnames(M1)

   if (nrow(M1) == 0 | ncol(M1) == 0) return(M_out)

   for (i in 1:nrow(M_out))
      for (j in 1:ncol(M_out))
         M_out[i, j] = fun(M1[i,j], M2[i, j], ...)

   return(M_out)
}

# Returns the position of the TRUE elements in a matrix of booleans
which_true_elements <- function(M, deduplicate = T) {
   stopifnot(is.matrix(M) & is.logical(M))

   if (is.null(rownames(M)) | is.null(colnames(M))) return(NULL)
   if (nrow(M) < 1 | ncol(M) < 1) return(list())

   positions <- lapply(rownames(M), function(i){
      lapply(colnames(M), function(j){
         if (!is.finite(M[i,j])) return(NULL)
         if (M[i,j]) c(i, j) else NULL
      })
   })
   positions <- unlist(positions, recursive = F)
   positions <- positions[!sapply(positions, is.null)]

   if (deduplicate){
      positions <- lapply(positions, sort)
      positions <- unique(positions)
   }

   return(positions)
}



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

      # Gets Cohen's d
      agg_var    <- (in_count - 1) * in_var + (out_count - 1) * out_var
      pooled_var <- sqrt(agg_var / (in_count + out_count - 2))
      delta <- (in_mean - out_mean) / pooled_var

      # Performs t.test
      t_test_out <- tryCatch(
         t.test(col_in, col_out)$p.value,
         error=function(e) return(NA)
      )

      # Returns
      c(size = delta, pvalue = t_test_out)
   })
   diffs <- as.matrix(diffs)
   colnames(diffs) <- view

   # Aggregates them
   agg_diffs <- mean(abs(diffs['size',]), na.rm = T)
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
      sd_diss <- (s_in - s_out) / max(s_in, s_out)

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
   agg_diffs <- mean(abs(diffs['sd_diss',]), na.rm = T)
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
   cor_diff <- f_in - f_out
   score <- mean(abs(cor_diff), na.rm = TRUE)
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

   if (!setequal(names(table_score), names(weights)))
      stop("Weights do not match Zig-Components")

   if (nrow(table_score) == 0) return(numeric(0))

   scores <- sapply(1:nrow(table_score), function(i){

      # Extracts all the scores for view i
      view_scores <- sapply(1:ncol(table_score), function(j){
         table_score[[i,j]]$score
      })
      view_scores <- as.numeric(view_scores)
      names(view_scores) <- names(table_score)

      # Aggregates
      weighted_zigs <- sapply(names(view_scores), function(z){
         view_scores[z] * weights[z]
      })
      sum(weighted_zigs, na.rm = T)

   })

   return(scores)
}


score_views <- function(views, target, data, zig_components){
   stopifnot(is.list(views))
   stopifnot(is.data.frame(data), nrow(data) >= 2)
   stopifnot(is.logical(target), length(target) == nrow(data), sum(target) > 0)
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
   in_sel  <- data[target,,drop=F]
   out_sel <- data[!target,,drop=F]

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
