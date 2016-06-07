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
      out <- paste0(out, ' and ', names(v[length(v)]))
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
      in_mean  <- mean(col_in)
      out_mean <- mean(col_out)
      in_sd  <- sd(col_in)
      out_sd <- sd(col_out)

      # Gets Glass Delta
      delta <- (in_mean - out_mean) / out_sd

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
   large_effects  <- colnames(diffs[,is_significant])
   tip <- if (length(large_effects) == 0){
      character(0)
   } else if (length(large_effects) >= 1){
      col_enum <- enumerate_char(large_effects)
      paste0('the difference between the means on ', col_enum)
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

      # Performs F test
      tryCatch({
         f_out <- var.test(col_in, col_out)
         c(size = as.numeric(f_out$statistic), pvalue = f_out$p.value)
      }, error=function(e) c(size = NA, pvalue = NA)
      )

   })
   diffs <- as.matrix(diffs)
   colnames(diffs) <- view

   # Aggregates them
   corr_F <- ifelse(diffs['size',] < 1,
                    sqrt(1 / diffs['size',]),
                    sqrt(diffs['size',]))
   agg_diffs <- mean(corr_F, na.rm = T)
   if(!is.finite(agg_diffs)) agg_diffs <- NA

   # Generates description
   is_significant <- (diffs['pvalue',] < P_VALUE_ZIG)
   large_effects  <- colnames(diffs[,is_significant])
   tip <- if (length(large_effects) == 0){
      character(0)
   } else if (length(large_effects) >= 1){
      col_enum <- enumerate_char(large_effects)
      paste0('the difference between the variances on ', col_enum)
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
   if (any(r < -1) | any(r > 1)) return(NA)
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
         detail = NULL,
         tip = character(0)
      ))

   # Computes correlation matrices
   cor_in  <- suppressWarnings(cor(view_in))
   cor_out <- suppressWarnings(cor(view_out))

   # Gets differences and aggregates into one score
   cor_diff <- fisher_transform(cor_in) - fisher_transform(cor_out)
   score <- mean(abs(cor_diff), na.rm = TRUE)
   if (!is.finite(score)) score <- NA

   # Gets p-values
   pvalues <- apply_2_matrices(cor_in, cor_out, corr_diff_test,
                               nrow(in_data), nrow(out_data))

   # Generates comments
   is_significant <- ( pvalues < P_VALUE_ZIG)
   tip <- if (!any(is_significant, na.rm = T)) {
      character(0)
   } else {
      sig_columns <- which_true_elements(is_significant)
      sig_columns_str <- sapply(sig_columns, paste, collapse = "/")
      sig_columns_str <- enumerate_char(sig_columns_str)
      tip <- paste0('Check the difference in correlations on these columns:',
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




##################################
# Generic View Analysis Function #
##################################
score_views <- function(views, target, data, zig_components){
   stopifnot(is.list(views))
   stopifnot(is.data.frame(data), nrow(data) >= 2)
   stopifnot(is.logical(target), length(target) == nrow(data), sum(target) > 0)

   col_exists <- unlist(views) %in% names(data)
   if (!all(col_exists)) stop('Column missing in dataset')

   # Separates the two populations
   in_sel  <- data[target,,drop=F]
   out_sel <- data[!target,,drop=F]

   # Does the heavy lifting
   zig_structure <- lapply(zig_components, function(zig_fun){
       lapply(views, zig_fun, in_sel, out_sel)
   })

   # Converts into a data frame
   names(zig_structure) <- names(zig_components)
   zig_structure <- lapply(zig_structure, I)
   zig_scores <- do.call(data.frame, zig_structure)

   return(zig_scores)
}
