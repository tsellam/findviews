######################################
# Zig-Dissimilarity for numeric data #
######################################
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

zig_means <- function(view, in_data, out_data) {
   stopifnot(is.data.frame(in_data))
   stopifnot(is.data.frame(out_data))
   stopifnot(is.character(view))

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

      # # Performs t.test
      # t_test_out <- tryCatch(
      #    t.test(col_in, col_out)$p.value,
      #    error=function(e) return(NA)
      # )

      # Returns
      # c(delta = delta, pvalue = t_test_out)
      delta
   })
   diffs <- as.matrix(diffs)
   names(diffs) <- view

   # Aggregates them
   agg_diffs <- mean(diffs, na.rm = T)
   if(!is.finite(agg_diffs)) agg_diffs <- NA

   # Generates description
   large_effects <- diffs[abs(diffs) > 0.2]
   tip <- if (length(large_effects) == 0){
      character(0)
   } else if (length(large_effects) >= 1){
      col_enum <- enumerate_char(names(large_effects))
      paste0('the difference between the means on ', col_enum)
   }

   list(
      score  = agg_diffs,
      detail = diffs,
      tip    = tip
   )

}

zig_sds <- function(view, in_data, out_data) {
   stopifnot(is.data.frame(in_data))
   stopifnot(is.data.frame(out_data))
   stopifnot(is.character(view))

   list(
      score  = c(),
      detail = c(),
      tip    = character(0)
   )

}

zig_corrs <- function(view, in_data, out_data) {
   stopifnot(is.data.frame(in_data))
   stopifnot(is.data.frame(out_data))
   stopifnot(is.character(view))

   list(
      score  = c(),
      detail = c(),
      tip    = character(0)
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
