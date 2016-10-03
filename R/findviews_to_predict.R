######################
# Information Theory #
######################
sum_prod_log <- function(probs){
   if (length(probs) == 0) return(0)
   if (all(is.na(probs))) return(NA_real_)

   stopifnot(is.numeric(probs))
   probs <- na.omit(probs)
   probs <- probs[probs!=0]
   sum(sapply(probs, function(x){x * log2(x)})) * -1
}

# Entropy of a series
entropy <- function(s1){
   if (length(s1) == 0) return(NA_real_)
   if (all(is.na(s1))) return(NA_real_)

   stopifnot(is.factor(s1))
   s1 <- na.omit(s1)
   freq <- table(s1)[]/length(s1)
   sum_prod_log(freq)
}

# Joint entropy
joint_entropy <- function(s1, s2){
   stopifnot(length(s1) == length(s2))
   if (length(s1) == 0) return(NA_real_)

   NAs <- is.na(s1) | is.na(s2)
   if (all(NAs)) return(NA_real_)
   s1 <- s1[!NAs]
   s2 <- s2[!NAs]

   stopifnot(is.factor(s1) & is.factor(s2))
   freq <- c(table(s1,s2)/length(s1))
   sum_prod_log(freq)
}

# Mutual information
mutual_information <- function(s1, s2){
   stopifnot(length(s1) == length(s2))
   if (length(s1) == 0) return(NA_real_)

   NAs <- is.na(s1) | is.na(s2)
   if (all(NAs)) return(NA_real_)
   s1 <- s1[!NAs]
   s2 <- s2[!NAs]

   stopifnot(is.factor(s1) & is.factor(s2))
   entropy(s1) + entropy(s2) - joint_entropy(s1,s2)
}

# Multivariate mutual information
multi_mutual_information <- function(df1, df2){
   stopifnot(is.data.frame(df1) & all(sapply(df1, is.factor))  |
                is.factor(df1))
   stopifnot(is.data.frame(df2) & all(sapply(df2, is.factor))  |
                is.factor(df2))

   s1 <- if (is.factor(df1)) df1
         else merge_factors(df1)

   s2 <- if (is.factor(df2)) df2
         else merge_factors(df2)

   mutual_information(s1, s2)
}

####################
# Scoring function #
####################
score_predictive_cat <- function(views, data, target){
   stopifnot(is.list(views))
   stopifnot(is.data.frame(data))
   stopifnot(all(unlist(views) %in% names(data)))
   stopifnot(all(sapply(data, is.factor)))
   stopifnot(is.factor(target))

   if (length(views) == 0) return(numeric(0))

   scores <- sapply(views, function(cols){
      multi_mutual_information(data[,cols,drop=F], target)
   })

   scores
}

score_predictive_num <- function(views, data, target){
   stopifnot(is.list(views))
   stopifnot(is.data.frame(data))
   stopifnot(all(unlist(views) %in% names(data)))
   stopifnot(all(sapply(data, is.numeric)))
   stopifnot(is.factor(target))

   if (length(views) == 0) return(numeric(0))

   # Discretizes the columns of data frame
   view_cols <- unique(unlist(views))
   discretized_data <- lapply(view_cols, function(colname){
      bin_equiwidth(data[[colname]], NBINS_CONT_VARIABLES)
   })
   discretized_data <- as.data.frame(discretized_data, col.names = view_cols)

   # Computes the scores
   scores <- sapply(views, function(cols){
      multi_mutual_information(discretized_data[,cols,drop=F], target)
   })

   scores
}

#################
# Main Function #
#################
preprocess_target <- function(target_data, nbins=4){

   # Case empty - who knows, this could be useful
   if (length(target_data) == 0) return(factor())

   # Case factor
   if (is.factor(target_data)){
      if (length(unique(target_data)) > DISTINCT_VALS_THRES)
         warning(paste0("The target vector contains many distinct values,",
                        " the computations will be slow!"))
      return(target_data)
   }

   # Case string
   if (is.character(target_data)){
      if (length(unique(target_data)) > DISTINCT_VALS_THRES)
         warning(paste0("The target vector contains many distinct values,",
                        " the computations will be slow!"))
      return(factor(target_data))
   }

   # Case boolean
   if (is.logical(target_data)) return(factor(target_data))

   # Case numeric
   if (is.numeric(target_data)){
      #cat("Numeric target detected, I am discretizing it\n")
      return(bin_equiwidth(target_data, nbins))
   }

   stop("Unknown data type for target column!")
}

#' Views of a multidimensional dataset, ranked by their prediction power, non-Shiny version.
#'
#' \code{findviews_to_predict_core} detects groups of mutually dependent
#' columns, and ranks them by their predictive power.  It produces the same
#' results as \code{\link[stats]{findviews_to_predict}}, but does \emph{not}
#' present them with a Shiny app.
#'
#'
#' The function \code{findviews_to_predict_core} takes a data set and a target
#' variable as input. It detects clusters of statistically dependent columns in
#' the data set - e.g., views - and ranks those groups according to how well
#' they predict the target variable.
#' See the documentation of \code{\link[stats]{findviews_to_predict}} for more
#' details.
#'
#' The  difference between \code{\link[stats]{findviews_to_predict}} and
#' \code{\link[stats]{findviews_to_predict_core}} is that the former presents its results
#' with a Shiny app, while the latter simply outputs them as R stuctures.
#'
#' @inheritParams findviews
#' @inheritParams findviews_to_predict
#'
#'
#' @examples
#' findviews_to_predict_core('mpg', mtcars)
#' findviews_to_predict_core('mpg', mtcars, view_size_max = 4)
#'
#' @export
findviews_to_predict_core <- function(target, data, view_size_max=NULL,
                                      clust_method="complete"){

   if (!is.character(target))
      stop("The target must be a column name.")
   if (length(target) != 1)
      stop("The argument target must contain exactly one column name")
   if (!(is.data.frame(data) | is.matrix(data)))
      stop("Input data must be a matrix or a data frame.")
   if (!target %in% names(data))
      stop("I could not find the target column in the data")

   # Separates the target from the rest of the data
   target_data <- data[[target]]
   data <- data[!names(data) %in% target]
   if (nrow(data) < 1) stop("The input data is empty")

   # Removes the missing values
   target_NAs <- is.na(target_data)
   if (all(target_NAs)) stop("The target column contains only NAs!")
   target_data <- target_data[!target_NAs]
   data <- data[!target_NAs,,drop=F]

   # If necessary, discretizes the target column
   target_data <- preprocess_target(target_data, NBINS_CONT_VARIABLES)

   # Creates the views
   data_and_views <- findviews_trunk(data, view_size_max, clust_method)
   data_num  <- data_and_views$data_num
   views_num <- data_and_views$views_num
   data_cat  <- data_and_views$data_cat
   views_cat <- data_and_views$views_cat
   excluded  <- data_and_views$excluded
   sampled_rows <- data_and_views$sampled_rows

   # Subsamples the target if necessary
   sampled_target_data <- if (!is.na(sampled_rows)) target_data[sampled_rows]
                          else target_data

   # Aggregates all the Diff-Components into one score
   prediction_scores_num <- score_predictive_num(views_num, data_num,
                                                 sampled_target_data)
   prediction_scores_cat <- score_predictive_cat(views_cat, data_cat,
                                                 sampled_target_data)

   # Ranks the views accordingly
   order_num <- order(prediction_scores_num, decreasing = T, na.last = T)
   order_cat <- order(prediction_scores_cat, decreasing = T, na.last = T)

   return(list(
      views_num   = views_num[order_num],
      scores_num  = prediction_scores_num[order_num],
      details_num = NA,
      views_cat   = views_cat[order_cat],
      scores_cat  = prediction_scores_cat[order_cat],
      details_cat = NA,
      excluded    = excluded,
      target_data = target_data
   ))
}
