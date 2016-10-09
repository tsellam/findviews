score_influence <- function(views, dep_mat){
   stopifnot(is.list(views))
   stopifnot(is.matrix(dep_mat) & nrow(dep_mat) == ncol(dep_mat))
   stopifnot(all(unlist(views) %in% colnames(dep_mat)))

   if (length(views) == 0) return(numeric(0))

   scores <- sapply(views, function(cols){
      out_weights <- dep_mat[!rownames(dep_mat) %in% cols, cols]
      if (length(out_weights) == 0) return(NA)
      sum(out_weights, na.rm=T)
   })
   scores <- as.numeric(scores)

   scores
}


#' Views of a multidimensional dataset.
#'
#' \code{findviews} detects and plots groups of mutually dependent columns.
#' It is based on Shiny and ggplot.
#'
#
#' The function \code{findviews} takes a data frame or a matrix as input. It
#' computes the pairwise dependency between the columns, detects clusters in the
#' resulting structure and displays the results with a Shiny app.
#'
#' \code{findviews} processes numerical and categorical data separately. It excludes
#' the columns with only one value, the columns in which all the values are
#' distinct (e.g., primary keys), and the columns with more than 75\% missing values.
#'
#' \code{findviews} computes the dependency between the columns differently
#' depending on their type. It uses Pearson's coefficient of correlation for
#' numerical data, and Cramer's V for categorical data.
#'
#' To cluster the columns, \code{findviews} uses the function
#' \code{\link[stats]{hclust}}, R's implementation of agglomerative hierarchical
#' clustering. The parameter \code{clust_method} specifies which flavor of
#' agglomerative clustering to use. The number of clusters is determined by the
#' parameter \code{view_size_max}.
#'
#'
#' @param data Data frame or matrix to be processed
#' @param view_size_max Maximum number of columns in the views. If set to
#'   \code{NULL}, findviews uses \code{log2(ncol(data))}, rounded upwards and
#'   capped at 5.
#' @param clust_method Character describing a clustering method, used internally
#'   by \code{\link[stats]{hclust}}. Example values are "complete", "single" or
#'   "average".
#' @param ... Optional Shiny parameters, used in Shiny's
#'   \code{\link[shiny]{runApp}} function.
#'
#' @examples
#' \dontrun{
#' findviews(mtcars)
#' findviews(mtcars, view_size_max = 4,  port = 7000)
#' }
#'
#' @export
findviews <- function(data, view_size_max=NULL, clust_method="complete", ...){
   # Generates the views
   fdviews_out <- findviews_core(data, view_size_max, clust_method)

   # Creates and launches the Shiny server
   data_name <- deparse(substitute(data))
   fdviews_app <- create_fdviews_app(fdviews_out, "findviews", data, data_name)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}


#' Views of a multidimensional dataset, non-Shiny version
#'
#' \code{findviews_core} generates views of a multidimensional data set. It
#' produces the same results as \code{\link{findviews}}, but does
#' \emph{not} present them with a Shiny app.
#'
#' \code{findviews_core} takes a data frame or a matrix as input. It computes the
#' pairwise dependency between the columns and detects clusters in the resulting
#' structure. See the documentation of \code{\link{findviews}} for more
#' details.
#'
#' The  difference between \code{\link{findviews}} and
#' \code{\link{findviews_core}} is that the former presents its results
#' with a Shiny app, while the latter simply outputs them as R stuctures.
#'
#' @inheritParams findviews
#'
#' @examples
#' \dontrun{
#' findviews_core(mtcars)
#' findviews_core(mtcars, view_size_max = 4)
#' }
#'
#' @export
findviews_core <- function(data, view_size_max=NULL, clust_method="complete"){

   # Creates the views
   data_and_views <- findviews_trunk(data, view_size_max, clust_method)
   dep_mat_num <- data_and_views$dependency_mat_num
   views_num   <- data_and_views$views_num
   dep_mat_cat <- data_and_views$dependency_mat_cat
   views_cat   <- data_and_views$views_cat
   excluded    <- data_and_views$excluded
   sampled_rows<- data_and_views$sampled_rows

   # Aggregates all the Diff-Components into one score
   influence_scores_num <- score_influence(views_num, dep_mat_num)
   influence_scores_cat <- score_influence(views_cat, dep_mat_cat)

   # Ranks the views accordingly
   order_num <- order(influence_scores_num, decreasing = T, na.last = T)
   order_cat <- order(influence_scores_cat, decreasing = T, na.last = T)

   return(list(
      views_cat   = views_cat[order_cat],
      scores_cat  = influence_scores_cat[order_cat],
      details_cat = NA,
      views_num   = views_num[order_num],
      scores_num  = influence_scores_num[order_num],
      details_num = NA,
      excluded    = excluded,
      sampled_rows = sampled_rows
   ))
}
