score_influence <- function(views, dep_mat){
   stopifnot(is.list(views))
   stopifnot(is.matrix(dep_mat) & nrow(dep_mat) == ncol(dep_mat))
   stopifnot(all(unlist(views) %in% colnames(dep_mat)))

   if (length(views) == 0) return(numeric(0))

   scores <- sapply(views, function(cols){
      out_weights <- dep_mat[!rownames(dep_mat) %in% cols, cols]
      if (length(out_weights) == 0) return(NA)
      mean(out_weights)
   })
   scores <- as.numeric(scores)

   scores
}


#' @export
findviews_core <- function(data, view_size_max=NULL, clust_method="complete"){

   # Creates the views
   data_and_views <- findviews_trunk(data, view_size_max, clust_method)
   dep_mat_num <- data_and_views$dependency_mat_num
   views_num   <- data_and_views$views_num
   dep_mat_cat <- data_and_views$dependency_mat_cat
   views_cat   <- data_and_views$views_cat
   excluded    <- data_and_views$excluded

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
      excluded    = excluded
   ))
}
