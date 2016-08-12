###########################
# View creation functions #
###########################
get_dend_attributes <- function(den, attr_name){
   stopifnot(class(den) == "dendrogram")
   stopifnot(class(attr_name) == "character")

   # Trivial cases: empty or terminal nodes
   if (length(den) == 0) return(c())
   if (length(den) == 1) return(attr(den, attr_name))

   # Recursive call is higher up in the hierarchy
   this_attr <- attr(den, attr_name)
   kids_attr <- unlist(lapply(den, get_dend_attributes, attr_name))
   return(c(this_attr, kids_attr))
}

cut_max_size <- function(den, max_size){
   stopifnot(class(den) == 'dendrogram')
   stopifnot(is.numeric(max_size))

   if (max_size < 1) stop('max_size must be at least one')
   if (length(den)==1) return(list(attr(den, 'label')))

   # Gets statistics for each node
   node_heights <- get_dend_attributes(den, 'height')
   node_members <- get_dend_attributes(den, 'members')

   # Detects split point
   over_thresh <- node_members >= max_size

   # If applicable, cuts and fetches node labels
   clusters <- if (!any(over_thresh)){
     get_dend_attributes(den, 'label')
   } else {
      split_point <- min(node_heights[over_thresh])
      subtrees <- cut(den, h = split_point)
      clusters <- lapply(subtrees$lower, get_dend_attributes, 'label')
   }

   return(clusters)
}

cluster_columns <- function(data, max_cols, dependency_name){
   stopifnot(is.data.frame(data), nrow(data) >= 2)
   stopifnot(is.numeric(max_cols))
   if (max_cols < 1) stop('max_cols must be at least one')
   stopifnot(is.character(dependency_name))

   # Checks and retrieves dependency function
   if (!exists(dependency_name))
      stop("Dependency function not found!")
   dependency_fun <- get(dependency_name)
   if (!is.function(dependency_fun))
      stop('Dependency function is not a function!')

   # Trivial case: 0 or 1 column
   if (ncol(data) == 0) return(list())
   if (ncol(data) == 1) return(list(names(data)))

   # Computes pairwise statistical dependencies
   dependency_mat <- dependency_fun(data)
   if (!is.matrix(dependency_mat) &
       !nrow(dependency_mat) == ncol(data) &
       !ncol(dependency_mat) == ncol(data))
      stop("Error during dependency matrix computations: wrong output")
   if (any(is.na(dependency_mat)))
      stop('NAs detected in dependency matrix')
   if (any(dependency_mat > 1 | dependency_mat < 0))
      stop('Dependency must vary between 0 and 1')

   # Runs complete link clustering on the remainder
   inv_dependency <- 1 - dependency_mat
   dist_obj <-  as.dist(inv_dependency)
   clust_out <- hclust(dist_obj, method = "complete")
   dendrogram <- as.dendrogram(clust_out)

   # Cuts the dendrogram
   clusters <- cut_max_size(dendrogram, max_cols)

   return(clusters)
}


#################
# Preprocessing #
#################
preprocess <- function(data){
   stopifnot(is.data.frame(data))

   # Type detection
   is_num_col <- sapply(data, function(col) is.numeric(col))
   is_num_col <- as.logical(is_num_col)
   is_cat_col <- sapply(data, function(col) is.factor(col) |
                                            is.character(col) |
                                            is.logical(col))
   is_cat_col <- as.logical(is_cat_col)
   no_type <- !(is_num_col | is_cat_col)

   # Separates columns of different types
   data_num <- data[,is_num_col,drop=F]
   data_cat <- data[,is_cat_col,drop=F]


   # Performs coercions if necessary
   to_cast <- sapply(data_cat, function(c) !is.factor(c))
   to_cast <- as.logical(to_cast)
   for (c in names(data_cat)[to_cast]) data_cat[[c]] <- as.factor(data_cat[[c]])


   # Removes flat columns
   is_flat_cat <- sapply(data_cat, function(col){
      n <- length(unique(col))
      return(n == nrow(data_cat) | n < 2)
   })
   is_flat_cat   <- as.logical(is_flat_cat)
   flat_cols_cat <- names(data_cat)[is_flat_cat]
   data_cat      <- data_cat[, !is_flat_cat, drop=FALSE]

   is_flat_num <- sapply(data_num, function(col){
      n <- length(unique(col))
      return(n < 2)
   })
   is_flat_num <- as.logical(is_flat_num)
   flat_cols_num <- names(data_num)[is_flat_num]
   data_num      <- data_num[, !is_flat_num, drop=FALSE]

   # Gets rejected columns
   unknown_type_cols <- names(data)[no_type]

   return(list(
      data_cat = data_cat,
      data_num = data_num,
      excluded = list(
         unknown_type = unknown_type_cols,
         flat_num     = flat_cols_num,
         flat_cat     = flat_cols_cat
      )
   ))
}

#################
# Main function #
#################
#' @export
characteristic_views <- function(data, target, max_cols=NULL){

   # Input checks
   if (is.matrix(data)) data <- data.frame(data)
   else if (!is.data.frame(data)) stop("Input data is not a data frame")
   if (nrow(data) < 2) stop("Data set is too small for Ziggy")

   stopifnot(is.logical(target))
   if (nrow(data) != length(target))
      stop("The target vector is not the same size as the data")
   if (sum(target) == 0)
      stop("The selection should contain between 1 and N-1 items")

   # Sets max_cols = log2(ncol(data)) is no value specified
   if (is.null(max_cols)) max_cols <- max(1, log2(ncol(data)))
   stopifnot(is.numeric(max_cols), max_cols >= 1)
   max_cols <- as.integer(max_cols)

   # Type detection and conversions
   # Flat columns = pimary keys, or columns with only 1 distinct value
   preprocessed <- preprocess(data)
   data_num <- preprocessed$data_num
   data_cat <- preprocessed$data_cat
   excluded <- preprocessed$excluded

   # Creates views
   views_num <- cluster_columns(data_num, max_cols, DEP_FUNC_NUM)
   views_cat <- cluster_columns(data_cat, max_cols, DEP_FUNC_CAT)

   # Dissimilarity analysis of each view
   zig_components_num <- score_views(views_num, target, data_num, ZIG_COMPONENTS_NUM)
   zig_components_cat <- score_views(views_cat, target, data_cat, ZIG_COMPONENTS_CAT)

   # Aggregates all the Zig-Components into one score
   zig_scores_num <- zig_aggregate(zig_components_num, WEIGHT_COMPONENTS_NUM)
   zig_scores_cat <- zig_aggregate(zig_components_cat, WEIGHT_COMPONENTS_CAT)

   # Ranks the views accordingly
   order_num <- order(zig_scores_num, decreasing = T)
   order_cat <- order(zig_scores_cat, decreasing = T)

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
ziggy_web <- function(data, target, max_cols=NULL, ...){
   ziggy_out    <- characteristic_views(data, target, max_cols)
   ziggy_app    <- create_ziggy_app(ziggy_out, data, target)
   shiny::runApp(ziggy_app, display.mode = "normal", ...)
}
