#############
# Constants #
#############
# Dependency calculation functions for each type of column
DEP_FUNC_NUM <- cor_matrix
DEP_FUNC_CAT <- cramerV_matrix

P_VALUE_ZIG <- 0.05

###########################
# View creation functions #
###########################
get_dend_attributes <- function(den, attr_name){
   stopifnot(class(den) == "dendrogram")
   stopifnot(class(attr_name) == "character")

   # Trivial cases: empty of terminal nodes
   if (length(den) == 0) return(numeric(0))
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
   under_thresh <- node_members <= max_size
   cand_splits <- node_heights[under_thresh]
   split_point <- max(cand_splits)

   # Cuts and fetches node labels
   subtrees <- cut(den, h = split_point)
   clusters <- lapply(subtrees$lower, get_dend_attributes, 'label')

   return(clusters)
}

cluster_columns <- function(data, max_cols, dependency_fun){
   stopifnot(is.data.frame(data), nrow(data) >= 2)
   stopifnot(is.numeric(max_cols))
   if (max_cols < 1) stop('max_cols must be at least one')
   stopifnot(is.function(dependency_fun))

   # Trivial case: 0 or 1 column
   if (ncol(data) == 0) return(list())
   if (ncol(data) == 1) return(list(names(data)))

   # Computes pairwise statistical dependencies
   dependency_mat <- dependency_fun(data)
   if (!is.matrix(dependency_mat) &
       !nrow(dependency_mat) == ncol(data) &
       !ncol(dependency_mat) == ncol(data))
      stop("Error during dependency matrix computations: wrong output")

   # Checks for NAs
   if (any(is.na(dependency_mat))){
      stop('NAs detected in dependency matrix')
   }

   # Runs complete link clustering on the remainder
   dist_obj <- as.dist(dependency_mat)
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
   if (sum(no_type) > 0){
      err_cols <- paste(names(data)[no_type])
      stop("Data type unknown for column(s):", err_cols)
   }

   # Separates columns of different types
   data_num <- data[,is_num_col,drop=F]
   data_cat <- data[,is_cat_col,drop=F]

   # Performs coercions if necessary
   to_cast <- sapply(data_cat, function(c) !is.factor(c))
   to_cast <- as.logical(to_cast)
   for (c in names(data_cat)[to_cast]) data_cat[[c]] <- as.factor(data_cat[[c]])

   # Gets flat columns
   is_flat_cat <- sapply(data_cat, function(col){
      n <- length(unique(col))
      return(n == nrow(data_cat) | n < 2)
   })
   is_flat_cat <- as.logical(is_flat_cat)
   flat_cols_cat <- names(data_cat)[is_flat_cat]

   is_flat_num <- sapply(data_num, function(col){
      n <- length(unique(col))
      return(n < 2)
   })
   is_flat_num <- as.logical(is_flat_num)
   flat_cols_num <- names(data_num)[is_flat_num]


   return(list(
      data_cat = data_cat,
      data_num = data_num,
      flat_cols_cat = flat_cols_cat,
      flat_cols_num = flat_cols_num
   ))
}

#################
# Main function #
#################
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

   if (is.null(max_cols)) max_cols <- max(1, log2(ncol(data)))
   stopifnot(is.numeric(max_cols), max_cols >= 1)
   max_cols <- as.integer(max_cols)

   # Type detection and conversions
   preprocessed <- preprocess(data)
   data_num <- preprocessed$data_num
   data_cat <- preprocessed$data_cat
   flat_cols_num <- preprocessed$flat_cols_num
   flat_cols_cat <- preprocessed$flat_cols_cat

   # View construction
   to_clust_num <- !colnames(data_num) %in% flat_cols_num
   to_clust_cat <- !colnames(data_cat) %in% flat_cols_cat

   col_clust_num <- cluster_columns(data_num[to_clust_num],
                                    max_cols,
                                    DEP_FUNC_NUM)
   col_clust_cat <- cluster_columns(data_cat[to_clust_cat],
                                    max_cols,
                                    DEP_FUNC_CAT)

   views_num <- c(as.list(flat_cols_num), col_clust_num)
   views_cat <- c(as.list(flat_cols_cat), col_clust_cat)

   return(list(
      views_cat = views_cat,
      scores_cat = data.frame(),
      views_num = views_num,
      scores_num = data.frame()
   ))
}
