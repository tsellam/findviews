#################
# Preprocessing #
#################
preprocess <- function(data){
   stopifnot(is.data.frame(data))

   # Subsamples if needed
  if (nrow(data) > SAMPLE_SIZE){
      warning('View creation: the dataframe contains more that ',
              SAMPLE_SIZE, ' rows, I am subsampling the data')
      sampled_rows <- sample(1:nrow(data), SAMPLE_SIZE, F)
      data <- data[sampled_rows,,drop=F]
   } else {
      sampled_rows <-  NA
   }

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
   for (c in names(data_cat)[to_cast])
      data_cat[[c]] <- as.factor(data_cat[[c]])

   # Removes flat columns
   is_flat_cat <- sapply(data_cat, function(col){
      n <- length(na.omit(unique(col)))
      return(n/nrow(data_cat) > .9 | n < 2)
   })
   is_flat_cat   <- as.logical(is_flat_cat)
   flat_cols_cat <- names(data_cat)[is_flat_cat]
   data_cat      <- data_cat[, !is_flat_cat, drop=FALSE]

   is_flat_num <- sapply(data_num, function(col){
      n <- length(na.omit(unique(col)))
      return(n < 2)
   })
   is_flat_num <- as.logical(is_flat_num)
   flat_cols_num <- names(data_num)[is_flat_num]
   data_num      <- data_num[, !is_flat_num, drop=FALSE]

   # Removes sparse columns
   is_sparse_cat <- sapply(data_cat, function(col){
      n <- length(na.omit(col))
      return(n/nrow(data_cat) < .25 | n < 3)
   })
   is_sparse_cat   <- as.logical(is_sparse_cat)
   sparse_cols_cat <- names(data_cat)[is_sparse_cat]
   data_cat      <- data_cat[, !is_sparse_cat, drop=FALSE]

   is_sparse_num <- sapply(data_num, function(col){
      n <- length(na.omit(col))
      return(n/nrow(data_num) < .25 | n < 3)
   })
   is_sparse_num   <- as.logical(is_sparse_num)
   sparse_cols_num <- names(data_num)[is_sparse_num]
   data_num      <- data_num[, !is_sparse_num, drop=FALSE]

   # Gets rejected columns
   unknown_type_cols <- names(data)[no_type]

   return(list(
      data_cat = data_cat,
      data_num = data_num,
      excluded = list(
         unknown_type = unknown_type_cols,
         flat_num     = flat_cols_num,
         flat_cat     = flat_cols_cat,
         sparse_cat   = sparse_cols_cat,
         sparse_num   = sparse_cols_num
      ),
      sampled_rows = sampled_rows
   ))
}

##################################
# Dependency matrix computations #
##################################
#--------------------------------------#
# For numeric data: simple correlation #
#--------------------------------------#
cor_matrix <- function(data){
   stopifnot(is.data.frame(data))

   if (nrow(data) < 2)
      stop('Computing cor matrix with less than two rows!')
   if (!all(sapply(data, is.numeric)))
      stop('Attempting to compute correlation on non-numeric data')

   if (ncol(data) == 0) return(matrix(nrow=0, ncol=0))
   if (ncol(data) == 1){
      m <- matrix(1)
      colnames(m) <- rownames(m) <- names(data)
      return(m)
   }

   abs(cor(data, use = "pairwise.complete.obs"))
}

#----------------------------------#
# For categorical data: Cramer's V #
#----------------------------------#
cramerV <- function(v1, v2){
   stopifnot(length(v1) > 0, length(v2) > 0)
   stopifnot(length(v1) == length(v2))

   if (!is.factor(v1)) v1 <- factor(v1)
   if (!is.factor(v2)) v2 <- factor(v2)

   r <- length(levels(v1))
   c <- length(levels(v2))

   if (r < 2 | c < 2) return(NA)

   chi2_out <- suppressWarnings(chisq.test(v1, v2, correct = F))
   chi2 <- as.numeric(chi2_out$stat)
   norm <- min(r - 1, c - 1)
   V <- sqrt(chi2 / (length(v1) * norm))

   return(V)
}

cramerV_matrix <- function(data){
   stopifnot(is.data.frame(data))

   if (nrow(data) < 2)
      stop('Computing Cramer V matrix with less than two rows!')
   if (!all(sapply(data, is.factor)))
      stop('Attempting to compute Cramer V on non factor data')

   # Trivial cases
   if (ncol(data) == 0) return(matrix(nrow=0, ncol=0))
   if (ncol(data) == 1){
      m <- matrix(1)
      colnames(m) <- rownames(m) <- names(data)
      return(m)
   }

   # Heavy lifting: computes dependecies
   dep_vect <- sapply(1:(ncol(data)-1), function(j1){
      sapply((j1+1):ncol(data), function(j2){
         cramerV(data[,j1], data[,j2])
      })
   })
   dep_vect <- as.numeric(unlist(dep_vect))

   # Creates the dependency matrix
   M <- matrix(nrow=ncol(data), ncol=ncol(data))
   M[lower.tri(M)] <- dep_vect
   M[upper.tri(M)] <- t(M)[upper.tri(M)]
   diag(M) <- 1

   colnames(M) <- colnames(data)
   rownames(M) <- colnames(data)

   return(M)
}

#################################
# Dependency Matrix computation #
#################################
dependency_matrix <- function(data, dependency_name){
   stopifnot(is.data.frame(data), nrow(data) >= 2)
   stopifnot(is.character(dependency_name))

   # Checks and retrieves dependency function
   if (!exists(dependency_name))
      stop("Dependency function not found!")
   dependency_fun <- get(dependency_name)
   if (!is.function(dependency_fun))
      stop('Dependency function is not a function!')

   # Computes pairwise statistical dependencies
   dependency_mat <- dependency_fun(data)

   # Checks output
   if (!is.matrix(dependency_mat) &
       !nrow(dependency_mat) == ncol(data) & !ncol(dependency_mat) == ncol(data))
      stop("Error during dependency matrix computations: wrong output")

   dependency_mat
}

###############################
# Column Clustering Functions #
###############################
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
   under_thresh <- node_members <= max_size

   # If applicable, cuts and fetches node labels
   clusters <- if (all(under_thresh)){
     list(get_dend_attributes(den, 'label'))
   } else {
      first_breach <- min(node_heights[!under_thresh])
      split_point  <- max(node_heights[under_thresh &
                                      node_heights < first_breach], 0)
      subtrees <- cut(den, h = split_point)
      clusters <- lapply(subtrees$lower, get_dend_attributes, 'label')
   }

   return(clusters)
}

cluster_columns <- function(dependency_mat, view_size_max,
                            clust_method = "complete"){
   stopifnot(is.matrix(dependency_mat))
   stopifnot(is.numeric(view_size_max))
   stopifnot(view_size_max >= 1)

   if (any(dependency_mat > 1 | dependency_mat < 0, na.rm = T))
      stop('Dependency must vary between 0 and 1')

   # Trivial case: 0 or 1 column
   if (ncol(dependency_mat) == 0) return(list())
   if (ncol(dependency_mat) == 1) return(list(colnames(dependency_mat)))

   if (all(is.na(dependency_mat)))
      stop("Could not compute a valid dependency matrix")

   # Runs complete link clustering on the remainder
   # Preprocesses the dependency matrix
   inv_dependency <- 1 - dependency_mat
   # Replaces NA by high dummy value - please mail me if you know better
   inv_dependency[is.na(inv_dependency)] <- 1.1

   # Clusters it
   dist_obj <-  as.dist(inv_dependency)
   clust_out <- hclust(dist_obj, method = clust_method)
   dendrogram <- as.dendrogram(clust_out)

   # Cuts the dendrogram
   clusters <- cut_max_size(dendrogram, view_size_max)

   return(clusters)
}


#####################################
# Trunk for all findview functions  #
#####################################
findviews_trunk <- function(data,  view_size_max=NULL, clust_method="complete"){

   # Input checks
   if (!is.character(clust_method))
      stop(paste0("clust_method must be a string describing a ",
                  "clustering method (e.g. 'complete' or 'complete')"))

   if (is.matrix(data)) data <- data.frame(data)
   else if (!is.data.frame(data)) stop("Input data is not a data frame")
   if (nrow(data) < 2) stop("The data set is too small.")

   # Sets view_size_max = min(log2(ncol(data)), 5) if no value specified
   if (is.null(view_size_max)) view_size_max <- max(1, log2(ncol(data)))
   stopifnot(is.numeric(view_size_max), view_size_max >= 1)
   view_size_max <- min(ceiling(view_size_max), 5)

   # Type detection and conversions
   # Flat columns = pimary keys, or columns with only 1 distinct value
   preprocessed <- preprocess(data)
   data_num <- preprocessed$data_num
   data_cat <- preprocessed$data_cat
   excluded <- preprocessed$excluded
   sampled_rows <- preprocessed$sampled_rows

   # Computes the dependency matrices
   dep_mat_num <- dependency_matrix(data_num, DEP_FUNC_NUM)
   dep_mat_cat <- dependency_matrix(data_cat, DEP_FUNC_CAT)

   # Creates views
   views_num <- cluster_columns(dep_mat_num, view_size_max, clust_method)
   views_cat <- cluster_columns(dep_mat_cat, view_size_max, clust_method)

   return(list(
      data_num           = data_num,
      dependency_mat_num = dep_mat_num,
      views_num          = views_num,
      data_cat           = data_cat,
      dependency_mat_cat = dep_mat_cat,
      views_cat          = views_cat,
      excluded           = excluded,
      sampled_rows       = sampled_rows
   ))
}
