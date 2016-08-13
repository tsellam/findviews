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
      return(n/nrow(data_cat) > .9 | n < 2)
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
      stop('Attempting to compute correlation on non factor data')

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

