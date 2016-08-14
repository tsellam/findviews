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

# Maps numeric data to a color gradient
map_to_colors <- function(data, start_col, end_col, missing_col = '#CCCCCC'){
   stopifnot(is.vector(data))

   NAs <- is.na(data)
   clean_data <- data[!NAs]

   min <- 0
   max <- max(clean_data)
   if (max <=  min) return(rep(missing_col, length(data)))
   clean_data <- (clean_data - min)  / (max - min)

   map_fn <- colorRamp(c(start_col, end_col))
   colors <- map_fn(clean_data)
   html_colors <- rgb(colors, max = 255)

   out <- character(length(data))
   out[NAs]  <- missing_col
   out[!NAs] <- html_colors

   return(out)
}


