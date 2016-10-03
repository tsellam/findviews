# Pretty prints a list of names
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
   if (length(clean_data) == 0) return(rep(missing_col, length(data)))

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


# Merges several factors of the same size
merge_factors <- function(df){
   stopifnot(is.data.frame(df))

   if (nrow(df) == 0) return(factor())
   if (ncol(df) == 0) return(factor(rep(NA, nrow(df))))

   stopifnot(sapply(df, is.factor))
   interaction(df, drop = T)
}

# Bins a vector of numerics
bin_equiwidth <- function(s, nbins){
   stopifnot(is.numeric(s))
   stopifnot(is.numeric(nbins))

   if (nbins < 2) stop("The number of bins must be at least 2")

   if (length(s) == 0) return(factor())
   if (all(is.na(s)))  return(factor(rep(NA), length(s)))
   if (min(s, na.rm = T) == max(s, na.rm = T)) return(factor(s))

   if (is.integer(s) & length(unique(s)) <= nbins)
      return(factor(s))

   cut(s, nbins, ordered_result = T)
}

# This is necessary to avoid strange effects in Chi-2 calculations
safe_table <- function(v){
   t <- table(v, useNA = "no")
   names(t)[nchar(names(t)) == 0] <- '_empty_'
   t
}

# Injects the levels of tab2 to tab1, with value 0
merge_table_names <- function(tab1, tab2){
   stopifnot(is.numeric(tab1) & is.numeric(tab2))

   missing <- setdiff(names(tab2), names(tab1))

   if (length(missing) == 0) return(tab1)
   new_elements <- rep(0, length(missing))
   names(new_elements) <- missing

   new_table <- c(tab1, new_elements)
   return(new_table)
}
