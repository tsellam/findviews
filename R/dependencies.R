########################################
# For numeric data: simple correlation #
########################################
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


####################################
# For categotical data: Cramer's V #
####################################
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
   diag(M) <- 1.0

   colnames(M) <- colnames(data)
   rownames(M) <- colnames(data)

   return(M)
}
