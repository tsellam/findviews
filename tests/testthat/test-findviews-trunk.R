source('generate_df.R')

#################
# Preprocessing #
#################
context("Trunk functions - preprocessing")
test_that("preprocessor does its job", {
   out_names <- c('data_cat', 'data_num', 'excluded')

   expect_is(preprocess(df_mix), 'list')
   expect_named(preprocess(df_mix), out_names, ignore.order=T)
   expect_named(preprocess(df_cat), out_names, ignore.order=T)
   expect_named(preprocess(df_num), out_names, ignore.order=T)
   expect_is(preprocess(df_num)$data_cat, 'data.frame')
   expect_is(preprocess(df_num)$data_num, 'data.frame')
   expect_is(preprocess(df_cat)$data_cat, 'data.frame')
   expect_is(preprocess(df_cat)$data_num, 'data.frame')
   expect_named(preprocess(df_zerocol), out_names, ignore.order=T)

   expect_true('x2' %in% preprocess(df_flat1)$excluded$flat_num)
})


###########################
# Depepdency computations #
###########################
context("Trunk functions - dependency calculations")

test_that("CramerV computations work", {
   expect_true(is.na(cramerV(c(1), c(2))))
   expect_true(is.na(cramerV(c(1,1), c(1,2))))

   test_data <- rbind(
      matrix(rep(c(0,0), 75), ncol=2, byrow = T),
      matrix(rep(c(1,0), 25), ncol=2, byrow = T),
      matrix(rep(c(0,1), 25), ncol=2, byrow = T),
      matrix(rep(c(1,1), 25), ncol=2, byrow = T)
   )
   expect_equal(cramerV(test_data[,1], test_data[,2]), 0.25)
})

test_that("Dependency functions work consistently", {
   expect_is(cor_matrix(df_zerocol), 'matrix')
   expect_is(cor_matrix(df_onecol), 'matrix')
   expect_is(cor_matrix(df_num), 'matrix')

   expect_is(cramerV_matrix(df_zerocol), 'matrix')
   expect_is(cramerV_matrix(df_onecol_cat), 'matrix')
   expect_is(cramerV_matrix(df_cat), 'matrix')
})

test_that("Cramer V function gives correct output", {

   test_V <- function(df){
      M <- cramerV_matrix(df)
      tests <- sapply(1:nrow(M), function(i){
         sapply(1:ncol(M), function(j){
            M[i,j] == abs(cramerV(df[,i], df[,j]))
         })
      })
      expect_true(all(tests))
   }

   test_V(df_onecol_cat)
   test_V(df_cat)
   test_V(df_cat[,1:2])
})



#####################
# Column clustering #
#####################
context("Trunk functions - column clustering")

test_that("dendrogram cutting function does its job", {

   cormat <- abs(cor(df_num))
   clust  <- hclust(as.dist(cormat))
   dend   <- as.dendrogram(clust)

   check_size <- function(li, MAX){
      max(sapply(li, length)) <= MAX
   }

   expect_error(cut_max_size(dend, 0L))
   expect_is(cut_max_size(dend, 2L), "list")

   expect_true(check_size(cut_max_size(dend, 1L), 1))
   expect_true(check_size(cut_max_size(dend, 2L), 2))
   expect_true(check_size(cut_max_size(dend, 3L), 3))
   expect_true(check_size(cut_max_size(dend, 35L), 35))

   expect_is(cut_max_size(dend[[1]][[1]], 2L), "list")
   expect_length(cut_max_size(dend[[1]][[1]], 2L), 1)
})


test_that("column clustering function works properly with num data", {
   clu <- cluster_columns(df_num, 2, "cor_matrix")
   expect_is(clu, "list")
   expect_true(length(clu) > 0)
   expect_true(length(clu) <= ncol(df_num))
   expect_is(cluster_columns(df_num, 2, "cor_matrix")[[1]], "character")

   expect_is(cluster_columns(df_zerocol, 2, "cor_matrix"), "list")
   expect_length(cluster_columns(df_zerocol, 2, "cor_matrix"), 0)
   expect_is(cluster_columns(df_onecol, 2, "cor_matrix"), "list")
   expect_length(cluster_columns(df_onecol, 2, "cor_matrix"), 1)
})

test_that("column clustering function works properly with cat data", {
   clu <- cluster_columns(df_cat, 2, "cramerV_matrix")
   expect_is(clu, "list")
   expect_true(length(clu) > 0)
   expect_true(length(clu) <= ncol(df_cat))
   expect_is(cluster_columns(df_cat, 2, "cramerV_matrix")[[1]], "character")

   expect_is(cluster_columns(df_zerocol, 2, "cramerV_matrix"), "list")
   expect_length(cluster_columns(df_zerocol, 2, "cramerV_matrix"), 0)
   expect_is(cluster_columns(df_onecol_cat, 2, "cramerV_matrix"), "list")
   expect_length(cluster_columns(df_onecol_cat, 2, "cramerV_matrix"), 1)
})

test_that("column clustering fails with NAs", {
   df_hell <- data.frame(
      x0 = c(4,3,2,1),
      x1 = c(1,2,3,4),
      x2 = c(1,1,1,1)
   )
   expect_error( suppressWarnings(cluster_columns(df_hell, 4, cor_matrix)))
})
