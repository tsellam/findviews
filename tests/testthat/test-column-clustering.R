context("Functions related to column clustering")

source('generate_df.R')

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
