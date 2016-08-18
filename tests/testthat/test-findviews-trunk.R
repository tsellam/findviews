source('generate_df.R')

#################
# Preprocessing #
#################
context("trunk functions - preprocessing")
test_that("preprocessor does its job", {
   out_names <- c('data_cat', 'data_num', 'excluded', 'sampled_rows')

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
context("trunk functions - dependency calculations")

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
context("trunk functions - column clustering")

test_that("dendrogram cutting function does its job", {

   dmat  <- 1 - dependency_matrix(df_num, DEP_FUNC_NUM)
   clust <- hclust(as.dist(dmat))
   dend  <- as.dendrogram(clust)

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
   dmat  <- dependency_matrix(df_num, DEP_FUNC_NUM)
   clu <- cluster_columns(dmat, 2)
   expect_is(clu, "list")
   expect_true(length(clu) > 0)
   expect_true(length(clu) <= ncol(df_num))
   expect_true(all(is.character(unlist(clu))))
})

test_that("column clustering function works properly with cat data", {
   dmat  <- dependency_matrix(df_cat, DEP_FUNC_CAT)
   clu <- cluster_columns(dmat, 2)
   expect_is(clu, "list")
   expect_true(length(clu) > 0)
   expect_true(length(clu) <= ncol(df_cat))
   expect_true(all(is.character(unlist(clu))))
})

test_that("column clustering function works properly with 0 col data", {
   dmat0 <- dependency_matrix(df_zerocol, DEP_FUNC_NUM)
   clu0  <- cluster_columns(dmat0, 2)
   expect_is(clu0, "list")
   expect_length(clu0, 0)

   dmat0 <- dependency_matrix(df_zerocol, DEP_FUNC_CAT)
   clu0  <- cluster_columns(dmat0, 2)
   expect_is(clu0, "list")
   expect_length(clu0, 0)
})

test_that("column clustering function works properly with 1 col data", {
   dmat1 <- dependency_matrix(df_onecol, DEP_FUNC_NUM)
   clu1  <- cluster_columns(dmat1, 2)
   expect_is(clu1, "list")
   expect_length(clu1, 1)

   dmat1 <- dependency_matrix(df_onecol_cat, DEP_FUNC_CAT)
   clu1  <- cluster_columns(dmat1, 2)
   expect_is(clu1, "list")
   expect_length(clu1, 1)
})

# test_that("column clustering fails with NAs", {
#    df_hell <- data.frame(
#       x0 = c(4,3,2,1),
#       x1 = c(1,2,3,4),
#       x2 = c(1,1,1,1)
#    )
#
#    dmat <- suppressWarnings(dependency_matrix(df_hell, DEP_FUNC_NUM))
#    stopifnot(is.matrix(dmat))
#    expect_error(cluster_columns(dmat, 4))
# })


##################
# Trunk function #
##################
context("trunk functions - main function")

source('generate_df.R')

check_output <- function(df, num, ...){
   # Runs Ziggy
   out <- findviews_trunk(df, num, ...)

   # Structure checks
   expect_is(out, "list")
   expect_named(out, c('views_cat',
                       'views_num',
                       'data_cat',
                       'data_num',
                       'excluded',
                       'dependency_mat_num',
                       'dependency_mat_cat',
                       'sampled_rows'), ignore.order=T)

   # Content checks
   expect_is(out$data_cat, "data.frame")
   expect_is(out$data_num, "data.frame")
   expect_is(out$excluded, "list")
   expect_true(setequal(
      unlist(c(names(out$data_num), names(out$data_cat), out$excluded)),
      names(df)
   ))

   expect_is(out$views_num, "list")
   expect_is(out$views_cat, "list")
   expect_true(all(names(df) %in% unlist(c(out$views_num,
                                           out$views_cat,
                                           out$excluded))))

   if (length(out$views_num) > 0){
      expect_true(all(sapply(out$views_num, is.character)))
      expect_true(all(sapply(out$views_num, function(v) length(v) <= num)))
      expect_true(all(sapply(out$views_num, function(v) length(v) > 0)))
   }

   if (length(out$views_cat) > 0){
      expect_true(all(sapply(out$views_cat, is.character)))
      expect_true(all(sapply(out$views_cat, function(v) length(v) <= num)))
      expect_true(all(sapply(out$views_cat, function(v) length(v) > 0)))
   }

   expect_named(out$excluded, c('unknown_type', 'flat_num', 'flat_cat'),
                ignore.order = T)

   expect_is(out$dependency_mat_num, "matrix")
   expect_is(out$dependency_mat_cat, "matrix")
   expect_equal(nrow(out$dependency_mat_num), ncol(out$data_num))
   expect_equal(ncol(out$dependency_mat_num), ncol(out$data_num))
   expect_equal(nrow(out$dependency_mat_cat), ncol(out$data_cat))
   expect_equal(ncol(out$dependency_mat_cat), ncol(out$data_cat))
}

test_that("findviews_trunk returns properly", {
   check_output(df_mix, 3)
   check_output(df_num, 3)
   check_output(df_cat, 2)
   check_output(df_zerocol, 3)
   check_output(df_onecol, 3)
   check_output(df_onecol_cat, 3)
})

test_that("findviews_trunk can deal with flat columns", {
   check_output(df_flat1, 2)
   check_output(df_flat2, 2)
   check_output(df_flat3, 2)
})

test_that("findviews_trunk can deal with NAs", {
   check_output(df_num_NA, 3)
   check_output(df_cat_NA, 3)
})

# Error checking
test_that("findviews_trunk fails properly", {
   expect_error(findviews_trunk(df_empty, 3))
   expect_error(findviews_trunk(df_onerow, 3))
})

# Checks sampling
test_that("sampling works properly", {
   OLD <- SAMPLE_SIZE
   SAMPLE_SIZE <<- 4
   expect_warning(findviews_trunk(df_mix))
   out <- suppressWarnings(findviews_trunk(df_mix))
   expect_true(all(!is.na(out$sampled_rows)))
   expect_equal(length(out$sampled_rows), SAMPLE_SIZE)
   expect_equal(nrow(out$data_num),SAMPLE_SIZE)
   expect_equal(nrow(out$data_cat),SAMPLE_SIZE)
   SAMPLE_SIZE <<- OLD
})
