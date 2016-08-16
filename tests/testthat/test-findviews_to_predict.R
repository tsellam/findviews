######################
# Information theory #
######################
context("findviews_to_predict - information theory")

test_that("mutual information function does the job", {
   check_MI <- function(s1, s2){
      f1 <- as.factor(s1)
      f2 <- as.factor(s2)
      m <- mutual_information(f1, f2)
      expect_true(is.numeric(m))
   }

   check_MI(c(1,2,3), c(1,1,2))
   check_MI(c(1,1,3), c(2,2,2))
   check_MI(c(1,1,NA), c(2,2,2))
   check_MI(c(1,NA), c(2,NA))
   check_MI(c(), c())
   check_MI(c(NA), c(NA))
   check_MI(numeric(0), numeric(0))

   f1 <- factor(c(1,1,2,3))
   f2 <- factor(c(1,2,2,3))
   expect_equal(mutual_information(f1, f2), 1.0)

   f1 <- factor(c(1,1,2,1))
   f2 <- factor(c(1,2,2,1))
   expect_equal(mutual_information(f1, f2), 0.311, tolerance = .001)
})

#################
# Preprocessors #
#################
context("findviews_to_predict - preprocessing")

test_that("preprocessor does the job", {
   check_pp <- function(s)
      expect_is(preprocess_target(s), "factor")

   check_pp(c(1,2,3,4,5))
   check_pp(c(1, NA))
   check_pp(c('2', '1'))
   check_pp(c())
})

###########
# Ranking #
###########
context("findviews_to_predict - ranking")

test_that("ranking works for cat data", {
   check_rk <- function(data, one_view = NULL){
      set.seed(55555)
      target <- sample(factor(c('1', '2', '3')), nrow(data), replace = T)
      views <-  if (is.null(one_view)) list(names(data))
                else views <- list(one_view)
      out <- score_predictive_cat(views, data, target)
      expect_is(out, "numeric")
      expect_length(out, length(views))
   }

   check_rk(df_cat)
   check_rk(df_cat, names(df_cat)[1])
   check_rk(df_cat, names(df_cat)[1==2])
   check_rk(df_cat_NA, names(df_cat_NA))
   check_rk(df_onecol_cat, names(df_onecol_cat))
})

test_that("ranking works for num data", {
   check_rk <- function(data, one_view = NULL){
      set.seed(55555)
      target <- sample(factor(c('1', '2', '3')), nrow(data), replace = T)
      views <-  if (is.null(one_view)) list(names(data))
                else views <- list(one_view)
      if (length(unlist(views)) == 0) views <- list()
      out <- score_predictive_num(views, data, target)
      expect_is(out, "numeric")
      expect_length(out, length(views))
   }

   check_rk(df_num)
   check_rk(df_num, names(df_num)[1])
   check_rk(df_num, names(df_num)[1==2])
   check_rk(df_num_NA, names(df_num_NA))
   check_rk(df_onecol, names(df_onecol))
})

###########
# Ranking #
###########
context("findviews_to_predict - main function")
check_output <- function(df, num, ...){

   # Generates a phony target column
   target <- sample(factor(c('1', '2', '3')), nrow(df), replace = T)
   df <- cbind(df, target)
   names(df)[[length(names(df))]] <- 'target'

   # Runs Ziggy
   out <- findviews_to_predict_core('target', df, view_size_max = num, ...)

   # Structure checks
   expect_is(out, "list")
   expect_named(out, c('views_cat',
                       'views_num',
                       'scores_cat',
                       'scores_num',
                       'details_num',
                       'details_cat',
                       'excluded',
                       'target_data'), ignore.order=T)

   # Content checks
   expect_is(out$views_num, "list")
   expect_is(out$views_cat, "list")
   expect_true(all(names(df) %in% unlist(c(out$views_num,
                                           out$views_cat,
                                           out$excluded,
                                           'target'))))

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

   expect_is(out$scores_num, "numeric")
   expect_is(out$scores_cat, "numeric")
   expect_length(out$scores_num, length(out$views_num))
   expect_length(out$scores_cat, length(out$views_cat))
}

test_that("findviews_to_predict returns properly", {
   check_output(df_mix, 3)
   check_output(df_num, 3)
   check_output(df_cat, 2)
   check_output(df_zerocol, 3)
   check_output(df_onecol, 3)
   check_output(df_onecol_cat, 3)
})

test_that("findviews_to_predict can deal with flat columns", {
   check_output(df_flat1, 2)
   check_output(df_flat2, 2)
   check_output(df_flat3, 2)
})

test_that("findviews_to_predict can deal with NAs", {
   check_output(df_num_NA, 3)
   check_output(df_cat_NA, 3)
})

# Error checking
test_that("findviews_to_predict fails properly", {
   expect_error(findviews_trunk(df_empty, 3))
   expect_error(findviews_trunk(df_onerow, 3))
})
