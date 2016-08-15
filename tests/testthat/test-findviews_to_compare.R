source('generate_df.R')


##################
# Diff-Components #
##################
context("findviews_to_compare - diff components")

check_diff_output <- function(difffun, ...){
   out <- difffun(...)
   expect_is(out, "list")
   expect_named(out, c('score', 'detail', 'tip'), ignore.order = T)
   expect_true(is.numeric(out$score) | is.na(out$score))
   expect_is(out$tip, "character")
   expect_is(out$detail, 'list')
   return(out)
}

#-------------------------------------#
# Numeric, univariate Diff-Components #
#-------------------------------------#
test_that("diff_mean does the job", {
   df1<- data.frame(x=c(1,2), y=c(1,2))
   df2<- data.frame(x=c(4,5), y=c(4,5))
   out <- check_diff_output(diff_means, c("x", "y"), df1, df2)
   expect_equal(out$score, 3 / sd(c(4,5)))

   df1  <- data.frame(x=c(1), y=c(1))
   df2 <- data.frame(x=c(10), y=c(10))
   out <- check_diff_output(diff_means, c("x", "y"), df1, df2)
   expect_equal(out$score, 9)
})

test_that("diff_sd does the job", {
   out <- check_diff_output(diff_sds, names(df_num),
                            df_num[1:16,], df_num[17:32,])
   expect_equal(out$score, 0.2314, tolerance = .001)

   df1<- data.frame(x=c(1,2), y=c(1,2))
   df2<- data.frame(x=c(4,10), y=c(4,20))
   out <- check_diff_output(diff_sds, c("x", "y"), df1, df2)
   expect_equal(out$score, 0.8854,  tolerance = .001)

   df1  <- data.frame(x=c(1), y=c(1))
   df2 <- data.frame(x=c(10), y=c(10))
   out <- check_diff_output(diff_sds, c("x", "y"), df1, df2)
   expect_equal(out$score, NA)
})

#------------------------------------#
# Numeric, bivariate Diff-Components #
#------------------------------------#
test_that("test for correlation coefficients works", {
   expect_true(is.na(corr_diff_test(1.1, .3, 20, 20)))
   expect_true(is.na(corr_diff_test(.1, .3, 2, 2)))
   expect_equal(corr_diff_test(.40196, .28250, 327, 273), 0.09987974)
})

test_that("function which_true_elements works", {
   M <- matrix(c(T, T, T,
                 T, F, F,
                 T, F, NA), nrow=3, byrow = T)
   rownames(M) <- colnames(M) <- c("a", "b", "c")
   expect_equal(which_true_elements(M), list(c("a", "a"), c("a", "b"), c("a", "c")))
})

test_that("diff_corr does the job", {
   out <- check_diff_output(diff_corr, names(df_num),
                            df_num[1:16,], df_num[17:32,])
   expect_equal(out$detail$pvalues['disp', 'cyl'], 0.07622094)

   out <- check_diff_output(diff_corr, names(df_num)[1],
                            df_num[1:16,], df_num[17:32,])
   expect_equal(out$score, NA)

   out <- check_diff_output(diff_corr, names(df_num)[1==2],
                            df_num[1:16,], df_num[17:32,])
   expect_equal(out$score, NA)
})


#-----------------------------------------#
# Categorical, univariate Diff-Components #
#-----------------------------------------#
test_that("distance and chi-squared test work", {
   tab_1  <- c(b=100, a=100, c=100)
   tab_2 <- c(b=100, d=100, c=200)

   expect_equal(hist_diss_score(tab_1, tab_2), 0.456, tolerance = .001)
   expect_equal(wrap_chi_squared(tab_1, tab_2)$chi2, 100, tolerance = .001)
})

test_that("diff_histogram does the job", {
   out <- check_diff_output(diff_histogram, names(df_cat),
                            df_cat[to_describe,], df_cat[!to_describe,])

   df1  <- data.frame(x=factor(c('a', 'b', 'c')))
   df2 <- data.frame(x=factor(c('b', 'c', 'c', 'd')))
   out <- check_diff_output(diff_histogram, c("x"), df1, df2)
   expect_equal(out$score, 0.456,  tolerance = .001)
})


############################
# General Scoring Function #
############################
context("findviews_to_compare - scoring functions")

test_score <- function(views, target, df, diff_components){
   out <- score_difference(views, target, !target, df, diff_components)

   expect_is(out, "data.frame")
   expect_equal(nrow(out), length(views))
   expect_equivalent(colnames(out), names(diff_components))

   t_obj <- sapply(out, function(col){
      sapply(col, function(diff){
         is.list(diff) & c('score', 'detail', 'tip') %in% names(diff)
      })
   })
   t_obj <- unlist(t_obj)
   expect_true(all(t_obj))

   out
}

test_that("scoring works for numeric data", {
   diff_components <- c(mean_diff = 'diff_means',
                        sd_diff    = 'diff_sds',
                        corr_dif   = 'diff_corr')

   test_score(list(c("mpg", "cyl"), c("disp"), c("drat", "wt", "qsec")),
              to_describe, df_num, diff_components)
   test_score(list(c("mpg", "cyl")), to_describe, df_num, diff_components)
   test_score(list(), to_describe, df_num, diff_components)
   test_score(list(c("mpg")), to_describe, df_onecol, diff_components)
})


test_that("scoring works for categorical data", {
   diff_components <- c(hist_diff = 'diff_histogram')

   test_score(list(c("mpg", "cyl"), c("disp"), c("drat", "wt", "qsec")),
              to_describe, df_cat, diff_components)
   test_score(list(c("mpg", "cyl")), to_describe, df_cat, diff_components)
   test_score(list(), to_describe, df_cat, diff_components)
   test_score(list(c("mpg")), to_describe, df_cat, diff_components)
})



##########################
# Calls to main function #
##########################
context("findviews_to_compare - main function")

check_output <- function(df, to_describe, num, ...){
   # Runs Ziggy
   out <- findviews_to_compare_core(to_describe, !to_describe, df, num, ...)

   # Structure checks
   expect_is(out, "list")
   expect_named(out, c('views_cat', 'views_num', 'scores_cat', 'scores_num',
                       'details_cat', 'details_num', 'excluded'), ignore.order=T)

   # Content check
   expect_is(out$views_num, "list")
   if (length(out$views_num) > 0){
      expect_true(all(sapply(out$views_num, is.character)))
      expect_true(all(sapply(out$views_num, function(v) length(v) <= num)))
      expect_true(all(sapply(out$views_num, function(v) length(v) > 0)))

      expect_named(out$details_num, names(DIFF_COMPONENTS_NUM), ignore.order=T)
      expect_equal(nrow(out$details_num), length(out$views_num))

      expect_is(out$scores_num, 'numeric')
   }

   expect_is(out$views_cat, "list")
   if (length(out$views_cat) > 0){
      expect_true(all(sapply(out$views_cat, is.character)))
      expect_true(all(sapply(out$views_cat, function(v) length(v) > 0)))

      expect_named(out$details_cat, names(DIFF_COMPONENTS_CAT), ignore.order=T)
      expect_equal(nrow(out$details_cat), length(out$views_cat))

      expect_is(out$scores_cat, 'numeric')
   }

   expect_is(out$excluded, "list")
   expect_named(out$excluded, c('unknown_type', 'flat_num', 'flat_cat'),
                ignore.order = T)

   expect_true(all(names(df) %in% unlist(c(out$views_num,
                                           out$views_cat,
                                           out$excluded))))

}

test_that("findviews_to_compare returns properly", {
   check_output(df_mix, to_describe, 3)
   check_output(df_num, to_describe, 3)
   check_output(df_cat, to_describe, 2)
   check_output(df_zerocol, to_describe, 3)
   check_output(df_onecol, to_describe, 3)
   check_output(df_onecol_cat, to_describe, 3)
})

test_that("findviews_to_compare can deal with flat columns", {
   check_output(df_flat1, to_describe_flat, 2)
   check_output(df_flat2, to_describe_flat, 2)
   check_output(df_flat3, to_describe_flat, 2)
})

test_that("findviews_to_compare can deal with NAs", {
   check_output(df_num_NA, to_describe, 3)
   check_output(df_cat_NA, to_describe, 3)
})

test_that("findviews_to_compare work for main function", {
   expect_is(findviews_to_compare_core(to_describe, !to_describe, df_mix), "list")
})

# Error checking
test_that("findviews_to_compare fails properly", {
   expect_error(findviews_to_compare_core(c(), c(), df_empty))
   expect_error(findviews_to_compare_core(c(), c(), df_onerow))
   expect_error(findviews_to_compare_core(c(), c(), df_mix))
})
