context("View scoring functions")

source('generate_df.R')


############################
# General Scoring Function #
############################
test_that("scoring works for numeric data", {
   zig_components <- c(mean_diff = zig_means)
                     # sd_diff    = zig_sds,
                     # corr_dif   = zig_corrs)

   test_score <- function(views, target, df){
      out <- score_views(views, target, df, zig_components)

      expect_is(out, "data.frame")
      expect_equal(nrow(out), length(views))
      expect_equivalent(colnames(out), names(zig_components))

      t_obj <- sapply(out, function(col){
         sapply(col, function(zig){
            is.list(zig) & c('score', 'detail', 'tip') %in% names(zig)
         })
      })
      t_obj <- unlist(t_obj)
      expect_true(all(t_obj))
   }

   test_score(list(c("mpg", "cyl"), c("disp"), c("drat", "wt", "qsec")),
               to_describe,
               df_num)
   test_score(list(c("mpg", "cyl")), to_describe, df_num)
   test_score(list(), to_describe, df_num)
   test_score(list(c("mpg")), to_describe, df_onecol)
})


##################
# Zig Components #
##################
check_zig_output <- function(zigfun, ...){
   out <- zigfun(...)
   expect_is(out, "list")
   expect_named(out, c('score', 'detail', 'tip'), ignore.order = T)
   expect_true(is.numeric(out$score) | is.na(out$score))
   expect_is(out$tip, "character")
   expect_is(out$detail, 'list')
   return(out)
}

#------------------------------------#
# Numeric, univariate Zig-Components #
#------------------------------------#
test_that("zig_mean does the job", {
   dfin<- data.frame(x=c(1,2), y=c(1,2))
   dfout<- data.frame(x=c(4,5), y=c(4,5))
   out <- check_zig_output(zig_means, c("x", "y"), dfin, dfout)
   expect_equal(out$score, 3 / sd(c(4,5)))

   dfin  <- data.frame(x=c(1), y=c(1))
   dfout <- data.frame(x=c(10), y=c(10))
   out <- check_zig_output(zig_means, c("x", "y"), dfin, dfout)
   expect_equal(out$score, NA)
})

test_that("zig_sd does the job", {
   dfin<- data.frame(x=c(1,2), y=c(1,2))
   dfout<- data.frame(x=c(4,10), y=c(4,20))
   out <- check_zig_output(zig_sds, c("x", "y"), dfin, dfout)
   expect_equal(out$score, 11)

   dfin  <- data.frame(x=c(1), y=c(1))
   dfout <- data.frame(x=c(10), y=c(10))
   out <- check_zig_output(zig_means, c("x", "y"), dfin, dfout)
   expect_equal(out$score, NA)
})

#-----------------------------------#
# Numeric, bivariate Zig-Components #
#-----------------------------------#
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

test_that("zig_corr does the job", {
   out <- check_zig_output(zig_corr, names(df_num),
                           df_num[1:16,], df_num[17:32,])
   expect_equal(out$detail$pvalues['disp', 'cyl'], 0.07622094)

   out <- check_zig_output(zig_corr, names(df_num)[1],
                           df_num[1:16,], df_num[17:32,])
   expect_equal(out$score, NA)

   out <- check_zig_output(zig_corr, names(df_num)[1==2],
                           df_num[1:16,], df_num[17:32,])
   expect_equal(out$score, NA)
})


#----------------------------------------#
# Categorical, univariate Zig-Components #
#----------------------------------------#
test_that("zig_histogram does the job", {
   out <- check_zig_output(zig_histogram, names(df_cat),
                           df_cat[to_describe,], df_cat[!to_describe,])
   # TODO! Normalize Chi-Squared into Cramer V
})
