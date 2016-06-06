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
   expect_true(is.matrix(out$detail))
   expect_equivalent(rownames(out$detail), c('size', 'pvalue'))
   expect_type(out$tip, "character")
   expect_is(out, "list")
   expect_named(out, c('score', 'detail', 'tip'), ignore.order = T)
}

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


