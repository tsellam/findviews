context("Calls to main Ziggy functions")

source('generate_df.R')

test_that("testing data is loaded", {
   expect_is(df_num, 'data.frame')
   expect_is(df_cat, 'data.frame')
   expect_is(df_mix, 'data.frame')
   expect_is(df_empty, 'data.frame')
   expect_is(df_onerow, 'data.frame')
   expect_is(df_onecol, 'data.frame')
   expect_is(df_zerocol, 'data.frame')
})

# Error checking
test_that("main function fails properly", {
   expect_error(findviews_to_compare_core(c(), c(), df_empty))
   expect_error(findviews_to_compare_core(c(), c(), df_onerow))
   expect_error(findviews_to_compare_core(c(), c(), df_mix))
})

# Preprocessing
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

# Function calls and output check
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

test_that("main function returns properly", {
   check_output(df_mix, to_describe, 3)
   check_output(df_num, to_describe, 3)
   check_output(df_cat, to_describe, 2)
   check_output(df_zerocol, to_describe, 3)
   check_output(df_onecol, to_describe, 3)
   check_output(df_onecol_cat, to_describe, 3)
})

test_that("main function can deal with flat columns", {
   check_output(df_flat1, to_describe_flat, 2)
   check_output(df_flat2, to_describe_flat, 2)
   check_output(df_flat3, to_describe_flat, 2)
})

test_that("main function can deal with NAs", {
   check_output(df_num_NA, to_describe, 3)
   check_output(df_cat_NA, to_describe, 3)
})

test_that("default parameters work for main function", {
   expect_is(findviews_to_compare_core(to_describe, !to_describe, df_mix), "list")
})
