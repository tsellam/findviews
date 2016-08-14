source('generate_df.R')

####################
# Scoring function #
####################
context("findviews - ranking")
test_that("scoring works properly", {

   dep_map <- matrix(c(.1, .2, .2, .1),
                     nrow = 2,
                     dimnames = list(c('x', 'y'),
                                     c('x', 'y')))
   views0 <- list()
   views1 <- list(c('x'))
   views2 <- list(c('x', 'y'), c('x'))
   views3 <- list(c('x', 'y'))

   expect_true(length(score_influence(views0, dep_map)) == 0)
   expect_equal(score_influence(views1, dep_map), .2)
   expect_equal(score_influence(views2, dep_map), c(NA, .2))
   expect_true(is.na(score_influence(views3, dep_map)))
})


#################
# Main function #
#################
context("findviews - main function")
check_output <- function(df, num, ...){
   # Runs Ziggy
   out <- findviews_core(df, num, ...)

   # Structure checks
   expect_is(out, "list")
   expect_named(out, c('views_cat',
                       'views_num',
                       'scores_cat',
                       'scores_num',
                       'details_num',
                       'details_cat',
                       'excluded'), ignore.order=T)

   # Content checks
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

   expect_is(out$scores_num, "numeric")
   expect_is(out$scores_cat, "numeric")
   expect_length(out$scores_num, length(out$views_num))
   expect_length(out$scores_cat, length(out$views_cat))
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
