source('generate_df.R')


context("utils")

test_that("HTML color gradient works with NAs", {
   start <- '#FFFFFF'
   end   <- '#000000'
   miss  <- '#CACACA'
   colors <- map_to_colors(c(1,2,3,NA),
                           start ,
                           end,
                           miss)
   expect_is(colors, 'character')
   expect_length(colors, 4)
   expect_equal(colors[4], '#CACACA')

   colors <- map_to_colors(c(NA),
                            start ,
                            end,
                            miss)
    expect_is(colors, 'character')
    expect_length(colors, 1)
    expect_equal(colors[1], '#CACACA')
})


test_that("Merge factor function does its job", {
   check_merge <- function(df){
      for(c in names(df)) df[[c]] <- as.factor(df[[c]])
      m <- merge_factors(df)
      expect_true(is.factor(m))
      expect_length(m, nrow(df))
   }

   check_merge(df_empty)
   check_merge(df_zerocol)
   check_merge(df_onerow)
   check_merge(df_onecol)
   check_merge(df_onecol_cat)
   check_merge(df_num_NA)
})

test_that("Binning function does the job", {
   test_bin <- function(s, n) expect_is(bin_equiwidth(s, n), 'factor')

   test_bin(numeric(0), 5)
   test_bin(c(NA_integer_), 5)
   test_bin(c(1,2,3,4), 5)
   test_bin(c(1,2,3,4,12,32), 5)
   test_bin(c(1,1,1,1,1,1,1), 5)
   test_bin(c(1,NA), 2)
})

test_that("Merge table levels does the job", {
   t1 <- table(c(1,2,3,3))
   t2 <- table(c(1,12,3,1))

   expect_named(
      merge_table_names(t1, t2),
      c("1", "2", "3", "12"),
      ignore.order=TRUE
   )
   expect_named(
      merge_table_names(t2, t1),
      c("1", "2", "3", "12"),
      ignore.order=TRUE
   )

})
