context("Various tests")

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

   # colors <- map_to_colors(c(NA),
   #                         start ,
   #                         end,
   #                         miss)
   # expect_is(colors, 'character')
   # expect_length(colors, 1)
   # expect_equal(colors[1], '#CACACA')
})
