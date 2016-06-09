context("Functions to compute statistical dependencies")

source('generate_df.R')

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
