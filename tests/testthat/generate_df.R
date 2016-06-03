data(mtcars)

# One DF with only numerics
df_num <- mtcars[,1:7]

# One DF with only factors
df_cat <- cbind(
   as.data.frame(lapply(mtcars[,1:7],  cut, 3)),
   as.data.frame(lapply(mtcars[,8:11], factor))
)

# One DF with both
df_mix <- cbind(
   mtcars[,1:7],
   as.data.frame(lapply(mtcars[,8:11], factor))
)

df_num_NA <- df_num
df_num_NA[1,2] <- NA

df_cat_NA <- df_cat
df_cat_NA[1,2] <- NA

# Undesired DFs
df_empty <- data.frame()
df_zerocol <- mtcars[,1>2]
df_onerow <-mtcars[1,,drop=F]
df_onecol <-mtcars[,1,drop=F]
df_onecol_cat <- df_cat[,1,drop=F]
to_describe <- (mtcars$cyl == 4)



df_flat1 <- data.frame(
   x0 = c(4,3,2,1),
   x1 = c(1,2,3,4),
   x2 = c(1,1,1,1)
)
to_describe_flat <- c(T,F,F,F)

df_flat2 <- data.frame(
   x1 = c(1,2,3,4),
   x2 = c(1,1,1,1)
)
df_flat3 <- data.frame(
   x1 = c(2,2,2,2),
   x2 = c(1,1,1,1)
)
