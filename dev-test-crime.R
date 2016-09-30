devtools::load_all(".")

# data(mtcars)
# to_describe <- (mtcars$cyl == 4)
# ziggy_web(mtcars, to_describe)

library(foreign)
crime <- read.arff(file = '~/Data/Files/crime/communities.arff')
to_describe <- crime$ViolentCrimesPerPop > 0.5

#crime$communityname <- NULL
crime$mess   <- rep(1, nrow(crime))
crime$troll  <- as.character(1:nrow(crime))
crime$troll2 <- rep('same', nrow(crime))

for (i in 20:30){
   if (names(crime)[i] %in% c('pctWFarmSelf', 'pctWRetire'))
      crime[[i]] <- cut(crime[[i]], breaks = 35)
   else
      crime[[i]] <- cut(crime[[i]], breaks = 10)
}

crime[['discrete_crime']] <- cut(crime$ViolentCrimesPerPop, breaks =5)

findviews(crime)
#findviews_to_compare(to_describe, !to_describe, crime)
#findviews_to_predict('discrete_crime', crime)
findviews_to_predict('ViolentCrimesPerPop', crime, view_size_max = 6)


