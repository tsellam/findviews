devtools::load_all(".")

OECD <- read.csv("~/Data/Files/oecd-wellbeing/OECD_wide.csv")
findviews(OECD)
#findviews_to_compare(to_describe, !to_describe, crime, view_size_max = 8)
#findviews_to_predict('ViolentCrimesPerPop', crime, view_size_max=5)


