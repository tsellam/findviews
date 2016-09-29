devtools::load_all(".")

OECD <- read.csv("~/Data/Files/oecd-wellbeing/OECD_wide.csv")
names(OECD)[names(OECD) == "patent_applications_per_million"] <- 'patent_applications'
OECD$InnovationTarget <- OECD$patent_applications > 150

#findviews(OECD)
#findviews_to_compare(OECD$patent_applications > 150, OECD$patent_applications < 150, OECD)
#findviews_to_predict('InnovationTarget', OECD)
#findviews_to_predict(target='patent_applications', data=OECD)
findviews_to_predict(target='InnovationTarget', data=OECD)
