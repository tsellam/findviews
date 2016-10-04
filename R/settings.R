############
# findcols #
############
# Dependency calculation functions for each type of column
DEP_FUNC_NUM <- 'cor_matrix'
DEP_FUNC_CAT <- 'cramerV_matrix'

#######################
# findcols_to_predict #
#######################
DISTINCT_VALS_THRES  <- 25
NBINS_CONT_VARIABLES <- 16

###############
# Web and GUI #
###############
# Web app parameters
APP_TYPES <- c('findviews', 'findviews_to_compare', 'findviews_to_predict')

# Graphing
MAX_LEVELS_HIST <- 20
MAX_LEVELS_FACET_HIST<- 10
MAX_CHAR_HIST <- 10
MAX_LEVELS_PER_LINE_HIST <- 20

############
# SAMPLING #
############
SAMPLE_SIZE <- 10000
PLOT_SAMPLE_SIZE <- 2500
