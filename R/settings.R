#######################
# findcols_to_compare #
#######################
# Dependency calculation functions for each type of column
DEP_FUNC_NUM <- 'cor_matrix'
DEP_FUNC_CAT <- 'cramerV_matrix'

# Zig-Components for each type of column
DIFF_COMPONENTS_NUM <- c(mean_diff = 'diff_means',
                        sd_diff   = 'diff_sds',
                        corr_dif  = 'diff_corr')
DIFF_COMPONENTS_CAT <- c(hist_diff = 'diff_histogram')

# Coefficients for each component
WEIGHT_COMPONENTS_NUM <- c(mean_diff = 1,
                          sd_diff   = 1,
                          corr_dif  = .5)
WEIGHT_COMPONENTS_CAT <- c(hist_diff = 1)

# Significance thresholds
P_VALUE_DIFF <- 0.05
P_VALUE_PEARSON_RESIDUALS <- 0.01


#######################
# findcols_to_predict #
#######################
DISTINCT_VALS_THRES  <- 25
NBINS_CONT_VARIABLES <- 8

###############
# Web and GUI #
###############
# Web app parameters
APP_TYPES <- c('findviews', 'findviews_to_compare', 'findviews_to_predict')

# Graphing parameters
NBARS_BEFORE_FLIP <- 50

############
# SAMPLING #
############
SAMPLE_SIZE <- 10000
PLOT_SAMPLE_SIZE <- 2500
