# Dependency calculation functions for each type of column
DEP_FUNC_NUM <- 'cor_matrix'
DEP_FUNC_CAT <- 'cramerV_matrix'

# Zig-Components for each type of column
ZIG_COMPONENTS_NUM <- c(mean_diff = 'zig_means',
                        sd_diff   = 'zig_sds',
                        corr_dif  = 'zig_corr')
ZIG_COMPONENTS_CAT <- c(hist_diff = 'zig_histogram')

# Coefficients for each component
WEIGHT_COMPONENTS_NUM <- c(mean_diff = 1,
                          sd_diff   = 1,
                          corr_dif  = 1)
WEIGHT_COMPONENTS_CAT <- c(hist_diff = 1)

# Significance thresholds
P_VALUE_ZIG <- 0.05
P_VALUE_PEARSON_RESIDUALS <- 0.05



# Graphing parameters
SCATTERPLOT_SAMPLE_SIZE <- 2500
