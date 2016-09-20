num_1d_view <- function(data, mapping, ...){
   p <- ggplot2::ggplot(data=data, mapping=mapping) +
      ggplot2::geom_density(...) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.text     = ggplot2::element_text(size = 12),
                     legend.key.size = ggplot2::unit(1, "cm"))

   if (is.ordered(data[[ncol(data)]]))
      p <- p + ggplot2::scale_colour_brewer() +
         ggplot2::scale_fill_brewer()

   p
}

num_2d_view <- function(data, mapping, ...){

   scat_pt_size <- if (nrow(data) > 1000) .5
   else if (nrow(data) > 500) .75
   else 1

   p <- ggplot2::ggplot(data=data, mapping=mapping) +
      ggplot2::geom_point(size = scat_pt_size, ...) +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.text     = ggplot2::element_text(size = 12),
                     legend.key.size = ggplot2::unit(1, "cm"))

   if (nrow(data) > 20) p <- p + ggplot2::geom_smooth(method=lm, se = F)
   if (is.ordered(data[[ncol(data)]]))
      p <- p + ggplot2::scale_colour_brewer() +
      ggplot2::scale_fill_brewer()

   p
}

cat_1d_view <- function(data, mapping, ...){
   p <- ggplot2::ggplot(data, mapping) +
      ggplot2::geom_bar(position = "dodge", ...) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=-35, hjust=0),
                     legend.position = "left")

   if (is.ordered(data[[ncol(data)]]))
      p <- p + ggplot2::scale_colour_brewer() +
         ggplot2::scale_fill_brewer()

   p
}


#### Wrappers
plot_selection_numeric <- function(data, target, app_type){
   col_is_num <- sapply(data, is.numeric)
   if (!all(col_is_num)) stop('Cannot plot, type not supported')

   # Prepares the data frame to be visualized
   data <- cbind(data, target)
   to_plot_index <- 1:(ncol(data)-1)
   to_plot_col   <- names(data)[to_plot_index]
   labels_col    <- names(data)[[ncol(data)]]


   # Sets plotting parameters, depending on app type
   if (app_type == 'findviews'){
      def_color    <- NULL
      def_fills    <- NULL
      def_cst_fill <- "grey"
      def_alpha   <- 1
      show_legend <- F

   } else if (app_type == 'findviews_to_compare'){
      def_color   <- labels_col
      def_fills   <- labels_col
      def_cst_fill <- NULL
      def_alpha   <- .5
      show_legend <- T

   } else if (app_type == 'findviews_to_predict'){
      def_color    <- labels_col
      def_fills    <- labels_col
      def_cst_fill <- NULL
      def_alpha    <- .5
      show_legend  <- T
   }

   # 1D data -> density plot
   if (ncol(data) == 2){
      title <- paste0('Density plot for the variable ', names(data)[[1]])
      plot_args <- list(data    = data,
                        mapping = ggplot2::aes_string(x = to_plot_col,
                                                      color = def_color,
                                                      fill  = def_fills),
                        alpha = def_alpha)
      if (!is.null(def_cst_fill)) plot_args[['fill']] <- def_cst_fill

      do.call(num_1d_view, plot_args) + ggplot2::ggtitle(title)

      # 2d and more -> scatterplot matrix
   } else if (ncol(data) >= 3){
      # Context-dependent graph parameters
      title <- "Density plots (diagonal) and 2D scatterplots (all the other charts)"

      lower_plots <- GGally::wrap(num_2d_view, alpha = def_alpha)
      diag_plots  <- if (!is.null(def_cst_fill))
         GGally::wrap(num_1d_view, fill = def_cst_fill)
      else GGally::wrap(num_1d_view, alpha = def_alpha)

      # Puts them all in matrix
      pairs <- GGally::ggpairs(data,
                               mapping = ggplot2::aes_string(color = def_color,
                                                             fill  = def_fills),
                               columns = to_plot_index,
                               lower = list('continuous' = lower_plots),
                               diag  = list('continuous' = diag_plots),
                               upper = list('continuous' = 'blank'),
                               legends = FALSE,
                               title   = title)

      # If necessary, generates and inserts the legend
      if (show_legend){
         plot_legend_fn <- GGally::gglegend(num_1d_view)
         legend <- plot_legend_fn(data, ggplot2::aes_string(x = labels_col[1],
                                                            color = labels_col,
                                                            fill  = labels_col))
         pairs[1, length(to_plot_col)] <- legend
      }

      # Done!
      pairs
   }
}

plot_selection_categorical <- function(data, target, app_type){

   # Prepares the data frame to be visualized
   data <- cbind(data, target)

   # Gets the column names
   to_plot_index <- 1:(ncol(data)-1)
   to_plot_col   <- names(data)[to_plot_index]
   labels_col    <- names(data)[[ncol(data)]]

   # Sets plotting parameters, depending on app type
   if (app_type == 'findviews'){
      def_color   <- NULL
      def_fills   <- NULL
      show_legend <- F
      nplots      <- length(to_plot_col)
      yLabel      <- "Frequency"

   } else if (app_type == 'findviews_to_compare'){
      def_color   <- labels_col
      def_fills   <- labels_col
      show_legend <- T
      nplots      <- length(to_plot_col) + 1
      yLabel      <- "Frequency in target group"

   } else if (app_type == 'findviews_to_predict'){
      def_color   <- labels_col
      def_fills   <- labels_col
      show_legend <- T
      nplots      <- length(to_plot_col) + 1
      yLabel      <- "Frequency in target group"
   }

   # Creates the series of plots
   plot_series <- lapply(to_plot_col, function(col){
      cat_1d_view(data,
                  mapping = ggplot2::aes_string(x = col,
                                                color = def_color,
                                                fill  = def_fills),
                  ggplot2::aes_string(y = '..prop..',
                                      group = labels_col))
   })

   # Generates the legend, if necessary
   if (show_legend){
      plot_legend_fn <- GGally::gglegend(cat_1d_view)
      legend <- plot_legend_fn(data, ggplot2::aes_string(x = labels_col[1],
                                                         color = def_color,
                                                         fill  = def_fills))
      # Places everything in a plot matrix,
      plot_series <- c(plot_series, list(legend))
   }

   GGally::ggmatrix(plot_series,
                    showStrips         = TRUE,
                    xAxisLabels        = c(to_plot_col, ""),
                    yAxisLabels        = yLabel,
                    showAxisPlotLabels = TRUE,
                    ncol               = nplots,
                    nrow               = 1)

}
