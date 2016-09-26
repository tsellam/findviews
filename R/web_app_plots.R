#########
# Utils #
#########
# Creates a lower triangular matrix with numbers from 1 to n
make_layout_matrix <- function(view_cols){
   stopifnot(is.vector(view_cols))
   stopifnot(length(view_cols) >= 1)

   layout_matrix <- matrix(NA, nrow=length(view_cols), ncol=length(view_cols))
   lower_triangle <- lower.tri(layout_matrix, diag = T)

   n_plots <- sum(lower_triangle, na.rm = T)
   seq <- 1:n_plots
   # This is necessary to fill the values by row (R's default is by column)
   ord <- order(
      row(lower_triangle)[lower_triangle],
      col(lower_triangle)[lower_triangle]
   )

   layout_matrix[lower_triangle] <- seq[ord]

   layout_matrix
}


# Removes axise labels for plots located inside a grid
remove_internal_labels <- function(plots, layout){
   stopifnot(is.list(plots))
   stopifnot(is.matrix(layout))
   stopifnot(max(layout, na.rm = T) == length(plots))
   stopifnot(nrow(layout) == ncol(layout))

   if(nrow(layout) <= 1) return(plots)

   for (i in 1:nrow(layout)){
      for (j in 1:ncol(layout)){
         # Fetches plot
         plot_index <- layout[i,j]
         if (is.na(plot_index)) next
         plot <- plots[[plot_index]]

         # Removes axises as necessary and ticks
         if (i < nrow(layout))
            plot <- plot + ggplot2::theme(
               axis.title.x=ggplot2::element_blank(),
               axis.text.x=ggplot2::element_blank()
            )
         if (j > 1)
            plot <- plot + ggplot2::theme(
               axis.title.y=ggplot2::element_blank(),
               axis.text.y=ggplot2::element_blank()
            )

         # Puts it back in list
         plots[[plot_index]] <- plot

      }
   }

   return(plots)
}

##################
# Plotting utils #
##################
ggplot_theme <- function(){
   ggplot2::theme_bw() +
   ggplot2::theme(legend.text = ggplot2::element_text(size = 12),
                  legend.key.size = ggplot2::unit(1, "cm"))
}

draw_1d_histogram <- function(data, colx){
   p <- ggplot2::ggplot(data, ggplot2::aes_string(x=colx)) +
      ggplot2::geom_bar(ggplot2::aes(y = (..count..)/sum(..count..))) +
      ggplot2::scale_y_continuous('Prop.', labels = scales::percent) +
      ggplot_theme()

   p
}

draw_1d_density <- function(data, colx, standalone=F){

   minx <- min(data[[colx]], na.rm = T)
   maxx <- max(data[[colx]], na.rm = T)

   if (!standalone){
      scale_x <- ggplot2::scale_x_continuous(limits=c(minx, maxx),
                                             breaks=c(minx, maxx))
      scale_y <- ggplot2::scale_y_continuous(colx,
                                             breaks=c(0,1))
      theme_extra <- ggplot2::theme(
         #axis.title.x = ggplot2::element_text(color="white"),
         #axis.title.y = ggplot2::element_text(color="white"),
         axis.text.y  = ggplot2::element_text(color="white"),
         axis.line.y  = ggplot2::element_blank(),
         axis.ticks.y = ggplot2::element_line(color="white"),
         panel.border = ggplot2::element_blank(),
         panel.grid.major = ggplot2::element_blank(),
         panel.grid.minor = ggplot2::element_blank(),
         axis.text.y = ggplot2::element_text(angle=90)
      )
   } else {
      scale_x <- ggplot2::scale_x_continuous()
      scale_y <- ggplot2::scale_y_continuous('Distribution (density function)')
      theme_extra <- ggplot2::theme(axis.text.y = ggplot2::element_text(angle=0))
   }

   p <- ggplot2::ggplot(data=data, ggplot2::aes_string(x = colx)) +
      ggplot2::geom_density(fill = 'grey', show.legend = TRUE) +
      scale_x +
      scale_y +
      ggplot_theme() + theme_extra

   return(p)
}

draw_2d_scatterplot <- function(data, colx, coly){

   minx <- min(data[[colx]], na.rm = T)
   maxx <- max(data[[colx]], na.rm = T)
   miny <- min(data[[coly]], na.rm = T)
   maxy <- max(data[[coly]], na.rm = T)

   scat_pt_size <- if (nrow(data) > 1000) .5
   else if (nrow(data) > 500) .75
   else 1

   p <- ggplot2::ggplot(data=data, ggplot2::aes_string(x = colx,
                                                       y = coly)) +
      ggplot2::geom_point(size = scat_pt_size) +
      ggplot2::scale_x_continuous(limits=c(minx, maxx), breaks=c(minx, maxx)) +
      ggplot2::scale_y_continuous(limits=c(miny, maxy), breaks=c(miny, maxy)) +
      ggplot_theme() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(angle=90))

   return(p)
}

###############################
# Plots for vanilla findviews #
###############################
plot_views_num <- function(data, view_cols){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))

   if (!all(view_cols %in% names(data)))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   # Layouting
   layout_matrix <- make_layout_matrix(view_cols)
   n_plots <- max(layout_matrix, na.rm = T)

   # Graph plotting
   plots <- vector("list", n_plots)
   for (i in 1:length(view_cols)){
      for (j in 1:i){
         # Retrieves columns
         col_i <- view_cols[i]
         col_j <- view_cols[j]
         # Draws the appropriate chart
         if (length(view_cols) == 1)
            p <- draw_1d_density(data=data, colx=col_i, standalone=T)
         else if (col_i == col_j)
            p <- draw_1d_density(data=data, colx=col_i)
         else
            p <- draw_2d_scatterplot(data=data, colx=col_j, coly=col_i)
         # Appends it to the list
         plot_index <- layout_matrix[i,j]
         plots[[plot_index]] <- p
      }
   }

   # Removes intermediate axis labels
   plots <- remove_internal_labels(plots, layout_matrix)

   # Done!
   gridExtra::grid.arrange(grobs=plots, layout_matrix = layout_matrix)
}

plot_views_cat <- function(data, view_cols){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))

   if (!all(view_cols %in% names(data)))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   n_plots <- length(view_cols)
   plots <- vector("list", n_plots)
   for (i in 1:n_plots){
     plots[[i]] <- draw_1d_histogram(data, view_cols[i])
   }

   # Done!
   ncol <- 2
   gridExtra::grid.arrange(grobs=plots, ncol=ncol)

}











# Old stuff!!!!
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
