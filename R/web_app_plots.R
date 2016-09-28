# Named used for dummy column representing groups
GROUP_COL_NAME <- 'GROUP_DUMMY__'

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

# creates a layout matrix for categorical data
make_layout_matrix_hist <- function(data, view_cols){
   stopifnot(is.data.frame(data))
   stopifnot(is.vector(view_cols))
   stopifnot(length(view_cols) >= 1)
   stopifnot(all(view_cols %in% names(data)))

   # If many charts, default to 2 column layout
   if (length(view_cols) > 5)
      return(matrix(seq_along(view_cols), ncol=2, byrow = T))

   # Gets distinct values per column, checks if exceed threshold
   nvalues <- sapply(view_cols, function(col){
      length(unique(data[[col]]))
   })
   needs_two_cols <- nvalues > MAX_LEVELS_PER_LINE_HIST

   # Puts bigger charts first
   view_cols_index <- order(needs_two_cols, decreasing = T)
   needs_two_cols <- needs_two_cols[view_cols_index]

   # creates the layout matrix
   s <- rep(view_cols_index, ifelse(needs_two_cols, 2, 1))
   length(s) <- ceiling(length(s) / 2) * 2
   layout_matrix <- matrix(s, ncol = 2, byrow = T)

   layout_matrix
}


# Removes axise labels for plots located inside a grid,
# scales the remaining ones to avoid clutter
format_axis_labels <- function(plots, layout, colnames){
   stopifnot(is.list(plots))
   stopifnot(is.matrix(layout))
   stopifnot(max(layout, na.rm = T) == length(plots))
   stopifnot(nrow(layout) == ncol(layout))

   if(nrow(layout) <= 1) return(plots)

   # Gets the label size for the axis
   size_label  <- if (length(colnames) < 4) 12
                 else if (length(colnames) < 5) 10
                 else 8

   # Transformation
   for (i in 1:nrow(layout)){
      for (j in 1:ncol(layout)){

         plot_index <- layout[i,j]
         if (is.na(plot_index)) next

         # Fetches plot
         plot <- plots[[plot_index]]

         # Removes/Scales axises and ticks
         if (i < nrow(layout))
            plot <- plot + ggplot2::theme(
               axis.title.x=ggplot2::element_blank(),
               axis.text.x=ggplot2::element_blank()
            )
         else
            plot <- plot + ggplot2::theme(
               axis.title.x = ggplot2::element_text(size=size_label)
            )

         if (j > 1)
            plot <- plot + ggplot2::theme(
               axis.title.y=ggplot2::element_blank(),
               axis.text.y=ggplot2::element_blank()
            ) else
               plot <- plot + ggplot2::theme(
                  axis.title.y=ggplot2::element_text(size=size_label)
               )

         # Puts it back in list
         plots[[plot_index]] <- plot

      }
   }

   return(plots)
}

preprocess_for_histogram <- function(data, colx){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx) & length(colx) == 1)
   stopifnot(colx %in% names(data))

   series <- data[[colx]]
   series <- as.character(series)

   # Groups small categories
   if (length(unique(series)) > MAX_LEVELS_HIST){
      tab <- table(series)
      tab <- sort(tab, decreasing = T)
      to_replace <- names(tab)[MAX_LEVELS_HIST:length(tab)]
      new_name <- paste0(length(tab) - MAX_LEVELS_HIST, ' others')
      series[series %in% to_replace] <- new_name
   }

   # Crops long labels
   long_labels <- nchar(series) > MAX_CHAR_HIST
   long_labels <- na.omit(long_labels)
   series[long_labels] <- substr(series[long_labels], 1, MAX_CHAR_HIST)

   data[[colx]] <- series
   return(data)
}

trim_axis_label <- function(label, max){
   if (nchar(label) <= max) return(label)
   label <- strtrim(label, max)
   label <- paste0(label, '...')
   label
}

##################
# Plotting utils #
##################
ggplot_theme <- function(...){
   ggplot2::theme_bw() +
   ggplot2::theme(legend.text = ggplot2::element_text(size = 12),
                  legend.key.size = ggplot2::unit(1, "cm"),
                  legend.key	 =
                     ggplot2::element_rect(color = 'white'),
                  legend.title =
                     ggplot2::element_text(face = 'bold'),
                  ...)
}

draw_1d_histogram <- function(data, colx, colgroup=NULL, standalone=F){

   data <- preprocess_for_histogram(data, colx)

   # Adjustements related to colgroup
   if (is.null(colgroup)){
      ytitle <- 'Prop.'
      bars <- ggplot2::geom_bar(ggplot2::aes(y=(..count..)/sum(..count..)))
      scale_fill <- NULL
      scale_color  <- NULL
   } else {
      ytitle <- 'Prop. per group'
      bars <- ggplot2::geom_bar(ggplot2::aes_string(y='..prop..',
                                                    group = colgroup,
                                                    color = colgroup,
                                                    fill  = colgroup),
                                position='dodge')

      legend_title <- if (colgroup == GROUP_COL_NAME) 'Group' else colgroup
      scale_fill  <- ggplot2::scale_fill_discrete(legend_title)
      scale_color <- ggplot2::scale_color_discrete(legend_title)
   }

   # Adjustements related to standalone
   if (standalone){
      extra_theme <- NULL
   } else {
      extra_theme <- ggplot2::theme(legend.position="none")
   }

   # Makes the actual chart
   p <- ggplot2::ggplot(data, ggplot2::aes_string(x=colx)) +
         bars +
         ggplot2::scale_y_continuous(ytitle, labels = scales::percent) +
         scale_fill +
         scale_color +
         ggplot_theme(axis.text.x=ggplot2::element_text(angle=-25, hjust=0)) +
         extra_theme

   p
}

draw_1d_density <- function(data, colx, standalone=F, colgroup=NULL){

   minx <- min(data[[colx]], na.rm = T)
   maxx <- max(data[[colx]], na.rm = T)

   x_title <- trim_axis_label(colx, MAX_XLABEL_SIZE)
   y_title <- trim_axis_label(colx, MAX_YLABEL_SIZE)

   # Adjustements related to standalone
   if (!standalone){
      scale_x <- ggplot2::scale_x_continuous(name = x_title,
                                             limits=c(minx, maxx),
                                             breaks=c(minx, maxx))
      scale_y <- ggplot2::scale_y_continuous(y_title,
                                             breaks=c(0,1))
      theme <- ggplot_theme(
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
      theme <- ggplot_theme()
   }

   # Adjustements related to colgroup
   if (is.null(colgroup)){
      density_curve <- ggplot2::geom_density(fill = 'grey')
      scale_fill <- NULL
      scale_color  <- NULL
   } else {
      mapping <- ggplot2::aes_string(color = colgroup,
                                     fill  = colgroup)
      density_curve <- ggplot2::geom_density(show.legend = standalone,
                                             mapping,
                                             alpha = 0.5)
      legend_title <- if (colgroup == GROUP_COL_NAME) 'Group' else colgroup
      scale_fill <- ggplot2::scale_fill_discrete(legend_title)
      scale_color  <- ggplot2::scale_color_discrete(legend_title)
   }

   # Actual plot definition
   p <- ggplot2::ggplot(data=data, ggplot2::aes_string(x = colx)) +
      density_curve +
      scale_x +
      scale_y +
      scale_fill +
      scale_color +
      theme

   return(p)
}

draw_2d_scatterplot <- function(data, colx, coly, colgroup=NULL){

   minx <- min(data[[colx]], na.rm = T)
   maxx <- max(data[[colx]], na.rm = T)
   miny <- min(data[[coly]], na.rm = T)
   maxy <- max(data[[coly]], na.rm = T)

   scat_pt_size <- if (nrow(data) > 1500) .35
                   else if (nrow(data) > 1000) .5
                   else if (nrow(data) > 500) .75
                   else 1

   x_title <- trim_axis_label(colx, MAX_XLABEL_SIZE)
   y_title <- trim_axis_label(coly, MAX_YLABEL_SIZE)


   # Adjustements related to colgroup
   if (is.null(colgroup)){
      points       <- ggplot2::geom_point(size = scat_pt_size)
      scale_fill   <- NULL
      scale_color  <- NULL
      smooth       <- NULL
   } else {
      mapping <- ggplot2::aes_string(color = colgroup, fill  = colgroup)
      alpha   <- if (nrow(data) > 50) .5 else 1
      points      <- ggplot2::geom_point(mapping,
                                         size = scat_pt_size,
                                         alpha = alpha)
      scale_fill  <- ggplot2::scale_fill_discrete(guide=FALSE)
      scale_color <- ggplot2::scale_color_discrete(guide=FALSE)
      smooth      <- if (nrow(data)>20) ggplot2::geom_smooth(method=lm,
                                                             mapping,
                                                             se = F)
                     else NULL
   }

   p <- ggplot2::ggplot(data=data, ggplot2::aes_string(x = colx,
                                                       y = coly)) +
      points +
      ggplot2::scale_x_continuous(name = x_title,
                                  limits=c(minx, maxx), breaks=c(minx, maxx)) +
      ggplot2::scale_y_continuous(name = y_title,
                                  limits=c(miny, maxy), breaks=c(miny, maxy)) +
      smooth +
      scale_fill +
      scale_color +
      ggplot_theme(axis.text.y = ggplot2::element_text(angle=90))

   return(p)
}

extract_legend <- function(plot){
   stopifnot('ggplot' %in% class(plot))

   grob <- ggplot2::ggplotGrob(plot)$grobs
   grob_is_legend <- sapply(grob, function(x) x$name) == "guide-box"

   if (!any(grob_is_legend)) return(NULL)
   grob[[which(grob_is_legend)]]
}

draw_legend <- function(data, view_cols, colgroup, type){
   stopifnot(type %in% c('num', 'cat'))

    dummy_col <- view_cols[1]
    legend_title <- if (colgroup == GROUP_COL_NAME) 'Group'
                    else colgroup

    geom <-  if (type=='num')
               ggplot2::geom_point(ggplot2::aes_string(y = dummy_col),
                                   size=8,
                                   alpha=.5)
             else
                ggplot2::geom_bar()

    dummy_plot <- ggplot2::ggplot(data = data,
                                  ggplot2::aes_string(x = dummy_col,
                                                      color = colgroup,
                                                      fill  = colgroup)) +
                  ggplot2::scale_fill_discrete(legend_title) +
                  ggplot2::scale_color_discrete(legend_title) +
                  geom +
                  ggplot_theme()

    legend <- extract_legend(dummy_plot)

    return(legend)
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

   # Simple case: just one plot
   if (length(view_cols) == 1){
      p <- draw_1d_density(data=data, colx=view_cols[1], standalone=T)
      return(p)
   }

   # From now on, creates a plot grid
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
         if (col_i == col_j)
            p <- draw_1d_density(data=data, colx=col_i)
         else
            p <- draw_2d_scatterplot(data=data, colx=col_j, coly=col_i)
         # Appends it to the list
         plot_index <- layout_matrix[i,j]
         plots[[plot_index]] <- p
      }
   }

   # Ajusts axis label sizes and removes them if necessary
   plots <- format_axis_labels(plots, layout_matrix, view_cols)

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

   # Simple case: just one plot
   if (length(view_cols) == 1){
      p <- draw_1d_histogram(data, view_cols[1])
      return(p)
   }

   # From now on, creates a plot grid
   # Generates the layout
   layout_matrix <- make_layout_matrix_hist(data, view_cols)

   # Generates the plots
   n_plots <- length(view_cols)
   plots <- vector("list", n_plots)
   for (i in 1:n_plots){
     plots[[i]] <- draw_1d_histogram(data, view_cols[i])
   }

   # Done!
   gridExtra::grid.arrange(grobs=plots, layout_matrix = layout_matrix)

}

plot_views_num_to_compare<- function(data, view_cols, group1, group2,
                                     group1_name, group2_name){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))
   stopifnot(is.logical(group1) & length(group1) == nrow(data))
   stopifnot(is.logical(group2) & length(group2) == nrow(data))
   stopifnot(is.character(group1_name) & is.character(group2_name))


   if (!all(view_cols %in% names(data)))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   # Adds a dummy column with the group name
   group <- rep(NA_character_, nrow(data))
   group[group1] <- group1_name
   group[group2] <- group2_name
   group[group1 & group2] <- "Both groups"

   data <- cbind(data, group)
   names(data)[length(names(data))] <- GROUP_COL_NAME
   # Removes the plots which are outside the selection
   data <- data[!is.na(group),]

   ## Simple case: just one plot
   if (length(view_cols) == 1){
      p <- draw_1d_density(data=data, colx=view_cols[1],
                           standalone=T, colgroup=GROUP_COL_NAME)
      return(p)
   }

   ## From now on, creates a plot grid
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
         if (col_i == col_j)
            p <- draw_1d_density(data=data, colx=col_i,
                                 colgroup=GROUP_COL_NAME)
         else
            p <- draw_2d_scatterplot(data=data,
                                     colx=col_j,
                                     coly=col_i,
                                     colgroup=GROUP_COL_NAME)
         # Appends it to the list
         plot_index <- layout_matrix[i,j]
         plots[[plot_index]] <- p
      }
   }

   # Ajusts axis label sizes and removes them if necessary
   plots <- format_axis_labels(plots, layout_matrix, view_cols)

   # Adds the legend
   legend_grob  <- draw_legend(data, view_cols, GROUP_COL_NAME, 'num')
   legend_index <- n_plots + 1
   plots[[legend_index]] <- legend_grob
   layout_matrix[1, ncol(layout_matrix)] <- legend_index

   # Done!
   gridExtra::grid.arrange(grobs=plots, layout_matrix = layout_matrix)
}


plot_views_cat_to_compare <- function(data, view_cols, group1, group2,
                                      group1_name, group2_name){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))

   if (!all(view_cols %in% names(data)))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   # Adds a dummy column with the group name
   group <- rep(NA_character_, nrow(data))
   group[group1] <- group1_name
   group[group2] <- group2_name
   group[group1 & group2] <- "Both groups"

   data <- cbind(data, group)
   names(data)[length(names(data))] <- GROUP_COL_NAME
   # Removes the plots which are outside the selection
   data <- data[!is.na(group),]

   ## Simple case: just one plot
   if (length(view_cols) == 1){
      p <- draw_1d_histogram(data, view_cols[1],
                             colgroup = GROUP_COL_NAME, standalone = T)
      return(p)
   }

   ## From now on, creates a plot grid
   # Generates the layout
   layout_matrix <- make_layout_matrix_hist(data, view_cols)

   # Generates the plots
   n_plots <- length(view_cols)
   plots <- vector("list", n_plots)
   for (i in 1:n_plots){
      plots[[i]] <- draw_1d_histogram(data, view_cols[i],
                                      colgroup = GROUP_COL_NAME, standalone = F)
   }

   # Creates the legend
   legend_grob  <- draw_legend(data, view_cols, GROUP_COL_NAME, 'cat')
   legend_index <- n_plots + 1
   plots[[legend_index]] <- legend_grob
   # If there is space in the grid, puts it there...
   if (any(is.na(layout_matrix))){
      empty_slots <- which(is.na(layout_matrix))
      layout_matrix[empty_slots[1]] <- legend_index
   } else {
   # ... otherwise places it on the right side
      extra_layout_col <- rep(NA, nrow(layout_matrix))
      extra_layout_col[1] <- legend_index
      layout_matrix <- cbind(layout_matrix, extra_layout_col)
   }

   # Done!
   gridExtra::grid.arrange(grobs=plots, layout_matrix = layout_matrix)
}




#
#
# # Old stuff!!!!
# num_1d_view <- function(data, mapping, ...){
#    p <- ggplot2::ggplot(data=data, mapping=mapping) +
#       ggplot2::geom_density(...) +
#       ggplot2::theme_bw() +
#       ggplot2::theme(legend.text     = ggplot2::element_text(size = 12),
#                      legend.key.size = ggplot2::unit(1, "cm"))
#
#    if (is.ordered(data[[ncol(data)]]))
#       p <- p + ggplot2::scale_colour_brewer() +
#          ggplot2::scale_fill_brewer()
#
#    p
# }
#
# num_2d_view <- function(data, mapping, ...){
#
#    scat_pt_size <- if (nrow(data) > 1000) .5
#    else if (nrow(data) > 500) .75
#    else 1
#
#    p <- ggplot2::ggplot(data=data, mapping=mapping) +
#       ggplot2::geom_point(size = scat_pt_size, ...) +
#       ggplot2::theme_bw() +
#       ggplot2::theme(legend.text     = ggplot2::element_text(size = 12),
#                      legend.key.size = ggplot2::unit(1, "cm"))
#
#    if (nrow(data) > 20) p <- p + ggplot2::geom_smooth(method=lm, se = F)
#    if (is.ordered(data[[ncol(data)]]))
#       p <- p + ggplot2::scale_colour_brewer() +
#       ggplot2::scale_fill_brewer()
#
#    p
# }
#
# cat_1d_view <- function(data, mapping, ...){
#    p <- ggplot2::ggplot(data, mapping) +
#       ggplot2::geom_bar(position = "dodge", ...) +
#       ggplot2::scale_y_continuous(labels = scales::percent) +
#       ggplot2::theme_bw() +
#       ggplot2::theme(axis.text.x=ggplot2::element_text(angle=-35, hjust=0),
#                      legend.position = "left")
#
#    if (is.ordered(data[[ncol(data)]]))
#       p <- p + ggplot2::scale_colour_brewer() +
#          ggplot2::scale_fill_brewer()
#
#    p
# }
#
#
# #### Wrappers
# plot_selection_numeric <- function(data, target, app_type){
#    col_is_num <- sapply(data, is.numeric)
#    if (!all(col_is_num)) stop('Cannot plot, type not supported')
#
#    # Prepares the data frame to be visualized
#    data <- cbind(data, target)
#    to_plot_index <- 1:(ncol(data)-1)
#    to_plot_col   <- names(data)[to_plot_index]
#    labels_col    <- names(data)[[ncol(data)]]
#
#
#    # Sets plotting parameters, depending on app type
#    if (app_type == 'findviews'){
#       def_color    <- NULL
#       def_fills    <- NULL
#       def_cst_fill <- "grey"
#       def_alpha   <- 1
#       show_legend <- F
#
#    } else if (app_type == 'findviews_to_compare'){
#       def_color   <- labels_col
#       def_fills   <- labels_col
#       def_cst_fill <- NULL
#       def_alpha   <- .5
#       show_legend <- T
#
#    } else if (app_type == 'findviews_to_predict'){
#       def_color    <- labels_col
#       def_fills    <- labels_col
#       def_cst_fill <- NULL
#       def_alpha    <- .5
#       show_legend  <- T
#    }
#
#    # 1D data -> density plot
#    if (ncol(data) == 2){
#       title <- paste0('Density plot for the variable ', names(data)[[1]])
#       plot_args <- list(data    = data,
#                         mapping = ggplot2::aes_string(x = to_plot_col,
#                                                       color = def_color,
#                                                       fill  = def_fills),
#                         alpha = def_alpha)
#       if (!is.null(def_cst_fill)) plot_args[['fill']] <- def_cst_fill
#
#       do.call(num_1d_view, plot_args) + ggplot2::ggtitle(title)
#
#       # 2d and more -> scatterplot matrix
#    } else if (ncol(data) >= 3){
#       # Context-dependent graph parameters
#       title <- "Density plots (diagonal) and 2D scatterplots (all the other charts)"
#
#       lower_plots <- GGally::wrap(num_2d_view, alpha = def_alpha)
#       diag_plots  <- if (!is.null(def_cst_fill))
#          GGally::wrap(num_1d_view, fill = def_cst_fill)
#       else GGally::wrap(num_1d_view, alpha = def_alpha)
#
#       # Puts them all in matrix
#       pairs <- GGally::ggpairs(data,
#                                mapping = ggplot2::aes_string(color = def_color,
#                                                              fill  = def_fills),
#                                columns = to_plot_index,
#                                lower = list('continuous' = lower_plots),
#                                diag  = list('continuous' = diag_plots),
#                                upper = list('continuous' = 'blank'),
#                                legends = FALSE,
#                                title   = title)
#
#       # If necessary, generates and inserts the legend
#       if (show_legend){
#          plot_legend_fn <- GGally::gglegend(num_1d_view)
#          legend <- plot_legend_fn(data, ggplot2::aes_string(x = labels_col[1],
#                                                             color = labels_col,
#                                                             fill  = labels_col))
#          pairs[1, length(to_plot_col)] <- legend
#       }
#
#       # Done!
#       pairs
#    }
# }
#
# plot_selection_categorical <- function(data, target, app_type){
#
#    # Prepares the data frame to be visualized
#    data <- cbind(data, target)
#
#    # Gets the column names
#    to_plot_index <- 1:(ncol(data)-1)
#    to_plot_col   <- names(data)[to_plot_index]
#    labels_col    <- names(data)[[ncol(data)]]
#
#    # Sets plotting parameters, depending on app type
#    if (app_type == 'findviews'){
#       def_color   <- NULL
#       def_fills   <- NULL
#       show_legend <- F
#       nplots      <- length(to_plot_col)
#       yLabel      <- "Frequency"
#
#    } else if (app_type == 'findviews_to_compare'){
#       def_color   <- labels_col
#       def_fills   <- labels_col
#       show_legend <- T
#       nplots      <- length(to_plot_col) + 1
#       yLabel      <- "Frequency in target group"
#
#    } else if (app_type == 'findviews_to_predict'){
#       def_color   <- labels_col
#       def_fills   <- labels_col
#       show_legend <- T
#       nplots      <- length(to_plot_col) + 1
#       yLabel      <- "Frequency in target group"
#    }
#
#    # Creates the series of plots
#    plot_series <- lapply(to_plot_col, function(col){
#       cat_1d_view(data,
#                   mapping = ggplot2::aes_string(x = col,
#                                                 color = def_color,
#                                                 fill  = def_fills),
#                   ggplot2::aes_string(y = '..prop..',
#                                       group = labels_col))
#    })
#
#    # Generates the legend, if necessary
#    if (show_legend){
#       plot_legend_fn <- GGally::gglegend(cat_1d_view)
#       legend <- plot_legend_fn(data, ggplot2::aes_string(x = labels_col[1],
#                                                          color = def_color,
#                                                          fill  = def_fills))
#       # Places everything in a plot matrix,
#       plot_series <- c(plot_series, list(legend))
#    }
#
#    GGally::ggmatrix(plot_series,
#                     showStrips         = TRUE,
#                     xAxisLabels        = c(to_plot_col, ""),
#                     yAxisLabels        = yLabel,
#                     showAxisPlotLabels = TRUE,
#                     ncol               = nplots,
#                     nrow               = 1)
#
# }
