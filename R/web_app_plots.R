# Named used for dummy column representing groups
GROUP_COL_NAME <- 'GROUP_DUMMY__'

#############################
#### Layouting Functions ####
#############################
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
   if (length(view_cols) > 4){
      s <- seq_along(view_cols)
      length(s) <- ceiling(length(s) / 2) * 2
      return(matrix(s, ncol=2, byrow = T))
   }

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

########################
#### Plotting utils ####
########################
get_breaks <- function(low, high){
   stopifnot(is.numeric(low), is.numeric(high), low<=high)

   r_breaks <- pretty(c(low, high), min.n=2)

   r_breaks <- r_breaks[r_breaks >= low & r_breaks <= high]
   if (length(na.omit(r_breaks)) <= 1){
      return(c(low, high))
   } else if (length(r_breaks) == 2){
      return(r_breaks)
   } else {
      return(c(min(r_breaks), max(r_breaks)))
   }
}

# Removes axise labels for plots located inside a grid
format_axis_labels <- function(plots, layout, colnames, ignore_diag=F){
   stopifnot(is.list(plots))
   stopifnot(is.matrix(layout))
   stopifnot(max(layout, na.rm = T) == length(plots))
   stopifnot(nrow(layout) == ncol(layout))

   if(nrow(layout) <= 1) return(plots)


   # Transformation
   for (i in 1:nrow(layout)){
      for (j in 1:ncol(layout)){

         plot_index <- layout[i,j]
         if (is.na(plot_index)) next

         # Fetches plot
         plot <- plots[[plot_index]]

         ## Special case = ignore_diagonal
         if (i==j & ignore_diag){
            next
         }

         # Removes/Scales axises and ticks
         if (i < nrow(layout))
            plot <- plot + ggplot2::theme(
               axis.title.x=ggplot2::element_text(color='white'),
               axis.text.x=ggplot2::element_text(color='white')
               # axis.title.x=ggplot2::element_blank(),
               # axis.text.x=ggplot2::element_blank()
            )

         if (j > 1)
            plot <- plot + ggplot2::theme(
               axis.title.y=ggplot2::element_text(color='white'),
               axis.text.y=ggplot2::element_text(color='white')
               # axis.title.y=ggplot2::element_blank(),
               # axis.text.y=ggplot2::element_blank()
            )

         # Puts it back in list
         plots[[plot_index]] <- plot

      }
   }

   return(plots)
}

preprocess_for_histogram <- function(data, colx, faceted = F){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx) & length(colx) == 1)
   stopifnot(colx %in% names(data))

   series <- data[[colx]]
   series <- as.character(series)

   # Groups small categories
   max_levels <- if (!faceted) MAX_LEVELS_HIST
                  else MAX_LEVELS_FACET_HIST
   if (length(unique(series)) > max_levels){
      tab <- table(series)
      tab <- sort(tab, decreasing = T)
      to_replace <- names(tab)[max_levels:length(tab)]
      new_name <- paste0(length(tab) - max_levels, ' others')
      series[series %in% to_replace] <- new_name
   }

   # Crops long labels
   long_labels <- nchar(series) > MAX_CHAR_HIST
   long_labels <- na.omit(long_labels)
   series[long_labels] <- substr(series[long_labels], 1, MAX_CHAR_HIST)

   data[[colx]] <- series
   return(data)
}




ggplot_theme <- function(...){
   ggplot2::theme_bw() +
   ggplot2::theme(panel.grid = ggplot2::element_blank(),
                  legend.text = ggplot2::element_text(size = 12),
                  legend.key	 =
                     ggplot2::element_rect(color = 'white'),
                  legend.title =
                     ggplot2::element_text(face = 'bold'),
                  legend.key.size =
                     ggplot2::unit(.4, "cm"),
                  ...)
}

##################################
# Axis labels manipulation utils #
##################################
create_label_setup_num <- function(n_cols){

   size <- if (n_cols < 4) 12
           else if (n_cols < 5) 10
           else 8

   nchar_x <- if (n_cols == 1) 80
              else if (n_cols <= 3) 40
              else if (n_cols <= 5) 30
              else if (n_cols > 5)  15

   nchar_y <- if (n_cols == 1) 40
            else if (n_cols <= 3) 20
            else if (n_cols <= 5) 15
            else if (n_cols > 5)  10

   return(list(
      size = size,
      nchar_x = nchar_x,
      nchar_y = nchar_y
   ))
}

create_label_setup_cat <- function(n_cols){

   size <- if (n_cols < 4) 12
           else if (n_cols < 5) 10
           else 8

   nchar_x <- if (n_cols == 1) 60
            else if (n_cols <= 4) 40
            else if (n_cols > 4) 30

   return(list(
      size = size,
      nchar_x = nchar_x
   ))
}

trim_axis_title <- function(label, max){
   if (nchar(label) <= max) return(label)
   label <- strtrim(label, max)
   label <- paste0(label, '.')
   label
}

#############################
# Legend Manipulation Utils #
#############################
extract_legend <- function(plot){
   stopifnot('ggplot' %in% class(plot))

   grob <- ggplot2::ggplotGrob(plot)$grobs
   grob_is_legend <- sapply(grob, function(x) x$name) == "guide-box"

   if (!any(grob_is_legend)) return(NULL)
   grob[[which(grob_is_legend)]]
}

draw_legend_cat <- function(data, view_cols, colgroup, type){
   stopifnot(type %in% c('num', 'cat'))

   dummy_col <- view_cols[1]
   legend_title <- if (colgroup == GROUP_COL_NAME) 'Group'
   else colgroup

   geom <-  if (type=='num')
      ggplot2::geom_point(ggplot2::aes_string(y = dummy_col), size=8,na.rm=T)
   else
      ggplot2::geom_bar(na.rm = T)

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

draw_legend_cont <- function(data, view_cols, target){

   dummy_plot <- ggplot2::ggplot(data = data,
                                 ggplot2::aes_string(x = view_cols[1],
                                                     y = view_cols[2],
                                                     z = target)) +
      ggplot2::stat_summary_2d(na.rm = T) +
      ggplot2::scale_fill_continuous(target) +
      ggplot2::scale_color_continuous(target) +
      ggplot_theme()

   legend <- extract_legend(dummy_plot)

   return(legend)
}



#############################
#### Collection of Plots ####
#############################
## 1D categorical data
draw_1d_histogram <- function(data, colx, setup){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx))
   stopifnot(colx %in% names(data))
   stopifnot(is.list(setup))
   stopifnot(all(c('size', 'nchar_x') %in% names(setup)))

   data <- preprocess_for_histogram(data, colx)

   size_title <- setup$size
   title_x <- trim_axis_title(colx, setup$nchar_x)

   # Makes the actual chart
      p <- ggplot2::ggplot(data, ggplot2::aes_string(x=colx)) +
         ggplot2::geom_bar(ggplot2::aes(y=(..count..)/sum(..count..)),
                           na.rm = T) +
         ggplot2::scale_x_discrete(name = title_x) +
         ggplot2::scale_y_continuous('Prop.', labels = scales::percent) +
         ggplot_theme(axis.text.x=ggplot2::element_text(size=size_title,
                                                        angle=-25,
                                                        hjust=0),
                      axis.text.y=ggplot2::element_text(size=size_title))

   p
}

draw_1d_faceted_histogram <- function(data, colx, colgroup, setup){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx))
   stopifnot(colx %in% names(data))
   stopifnot(is.character(colgroup))
   stopifnot(colgroup %in% names(data))
   stopifnot(is.list(setup))
   stopifnot(all(c('size', 'nchar_x') %in% names(setup)))

   data <- preprocess_for_histogram(data, colx, faceted=T)

   size_title <- setup$size
   title_x <- trim_axis_title(colx, setup$nchar_x)

   # Makes the actual chart
   p <- ggplot2::ggplot(data, ggplot2::aes_string(x=colx)) +
      ggplot2::geom_bar(ggplot2::aes_string(y='..prop..', group = colgroup),
                        na.rm = T) +
      ggplot2::scale_x_discrete(name = title_x) +
      ggplot2::scale_y_continuous('Prop.', labels = scales::percent) +
      ggplot_theme(
         axis.text.x=ggplot2::element_text(size=size_title, angle=-30, hjust=0),
         axis.text.y=ggplot2::element_text(size=size_title)
      ) +
      ggplot2::facet_grid(paste0('.~', colgroup))

   p
}


draw_1d_stacked_histogram <- function(data, colx, colgroup, setup, standalone=F){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx))
   stopifnot(colx %in% names(data))
   stopifnot(is.character(colgroup))
   stopifnot(colgroup %in% names(data))
   stopifnot(is.list(setup))
   stopifnot(all(c('size', 'nchar_x') %in% names(setup)))
   data <- preprocess_for_histogram(data, colx)

   ytitle <- 'Composition'
   size_title <- setup$size
   title_x <- trim_axis_title(colx, setup$nchar_x)

   # Adjustements related to standalone
   if (standalone){
      extra_theme <- NULL
   } else {
      extra_theme <- ggplot2::theme(legend.position="none")
   }

   # Makes the actual chart
   p <- ggplot2::ggplot(data, ggplot2::aes_string(x=colx)) +
      ggplot2::geom_bar(ggplot2::aes_string(y='(..count..)/sum(..count..)',
                                            color = colgroup,
                                            fill  = colgroup,
                                            ),
                        na.rm = T,
                        position = 'stack') +
      ggplot2::scale_x_discrete(name = title_x) +
      ggplot2::scale_y_continuous(ytitle, labels = scales::percent) +
      ggplot2::scale_fill_discrete() +
      ggplot2::scale_color_discrete() +
      ggplot_theme(
         axis.text.x = ggplot2::element_text(size = size_title, angle=-25, hjust=0),
         axis.title.y =ggplot2::element_text(face="bold.italic")) +
      extra_theme

   p
}

draw_1d_cat_influence <- function(data, colx, target, setup){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx))
   stopifnot(colx %in% names(data))
   stopifnot(is.character(target))
   stopifnot(target %in% names(data))
   stopifnot(is.list(setup))
   stopifnot(all(c('size', 'nchar_x') %in% names(setup)))

   data <- preprocess_for_histogram(data, colx)

   size_title <- setup$size
   title_x <- trim_axis_title(colx, setup$nchar_x)


   data <- preprocess_for_histogram(data, colx)

   # Makes the actual chart
   p <- ggplot2::ggplot(data, ggplot2::aes_string(x=colx, y=target)) +
      ggplot2::geom_boxplot(na.rm = T) +
      ggplot2::scale_x_discrete(name = title_x) +
      ggplot2::scale_y_continuous() +
      ggplot_theme(
         axis.text.x=ggplot2::element_text(size=size_title, angle=-25, hjust=0),
         axis.title.y=ggplot2::element_text(size = size_title, face="bold.italic")
      )
   p
}


## 1D continuous data
draw_1d_density <- function(data, colx, setup, standalone=F,
                            colgroup=NULL, stacked=F){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx))
   stopifnot(colx %in% names(data))
   stopifnot(is.list(setup))
   stopifnot(all(c('size', 'nchar_x', 'nchar_y') %in% names(setup)))

   minx <- min(data[[colx]], na.rm = T)
   maxx <- max(data[[colx]], na.rm = T)

   title_x <- trim_axis_title(colx, setup$nchar_x)
   size_title <- setup$size

   # Adjustements related to 'standalone' and 'stacked'
   if (!standalone){
      scale_x <- ggplot2::scale_x_continuous(name = title_x,
                                             limits=c(minx, maxx),
                                             breaks=get_breaks(minx, maxx))
      scale_y <- ggplot2::scale_y_continuous(name = 'Distribution',
                                             breaks=c(0,1))
      theme <- ggplot_theme(
         axis.title.x  = ggplot2::element_text(size = size_title),
         axis.title.y  = ggplot2::element_text(size = size_title,
                                               face ="bold.italic"),
         axis.text.y  = ggplot2::element_text(color="white", angle=90),
         axis.line.y  = ggplot2::element_blank(),
         axis.ticks.y = ggplot2::element_line(color="white"),
         panel.border = ggplot2::element_blank(),
         panel.background = ggplot2::element_rect(fill = "grey95")
      )
   } else {
      scale_x <- ggplot2::scale_x_continuous(name=title_x)
      title <- paste0('Distribution (density function, ',
                      ifelse(stacked, 'stacked', 'normalized'),
                     ' )')
      scale_y <- ggplot2::scale_y_continuous(title)
      theme <- ggplot_theme(
         axis.title.x  = ggplot2::element_text(size = size_title),
         axis.title.y  = ggplot2::element_text(size = size_title,
                                               face ="bold.italic"),
         panel.background = ggplot2::element_rect(fill = "grey95"),
         axis.title.y  = ggplot2::element_text(face="bold.italic")
      )
   }


   # Adjustements related to 'colgroup' and 'stacked'
   if (is.null(colgroup)){
      density_curve <- ggplot2::geom_density(fill = 'grey', na.rm = T)
      scale_fill <- NULL
      scale_color  <- NULL
   } else {
      if (!stacked){
         mapping <- ggplot2::aes_string(color = colgroup,
                                        fill  = colgroup)
         density_curve <- ggplot2::geom_density(show.legend = standalone,
                                                mapping,
                                                alpha = 0.5,
                                                na.rm = T)
      } else {
         mapping <- ggplot2::aes_string(y = '..count..',
                                        color = colgroup,
                                        fill  = colgroup)
         density_curve <- ggplot2::geom_density(show.legend = standalone,
                                                mapping,
                                                position = "stack",
                                                alpha = 0.75,
                                                na.rm = T)
      }
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

draw_2d_scatterplot <- function(data, colx, coly, setup,
                                colgroup=NULL){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx) & is.character(coly))
   stopifnot(colx %in% names(data) & coly %in% names(data))
   stopifnot(is.list(setup))
   stopifnot(all(c('size', 'nchar_x', 'nchar_y') %in% names(setup)))

   minx <- min(data[[colx]], na.rm = T)
   maxx <- max(data[[colx]], na.rm = T)
   miny <- min(data[[coly]], na.rm = T)
   maxy <- max(data[[coly]], na.rm = T)

   scat_pt_size <- if (nrow(data) > 1500) .35
                   else if (nrow(data) > 1000) .5
                   else if (nrow(data) > 500) .75
                   else 1

   title_x <- trim_axis_title(colx, setup$nchar_x)
   title_y <- trim_axis_title(coly, setup$nchar_y)
   size_title <- setup$size


   # Adjustements related to colgroup
   if (is.null(colgroup)){
      points       <- ggplot2::geom_point(size = scat_pt_size, na.rm = T)
      scale_fill   <- NULL
      scale_color  <- NULL
      smooth       <- NULL
   } else {
      mapping <- ggplot2::aes_string(color = colgroup, fill  = colgroup)
      alpha   <- if (nrow(data) > 50) .5 else 1
      points      <- ggplot2::geom_point(mapping,
                                         size = scat_pt_size,
                                         alpha = alpha,
                                         na.rm = T)
      scale_fill  <- ggplot2::scale_fill_discrete(guide=FALSE)
      scale_color <- ggplot2::scale_color_discrete(guide=FALSE)
   }

   # Attempts a 2D kernel estimate
  if (nrow(data) > 50 & !is.null(colgroup)){
      surface <- ggplot2::geom_density_2d(
         ggplot2::aes_string(color = colgroup),
         na.rm=T
      )
  } else {
     surface <- NULL
  }


   p <- ggplot2::ggplot(data=data, ggplot2::aes_string(x = colx,
                                                       y = coly)) +
      points + surface +
      ggplot2::scale_x_continuous(name = title_x,
                                  limits=c(minx, maxx),
                                  breaks=get_breaks(minx, maxx)) +
      ggplot2::scale_y_continuous(name = title_y,
                                  limits=c(miny, maxy),
                                  breaks=get_breaks(miny, maxy)) +
      scale_fill +
      scale_color +
      ggplot_theme(
         axis.title.x = ggplot2::element_text(size = size_title),
         axis.title.y = ggplot2::element_text(size = size_title),
         axis.text.y = ggplot2::element_text(angle=90)
      )

   return(p)
}

draw_1d_num_influence <- function(data, colx, target, setup, standalone=F){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx) & is.character(target))
   stopifnot(colx %in% names(data) & target %in% names(data))
   stopifnot(is.list(setup))
   stopifnot(all(c('size', 'nchar_x', 'nchar_y') %in% names(setup)))

   minx <- min(data[[colx]], na.rm = T)
   maxx <- max(data[[colx]], na.rm = T)

   miny <- min(data[[target]], na.rm = T)
   maxy <- max(data[[target]], na.rm = T)


   scat_pt_size <- if (nrow(data) > 1500) .35
   else if (nrow(data) > 1000) .5
   else if (nrow(data) > 500) .75
   else 1

   title_x <- trim_axis_title(colx, setup$nchar_x)
   title_y <- trim_axis_title(target, setup$nchar_y)
   size_title <- setup$size

   if (standalone){
      theme <- ggplot_theme(
         axis.title.x = ggplot2::element_text(size = size_title),
         axis.title.y = ggplot2::element_text(size = size_title,
                                              face ="bold.italic"),
         panel.background = ggplot2::element_rect(fill = "grey95")
      )
      geom <- ggplot2::geom_point(size=scat_pt_size, na.rm = T)
      x_axis <- ggplot2::scale_x_continuous(name = title_x)
      y_axis <- ggplot2::scale_y_continuous(name = title_y)

   } else {
      theme <- ggplot_theme(
         axis.title.x = ggplot2::element_text(size = size_title),
         axis.title.y =ggplot2::element_text(size = size_title,
                                             face ="bold.italic"),
         panel.background = ggplot2::element_rect(fill = "grey95"),
         legend.position="none",
         axis.text.y = ggplot2::element_text(angle=90)
      )
      geom <- ggplot2::geom_point(ggplot2::aes_string(color=target),
                                  size=scat_pt_size, na.rm = T)
      x_axis <- ggplot2::scale_x_continuous(name = title_x,
                                            breaks = get_breaks(minx,maxx),
                                            limits = c(minx, maxx))
      y_axis <- ggplot2::scale_y_continuous(name = title_y,
                                            breaks = get_breaks(miny,maxy),
                                            limits =  c(miny,maxy))

   }

   g <- ggplot2::ggplot(data, ggplot2::aes_string(x = colx,
                                                  y = target)) +
      geom +
      ggplot2::geom_smooth(se = F, na.rm = T) +
      x_axis +
      y_axis +
      theme
   g
}



draw_2d_num_influence <- function(data, colx, coly, target, setup, standalone = F){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(colx) & is.character(coly))
   stopifnot(colx %in% names(data) & coly %in% names(data))
   stopifnot(is.list(setup))
   stopifnot(all(c('size', 'nchar_x', 'nchar_y') %in% names(setup)))

   minx <- min(data[[colx]], na.rm = T)
   maxx <- max(data[[colx]], na.rm = T)
   miny <- min(data[[coly]], na.rm = T)
   maxy <- max(data[[coly]], na.rm = T)

   title_x <- trim_axis_title(colx, setup$nchar_x)
   title_y <- trim_axis_title(coly, setup$nchar_y)
   size_title <- setup$size

   p <- ggplot2::ggplot(data=data, ggplot2::aes_string(x = colx,
                                                       y = coly)) +
      ggplot2::stat_summary_2d(ggplot2::aes_string(z = target), na.rm=T) +
      ggplot2::scale_x_continuous(name = title_x,
                                  breaks=get_breaks(minx, maxx),
                                  limits =c(minx,maxx),
                                  oob = scales::squish) +
      ggplot2::scale_y_continuous(name = title_y,
                                  breaks=get_breaks(miny, maxy),
                                  limits =  c(miny,maxy),
                                  oob = scales::squish) +
      ggplot2::scale_fill_continuous(guide = if(standalone) 'colourbar' else FALSE) +
      ggplot_theme(
         axis.title.x = ggplot2::element_text(size = size_title),
         axis.title.y = ggplot2::element_text(size = size_title),
         axis.text.y = ggplot2::element_text(angle=90)
      )


   return(p)
}



#######################################
#### High Level Plotting Functions ####
#######################################
plot_views_num <- function(data, view_cols){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))

   if (!all(view_cols %in% names(data)))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   label_setup <- create_label_setup_num(length(view_cols))

   ## Simple case: just one plot
   if (length(view_cols) == 1){
      p <- draw_1d_density(data=data, colx=view_cols[1], label_setup,
                           standalone=T)
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
            p <- draw_1d_density(data=data, colx=col_i, label_setup)
         else
            p <- draw_2d_scatterplot(data=data, colx=col_j, coly=col_i, label_setup)
         # Appends it to the list
         plot_index <- layout_matrix[i,j]
         plots[[plot_index]] <- p
      }
   }

   # Ajusts axis label sizes and removes them if necessary
   plots <- format_axis_labels(plots, layout_matrix, view_cols, ignore_diag = T)

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
   label_setup <- create_label_setup_cat(n_plots)

   ## Simple case: just one plot
   if (n_plots == 1){
      p <- draw_1d_histogram(data, view_cols[1], label_setup)
      return(p)
   }

   ## From now on, creates a plot grid
   # Generates the layout
   layout_matrix <- make_layout_matrix_hist(data, view_cols)

   # Generates the plots
   plots <- vector("list", n_plots)
   for (i in 1:n_plots){
     plots[[i]] <- draw_1d_histogram(data, view_cols[i], label_setup)
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

   label_setup <- create_label_setup_num(length(view_cols))

   ## Simple case: just one plot
   if (length(view_cols) == 1){
      p <- draw_1d_density(data=data, colx=view_cols[1], label_setup,
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
            p <- draw_1d_density(data=data, colx=col_i, label_setup,
                                 colgroup=GROUP_COL_NAME)
         else
            p <- draw_2d_scatterplot(data=data,
                                     colx=col_j,
                                     coly=col_i,
                                     label_setup,
                                     colgroup=GROUP_COL_NAME)
         # Appends it to the list
         plot_index <- layout_matrix[i,j]
         plots[[plot_index]] <- p
      }
   }

   # Ajusts axis label sizes and removes them if necessary
   plots <- format_axis_labels(plots, layout_matrix, view_cols, ignore_diag=T)

   # Adds the legend
   legend_grob  <- draw_legend_cat(data, view_cols, GROUP_COL_NAME, 'num')
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

   n_plots <- length(view_cols)
   label_setup <- create_label_setup_cat(n_plots)

   ## Simple case: just one plot
   if (n_plots == 1){
      p <- draw_1d_faceted_histogram(data, view_cols[1],
                             colgroup = GROUP_COL_NAME,
                             label_setup)
      return(p)
   }

   ## From now on, creates a plot grid
   # Generates the layout
   layout_matrix <- make_layout_matrix_hist(data, view_cols)

   # Generates the plots
   plots <- vector("list", n_plots)
   for (i in 1:n_plots){
      plots[[i]] <- draw_1d_faceted_histogram(data, view_cols[i],
                             colgroup = GROUP_COL_NAME,
                             label_setup)
   }

   # Done!
   gridExtra::grid.arrange(grobs=plots, layout_matrix = layout_matrix)
}

plot_views_num_to_predict_cat <- function(data, view_cols, target){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))

   if (!target%in% names(data))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   # Removes the plots without a target
   data <- data[!is.na(data[[target]]),]
   if (nrow(data) < 2){
      warning('Target columns contains (almost) only missing values')
      return(NA)
   }

   label_setup <- create_label_setup_num(length(view_cols))

   ## Easy case: just one plot
   if (length(view_cols) == 1){
      p <- draw_1d_density(data,
                           view_cols[1],
                           label_setup,
                           standalone = TRUE,
                           colgroup = target,
                           stacked = TRUE)
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
            p <- draw_1d_density(data,
                                 col_i,
                                 label_setup,
                                 colgroup = target,
                                 stacked = T)
         else
            p <- draw_2d_scatterplot(data=data,
                                     colx=col_j,
                                     coly=col_i,
                                     label_setup,
                                     colgroup=target)
         # Appends it to the list
         plot_index <- layout_matrix[i,j]
         plots[[plot_index]] <- p
      }
   }

   # Ajusts axis label sizes and removes them if necessary
   plots <- format_axis_labels(plots, layout_matrix, view_cols, ignore_diag=T)

   # Adds the legend
   legend_grob  <- draw_legend_cat(data, view_cols, target, 'num')
   legend_index <- n_plots + 1
   plots[[legend_index]] <- legend_grob
   layout_matrix[1, ncol(layout_matrix)] <- legend_index

   # Done!
   gridExtra::grid.arrange(grobs=plots, layout_matrix = layout_matrix)

}

plot_views_cat_to_predict_cat <- function(data, view_cols, target){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))

   if (!all(view_cols %in% names(data)))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   # Removes the plots without a target
   data <- data[!is.na(data[[target]]),]
   if (nrow(data) < 2){
      warning('Target columns contains (almost) only missing values')
      return(NA)
   }

   n_plots <- length(view_cols)
   label_setup <- create_label_setup_cat(n_plots)

   ## Simple case: just one plot
   if (n_plots == 1){
      p <- draw_1d_stacked_histogram(data, view_cols[1],
                                     colgroup = target,
                                     standalone = T,
                                     label_setup)
      return(p)
   }

   ## From now on, creates a plot grid
   # Generates the layout
   layout_matrix <- make_layout_matrix_hist(data, view_cols)

   # Generates the plots
   n_plots <- length(view_cols)
   plots <- vector("list", n_plots)
   for (i in 1:n_plots){
      plots[[i]] <- draw_1d_stacked_histogram(data, view_cols[i],
                                              colgroup = target,
                                              label_setup)
   }

   # Creates the legend
   legend_grob  <- draw_legend_cat(data, view_cols, target, 'cat')
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

plot_views_num_to_predict_num <- function(data, view_cols, target){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))

   if (!target%in% names(data))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   # Removes the plots without a target
   data <- data[!is.na(data[[target]]),]
   if (nrow(data) < 2){
      warning('Target columns contains (almost) only missing values')
      return(NA)
   }

   label_setup <- create_label_setup_num(length(view_cols))

   ## Easy case: just one plot
   if (length(view_cols) == 1){
      p <- draw_1d_num_influence(data, view_cols[1],
                                 target=target, label_setup, standalone = T)
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
            p <- draw_1d_num_influence(data,
                                 col_i,
                                 target = target,
                                 label_setup,
                                 standalone = F)
         else
            p <- draw_2d_num_influence(data,
                                  col_j, col_i,
                                  target = target,
                                  label_setup,
                                  standalone = F)
         # Appends it to the list
         plot_index <- layout_matrix[i,j]
         plots[[plot_index]] <- p
      }
   }

   # Ajusts axis label sizes and removes them if necessary
   plots <- format_axis_labels(plots, layout_matrix, view_cols, ignore_diag=T)

   # Creates and places the legend
   legend_grob  <- draw_legend_cont(data, view_cols, target)
   legend_index <- n_plots + 1
   plots[[legend_index]] <- legend_grob
   layout_matrix[1, ncol(layout_matrix)] <- legend_index

   # Done!
   gridExtra::grid.arrange(grobs=plots, layout_matrix = layout_matrix)
}

plot_views_cat_to_predict_num <- function(data, view_cols, target){
   stopifnot(is.data.frame(data))
   stopifnot(is.character(view_cols))

   if (!all(view_cols %in% names(data)))
      stop("Cannot find the requested columns in the dataset!")
   if (length(view_cols) < 1)
      stop("I cannot plot less than one column")

   # Removes the plots without a target
   data <- data[!is.na(data[[target]]),]
   if (nrow(data) < 2){
      warning('Target columns contains (almost) only missing values')
      return(NA)
   }

   n_plots <- length(view_cols)
   label_setup <- create_label_setup_cat(n_plots)

   ## Simple case: just one plot
   if (n_plots == 1){
      p <- draw_1d_cat_influence(data, view_cols[1], target, label_setup)
      return(p)
   }

   ## From now on, creates a plot grid
   # Generates the layout
   layout_matrix <- make_layout_matrix_hist(data, view_cols)

   # Generates the plots
   plots <- vector("list", n_plots)
   for (i in 1:n_plots){
      plots[[i]] <- draw_1d_cat_influence(data, view_cols[i],
                                          target, label_setup)
   }

   # Done!
   gridExtra::grid.arrange(grobs=plots, layout_matrix = layout_matrix)
}
