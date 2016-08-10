##############
# View Table #
##############
#----------------------------#
# Setup for DataTable Widget #
#----------------------------#
data_table_options <- list(
   "scrollY" = "400px",
   "scrollCollapse" = TRUE,
   "paging"= FALSE,
   "ordering" = FALSE,
   "info"     = FALSE,
   "lengthChange" = FALSE,
   "searching"    = FALSE,
   "columnDefs"   =  list(
      list('targets' = 0, 'visible' = FALSE),
      list('targets' = 1, 'visible' = FALSE),
      list('targets' = 2, 'title' = NULL),
      list('targets' = 3, 'title' = NULL)
   ),
   "dom" = "tp"
)

data_table_js <- "
   function(table) {
      // Clicking behavior
      table.on('click.dt', 'tr', function() {
            table.$('tr.selected').removeClass('selected');
            $(this).toggleClass('selected');

            var viewId = table.rows('.selected').data()[0][0];
            $('div#view-specs input#currentView').val(viewId);
            $('div#view-specs input#currentView').trigger('change');
      });

      table.on('init', function() {
            table.rows().eq(0).each(function(index){
               var row = table.row( index );
               var colLevel = row.data()[1];
               table.$('tr').eq(index).children('td').eq(1)
                  .css('background-color', colLevel);
            })
      })

   }"


map_to_colors <- function(data, start_col='#FFFFFF', end_col ='#333333'){
   stopifnot(is.vector(data))

   min <- min(data, na.rm = T)
   max <- max(data, na.rm = T)
   data <- (data - min)  / (max - min)

   map_fn <- colorRamp(c(start_col, end_col))
   colors <- map_fn(data)
   html_colors <- rgb(colors, max = 255)
   return(html_colors)
}

create_view_table <- function(view_type, ziggy_out){
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat',
               'scores_num', 'scores_cat') %in%names(ziggy_out))

   # Retrieves the views to output
   table_to_output <- if (view_type == 'num') ziggy_out$views_num
                else ziggy_out$views_cat
   scores <- if (view_type == 'num') ziggy_out$scores_num
              else ziggy_out$scores_cat

   # Formats them
   view_strings <- sapply(table_to_output, function(view_cols){
      paste0(view_cols, collapse = ', ')
   })
   view_strings <- as.character(view_strings)

   # Genenerates the HTML color codes
   html_colors <- map_to_colors(scores, '#e9edf1', '#23527C')

   # Done
   data.frame(viewId   = seq_along(view_strings),
              colors   = html_colors,
              viewName = view_strings,
              viewScore= rep("", length(view_strings)))
}

###########################
# Plotting & View Details #
###########################
retrieve_view <- function(view_id, view_type, ziggy_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(ziggy_out))

   to_output <- if (view_type == 'num') ziggy_out$views_num
                 else ziggy_out$views_cat

   if (!view_id >= 1 | !view_id <= length(to_output))
      stop("Incorrect view requested.")

   return(to_output[[view_id]])
}

retrieve_details <- function(view_id, view_type, ziggy_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(ziggy_out))

   to_output <- if (view_type == 'num') ziggy_out$details_num
                else ziggy_out$details_cat

   if (!view_id >= 1 | !view_id <= nrow(to_output))
      stop("Incorrect view requested.")

   out <- unlist(to_output[view_id,,drop=F], recursive = F)
   return(out)
}

#-------#
# Title #
#-------#
create_view_title <- function(view_id, view_type, ziggy_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(ziggy_out))

   view_cols <- retrieve_view(view_id, view_type, ziggy_out)
   col_string  <- paste0(view_cols, collapse = ', ')
   full_string <- paste0("Plots for the view ", col_string)

   html_block <- h4(full_string)

   return(html_block)
}


#----------#
# Plotting #
#----------#
### Plotting functions
num_1d_view <- function(data, mapping, ...){
   p <- ggplot2::ggplot(data=data, mapping=mapping) +
      ggplot2::geom_density(..., alpha = 0.5) +
      ggplot2::theme(legend.text     = ggplot2::element_text(size = 12),
                     legend.key.size = ggplot2::unit(1, "cm"),
                     legend.title    = ggplot2::element_text(size = 0))
   p
}

num_2d_view <- function(data, mapping, ...){

   scat_pt_size <- if (nrow(data) > 1000) .5
                   else if (nrow(data) > 500) .75
                   else 1


   p <- ggplot2::ggplot(data=data, mapping=mapping) +
      ggplot2::geom_point(size = scat_pt_size, ...) +
      ggplot2::theme(legend.text     = ggplot2::element_text(size = 12),
                     legend.key.size = ggplot2::unit(1, "cm"),
                     legend.title    = ggplot2::element_text(size = 0))

   if (nrow(data) > 20) p <- p + ggplot2::geom_smooth(method=lm, se = F)

   p
}

cat_1d_view <- function(data, mapping, ...){
   ggplot2::ggplot(data, mapping) +
      ggplot2::geom_bar(position = "dodge", ...) +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::theme(axis.text.x=ggplot2::element_text(angle=-30, hjust=0),
                     legend.position = "left",
                     legend.title = ggplot2::element_blank())


}


#### Wrappers
plot_selection_numeric <- function(data, target){
   col_types <- sapply(data, class)
   if (!all(col_types == 'numeric')) stop('Cannot plot, type not supported')

   # Prepares the data frame to be visualized
   zig_in_target <- ifelse(target, 'In the selection', 'Outside the selection')
   data <- cbind(data, zig_in_target)

   # Subsamples if necessary
   if (nrow(data) > SCATTERPLOT_SAMPLE_SIZE){
      warning('The dataframe contains more that',SCATTERPLOT_SAMPLE_SIZE,
              ' rows, I am subsampling the data')
      data <- data[sample(1:nrow(data), SCATTERPLOT_SAMPLE_SIZE, F)]
   }

   # 1D data -> density plot
   if (ncol(data) == 2){

      title <- paste0('Density plot for the variable ', names(data)[[1]])
      num_1d_view(data,
                  ggplot2::aes_string(x = names(data)[[1]],
                                      color = names(data)[[2]],
                                      fill  = names(data)[[2]])) +
                  ggplot2::ggtitle(title)

   # 2d and more -> scatterplot matrix
   } else if (ncol(data) >= 3){

      to_plot_index <- 1:(ncol(data)-1)
      to_plot_col   <- names(data)[to_plot_index]
      labels_col    <- names(data)[[ncol(data)]]

      # Context-dependent graph parameters
      alpha_default   <- if (sum(target) > nrow(data) / 3) .5
                         else 1
      title <- "Density plots (diagonal) and 2D scatterplots (all the other charts)"

      # Puts them all in matrix
      pairs <- GGally::ggpairs(data,
                      mapping = ggplot2::aes_string(color = labels_col,
                                                    fill  = labels_col),
                      columns = to_plot_index,
                      lower = list('continuous' = GGally::wrap(num_2d_view,
                                                               alpha = alpha_default)),
                      diag  = list('continuous' = num_1d_view),
                      upper = list('continuous' = 'blank'),
                      legends = FALSE,
                      title   = title)

      # Generates and inserts the legend
      plot_legend_fn <- GGally::gglegend(num_1d_view)
      legend <- plot_legend_fn(data, ggplot2::aes_string(x = labels_col[1],
                                                         color = labels_col,
                                                         fill  = labels_col))
      pairs[1, length(to_plot_col)] <- legend

      # Done!
      pairs
   }
}

plot_selection_categorical <- function(data, target){

   # Prepares the data frame to be visualized
   zig_in_target <- ifelse(target, 'In the selection', 'Outside the selection')
   data <- cbind(data, zig_in_target)

   # Gets the column names
   to_plot_index <- 1:(ncol(data)-1)
   to_plot_col   <- names(data)[to_plot_index]
   labels_col    <- names(data)[[ncol(data)]]

   # Creates the series of plots
   plot_series <- lapply(to_plot_col, function(col){
      cat_1d_view(data,
                  mapping = ggplot2::aes_string(x = col,
                                                color = labels_col,
                                                fill  = labels_col),
                  ggplot2::aes_string(y = '..prop..',
                                      group = labels_col))
   })

   # Generates the legend
   plot_legend_fn <- GGally::gglegend(cat_1d_view)
   legend <- plot_legend_fn(data, ggplot2::aes_string(x = labels_col[1],
                                                      color = labels_col,
                                                      fill  = labels_col))

   # Places everything in a plot matrix,
   plot_series <- c(plot_series, list(legend))

   GGally::ggmatrix(plot_series,
                    showStrips         = TRUE,
                    xAxisLabels        = c(to_plot_col, ""),
                    yAxisLabels        = 'Frequency (per group)',
                    showAxisPlotLabels = TRUE,
                    ncol               = length(to_plot_col) + 1,
                    nrow               = 1)

}

plot_selection <- function(view_id, view_type, ziggy_out, target, data){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(ziggy_out))
   stopifnot(is.logical(target))
   stopifnot(is.data.frame(data))

   # Retrieves the views to output
   view_cols <- retrieve_view(view_id, view_type, ziggy_out)

   plot <- if (view_type=='num') plot_selection_numeric(data[view_cols],target)
           else plot_selection_categorical(data[view_cols], target)

   return(plot)
}

#-------------------#
# Comments the view #
#-------------------#
rewrite_comment_text <- function(comment){
   stopifnot(is.character(comment))

   rw_rules <- list(
      c(paste0("<li>the difference between the ([a-zA-Z]+) on ([a-zA-Z, ]+)</li>",
               "\n<li>the difference between the ([a-zA-Z]+) on \\2"),
        paste0("<li>the difference between the \\1 on \\2</li>",
               "\n<li>the difference between the \\3 on the same columns"))
       #c("<ul>\n<li>the difference between the ([a-zA-Z]+)",
       #  paste0("<li>the difference between the \\1 ",
       #         "of the tuples inside and outside the selection"))
   )

   for (rule in rw_rules)
      comment <- gsub(rule[[1]], rule[[2]], comment)

   return(comment)
}

create_view_comments <- function(view_id, view_type, ziggy_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('details_num', 'details_cat') %in% names(ziggy_out))

   components <- retrieve_details(view_id, view_type, ziggy_out)

   tip_lines <- sapply(components, function(component){
      if ('tip' %in% names(component)) component$tip
      else NA
   })
   tip_lines <- as.character(tip_lines)
   tip_lines <- na.omit(tip_lines)
   tip_lines <- tip_lines[nchar(tip_lines) > 0]

   if (length(tip_lines) == 0) {
      tip_html <- ""
      tip_lines_html <- ""

   } else if (length(tip_lines) == 1) {
      tip_html <- "I chose this view because of "
      tip_lines_html <- tip_lines

   } else if (length(tip_lines) > 1) {
      tip_html <- "I chose this view because of:\n"
      tip_lines_html <- sapply(tip_lines,
                               function(s) paste0('<li>',s,'</li>'))
      tip_lines_html <- paste0(tip_lines_html, collapse = "\n")
      tip_lines_html <- paste0('<ul>\n',tip_lines_html,'\n</ul>')
   }

   html_block <- paste0(tip_html, tip_lines_html, collapse = ' ')
   if (nchar(html_block) > 0){
      html_block <- rewrite_comment_text(html_block)
      html_block <- paste0("<div><h4>Ziggy's comments</h4>",
                           html_block,
                           "</div>",
                           collapse = '\n')
   }

   html_block <- HTML(html_block)

   return(html_block)
}

######################
# Actual server code #
######################
shinyServer(function(input, output) {

   # Side panel maintenance
   output$numViewsTable <- renderDataTable(
      create_view_table('num', ziggy_out),
      options = data_table_options,
      callback = data_table_js
   )

   output$catViewsTable <- renderDataTable(
      create_view_table('cat', ziggy_out),
      options = data_table_options,
      callback = data_table_js
   )

   # Main panel maintenance
   # Reactive variables
   selected_view_id <- reactive({
      as.integer(input$currentView)
   })

   # Output bindings
   output$viewTitle <- renderUI({
      view_type <- isolate(input$viewTab)
      view_id   <- selected_view_id()
      if(is.na(view_id)) return(NULL)

      create_view_title(view_id, view_type, ziggy_out)
   })

   output$viewPlot <- renderPlot({
      view_type <- isolate(input$viewTab)
      view_id   <- selected_view_id()
      if(is.na(view_id)) return(NULL)

      plot_selection(view_id, view_type,
                     ziggy_out, ziggy_target, ziggy_data)
   })

   output$viewComment <- renderUI({
      view_type <- isolate(input$viewTab)
      view_id   <- selected_view_id()
      if(is.na(view_id)) return(NULL)

      create_view_comments(view_id, view_type, ziggy_out)
   })

})
