##############################
# Setup for DataTable Widget #
##############################
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
      list('targets' = 1, 'title' = 'Views')
   ),
   "dom" = "tp"
)

data_table_js <- "
   function(table) {
      table.on('click.dt', 'tr', function() {
            table.$('tr.selected').removeClass('selected');
            $(this).toggleClass('selected');

            var viewType = 'num';
            $('div#view-specs input#currentViewType').val(viewType);
            $('div#view-specs input#currentViewType').trigger('change');

            var viewId = table.rows('.selected').data()[0][0];
            $('div#view-specs input#currentView').val(viewId);
            $('div#view-specs input#currentView').trigger('change');

            $('div#view-specs button#submitView').click();
      });
   }"

##############
# View Table #
##############
create_view_table <- function(view_type, ziggy_out){
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(ziggy_out))

   # Retrieves the views to output
   to_output <- if (view_type == 'num') ziggy_out$views_num
                else ziggy_out$views_cat

   # Formats them
   view_strings <- sapply(to_output, function(view_cols){
      paste0(view_cols, collapse = ', ')
   })
   view_strings <- as.character(view_strings)

   # Done
   data.frame(viewId   = seq_along(view_strings),
              viewName = view_strings)
}

###########################
# Plotting & View Details #
###########################
retrieve_view <- function(view_id, view_type, ziggy_out){
   to_output <- if (view_type == 'num') ziggy_out$views_num
   else ziggy_out$views_cat

   if (!view_id >= 1 | !view_id <= length(to_output))
      stop("Incorrect view requested.")

   return(to_output[[view_id]])
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
   full_string <- paste0("View: ", col_string)

   return(full_string)
}

#----------#
# Plotting #
#----------#
# Util function (1) takes the the legend of the (i,j)th plot of a grid
# and places it at the top right corner, and (2) erases all the other legends
factorize_legend <- function(grid_plot, i_reference=1, j_reference=1){
   stopifnot('ggmatrix' %in% class(grid_plot))

   ndims <- grid_plot$nrow

   for (i in 1:ndims){
      for (j in 1:ndims){
         if (i==i_reference & j==j_reference) next
         grid_plot[i,j] <- grid_plot[i,j] +
                           ggplot2::theme(legend.position="none")
      }
   }

   grid_plot[i_reference,j_reference] <- grid_plot[i_reference,j_reference] +
                     ggplot2::theme(legend.position = c(ndims,1),
                                    legend.justification = c(1,1))

   grid_plot
}

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

   # Other settings
   scat_pt_size_default <- if (nrow(data) > 1000) .5
                           else if (nrow(data) > 500) .75
                           else 1
   scat_alpha_default   <- if (sum(target) > nrow(data) / 3) .5
                           else 1

   # 1D data -> density plot
   if (ncol(data) == 2){
      title <- paste0('Density plot for the variable ', names(data)[[1]])

      ggplot2::ggplot(data,
                      ggplot2::aes_string(x = names(data)[[1]],
                                         color = names(data)[[2]],
                                         fill  = names(data)[[2]])) +
         ggplot2::geom_density(alpha = .5) +
         ggplot2::ggtitle(title) +
         ggplot2::theme(legend.text     = ggplot2::element_text(size = 12),
                        legend.key.size = ggplot2::unit(1, "cm"),
                        legend.title    = ggplot2::element_text(size = 0))


   # 2d and more -> scatterplot matrix
   } else if (ncol(data) >= 3){

      to_plot_index <- 1:(ncol(data)-1)
      to_plot_col   <- names(data)[to_plot_index]
      labels_col    <- names(data)[[ncol(data)]]

      # Main plots
      pairs <- GGally::ggpairs(data,
                      mapping = ggplot2::aes_string(color = labels_col,
                                                    fill  = labels_col),
                      columns = to_plot_index,
                      lower = list('continuous' = GGally::wrap('points',
                                                    size = scat_pt_size_default,
                                                    alpha = scat_alpha_default)
                                                   ),
                      diag  = list('continuous' = GGally::wrap('densityDiag',
                                                               alpha = 0.5)),
                      upper = list('continuous' = 'blank'),
                      legends = TRUE,
                      title   = "Density plots (diagonal) and 2D scatterplots (all the other charts)") +
               ggplot2::theme(legend.text     = ggplot2::element_text(size = 12),
                              legend.key.size = ggplot2::unit(1, "cm"),
                              legend.title    = ggplot2::element_text(size = 0))

      pairs <- factorize_legend(pairs)
      pairs

   }

}

plot_selection <- function(view_id, view_type, ziggy_out, target, data){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(ziggy_out))
   stopifnot(is.logical(target))
   stopifnot(is.data.frame(data))

   # Retrieves the views to output
   view_cols <- retrieve_view(view_id, view_type, ziggy_out)

   plot <- if (view_type == 'num') plot_selection_numeric(data[view_cols], target)
   else plot_selection_categorical(data[view_cols], target)

   return(plot)
}

######################
# Actual server code #
######################
shinyServer(function(input, output) {

   # Side panel maintenance
   output$viewsTable <- renderDataTable(
      create_view_table('num', ziggy_out),
      options = data_table_options,
      callback = data_table_js
   )

   # Main panel maintenance
   view_id <- eventReactive(input$submitView, {
      as.integer(input$currentView)
   })

   view_type <- eventReactive(input$submitView, {
      if (!input$currentViewType %in% c('num', 'cat', ''))
         stop('Incorrect request to Ziggy server.')
      input$currentViewType
   })

   output$viewTitle <- renderUI({
      if (is.na(view_id()) | is.na(view_type()))
         return("")

      view_title <- create_view_title(view_id(),
                                      view_type(),
                                      ziggy_out)
      h4(view_title)
   })

   output$viewPlot <- renderPlot({
      if (is.na(view_id()) | is.na(view_type()))
         return(NULL)

      plot_selection(view_id(), view_type(),
                     ziggy_out, ziggy_target, ziggy_data)
   })

})
