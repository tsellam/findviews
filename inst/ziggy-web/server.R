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

            var oldViewType = $('div#view-specs input#currentViewType').val();
            var viewType = 'num';
            $('div#view-specs input#currentViewType').val(viewType);
            if (oldViewType != viewType)
               $('div#view-specs input#currentViewType').trigger('change');

            var oldViewId = $('div#view-specs input#currentView').val();
            var viewId = table.rows('.selected').data()[0][0];
            $('div#view-specs input#currentView').val(viewId);
            if (oldViewId != viewId)
               $('div#view-specs input#currentView').trigger('change');
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
plot_selection_numeric <- function(data, target){
   col_types <- sapply(data, class)
   if (!all(col_types == 'numeric')) stop('Cannot plot, type not supported')

   zig_in_target <- ifelse(target, 'In the selection', 'Outside the selection')
   data <- cbind(data, zig_in_target)

   # 1D data -> density plot
   if (ncol(data) == 2){
      title <- paste0('Density plot for the variable ', names(data)[[1]])

      ggplot2::ggplot(data,
                      ggplot2::aes_string(x    = names(data)[[1]],
                                         color = names(data)[[2]],
                                         fill  = names(data)[[2]])) +
         ggplot2::geom_density(alpha = .5) +
         ggplot2::scale_color_discrete('') +
         ggplot2::scale_fill_discrete('') +
         ggplot2::ggtitle(title)


   # 2D data -> scatter plot
   } else if (ncol(data) == 3){
      title <- paste0('Scatter plot of the variables ', names(data)[[1]],
                      ' and ', names(data)[[2]])

      ggplot2::ggplot(data,
                      ggplot2::aes_string(x    = names(data)[[1]],
                                          y    = names(data)[[2]],
                                          color = names(data)[[3]],
                                          fill  = names(data)[[3]]),
                                          shape = names(data)[[3]]) +
         ggplot2::geom_point() +
         ggplot2::scale_color_discrete('') +
         ggplot2::scale_fill_discrete('') +
         ggplot2::scale_shape_discrete('') +
         ggplot2::ggtitle(title)
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
   view_id <- reactive({
      as.integer(input$currentView)
   })
   view_type <- reactive({
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
