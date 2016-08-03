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

##############################
# Formatters and prettifiers #
##############################
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

create_view_title <- function(view_id, view_type, ziggy_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(ziggy_out))

   # Retrieves the views to output
   to_output <- if (view_type == 'num') ziggy_out$views_num
   else ziggy_out$views_cat

   if (!view_id >= 1 | !view_id <= length(to_output))
      stop("Incorrect view requested.")

   col_string  <- paste0(to_output[[view_id]], collapse = ', ')
   full_string <- paste0("View: ", col_string)

   return(full_string)
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
      if (is.na(view_id())) return("")

      view_title <- create_view_title(view_id(),
                                      view_type(),
                                      ziggy_out)
      h4(view_title)
   })

   output$viewPlot <- renderPlot({
      plot(ziggy_data[,c(1,2)])
   })

})
