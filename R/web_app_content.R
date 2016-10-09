#####################
#### View Table #####
#####################
#-------------------------#
# Table-related functions #
#-------------------------#
data_table_options <- function(app_type){
   stopifnot(app_type %in% APP_TYPES)

   # Basic parameters, common for all app types
   params <- list(
      "scrollY" = "400px",
      "scrollCollapse" = TRUE,
      "paging"= FALSE,
      "ordering" = FALSE,
      "info"     = FALSE,
      "lengthChange" = FALSE,
      "searching"    = FALSE,
      "dom" = "tp"
   )

   # App specific parameters
   params[["columnDefs"]] <- if (app_type == 'findviews')
      list(
         list('targets' = 0, 'visible' = FALSE),
         list('targets' = 1, 'title' = NULL, 'className' = 'view_cell')
      )
   else if (app_type %in% c('findviews_to_compare', 'findviews_to_predict'))
      list(
         list('targets' = 0, 'visible' = FALSE),
         list('targets' = 1, 'visible' = FALSE),
         list('targets' = 2, 'title' = NULL),
         list('targets' = 3, 'title' = NULL, 'className' = 'view_cell')
      )

   return(params)
}

data_table_js <- function(app_type){
   stopifnot(app_type %in% APP_TYPES)

   if (app_type == 'findviews')
      # Table with no support for heatmaps
      return("
          function(table) {
               // Clicking behavior
               table.on('click.dt', 'tr', function() {
                  table.$('tr.selected').removeClass('selected');
                  $(this).toggleClass('selected');

                  var viewId = table.rows('.selected').data()[0][0];
                  $('div#view-specs input#currentView').val(viewId);
                  $('div#view-specs input#currentView').trigger('change');
               });
          }
    ")

   # For other app types, we need the heatmaps to show the scores
   return("
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
                table.$('tr').eq(index).children('td').eq(0)
                .css('background-color', colLevel);
            })
          })
         }
   ")
}


create_view_table <- function(view_type, app_type, fdviews_out){
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(app_type %in% APP_TYPES)
   stopifnot(c('views_num', 'views_cat', 'scores_num', 'scores_cat') %in%
                names(fdviews_out))

   # Retrieves and formats the views to output
   table_to_output <- if (view_type == 'num') fdviews_out$views_num
                      else fdviews_out$views_cat

   view_strings <- sapply(table_to_output, function(view_cols){
      paste0(view_cols, collapse = ', ')
   })
   view_strings <- as.character(view_strings)

   # If type = findiviews, that's all we neeed
   if (app_type == 'findviews')
      return(data.frame(
         viewId       = seq_along(view_strings),
         viewName     = view_strings
      ))

   # Get scores, and genenerates the necessary heatmap data
   scores <- if (view_type == 'num') fdviews_out$scores_num
             else fdviews_out$scores_cat

   html_colors <- map_to_colors(scores, '#e9edf1', '#23527C')
   heatmap_col <- rep("", length(view_strings))

   # Done
   data.frame(viewId       = seq_along(view_strings),
              colors       = html_colors,
              scoreHeatmap = heatmap_col,
              viewName     = view_strings)
}


################################
#### Excluded Columns Text #####
################################
this_or_these_column_s <- function(cols){
   if (length(cols) == 1) "this column"
   else "these columns"
}

describeExclusions <- function(fdviews_out){
   stopifnot('excluded' %in% names(fdviews_out))
   stopifnot(c('flat_num', 'flat_cat', 'unknown_type') %in%
                names(fdviews_out$excluded))

   comments <- character(length(fdviews_out$excluded))

   # Text for the columns which type is not supported
   cols_notype <- fdviews_out$excluded$unknown_type
   comments[1] <- if (length(cols_notype) > 0){
      s1 <- enumerate_char(cols_notype)
      s2 <- " because I do not recognize of support the type of "
      s3 <- this_or_these_column_s(cols_notype)
      paste0(s1, s2, s3, ".")
   } else {
      NA
   }

   # Text for the flat num columns
   cols_flat_num <- fdviews_out$excluded$flat_num
   comments[2]  <- if (length(cols_flat_num) > 0){
      s1 <- enumerate_char(cols_flat_num)
      s2 <- " because "
      s3 <- this_or_these_column_s(cols_flat_num)
      s4 <- " have a constant value."
      paste0(s1, s2, s3, s4)
   } else {
      NA
   }

   # Text for the flat cat columns
   cols_flat_cat <- fdviews_out$excluded$flat_cat
   comments[3] <- if (length(cols_flat_cat) > 0){
      s1 <- enumerate_char(cols_flat_cat)
      s2 <- " because "
      s3 <- this_or_these_column_s(cols_flat_cat)
      s4 <- " have either only one or too many distinct values."
      paste0(s1, s2, s3, s4)
   } else {
      NA
   }


   # Text for the sparse num columns
   cols_sparse_num <- fdviews_out$excluded$sparse_num
   comments[4]  <- if (length(cols_sparse_num) > 0){
      s1 <- enumerate_char(cols_sparse_num)
      s2 <- " because "
      s3 <- this_or_these_column_s(cols_sparse_num)
      s4 <- " have too many missing values."
      paste0(s1, s2, s3, s4)
   } else {
      NA
   }

   # Text for the sparse cat columns
   cols_sparse_cat <- fdviews_out$excluded$sparse_cat
   comments[5] <- if (length(cols_sparse_cat) > 0){
      s1 <- enumerate_char(cols_sparse_cat)
      s2 <- " because "
      s3 <- this_or_these_column_s(cols_sparse_cat)
      s4 <- " have too many missing values."
      paste0(s1, s2, s3, s4)
   } else {
      NA
   }

   comments <- stats::na.omit(comments)

   html <- if (length(comments) == 0){
      ""
   } else if (length(comments) == 1){
      s1 <- 'I excluded '
      paste0(s1, comments[1])
   } else if (length(comments) > 1) {
      s1 <- 'I excluded the following columns:\n<ul>'
      s2_elts <- sapply(comments, function(s)
         paste0('<li>', s, '</li>')
      )
      s2 <- paste0(s2_elts, collapse = '\n')
      s3 <- '</ul>'
      paste0(s1, s2, s3)
   }

   html <- paste0("<div class='shiny-text-output'><span>", html,
                  '</span></div>')

   return(shiny::HTML(html))
}


#################################
#### Plotting & View Details ####
#################################
retrieve_view <- function(view_id, view_type, fdviews_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(fdviews_out))

   to_output <- if (view_type == 'num') fdviews_out$views_num
   else fdviews_out$views_cat

   if (!view_id >= 1 | !view_id <= length(to_output))
      stop("Incorrect view requested.")

   return(to_output[[view_id]])
}

#-------#
# Title #
#-------#
create_view_title <- function(view_id, view_type, fdviews_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(fdviews_out))

   view_cols <- retrieve_view(view_id, view_type, fdviews_out)
   col_string  <- paste0(view_cols, collapse = ', ')
   full_string <- paste0("Plots for the view ", col_string)

   html_block <- shiny::h4(full_string)

   return(html_block)
}


#----------#
# Plotting #
#----------#
plot_selection <- function(view_id, view_type, app_type,
                           fdviews_out, data,
                           group1=NULL, group2=NULL,
                           group1_name=NULL, group2_name=NULL,
                           target=NULL){

   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(app_type %in% APP_TYPES)
   stopifnot(c('views_num', 'views_cat') %in% names(fdviews_out))
   stopifnot(is.data.frame(data))
   stopifnot(is.logical(group1) | is.null(group1))
   stopifnot(is.logical(group1) | is.null(group1))
   stopifnot(is.character(group1_name) | is.null(group1_name))
   stopifnot(is.character(group2_name) | is.null(group2_name))
   stopifnot(is.character(target) | is.null(target))

   # Retrieves the views to output
   view_cols <- retrieve_view(view_id, view_type, fdviews_out)

   # If applicable, subsamples
   if (nrow(data) > PLOT_SAMPLE_SIZE){

      warning('View plotting: the dataframe contains more that ',
              PLOT_SAMPLE_SIZE, ' rows, I am subsampling the data')
      sample_index <- sample(1:nrow(data), PLOT_SAMPLE_SIZE, F)

      data   <- data[sample_index,,drop=F]

      if (!is.null(group1) & !is.null(group2)){
         group1 <- group1[sample_index]
         group2 <- group2[sample_index]
      }
   }

   # If applicable, checks the target type
   target_type <- if (is.null(target)){
      NA
   } else {
      stopifnot(target %in% names(data))
      target_col <- data[[target]]
      if (is.numeric(target_col))
         'num'
      else if(is.factor(target_col) |
              is.character(target_col) |
              is.logical(target_col))
         'cat'
      else NA
   }


   # Produces the correct plot
   plot <- if (app_type == 'findviews' & view_type == 'num')
            plot_views_num(data, view_cols)
          else if (app_type == 'findviews' & view_type == 'cat')
             plot_views_cat(data, view_cols)
          else if (app_type == 'findviews_to_compare' & view_type == 'num')
             plot_views_num_to_compare(data, view_cols,
                                       group1, group2,
                                       group1_name, group2_name)
          else if (app_type == 'findviews_to_compare' & view_type == 'cat')
             plot_views_cat_to_compare(data, view_cols,
                                       group1, group2,
                                       group1_name, group2_name)
          else if (app_type    == 'findviews_to_predict' &
                   view_type   == 'num' &
                   target_type == 'cat')
             plot_views_num_to_predict_cat(data, view_cols, target)
          else if (app_type    == 'findviews_to_predict' &
                   view_type   == 'cat' &
                   target_type == 'cat')
             plot_views_cat_to_predict_cat(data, view_cols, target)
          else if (app_type    == 'findviews_to_predict' &
                   view_type   == 'num' &
                   target_type == 'num')
             plot_views_num_to_predict_num(data, view_cols, target)
          else if (app_type    == 'findviews_to_predict' &
                   view_type   == 'cat' &
                   target_type == 'num')
             plot_views_cat_to_predict_num(data, view_cols, target)
          else NA

   return(plot)
}
