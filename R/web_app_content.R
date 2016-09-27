##############
# View Table #
##############
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
         list('targets' = 1, 'title' = NULL)
      )
   else if (app_type %in% c('findviews_to_compare', 'findviews_to_predict'))
      list(
         list('targets' = 0, 'visible' = FALSE),
         list('targets' = 1, 'visible' = FALSE),
         list('targets' = 2, 'title' = NULL),
         list('targets' = 3, 'title' = NULL)
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


#-------------------------------#
# Columns exclusion description #
#-------------------------------#
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

   # Text for the flat num columns
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

   comments <- na.omit(comments)

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


###########################
# Plotting & View Details #
###########################
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

retrieve_details <- function(view_id, view_type, fdviews_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(c('views_num', 'views_cat') %in% names(fdviews_out))

   to_output <- if (view_type == 'num') fdviews_out$details_num
   else fdviews_out$details_cat

   if (!view_id >= 1 | !view_id <= nrow(to_output))
      stop("Incorrect view requested.")

   out <- unlist(to_output[view_id,,drop=F], recursive = F)
   return(out)
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

   # Retrieves the views to output
   view_cols <- retrieve_view(view_id, view_type, fdviews_out)

   # Subsamples if necessary
   if (nrow(data) > PLOT_SAMPLE_SIZE){

      warning('View plotting: the dataframe contains more that ',
              PLOT_SAMPLE_SIZE, ' rows, I am subsampling the data')
      sample_index <- sample(1:nrow(data), PLOT_SAMPLE_SIZE, F)

      data   <- data[sample_index,,drop=F]

      if (!is.null(target))
         target <- target[sample_index]

      if (!is.null(group1) & !is.null(group2)){
         group1 <- group1[sample_index]
         group2 <- group2[sample_index]
      }
   }

   plot <- if (app_type == 'findviews' & view_type == 'num')
            plot_views_num(data, view_cols)
          else if (app_type == 'findviews' & view_type == 'cat')
             plot_views_cat(data, view_cols)
          else if (app_type == 'findviews_to_compare' & view_type == 'num')
             plot_views_num_to_compare(data, view_cols,
                                       group1, group2,
                                       group1_name, group2_name)
          else if (app_type =='findviews_to_predict')
            NA
          else NA

   return(plot)
}
#
# plot_selection <- function(view_id, view_type, app_type,
#                            fdviews_out, data,
#                            group1=NULL, group2=NULL, target=NULL){
#
#    stopifnot(is.integer(view_id))
#    stopifnot(view_type %in% c('num', 'cat'))
#    stopifnot(app_type %in% APP_TYPES)
#    stopifnot(c('views_num', 'views_cat') %in% names(fdviews_out))
#    stopifnot(is.data.frame(data))
#
#    # Retrieves the views to output
#    view_cols <- retrieve_view(view_id, view_type, fdviews_out)
#
#    # Generates a target vector, used later to color the plots
#    if (app_type == 'findviews'){
#       target_data <- rep(NA, nrow(data))
#
#    } else if (app_type == 'findviews_to_compare'){
#       # Generates a target vector
#       target_data <- integer(nrow(data))
#       target_data[group1] <- 1
#       target_data[group2] <- 2
#       target_data <- factor(paste0("Group ", target_data))
#
#       # Trims the data to the user's selection
#       row_selection <- group1 | group2
#       data   <- data[row_selection, view_cols, drop=F]
#       target_data <- target_data[row_selection]
#
#    } else if (app_type == 'findviews_to_predict'){
#       stopifnot("target_data" %in% names(fdviews_out))
#       target_data <- fdviews_out$target_data
#       stopifnot(is.factor(target_data))
#
#    }
#
#    # Subsamples if necessary
#    if (nrow(data) > PLOT_SAMPLE_SIZE){
#       warning('View plotting: the dataframe contains more that ',
#               PLOT_SAMPLE_SIZE, ' rows, I am subsampling the data')
#       sample_index <- sample(1:nrow(data), PLOT_SAMPLE_SIZE, F)
#       data   <- data[sample_index,,drop=F]
#       target_data <- target_data[sample_index]
#    }
#
#    plot <- if (view_type=='num') plot_selection_numeric(data[view_cols],
#                                                         target_data,
#                                                         app_type)
#    else plot_selection_categorical(data[view_cols],
#                                    target_data,
#                                    app_type)
#
#    return(plot)
# }


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

create_view_comments <- function(view_id, view_type, app_type, fdviews_out){
   stopifnot(is.integer(view_id))
   stopifnot(view_type %in% c('num', 'cat'))
   stopifnot(app_type %in% APP_TYPES)

   # For most app types, we can already stop here
   if (app_type %in% c("findviews", "findviews_to_predict"))
      return(shiny::HTML(""))

   # For other types, we continue
   stopifnot(c('details_num', 'details_cat') %in% names(fdviews_out))
   components <- retrieve_details(view_id, view_type, fdviews_out)

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
      tip_html <- "I chose this view because of\n"
      tip_lines_html <- sapply(tip_lines,
                               function(s) paste0('<li>',s,'</li>'))
      tip_lines_html <- paste0(tip_lines_html, collapse = "\n")
      tip_lines_html <- paste0('<ul>\n',tip_lines_html,'\n</ul>')
   }

   html_block <- paste0(tip_html, tip_lines_html, collapse = ' ')
   if (nchar(html_block) > 0){
      html_block <- rewrite_comment_text(html_block)
      html_block <- paste0("<div><h4>Comments</h4>",
                           html_block,
                           "</div>",
                           collapse = '\n')
   }

   html_block <- shiny::HTML(html_block)

   return(html_block)
}
