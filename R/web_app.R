################################
# Shiny code to create the app #
################################
create_fdviews_client <- function(app_type, data_name, target=NULL){
   stopifnot(is.character(app_type))
   stopifnot(app_type %in% APP_TYPES)

   shiny::shinyUI(shiny::fluidPage(

      shiny::tags$head(
         shiny::tags$style(HTML("
            div.exclusion-container {
               margin-top: 5px;
               overflow: scroll;
            }
           "))
         ),

      if (app_type == 'findviews'){
         title <- paste0("Views of ", data_name)
         shiny::titlePanel(title, windowTitle = app_type)
      } else if (app_type == 'findviews_to_compare'){
         title <- paste0("Views of ", data_name, ", sorted by discrimination power")
         shiny::titlePanel(title, windowTitle = app_type)
      } else if (app_type == 'findviews_to_predict'){
         title <- paste0("Views of ", data_name, ", sorted by prediction power")
         shiny::titlePanel(title, windowTitle = app_type)
      },

      shiny::sidebarLayout(

         shiny::sidebarPanel(
            shiny::tabsetPanel(id = "viewTab",
                               shiny::tabPanel("Continuous",
                                 shiny::dataTableOutput("numViewsTable"),
                                 value = "num"),
                               shiny::tabPanel("Categorical",
                                 shiny::dataTableOutput("catViewsTable"),
                                 value = "cat"),
                               shiny::tabPanel("Excluded",
                                       shiny::htmlOutput(
                                          "exclusionComments",
                                          container = shiny::tags$div,
                                          class="exclusion-container"),
                                 value = "exc")
            ),
            shiny::div(id="view-specs", class="hidden",
                       shiny::textInput("currentView", NULL)
            )
         ),

         shiny::mainPanel(
            shiny::htmlOutput("viewTitle"),
            shiny::plotOutput("viewPlot")
          )
       )
   ))
}

#' @import shiny
create_fdviews_server <- function(fdviews_out, app_type, data,
                                  fdviews_group1 = NULL, fdviews_group2 = NULL,
                                  fdviews_group1_name=NULL, fdviews_group2_name=NULL,
                                  target = NULL){
   stopifnot(is.character(app_type))
   stopifnot(app_type %in% APP_TYPES)

   shiny::shinyServer(function(input, output) {

       # Side panel maintenance
       output$numViewsTable <- shiny::renderDataTable(
          create_view_table('num', app_type, fdviews_out),
          options = data_table_options(app_type),
          callback = data_table_js(app_type)
       )

       output$catViewsTable <- shiny::renderDataTable(
          create_view_table('cat', app_type, fdviews_out),
          options = data_table_options(app_type),
          callback = data_table_js(app_type)
       )

       output$exclusionComments <- shiny::renderUI({
          describeExclusions(fdviews_out)
       })

      # Main panel maintenance
      # Reactive variables
      selected_view_id <- shiny::reactive({
         as.integer(input$currentView)
      })

      # Output bindings
      output$viewTitle <- shiny::renderUI({
         view_type <- shiny::isolate(input$viewTab)
         view_id   <- selected_view_id()
         if(is.na(view_id)) return(NULL)

         create_view_title(view_id, view_type, fdviews_out)
      })

      output$viewPlot <- shiny::renderPlot({
         view_type <- shiny::isolate(input$viewTab)
         view_id   <- selected_view_id()
         if(is.na(view_id) | view_type == 'exc') return(NULL)

         plot_selection(view_id, view_type, app_type,
                        fdviews_out, data,
                        fdviews_group1, fdviews_group2,
                        fdviews_group1_name, fdviews_group2_name,
                        target)
      })

   })
}


create_fdviews_app <- function(fdviews_out, app_type,
                               data, data_name,
                               fdviews_group1=NULL, fdviews_group2=NULL,
                               fdviews_group1_name=NULL, fdviews_group2_name=NULL,
                               target=NULL){

   stopifnot(is.character(app_type))
   stopifnot(app_type %in% APP_TYPES)
   stopifnot(is.data.frame(data))

   fdviews_app <- shiny::shinyApp(
      ui     = create_fdviews_client(app_type, data_name, target),
      server = create_fdviews_server(fdviews_out, app_type, data,
                                     fdviews_group1, fdviews_group2,
                                     fdviews_group1_name, fdviews_group2_name,
                                     target)
   )

   return(fdviews_app)
}


#####################################
# Wrappers for viewsearch functions #
#####################################

#' Views of a multidimensional dataset.
#'
#' \code{findviews} detects and plots groups of mutually dependent columns.
#' It is based on Shiny and ggplot.
#'
#
#' The function \code{findviews} takes a data frame or a matrix as input. It
#' computes the pairwise dependency between the columns, detects clusters in the
#' resulting structure, and starts a Shiny application to display the results.
#'
#' The function processes numerical and categorical data separately. It excludes
#' the columns with only one value, the columns in which all the values are
#' distinct (e.g., primary keys), and the columns with more than 75\% missing values.
#'
#' \code{findviews} computes the dependency between the columns differently
#' depending on their type. It uses Pearson's coefficient of correlation for
#' numerical data, and Cramer's V for categorical data.
#'
#' To cluster the columns, \code{findviews} uses the function
#' \code{\link[stats]{hclust}}, R's implementation of agglomerative hierarchical
#' clustering. The number of clusters is determined by the parameter
#' \code{view_size_max}. The parameter \code{clust_method} specifies which
#' flavor of agglomerative clustering to use.
#'
#'
#' @param data Data frame or matrix to be visualized
#' @param view_size_max Maximum number of columns in the views. If set to
#'   \code{NULL}, findviews uses \code{log2(ncol(data))}, rounded upwards and
#'   capped at 6.
#' @param clust_method Character describing a clustering method, used internally
#'   by \code{\link[stats]{hclust}}. Example values are "complete", "single" or
#'   "average".
#' @param ... Optional Shiny parameters, used in Shiny's
#'   \code{\link[shiny]{runApp}} function.
#'
#' @examples
#' findviews(mtcars)
#' findviews(mtcars, view_size_max = 4,  port = 7000)
#'
#' @export
findviews <- function(data, view_size_max=NULL, clust_method="complete", ...){
   # Generates the views
   fdviews_out <- findviews_core(data, view_size_max, clust_method)

   # Creates and launches the Shiny server
   data_name <- deparse(substitute(data))
   fdviews_app <- create_fdviews_app(fdviews_out, "findviews", data, data_name)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}




#' Views of a multidimensional dataset, ranked by their differentiation power.
#'
#' \code{findviews_to_compare} detects views that show how two arbitrary sets
#' of rows differ. It plots the results with ggplot and Shiny.
#'
#'
#' The function \code{findviews_to_compare} takes two groups of rows as input
#' and detects views on which the statistical distribution of those two groups
#' differ.
#'
#' To detect the set of views, \code{findviews_to_compare} eliminates
#' the rows which are present in neither group and applies \code{\link{findviews}}.
#'
#' To evaluate the differentiation power of the views, findviews computes the
#' histograms of both groups and compares them with the Euclidean distance.
#'
#' This method is loosely based on the following paper: \preformatted{
#' Fast, Explainable View Detection to Characterize Exploration Queries
#' Thibault Sellam, Martin Kersten
#' SSDBM, 2016}
#'
#' @inheritParams findviews
#' @param group1 Logical vector of size \code{nrow(data)}, which describes the
#'   first group to compare. The value \code{TRUE} at position i indicates the
#'   the i-th row of \code{data} belongs to the group.
#' @param group2 Logical vector, which describes the second group to compare.
#'   The value \code{TRUE} at position i indicates the the i-th row of
#'   \code{data} belongs to the group.
#'
#' @examples
#' findviews_to_compare(mtcars$mpg >= 20 , mtcars$mpg < 20 , mtcars)
#'
#' @export
findviews_to_compare <- function(group1, group2, data,
                                 view_size_max=NULL, clust_method="complete", ...){
   fdviews_out <- findviews_to_compare_core(group1, group2, data,
                                            view_size_max, clust_method)

   # Creates and launches the Shiny server
   data_name <- deparse(substitute(data))
   group1_name <- deparse(substitute(group1))
   group2_name <- deparse(substitute(group2))

   fdviews_app <- create_fdviews_app(fdviews_out, "findviews_to_compare",
                                     data, data_name = data_name,
                                     fdviews_group1 = group1,
                                     fdviews_group2 = group2,
                                     fdviews_group1_name = group1_name,
                                     fdviews_group2_name = group2_name
   )
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}




#' Views of a multidimensional dataset, ranked by their prediction power.
#'
#' \code{findviews_to_predict} detects groups of mutually dependent columns,
#' ranks them by their predictive power, and plots them with Shiny and ggplot.
#'
#'
#' The function \code{findviews_to_predict} takes a data set and a target
#' variable as input. It detects clusters of statistically dependent columns in
#' the data set - e.g., views - and ranks those groups according to how well
#' they predict the target variable.
#'
#' To detect the views, \code{findviews_to_predict} relies on \code{findviews}.
#' To evaluate their predictive power, it uses the \emph{mutual information} between
#' the joint distribution of the columns and that of the target variable.
#'
#' Internally, \code{findviews_to_predict} discretizes all the continuous
#' variables with equi-width binning. The parameter \code{nbins} determines the
#' number of bins used for the target variable.
#'
#'
#' @inheritParams findviews
#' @param target Name of the variable to be predicted.
#'
#' @examples
#' findviews_to_predict('mpg', mtcars)
#' findviews_to_predict('mpg', mtcars, view_size_max = 4)
#'
#' @export
findviews_to_predict <- function(target, data, view_size_max=NULL,
                                 clust_method="complete", ...){
   fdviews_out <- findviews_to_predict_core(target, data,
                                            view_size_max, clust_method)

   # Creates and launches the Shiny server
   data_name <- deparse(substitute(data))
   fdviews_app <- create_fdviews_app(fdviews_out, "findviews_to_predict",
                                     data, data_name = data_name,
                                     target = target)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}
