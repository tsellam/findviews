################################
# Shiny code to create the app #
################################
create_fdviews_client <- function(app_type, target=NULL){
   stopifnot(is.character(app_type))
   stopifnot(app_type %in% APP_TYPES)

   shiny::shinyUI(shiny::fluidPage(

      if (app_type == 'findviews')
         shiny::titlePanel("Views", windowTitle = app_type)
      else if (app_type == 'findviews_to_compare')
         shiny::titlePanel("Views to Compare", windowTitle = app_type)
      else if (app_type == 'findviews_to_predict')
         shiny::titlePanel(paste0("Views to Predict ", target), windowTitle = app_type),

      shiny::sidebarLayout(

         shiny::sidebarPanel(
            shiny::tabsetPanel(id = "viewTab",
                               shiny::tabPanel("Continuous",
                                 dataTableOutput("numViewsTable"),
                                 value = "num"),
                               shiny::tabPanel("Categorical",
                                 dataTableOutput("catViewsTable"),
                                 value = "cat"),
                               shiny::tabPanel("Excluded",
                                 htmlOutput("exclusionComments"),
                                 value = "exc")
            ),
            shiny::div(id="view-specs", class="hidden",
                       shiny::textInput("currentView", NULL)
            )
         ),

         shiny::mainPanel(
            shiny::htmlOutput("viewTitle"),
            shiny::plotOutput("viewPlot"),
            shiny::htmlOutput("viewComment")
          )
       )
   ))
}

#' @import shiny
create_fdviews_server <- function(fdviews_out, app_type, data,
                                  fdviews_group1 = NULL, fdviews_group2 = NULL,
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
         if(is.na(view_id)) return(NULL)

         plot_selection(view_id, view_type, app_type,
                        fdviews_out, data,
                        fdviews_group1, fdviews_group2, target)
      })

      output$viewComment <- shiny::renderUI({
         view_type <- shiny::isolate(input$viewTab)
         view_id   <- selected_view_id()
         if(is.na(view_id)) return(NULL)

         create_view_comments(view_id, view_type, app_type, fdviews_out)
      })
   })
}


create_fdviews_app <- function(fdviews_out, app_type, data,
                               fdviews_group1=NULL, fdviews_group2=NULL,
                               target=NULL){

   stopifnot(is.character(app_type))
   stopifnot(app_type %in% APP_TYPES)
   stopifnot(is.data.frame(data))

   fdviews_app <- shiny::shinyApp(
      ui     = create_fdviews_client(app_type, target),
      server = create_fdviews_server(fdviews_out, app_type, data,
                                     fdviews_group1, fdviews_group2,
                                     target)
   )

   return(fdviews_app)
}


#####################################
# Wrappers for viewsearch functions #
#####################################

#' @export
findviews <- function(data, view_size_max=NULL, clust_method="single", ...){
   fdviews_out <- findviews_core(data, view_size_max)
   fdviews_app <- create_fdviews_app(fdviews_out, "findviews", data)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}

#' @export
findviews_to_compare <- function(group1, group2, data,
                                 view_size_max=NULL, clust_method="single", ...){
   fdviews_out <- findviews_to_compare_core(group1, group2, data, view_size_max)
   fdviews_app <- create_fdviews_app(fdviews_out, "findviews_to_compare", data,
                                     fdviews_group1=group1, fdviews_group2=group2)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}

#' @export
findviews_to_predict <- function(target, data,
                                 view_size_max=NULL, clust_method="single",
                                 nbins=4,...){
   fdviews_out <- findviews_to_predict_core(target, data, view_size_max, nbins)
   fdviews_app <- create_fdviews_app(fdviews_out, "findviews_to_predict", data, target=target)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}
