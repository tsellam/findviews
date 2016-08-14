################################
# Shiny code to create the app #
################################
#' @import shiny
create_fdviews_client <- function(app_type){
   stopifnot(is.character(app_type))
   stopifnot(app_type %in% APP_TYPES)

   shinyUI(fluidPage(

      if (app_type == 'findviews')
         titlePanel("Views", windowTitle = app_type)
      else if (app_type == 'findviews_to_compare')
         titlePanel("Views to Compare", windowTitle = app_type),

      sidebarLayout(

         sidebarPanel(
            tabsetPanel(id = "viewTab",
                        tabPanel("Continuous",
                                 dataTableOutput("numViewsTable"),
                                 value = "num"),
                        tabPanel("Categorical",
                                 dataTableOutput("catViewsTable"),
                                 value = "cat"),
                        tabPanel("Excluded",
                                 htmlOutput("exclusionComments"),
                                 value = "exc")
            ),
            div(id="view-specs", class="hidden",
                textInput("currentView", NULL)
            )
         ),

          mainPanel(
             htmlOutput("viewTitle"),
             plotOutput("viewPlot"),
             htmlOutput("viewComment")
          )
       )
   ))
}

#' @import shiny
create_fdviews_server <- function(fdviews_out, app_type, data,
                                  fdviews_group1 = NULL, fdviews_group2 = NULL){
   stopifnot(is.character(app_type))
   stopifnot(app_type %in% APP_TYPES)

   shinyServer(function(input, output) {

       # Side panel maintenance
       output$numViewsTable <- renderDataTable(
          create_view_table('num', app_type, fdviews_out),
          options = data_table_options(app_type),
          callback = data_table_js(app_type)
       )

       output$catViewsTable <- renderDataTable(
          create_view_table('cat', app_type, fdviews_out),
          options = data_table_options(app_type),
          callback = data_table_js(app_type)
       )

       output$exclusionComments <- renderUI({
          describeExclusions(fdviews_out)
       })

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

         create_view_title(view_id, view_type, fdviews_out)
      })

      output$viewPlot <- renderPlot({
         view_type <- isolate(input$viewTab)
         view_id   <- selected_view_id()
         if(is.na(view_id)) return(NULL)

         plot_selection(view_id, view_type, app_type,
                        fdviews_out, data,
                        fdviews_group1, fdviews_group2)
      })

      output$viewComment <- renderUI({
         view_type <- isolate(input$viewTab)
         view_id   <- selected_view_id()
         if(is.na(view_id)) return(NULL)

         create_view_comments(view_id, view_type, app_type, fdviews_out)
      })
   })
}


create_fdviews_app <- function(fdviews_out, app_type, data,
                               fdviews_group1=NULL, fdviews_group2=NULL){

   stopifnot(is.character(app_type))
   stopifnot(app_type %in% APP_TYPES)
   stopifnot(is.data.frame(data))

   fdviews_app <- shiny::shinyApp(
      ui     = create_fdviews_client(app_type),
      server = create_fdviews_server(fdviews_out, app_type, data,
                                     fdviews_group1, fdviews_group2)
   )

   return(fdviews_app)
}


#####################################
# Wrappers for viewsearch functions #
#####################################

#' @export
findviews <- function(data, view_size_max=NULL, ...){
   fdviews_out <- findviews_core(data, view_size_max)
   fdviews_app <- create_fdviews_app(fdviews_out, "findviews", data)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}

#' @export
findviews_to_compare <- function(group1, group2, data, view_size_max=NULL, ...){
   fdviews_out <- findviews_to_compare_core(group1, group2, data, view_size_max)
   fdviews_app <- create_fdviews_app(fdviews_out, "findviews_to_compare", data,
                                     group1, group2)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}

