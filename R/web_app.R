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
