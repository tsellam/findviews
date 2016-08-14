################################
# Shiny code to create the app #
################################
#' @import shiny
create_fdviews_client <- function(){
   shinyUI(fluidPage(

      titlePanel("Let's Look at those Tuples", windowTitle = "findviews"),

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
create_fdviews_server <- function(fdviews_out,
                                fdviews_group1, fdviews_group2, fdviews_data){

   shinyServer(function(input, output) {

      # Side panel maintenance
      output$numViewsTable <- renderDataTable(
         create_view_table('num', fdviews_out),
         options = data_table_options,
         callback = data_table_js
      )

      output$catViewsTable <- renderDataTable(
         create_view_table('cat', fdviews_out),
         options = data_table_options,
         callback = data_table_js
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

         plot_selection(view_id, view_type, fdviews_out,
                        fdviews_group1, fdviews_group2, fdviews_data)
      })

      output$viewComment <- renderUI({
         view_type <- isolate(input$viewTab)
         view_id   <- selected_view_id()
         if(is.na(view_id)) return(NULL)

         create_view_comments(view_id, view_type, fdviews_out)
      })
   })
}


create_fdviews_app <- function(fdviews_out, fdviews_group1, fdviews_group2, fdviews_data){

   fdviews_app <- shiny::shinyApp(
      ui     = create_fdviews_client(),
      server = create_fdviews_server(fdviews_out, fdviews_group1, fdviews_group2,
                                   fdviews_data)
   )

   return(fdviews_app)
}


#####################################
# Wrappers for viewsearch functions #
#####################################

#' @export
findviews_to_compare <- function(group1, group2, data, view_size_max=NULL, ...){
   fdviews_out    <- findviews_to_compare_core(group1, group2, data, view_size_max)

   #cat('Starting server...\n')
   fdviews_app <- create_fdviews_app(fdviews_out, group1, group2, data)
   shiny::runApp(fdviews_app, display.mode = "normal", ...)
}
