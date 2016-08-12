#' @import shiny
create_ziggy_client <- function(){
   shinyUI(fluidPage(

      titlePanel("Let's Look at those Tuples", windowTitle = "Ziggy"),

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
create_ziggy_server <- function(ziggy_out, ziggy_data, ziggy_target){

   shinyServer(function(input, output) {

      # Side panel maintenance
      output$numViewsTable <- renderDataTable(
         create_view_table('num', ziggy_out),
         options = data_table_options,
         callback = data_table_js
      )

      output$catViewsTable <- renderDataTable(
         create_view_table('cat', ziggy_out),
         options = data_table_options,
         callback = data_table_js
      )

      output$exclusionComments <- renderUI({
         describeExclusions(ziggy_out)
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

         create_view_title(view_id, view_type, ziggy_out)
      })

      output$viewPlot <- renderPlot({
         view_type <- isolate(input$viewTab)
         view_id   <- selected_view_id()
         if(is.na(view_id)) return(NULL)

         plot_selection(view_id, view_type,
                        ziggy_out, ziggy_target, ziggy_data)
      })

      output$viewComment <- renderUI({
         view_type <- isolate(input$viewTab)
         view_id   <- selected_view_id()
         if(is.na(view_id)) return(NULL)

         create_view_comments(view_id, view_type, ziggy_out)
      })
   })
}


create_ziggy_app <- function(ziggy_out, ziggy_data, ziggy_target){

   ziggy_app <- shiny::shinyApp(
      ui     = create_ziggy_client(),
      server = create_ziggy_server(ziggy_out, ziggy_data, ziggy_target)
   )

   return(ziggy_app)
}
