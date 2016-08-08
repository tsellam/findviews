shinyUI(fluidPage(

   titlePanel("Let's Look at those Tuples", windowTitle = "Ziggy"),

   sidebarLayout(

      sidebarPanel(
         tabsetPanel(id = "viewTab",
            tabPanel("Continuous", dataTableOutput("numViewsTable"), value = "num"),
            tabPanel("Categorical", dataTableOutput("catViewsTable"), value = "cat")
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
