shinyUI(fluidPage(

   titlePanel("Let's Look at those Tuples", windowTitle = "Ziggy"),

   sidebarLayout(

      sidebarPanel(
         dataTableOutput("viewsTable"),
         div(id="view-specs",#class="hidden",
            textInput("currentView", NULL),
            textInput("currentViewType", NULL)
         )
      ),

      mainPanel(
         htmlOutput("viewTitle"),
         plotOutput("viewPlot"),
         htmlOutput("viewComment")
      )
   )
))
