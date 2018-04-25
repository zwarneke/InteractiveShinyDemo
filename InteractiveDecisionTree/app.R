library(shiny)
library(rpart.plot)
library(googlesheets)

gs_auth(token = "google_sheets_app_token.rds")
RESPONSE_GOOGLE_SHEET_KEY = "1zFS-bjEEAGoShCI-lK8TUnoud65vv-LXmDRCPHHp1Sw"

MakeFormula = function(X,dep) {
  Xname = names(X)
  Xname = Xname[Xname != dep]
  ind = paste(Xname, collapse= "+")
  y = paste(dep,' ~ ')
  f = as.formula(paste(y, ind))
  return(f)
}

ui <- navbarPage("Interactive Shiny Demo", id = "MainNavbar",
  tabPanel("Survey",
           id = "Survey",
           titlePanel("Interactive Shiny Survey"),
           fluidRow(
             column(4,
                    selectInput("CohortYear", h3("Cohort"),
                                choices = list("2015", "2016"),
                                selected = "2015")
                    ),
             column(4,
                    selectInput("FavoriteMLMethod", h3("Favorite ML Method"),
                                choices = list("Perceptron", "Neural Network"),
                                selected = "Perceptron")
             ),
             column(4,
                    selectInput("FavoriteProfessor", h3("Favorite Professor"),
                                choices = list("Keck", "Bourke", "Valentine", "Vinod", "Cooper"),
                                selected = "Keck")
             )
           ),
           fluidRow(
             column(4,
                    sliderInput("AverageSalt", h3("Saltiness"),
                                min = 0, max = 10, value = 5)
             ),
             column(4,
                    sliderInput("DataModelsClassRating", h3("D&M Class Rating"),
                                min = 0, max = 10, value = 5)
             ),
             column(4,
                    numericInput("FreeTime", h3("Daily Free Time (hours)"),
                                value = 5)
             )
           ),
           fluidRow(
             column(4, offset = 4,
                    br(),
                    actionButton("submitButton", "Submit Response", width = "100%")
                    )
           )
           ),
  tabPanel("Results",
           id = "Results",
           # display decision tree output, with ability to modify parameters
           sidebarLayout(
             sidebarPanel(
               helpText("Adjust the decision tree parameters. NOTE: Decision tree will not update automatically due to loading time."),
               sliderInput("MinSplit", h3("Min Split"),
                           min = 1, max = 75, value = 5),
               sliderInput("MinBucket", h3("Min Bucket"),
                           min = 1, max = 75, value = 5),
               sliderInput("CP", h3("Complexity Parameter"),
                           min = 0, max = 1, value = 0.01, step = 0.001),
               actionButton("viewButton", "View Decision Tree", width = "100%")
               ),
             
             mainPanel(plotOutput("decisionTree"))
           )
           )
)

server <- function(input, output, session) {
  
  observeEvent(input$submitButton, {
    # save data to google sheet
    googleSheet = gs_key(RESPONSE_GOOGLE_SHEET_KEY)
    gs_add_row(googleSheet,
               input = c(input$CohortYear, input$AverageSalt, input$DataModelsClassRating, input$FavoriteMLMethod, input$FreeTime, input$FavoriteProfessor))
    
    # redirect to results page
    updateNavbarPage(session, "MainNavbar", selected = "Results")
  })
  
  computeDecisionTree = eventReactive(input$viewButton, {
    # read data from google sheets
    googleSheet = gs_key(RESPONSE_GOOGLE_SHEET_KEY)
    studentInputData = data.frame(gs_read_csv(googleSheet), stringsAsFactors = TRUE)
    inputFormula = MakeFormula(studentInputData, "CohortYear")
    
    rpart(inputFormula,
          data = studentInputData,
          method = "class",
          parms = list(split = "information"),
          control = rpart.control(minsplit = input$MinSplit, minbucket = input$MinBucket, cp = input$CP))
  })
   output$decisionTree <- renderPlot({
     # plot model
     rpart.plot(computeDecisionTree(), clip.right.labs = FALSE, extra = 2)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

