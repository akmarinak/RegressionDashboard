# Package yang mungkin digunakan
# library(shiny)
# library(shinydashboard)
# library(shinydashboardPlus)
# library(RPostgreSQL)
# library(DBI)
# library(DT)
# # library(bs4Dash)
# library(dplyr)
# library(recipes)
library(shinythemes)
library(shinyWidgets)
library(shiny)
library(shinydashboard)
library(recipes)
data(mtcars)
#=========================Dataset Contoh=========================#
# Define df1
tabel01 <- data(women)
variables=c("mpg","cyl","disp","hp","drat","wt","qsec","vs")
#============================Interface===========================#
# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage(
    "Dashboard Kami hehe",
    tabPanel(
      "Regression",
      tabName = "regression",
      selectInput(inputId = "dependent",
                  label = "Dependent Variables",
                  choices = as.list(variables)),
      uiOutput("indep"),
      verbatimTextOutput(outputId = "RegOut")
    )
  )
)
#==========================Back - End===========================#
# Define server logic required to draw a histogram
server <- #============================= SERVER (back-end) ==============================#
  function(input, output){
    output$indep <- renderUI({
      selectInput(inputId = "indep", label = "Independent Variables", 
                  multiple = TRUE, choices = as.list(AttributeChoices[AttributeChoices!= input$dependent]), selected = AttributeChoices[1])
    })
    
    formula <- reactive({
      req(input$indep)
      mtcars %>%
        recipe() %>%
        update_role(!!!input$dependent, new_role = "outcome") %>%
        update_role(!!!input$indep, new_role = "predictor") %>%
        prep() %>% 
        formula()
    })
    
    lm_reg <- reactive(
      lm(formula(),data = mtcars)
    )
    
    output$RegOut = renderPrint({
      summary(lm_reg())
    })
  }

# Run the application 
shinyApp(ui = ui, server = server)
