#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
data <- read_csv("DataSimulation_RBarLME4.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Mixed Effects Linear Modelling with lme4"),
    
    p("This is an app demonstrating how each parameter in a mixed effects linear model affects the predictions the model makes. The data are shown in
    black and the predictions made by the model you build are shown in red. You can manipulate  the overall intercept, the overall slope, the intercept
    for each PARTICIPANT and each ITEM and the slope of each PARTICIPANT by CATEGORY.
      On the far left are the model intercept and slope values, where the slope reflects the main effect of your manipulated variable, here the type
      of word. The four sliders in the top row are the first random effects on intercepts, or the individual intercepts for each participant.
      The next four are the second random effects on intercepts, or the individual intercepts for each item. The last four are the
      random effects on slopes, or the individual slopes for each participant's reaction to each category.
      The app takes these values and computes y = mx + b where b is the overall intercept plus the intercept you've assigned to that participant
      and item and m is the overall slope plus the slope you've assigned to that participant."),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(column(2, sliderInput("Intercept",
                                   "Choose an intercept value:",
                                   min = 700,
                                   max = 950,
                                   value = 800)),
             column(2, sliderInput("RandIntP1",
                                   "PARTICIPANT 1's intercept:",
                                   min = -200,
                                   max = 200,
                                   value = 0)),
             column(2, sliderInput("RandIntP2",
                                "PARTICIPANT 2's intercept:",
                                min = -200,
                                max = 200,
                                value = 0)),
             column(2, sliderInput("RandIntP3",
                                "PARTCIPANT 3's intercept:",
                                min = -200,
                                max = 200,
                                value = 0)),
             column(2, sliderInput("RandIntP4",
                                "PARTICIPANT 4's intercept:",
                                min = -200,
                                max = 200,
                                value = 0))),
    fluidRow(
      column(2, sliderInput("FixedEffect",
                  "Choose a value for 'm', or the size of the effect of the manipulated variable:",
                  min = -100,
                  max = 100,
                  value = 0)),
      column(2, sliderInput("RandIntI1",
                            "ITEM 1's intercept:",
                            min = -200,
                            max = 200,
                            value = 0)),
      column(2, sliderInput("RandIntI2",
                                        "ITEM 2's intercept:",
                                        min = -200,
                                        max = 200,
                                        value = 0)),
      column(2, sliderInput("RandIntI3",
                  "ITEM 3's intercept:",
                  min = -200,
                  max = 200,
                  value = 0)),
      column(2, sliderInput("RandIntI4",
                  "ITEM 4's intercept:",
                  min = -200,
                  max = 200,
                  value = 0))),
    fluidRow(
      column(2, offset = 2, sliderInput("RandSlopeP1",
                  "PARTICIPANT 1's slope:",
                  min = -300,
                  max = 300,
                  value = 0)),
      column(2, sliderInput("RandSlopeP2",
                  "PARTICIPANT 2's slope:",
                  min = -300,
                  max = 300,
                  value = 0)),
      column(2, sliderInput("RandSlopeP3",
                  "PARTICIPANT 3's slope:",
                  min = -300,
                  max = 300,
                  value = 0)),
      column(2, sliderInput("RandSlopeP4",
                  "PARTICIPANT 4's slope:",
                  min = -300,
                  max = 300,
                  value = 0))),
    
    fluidRow(
        mainPanel(
            plotOutput("modelPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    reactfunc <- reactive({
        b0 <- input$Intercept
        m0 <- input$FixedEffect
        b1 <- input$RandIntP1
        b2 <- input$RandIntP2
        b3 <- input$RandIntP3
        b4 <- input$RandIntP4
        m1 <- input$RandSlopeP1
        m2 <- input$RandSlopeP2
        m3 <- input$RandSlopeP3
        m4 <- input$RandSlopeP4
        b1i <- input$RandIntI1
        b2i <- input$RandIntI2
        b3i <- input$RandIntI3
        b4i <- input$RandIntI4
        x <- 1
        
        data$Predictions[data$Subject == 1 & data$Item == 1] <- ((m0+m1)*x) + (b0 + b1 + b1i)
        data$Predictions[data$Subject == 1 & data$Item == 2] <- ((m0+m1)*x) + (b0 + b1 + b2i)
        data$Predictions[data$Subject == 1 & data$Item == 3] <- (((2*m0)+(2*m1))*x) + (b0 + b1 + b3i)
        data$Predictions[data$Subject == 1 & data$Item == 4] <- (((2*m0)+(2*m1))*x) + (b0 + b1 + b4i)
        data$Predictions[data$Subject == 2 & data$Item == 1] <- ((m0+m2)*x) + (b0 + b2 + b1i)
        data$Predictions[data$Subject == 2 & data$Item == 2] <- ((m0+m2)*x) + (b0 + b2 + b2i)
        data$Predictions[data$Subject == 2 & data$Item == 3] <- (((2*m0)+(2*m2))*x) + (b0 + b2 + b3i)
        data$Predictions[data$Subject == 2 & data$Item == 4] <- (((2*m0)+(2*m2))*x) + (b0 + b2 + b4i)
        data$Predictions[data$Subject == 3 & data$Item == 1] <- ((m0+m3)*x) + (b0 + b3 + b1i)
        data$Predictions[data$Subject == 3 & data$Item == 2] <- ((m0+m3)*x) + (b0 + b3 + b2i)
        data$Predictions[data$Subject == 3 & data$Item == 3] <- (((2*m0)+(2*m3))*x) + (b0 + b3 + b3i)
        data$Predictions[data$Subject == 3 & data$Item == 4] <- (((2*m0)+(2*m3))*x) + (b0 + b3 + b4i)
        data$Predictions[data$Subject == 4 & data$Item == 1] <- ((m0+m4)*x) + (b0 + b4 + b1i)
        data$Predictions[data$Subject == 4 & data$Item == 2] <- ((m0+m4)*x) + (b0 + b4 + b2i)
        data$Predictions[data$Subject == 4 & data$Item == 3] <- (((2*m0)+(2*m4))*x) + (b0 + b4 + b3i)
        data$Predictions[data$Subject == 4 & data$Item == 4] <- (((2*m0)+(2*m4))*x) + (b0 + b4 + b4i)
        
        return(data)
    })
    
    output$modelPlot <- renderPlot({
        # create plot
        ggplot(reactfunc(),
               aes(x = Item, y = RT)) +
            geom_point(size = 2) +
            geom_point(aes(x = Item, y = Predictions), size = 3, col = "Red") +
            facet_grid(~ Subject) +
            theme_bw()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
