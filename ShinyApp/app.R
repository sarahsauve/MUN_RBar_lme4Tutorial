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
    for each PARTICIPANT and the slope for each ITEM.
      On the far left are the model intercept and slope values, where the slope reflects the main effect of your manipulated variable, here the type
      of word. The next four sliders are the random effects on intercepts, or the individual intercepts for each participant. The last four are the
      random effects on slopes, or the individual slopes for each item.
      The app takes these values and computes y = mx + b where b is the overall intercept plus the intercept you've assigned to that participant
      and m is the overall slope plus the slope you've assigned to that item."),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(column(2, sliderInput("Intercept",
                                   "Choose an intercept value:",
                                   min = 700,
                                   max = 950,
                                   value = 800),
                    sliderInput("FixedEffect",
                                "Choose a value for 'm', or the size of the effect of the manipulated variable:",
                                min = -100,
                                max = 100,
                                value = 0)),
             column(2, sliderInput("RandIntP1",
                                   "Choose a value by which PARTICIPANT 1's intercept will be different from the primary intercept:",
                                   min = -200,
                                   max = 200,
                                   value = 0),
                    sliderInput("RandIntP2",
                                "Choose a value by which PARTICIPANT 2's intercept will be different from the primary intercept:",
                                min = -200,
                                max = 200,
                                value = 0)),
             column(2, sliderInput("RandIntP3",
                                   "Choose a value by which PARTCIPANT 3's intercept will be different from the primary intercept:",
                                   min = -200,
                                   max = 200,
                                   value = 0),
                    sliderInput("RandIntP4",
                                "Choose a value by which PARTICIPANT 4's intercept will be different from the primary intercept:",
                                min = -200,
                                max = 200,
                                value = 0)),
             column(2, sliderInput("RandSlopeI1",
                                    "Choose a value by which ITEM 1's slope will be different from the primary slope:",
                                    min = -300,
                                    max = 300,
                                    value = 0),
                    sliderInput("RandSlopeI2",
                                "Choose a value by which ITEM 2's slope will be different from the primary slope:",
                                min = -300,
                                max = 300,
                                value = 0)),
             column(2, sliderInput("RandSlopeI3",
                                    "Choose a value by which ITEM 3's slope will be different from the primary slope:",
                                    min = -300,
                                    max = 300,
                                    value = 0),
                    sliderInput("RandSlopeI4",
                                "Choose a value by which ITEM 4's slope will be different from the primary slope:",
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
        m1 <- input$RandSlopeI1
        m2 <- input$RandSlopeI2
        m3 <- input$RandSlopeI3
        m4 <- input$RandSlopeI4
        x <- 1
        
        data$Predictions[data$Subject == 1 & data$Item == 1] <- ((m0+m1)*x) + (b0 + b1)
        data$Predictions[data$Subject == 1 & data$Item == 2] <- (((2*m0)+(2*m2))*x) + (b0 + b1)
        data$Predictions[data$Subject == 1 & data$Item == 3] <- (((3*m0)+(3*m3))*x) + (b0 + b1)
        data$Predictions[data$Subject == 1 & data$Item == 4] <- (((4*m0)+(4*m4))*x) + (b0 + b1)
        data$Predictions[data$Subject == 2 & data$Item == 1] <- ((m0+m1)*x) + (b0 + b2)
        data$Predictions[data$Subject == 2 & data$Item == 2] <- (((2*m0)+(2*m2))*x) + (b0 + b2)
        data$Predictions[data$Subject == 2 & data$Item == 3] <- (((3*m0)+(3*m3))*x) + (b0 + b2)
        data$Predictions[data$Subject == 2 & data$Item == 4] <- (((4*m0)+(4*m4))*x) + (b0 + b2)
        data$Predictions[data$Subject == 3 & data$Item == 1] <- ((m0+m1)*x) + (b0 + b3)
        data$Predictions[data$Subject == 3 & data$Item == 2] <- (((2*m0)+(2*m2))*x) + (b0 + b3)
        data$Predictions[data$Subject == 3 & data$Item == 3] <- (((3*m0)+(3*m3))*x) + (b0 + b3)
        data$Predictions[data$Subject == 3 & data$Item == 4] <- (((4*m0)+(4*m4))*x) + (b0 + b3)
        data$Predictions[data$Subject == 4 & data$Item == 1] <- ((m0+m1)*x) + (b0 + b4)
        data$Predictions[data$Subject == 4 & data$Item == 2] <- (((2*m0)+(2*m2))*x) + (b0 + b4)
        data$Predictions[data$Subject == 4 & data$Item == 3] <- (((3*m0)+(3*m3))*x) + (b0 + b4)
        data$Predictions[data$Subject == 4 & data$Item == 4] <- (((4*m0)+(4*m4))*x) + (b0 + b4)
        
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
