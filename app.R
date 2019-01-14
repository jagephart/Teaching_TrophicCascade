#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(tidyverse)
library(ggthemes)

# Enter data
df <- data.frame(Group = c("Piscivore", "Planktivore", "Zooplankton", "Phytoplankton"),
                 Biomass = c(50, 250, 1250, 5000),
                 Data = rep("Baseline", 4))

# Write trophic cascade functions
TC_Piscivore <- function(Piscivore){
  Planktivore <- (-Piscivore+100)*5
  Zooplankton <- Piscivore*25
  Phytoplankton <- (-Piscivore+100)*100
  
  return(data.frame(Group = c("Piscivore", "Planktivore", "Zooplankton", "Phytoplankton"),
                    Biomass = c(Piscivore, Planktivore, Zooplankton, Phytoplankton), 
                    Data = rep("Scenario", 4)))
}

TC_Nutrient <- function(Nutrient){
  Phytoplankton <- Nutrient*1000
  Zooplankton <- Phytoplankton*0.25
  Planktivore <- ((-0.1097*Nutrient^2)+(5.3709*Nutrient)-18.382)*43.65
  Piscivore <- ((-0.1097*Nutrient^2)+(5.3709*Nutrient)-18.382)*8.8
  
  return(data.frame(Group = c("Piscivore", "Planktivore", "Zooplankton", "Phytoplankton"),
                    Biomass = c(Piscivore, Planktivore, Zooplankton, Phytoplankton), 
                    Data = rep("Scenario", 4)))
}

TC_Phytoplankton <- function(Phytoplankton){
  Zooplankton <- Phytoplankton*0.25
  Planktivore <- Phytoplankton*0.05
  Piscivore <- Phytoplankton*0.01
  
  return(data.frame(Group = c("Piscivore", "Planktivore", "Zooplankton", "Phytoplankton"),
                    Biomass = c(Piscivore, Planktivore, Zooplankton, Phytoplankton), 
                    Data = rep("Scenario", 4)))
}

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Trophic cascades caused by human actions: top-down and bottom-up controls on food webs"),
   
   # Input and resulting plots, laid out vertically
   verticalLayout(
    # Scenario 1
    h4("Scenario 1: Lack of fishing regulations for this lake has resulted in the loss of the top predator. 
        Change the piscivore biomass below and observe the cascading effects on the lower trophic levels."),
     
    h4("Record your hypothesis and then test your hypothesis by changing the piscivore biomass below. Record your observations."),
      
    numericInput("Piscivore1",
                    "Removal of top predator: Piscivore Biomass (min=1, max=50)",
                    min = 1,
                    max = 50,
                    value = 50),
      
    # Show a plot of the generated distribution
    plotOutput("distPlot1"),

    
    # Scenario 2
    h4("Scenario 2: The lake association wants to stock largemouth bass to increase fishing tourism on the lake. 
      Change the piscivore biomass and observe the cascading effects on the other trophic levels."),

    h4("Record your hypothesis and then test your hypothesis by changing the piscivore biomass below. Record your observations."),

    numericInput("Piscivore2",
             "Stocking a top predator: Piscivore Biomass (min=50, max=99)",
             min = 50,
             max = 99,
             value = 50),

    # Show a plot of the generated distribution
    plotOutput("distPlot2"),
    
    # Scenario 3
    h4("Scenario 3: Farmers around Lake Pace apply fertilizer to help improve crop yields. However, since no plants have grown yet, there is excess fertilizer on the ground. 
       After a rain event significant loads of fertilizer run off the fields into Lake Pace causing eutrophication."),
    
    h4("Record your hypothesis and then test your hypothesis by changing the nutrient loading value below. Record your observations."),
    
    numericInput("Nutrient",
                 "Fertilizer Runoff: Nutrient loading (min=5, max=50)",
                 min = 5,
                 max = 50,
                 value = 5),
    
    # Show a plot of the generated distribution
    plotOutput("distPlot3"),
    
    # Scenario 4
    h4("Scenario 4: The introduction of the invasive zebra mussel, a very efficient filter feeder, drastically decreases phytoplankton biomass. 
       Change the phytoplankton biomass below and observe the cascading effects."),
    
    h4("Record your hypothesis and then test your hypothesis by changing the phytoplankton biomass below. Record your observations."),
    
    numericInput("Phytoplankton",
                 "Invasive Species: Phytoplankton Biomass (min=100, max=5000)",
                 min = 100,
                 max = 5000,
                 value = 5000),
    
    # Show a plot of the generated distribution
    plotOutput("distPlot4")
   )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot1 <- renderPlot({
     # generate scenario data
     out_df <- rbind(df, TC_Piscivore(input$Piscivore1))
     out_df <- mutate(out_df, logBiomass = log(Biomass))
     
     # Correct factor level order
     out_df$Group <- factor(out_df$Group, levels(out_df$Group)[c(1, 4, 3, 2)])
      
     # draw the barplot
     ggplot(out_df, aes(x = Group, y = logBiomass, fill= Data)) +
       geom_bar(stat='identity', position="dodge") +
       coord_flip() +
       theme_classic(base_size = 20) +
       xlab("") +
       ylab("log(Biomass)")
       
   })
   
   output$distPlot2 <- renderPlot({
     # generate scenario data
     out_df <- rbind(df, TC_Piscivore(input$Piscivore2))
     out_df <- mutate(out_df, logBiomass = log(Biomass))
     
     # Correct factor level order
     out_df$Group <- factor(out_df$Group, levels(out_df$Group)[c(1, 4, 3, 2)])
     
     # draw the barplot
     ggplot(out_df, aes(x = Group, y = logBiomass, fill= Data)) +
       geom_bar(stat='identity', position="dodge") +
       coord_flip() +
       theme_classic(base_size = 20) +
       xlab("") +
       ylab("log(Biomass)")
     
   })
   
   output$distPlot3 <- renderPlot({
     # generate scenario data
     out_df <- rbind(df, TC_Nutrient(input$Nutrient))
     out_df <- mutate(out_df, logBiomass = log(Biomass))
     
     # Correct factor level order
     out_df$Group <- factor(out_df$Group, levels(out_df$Group)[c(1, 4, 3, 2)])
     
     # draw the barplot
     ggplot(out_df, aes(x = Group, y = logBiomass, fill= Data)) +
       geom_bar(stat='identity', position="dodge") +
       coord_flip() +
       theme_classic(base_size = 20) +
       xlab("") +
       ylab("log(Biomass)")
     
   })
   
   output$distPlot4 <- renderPlot({
     # generate scenario data
     out_df <- rbind(df, TC_Phytoplankton(input$Phytoplankton))
     out_df <- mutate(out_df, logBiomass = log(Biomass))
     
     # Correct factor level order
     out_df$Group <- factor(out_df$Group, levels(out_df$Group)[c(1, 4, 3, 2)])
     
     # draw the barplot
     ggplot(out_df, aes(x = Group, y = logBiomass, fill= Data)) +
       geom_bar(stat='identity', position="dodge") +
       coord_flip() +
       theme_classic(base_size = 20) +
       xlab("") +
       ylab("log(Biomass)")
     
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

