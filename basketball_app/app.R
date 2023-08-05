#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Link to shiny app: https://eidelhochm.shinyapps.io/basketball_app/


knitr::opts_chunk$set(
  echo = TRUE,
  size = "small",
  collapse = TRUE,
  comment = NA,
  warning = FALSE,
  message = FALSE,
  error = TRUE,
  eval = TRUE
) # change it to TRUE

# Install necessary packages
# install.packages(c("shiny", "shinythemes", "shinyWidgets", "leaflet", "tidyverse", "janitor", "DT", "maps", "maptools", "sp"))

library(shiny)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(janitor)
library(DT)
library(maps)
library(maptools)
library(sp)
library(tidyverse)
library(purrr)
library(rvest)
library(polite)
library(readr)
library(plotly)

qualifying_data1 <- read_csv("qualifying_data1.csv")
college3 <- read_csv("college3.csv")


ui <- navbarPage(
  theme = shinytheme("cerulean"), "NBA Data Analysis",
  windowTitle = "NBA Stats!",
  tabPanel(
    "Welcome!",
    fluidPage(
      uiOutput("text")
    )
  ),
  tabPanel(
    "Individual NBA Players",
    fluidPage(
      varSelectInput(inputId = "x", label = "Select X-Axis Variable", data = qualifying_data1 %>% select(c(-1, -2, -3, -30)), selected = names(qualifying_data1)[1], multiple = FALSE),
      varSelectInput(inputId = "y", label = "Select Y-Axis Variable", data = qualifying_data1 %>% select(c(-1, -2, -3, -30)), selected = names(qualifying_data1)[1], multiple = FALSE),
      checkboxInput(inputId = "color", label = "Color by Team", value = FALSE),
      plotOutput("plot", height = 600, click = "my_click"),
      dataTableOutput("table")
    )
  ),
  tabPanel(
    "Stats By Team",
    fluidPage(
      radioButtons("team_chosen", "Pick an NBA Team: ", choices = unique(qualifying_data1$team), inline = TRUE),
      varSelectInput(inputId = "x2", label = "Select X-Axis Variable", data = qualifying_data1 %>% select(c(-1, -2, -3, -30)), selected = names(qualifying_data1)[1], multiple = FALSE),
      varSelectInput(inputId = "y2", label = "Select Y-Axis Variable", data = qualifying_data1 %>% select(c(-1, -2, -3, -30)), selected = names(qualifying_data1)[1], multiple = FALSE),
      plotlyOutput("plot2")
    )
  ),
  tabPanel(
    "Players From Colleges",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          # Slider input for selecting the number of bars
          sliderInput("num_bars", "Number of Colleges", min = 1, max = 30, value = 5),
          checkboxInput("no_college", "Check to Remove No College/Unknown", value = FALSE),
          actionButton("go_button", "Go!")
        ),
        mainPanel(
          plotlyOutput("barplot")
        )
      )
    )
  )
)




server <- function(input, output) {
  output$plot <- renderPlot({
    g <- ggplot(qualifying_data1, aes(x = !!(input$x), y = !!(input$y)))
    if (input$color) {
      g + geom_point(aes(color = team))
    } else {
      g + geom_point()
    }
  })
  output$plot2 <- renderPlotly({
    h <- ggplot(qualifying_data1 %>% filter(team == input$team_chosen), aes(x = !!(input$x2), y = !!(input$y2), label = name, text = paste("position: ", position, "\n", "age: ", age, "\n", "college: ", college, "\n", sep = ""))) +
      geom_point()
    h
  })
  
  output$table <- renderDataTable({
    near_data <- nearPoints(qualifying_data1, input$my_click)
    near_data
  })
  
  
  output$barplot <- renderPlotly({
    input$go_button
    
    ({
    
    if (isolate(input$no_college)) {
      # Generate the bar plot using ggplot2
      w <- ggplot(college3 %>% filter(college != "No College/Unknown") %>% slice_max(count, n = isolate(input$num_bars), with_ties = FALSE), aes(x = count, y = college, label = mean_salary)) +
        geom_col(fill = "steelblue") 
      w
    } else {
      z <- ggplot(college3 %>% slice_max(count, n = isolate(input$num_bars), with_ties = FALSE), aes(x = count, y = college, label = mean_salary)) +
        geom_col(fill = "steelblue") 
      z
    }
  })
  })
  
  output$text <- renderUI({
    HTML("<h2>Are You Interested in The NBA?</h2>
          <p>Check out the Individual Players tab to learn which players are the best at various stats. And learn how those stats might be related to eachother! Not suprisingly,we see that minutes played is related to how many points scored. But did you know theres also a relationship between turnovers and free throws made?</p>
          <p>Want to know how these stats are broken down at a team level? Go to the Stats By Team tab to interact with each team in the league and hover over the graph to see which player is represented by each data point. </p>
          <p>Finally, are you interested in which college will give you the best shot of making it to the NBA? Go to the Players From Colleges tab and select the amount of schools you want to view to see how many players currently in the NBA graduated from each school. Additionally, hover over the school to learn the average salary of current NBA graduates from that college! </p>
          <p>We hope you learn a lot from our interactive page!</p>")
  })
}

# You can modify the height to avoid scrolling
shinyApp(ui, server, options = list(height = 600))

