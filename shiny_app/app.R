library(conflicted)
# Automatically resolve conflicts by preferring dplyr's functions
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("lag", "dplyr")
library(shiny)
library(readr)
library(knitr)
library(tidyverse)
library(readxl)
library(dplyr)
library(DT)

# Data pre-processing
read_and_process_data <- function() {
  # Read Excel file
  data <- read_excel("../files/students_participation_tracker_BP.xlsx", sheet = "Sheet1")
  # Process your data as needed
  data <- data %>%
    select(4:19) %>%
    rename(
      week_1 = "Week1_activities [Total Pts: 100 Score] |2715291",
      week_2 = "Week2_activities [Total Pts: 100 Score] |2715292",
      week_3 = "Week3_activities [Total Pts: 100 Score] |2715293",
      week_6 = "Week6_activities [Total Pts: 100 Score] |2993622",
      week_7 = "Week7_activities [Total Pts: 100 Score] |2715295",
      week_11 = "Week11_activities [Total Pts: 100 Score] |2779117",
      week_13 = "Week13_Activities [Total Pts: 100 Score] |2779113",
      tech_test = "Technology test [Total Pts: 6 Score] |2694609",
      preliminary_survey = "Preliminary survey",
      midterm_survey = "Midterm survey",
      exit_survey = "Exit Survey",
      in_class_participation = "In-class participation",
      teams_participation = "Teams participationon 100"
    ) %>%
    mutate(
      "Weekly Activity Overall" = week_1 + week_2 + week_3 + week_6 + week_7 + week_11 + week_13,
      "Survey/Poll/Tech. Test" = tech_test + preliminary_survey + midterm_survey + exit_survey,
      "In-Class Participation" = in_class_participation,
      "Teams Participation" = teams_participation
    )
  
  return(data)
}

# Define UI
ui <- fluidPage(  
  mainPanel(
    tabsetPanel(
      tabPanel("Overall Student Data", DT::dataTableOutput("overall_table")),
      tabPanel("Weekly Activity Data", DT::dataTableOutput("weekly_table")),
      tabPanel("In-Class Participation Data", DT::dataTableOutput("in_class_table")),
      tabPanel("Teams Participation Data", DT::dataTableOutput("teams_table")),
      tabPanel("Professionalism Data", DT::dataTableOutput("prof_table")),
      tabPanel("Survey/Poll/Tech. Test Data", DT::dataTableOutput("survey_table"))
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive function to read and process data
  data <- reactive({
    read_and_process_data()
  })
  
  # Function to generate table based on column name
  generate_table <- function(data, column_name) {
    data %>%
      arrange(desc(.data[[column_name]])) %>%
      mutate(Rank = row_number(), Points = round(.data[[column_name]], 2)) %>%
      select(Rank, Name = 1, Points) %>%
      filter(!is.na(Name))
  }
  
  # Render tables dynamically
  output$overall_table <- DT::renderDataTable({
    generate_table(data(), "Total Points")
  })
  
  output$weekly_table <- DT::renderDataTable({
    generate_table(data(), "Weekly Activity Overall")
  })
  
  output$in_class_table <- DT::renderDataTable({
    generate_table(data(), "In-Class Participation")
  })
  
  output$teams_table <- DT::renderDataTable({
    generate_table(data(), "Teams Participation")
  })
  
  output$prof_table <- DT::renderDataTable({
    generate_table(data(), "Professionalism")
  })
  
  output$survey_table <- DT::renderDataTable({
    generate_table(data(), "Survey/Poll/Tech. Test")
  })
}

# Run the application
shinyApp(ui = ui, server = server)