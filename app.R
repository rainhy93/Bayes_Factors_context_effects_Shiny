library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(readr)

source("get_bf_individual.R")

bfs_all_sub_00_to_90 <- read_csv("results_00_to_90.csv")

subjects = bfs_all_sub_00_to_90 %>% select(Subject) %>% distinct(.) %>% pull(-1)



ui = fluidPage(
  titlePanel("Bayes Factors for Your Context Effects Specification"),
  
  sidebarLayout(
    sidebarPanel(
      # let the user choose two tau values: one for preference, the other for indifference
      
      selectInput(
        "error_pref",
        "Select the value of maximum deviation from a deterministic choice:",
        choices = seq(0.05, 0.5, by = 0.025)
      ),
      uiOutput("error_pref_selection"),
      
      
      selectInput(
        "error_indiff",
        "Select the value of maximum deviation from coin flippng:",
        choices = seq(0, 0.45, by = 0.025)
      ),
      uiOutput("error_indiff_selection"),
      
      
      p("Select the predicted patterns for bins based on your hypothesis:"),
      splitLayout(
        checkboxGroupInput(
          "bins_pref_a",
          "A",
          choiceNames = paste0("Bin ", 1:34),
          choiceValues = 1:34
        ),
        checkboxGroupInput(
          "bins_indiff",
          "Indifferent",
          choiceNames = paste0("Bin ", 1:34),
          choiceValues = 1:34
        ),
        checkboxGroupInput(
          "bins_pref_b",
          "B",
          choiceNames = paste0("Bin ", 1:34),
          choiceValues = 1:34
        )
      )
    ),
    mainPanel(
      h3("Attribute Map", align = "left"),
      img(
        src = "attribute map.png",
        height = 550,
        width = 700,
        style = "display: block; margin-left: auto; margin-right: auto;"
      ),
      tags$br(),
      actionButton(inputId = "go", label = "Run Analysis"),
      plotlyOutput("bf_dist"),
      tags$hr(),
      textOutput("max_bf"),
      textOutput("group_bf"),
      # export bfs
      downloadButton(outputId = "exportBFs", label = "Export Bayes Factors")
    )
    
  )
)

server = function(input, output) {
  bins_a = eventReactive(input$go, {
    input$bins_pref_a
  })
  bins_b = eventReactive(input$go, {
    input$bins_pref_b
  })
  bins_indiff = eventReactive(input$go, {
    input$bins_indiff
  })
  
  
  observe({
    input$error_pref
    output$error_pref_selection = renderUI({
      x = as.numeric(input$error_pref)
      withMathJax(
        sprintf(
          "The inequality constraints for the deviation from a deterministic choice are
                           $$\\quad %.03f \\leq P \\leq 1$$",
          1 - x
        )
      )
    })
  })
  
  
  observe({
    input$error_indiff
    output$error_indiff_selection = renderUI({
      y = as.numeric(input$error_indiff)
      withMathJax(
        sprintf(
          "The inequality constraints for the deviation from coin flipping are
                          $$%.03f \\leq P \\leq %.03f$$",
          0.5 - y,
          0.5 + y
        )
      )
    })
  })
  
  # error_val_pref = eventReactive(input$go, {
  #   as.numeric(input$error_pref)
  # })
  # 
  # error_val_indiff = eventReactive(input$go, {
  #   as.numeric(input$error_indiff)
  # })
  
  
  
  results = eventReactive(input$go, {
    req(subjects, bfs_all_sub_00_to_90)
    
    bfs = sapply(
      subjects,
      get_bf_individual,
      df = bfs_all_sub_00_to_90,
      bins_indiff = bins_indiff(),
      bins_pref_a = bins_a(),
      bins_pref_b = bins_b(),
      tau_value_indiff = 2*as.numeric(input$error_indiff),
      tau_value_pref = 1-2*as.numeric(input$error_pref)
    )
    
    bfs = as.data.frame(bfs)
    colnames(bfs) = 'bf'
    bfs = bfs %>%
      mutate(
        interpretation = case_when(
          bf < 1 / 100 ~ "decisive evidence for unconstrained (-inf, 1/100)",
          bf >= 1 / 100 &
            bf < 1 / 30 ~ "very strong evidence for unconstrained [1/100, 1/30)",
          bf >= 1 / 30 &
            bf < 1 / 10 ~ "strong evidence for unconstrained [1/30, 1/10)",
          bf >= 1 / 10 &
            bf < 1 / 3 ~ "substantial evidence for unconstrained [1/10, 1/3)",
          bf >= 1 / 3 &
            bf < 1 ~ "anecdotal evidence for unconstrained [1/3, 1)",
          bf >= 1 &
            bf < 3 ~ "anecdotal evidence for constrained [1, 3)",
          bf >= 3 &
            bf < 10 ~ "substantial evidence for constrained [3, 10)",
          bf >= 10 &
            bf < 30 ~ "strong evidence for constrained [10, 30)",
          bf >= 30 &
            bf < 100 ~ "very strong evidence for constrained [30, 100)",
          bf >= 100 ~ "decisive evidence for constrained [100, inf)"
        )
      )
    
    bfs$interpretation = factor(
      bfs$interpretation,
      levels = c(
        "decisive evidence for unconstrained (-inf, 1/100)",
        "very strong evidence for unconstrained [1/100, 1/30)",
        "strong evidence for unconstrained [1/30, 1/10)",
        "substantial evidence for unconstrained [1/10, 1/3)",
        "anecdotal evidence for unconstrained [1/3, 1)",
        "anecdotal evidence for constrained [1, 3)",
        "substantial evidence for constrained [3, 10)",
        "strong evidence for constrained [10, 30)",
        "very strong evidence for constrained [30, 100)",
        "decisive evidence for constrained [100, inf)"
      )
    )
    
    bfs
  })
  
  output$bf_dist = renderPlotly({
    p = results() %>% count(interpretation) %>%
      ggplot(aes(x = interpretation, y = n)) +
      geom_col(fill = "navy") +
      coord_flip() +
      labs(x = "Bayes Factor & Interpretation", y = "Count")
    
    ggplotly(p, tooltip = "y")
    
  })
  
  #output$max_bf = renderText(paste("The maximum possible Bayes Factor for your specified constraints
  #                                for one subject is", ))
  
  
  output$group_bf = renderText(paste("The Group Bayes Factor across all subjects is", exp(sum(log(
    results()$bf
  )))))
  
  output$exportBFs = downloadHandler(
    filename = function() {
      paste('bayes_factors', Sys.Date(), '.csv', sep = '')
    },
    content = function(con) {
      write.csv(results()$bf, con)
    }
  )
  
}


shinyApp(ui = ui, server = server)
