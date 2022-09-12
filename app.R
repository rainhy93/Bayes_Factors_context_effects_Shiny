library(shiny)
library(tidyverse)
library(ggplot2)
library(plotly)
library(readr)

source("get_bf_individual.R")


bfs_all_sub_00_to_33 <- read_csv("./bfs_all_sub_00_to_33.csv", 
                                 col_types = cols(...1 = col_skip()))

subjects = bfs_all_sub_00_to_33 %>% select(Subject) %>% distinct(.) %>% pull(-1)



ui = fluidPage(
  titlePanel("Bayes Factors for Your Context Effects Specification"),
  
  sidebarLayout(
    
    sidebarPanel(
      p("Select a value for the width of indifference between A and B:"),
      selectInput("tau", "Select tau",
                  choices = c(seq(0,0.33,by=0.05),0.33)),
      
      p("Choose location bins of an alternative that if added to the the choices between A and B, will change a decision maker's preference:"),
      splitLayout(
        checkboxGroupInput("bins_pref_a", "Increase preference for A", 
                         choiceNames = paste0("Bin ", 1:34),
                         choiceValues = 1:34),
        
        checkboxGroupInput("bins_pref_b", "Increase preference for B", 
                           choiceNames = paste0("Bin ", 1:34),
                           choiceValues = 1:34)
      )
      ),
    mainPanel(h3("Attribute Map", align = "left"),
              img(src = "attribute map.png", height = 550, width = 700,  
                  style = "display: block; margin-left: auto; margin-right: auto;"),
              tags$br(),
              actionButton(inputId = "go", label = "Update"),
              plotlyOutput("bf_dist"),
              tags$hr(),
              textOutput("group_bf"))
    
  )
)

server = function(input, output){
  
    
    bins_a = eventReactive(input$go, {
      input$bins_pref_a
    })
    bins_b = eventReactive(input$go, {
      input$bins_pref_b
    })
    bins_indiff = eventReactive(input$go, {
      setdiff(1:34, c(bins_a(), bins_b()))
    })
    tau = eventReactive(input$go, {
      input$tau
    })
    
    
    results = eventReactive(input$go, {
      
      req(subjects, bfs_all_sub_00_to_33)
      
      bfs = sapply(subjects, get_bf_individual,  
                   df = bfs_all_sub_00_to_33, 
                   bins_indiff = bins_indiff(), 
                   bins_pref_a = bins_a(), 
                   bins_pref_b = bins_b(),
                   tau_value = tau())
      
      bfs = as.data.frame(bfs)
      colnames(bfs) = 'bf'
      bfs = bfs %>%
        mutate(interpretation = case_when(bf < 1/100 ~ "decisive evidence for null",
                                          bf >= 1/100 & bf < 1/30 ~ "very strong evidence for null",
                                          bf >= 1/30 & bf < 1/10 ~ "strong evidence for null",
                                          bf >= 1/10 & bf < 1/3 ~ "substantial evidence for null",
                                          bf >= 1/3 & bf < 1 ~ "anecdotal evidence for null",
                                          bf >= 1 & bf < 3 ~ "anecdotal evidence for constrained",
                                          bf >= 3 & bf < 10 ~ "substantial evidence for constrained",
                                          bf >= 10 & bf < 30 ~ "strong evidence for constrained",
                                          bf >= 30 & bf < 100 ~ "very strong evidence for constrained",
                                          bf >= 100 ~ "decisive evidence for constrained"))
      
      bfs
    })
    
    
    output$bf_dist = renderPlotly({
      
    p = results() %>% count(interpretation) %>%
        ggplot(aes(x = reorder(interpretation, n), y = n)) + 
        geom_col(fill = "navy") + 
        coord_flip() +
        labs(x = "Bayes Factor Interpretation", y = "Count")
    
      ggplotly(p, tooltip = "y")
      
      })
    
    
    output$group_bf = renderText(

      paste("The Group Bayes Factor across all subjects is", exp(sum(log(results()$bf))))

    )
  
}


shinyApp(ui = ui, server = server)
