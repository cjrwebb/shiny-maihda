library(shiny)
library(bs4Dash)
library(tidyverse)
library(plotly)
library(shinyjs)


shinyServer(function(input, output, session){
  
  useAutoColor()
  
  multi_eff_data <- reactive({
    req(input$uploaded_multiple_effects)
    
    ext <- tools::file_ext(input$uploaded_multiple_effects$name)
    switch(ext,
           csv = vroom::vroom(input$uploaded_multiple_effects$datapath, delim = ","),
           tsv = vroom::vroom(input$uploaded_multiple_effects$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  full_eff_data <- reactive({
    req(input$uploaded_full_effects)
    
    ext <- tools::file_ext(input$uploaded_full_effects$name)
    switch(ext,
           csv = vroom::vroom(input$uploaded_full_effects$datapath, delim = ","),
           tsv = vroom::vroom(input$uploaded_full_effects$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  output$est_select_multi <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("est_selectize_multi", 
                   "Estimate Column:",
                   choices = c("Please select...", names(multi_eff_data())),
                   selected = "Please select...",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })
  
  output$se_select_multi <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("se_selectize_multi", 
                   "S.E. Column:",
                   choices = c("Please select...", names(multi_eff_data()), "N/A"),
                   selected = "Please select...",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })
  
  output$lb_select_multi <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("lb_selectize_multi", 
                   "CI Lower Bound Column:",
                   choices = c("Please select...", names(multi_eff_data()), "N/A"),
                   selected = "Please select...",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })
  
  output$ub_select_multi <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("ub_selectize_multi", 
                   "CI Upper Bound Column:",
                   choices = c("Please select...", names(multi_eff_data()), "N/A"),
                   selected = "Please select...",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })
  
  output$id_select_multi <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("id_selectize_multi", 
                   "Strata Columns:",
                   choices = names(multi_eff_data()),
                   selected = NULL,
                   multiple = TRUE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })
  
  
  output$est_select_full <- renderUI({ 
    
    req(input$uploaded_full_effects)
    
    selectizeInput("est_selectize_full", 
                   "Estimate Column:",
                   choices = c("Please select...", names(full_eff_data())),
                   selected = "Please select...",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(full = TRUE, maxOptions = 1000))
  })
  
  output$se_select_full <- renderUI({ 
    
    req(input$uploaded_full_effects)
    
    selectizeInput("se_selectize_full", 
                   "S.E. Column:",
                   choices = c("Please select...", names(full_eff_data()), "N/A"),
                   selected = "Please select...",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(full = TRUE, maxOptions = 1000))
  })
  
  output$lb_select_full <- renderUI({ 
    
    req(input$uploaded_full_effects)
    
    selectizeInput("lb_selectize_full", 
                   "CI Lower Bound Column:",
                   choices = c("Please select...", names(full_eff_data()), "N/A"),
                   selected = "Please select...",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(full = TRUE, maxOptions = 1000))
  })
  
  output$ub_select_full <- renderUI({ 
    
    req(input$uploaded_full_effects)
    
    selectizeInput("ub_selectize_full", 
                   "CI Upper Bound Column:",
                   choices = c("Please select...", names(full_eff_data()), "N/A"),
                   selected = "Please select...",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(full = TRUE, maxOptions = 1000))
  })
  
  output$id_select_full <- renderUI({ 
    
    req(input$uploaded_full_effects)
    
    selectizeInput("id_selectize_full", 
                   "Strata Columns:",
                   choices = names(full_eff_data()),
                   selected = NULL,
                   multiple = TRUE,  
                   width = "100%",
                   options = list(full = TRUE, maxOptions = 1000))
  })
  
  
  output$strata_select <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("strata_selectize", 
                   "Select strata characteristics:",
                   choices = c("", get_strata_labels(multi_eff_data())),
                   selected = "",
                   multiple = TRUE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })
  
  output$caterpillar_plot_out <- renderPlotly({
    
    req(input$uploaded_multiple_effects)
    req(input$uploaded_full_effects)
    # validate(need(!is.null(input$uploaded_multiple_effects) & !is.null(input$uploaded_full_effects), 
    #               message = "Please upload both multiplicative random intercepts and full predictions"))
    
    print(input$strata_selectize)
    
    if (is.null(input$strata_selectize)) {
      create_interactive_plot(multi_eff_data(), full_eff_data())
    } else {
      create_selected_plot(multi_eff_data(), full_eff_data(), input$strata_selectize)
    }
  })
  
  
})