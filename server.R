library(shiny)
library(bs4Dash)
library(tidyverse)
library(plotly)
library(shinyjs)


shinyServer(function(input, output, session){
  
  useAutoColor()
  
  multi_eff_data <- reactive({
    if (is.null(input$uploaded_multiple_effects))
      return(NULL)
    
    ext <- tools::file_ext(input$uploaded_multiple_effects$name)
    switch(ext,
           csv = vroom::vroom(input$uploaded_multiple_effects$datapath, delim = ","),
           tsv = vroom::vroom(input$uploaded_multiple_effects$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  full_eff_data <- reactive({
    if (is.null(input$uploaded_full_effects))
      return(NULL)
    
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
                   choices = c("", names(multi_eff_data())),
                   selected = "",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })

  output$helper_multi <- renderUI({
    req(input$uploaded_multiple_effects)
    HTML('<div class="sans-serif">Use the drop-down boxes below to identify which columns in your uploaded file correspond to each parameter<br><br></div>')
  })
  
  output$helper_full <- renderUI({
    req(input$uploaded_multiple_effects)
    HTML('<div class="sans-serif">Use the drop-down boxes below to identify which columns in your uploaded file correspond to each parameter<br><br></div>')
  })
    
  output$se_blurb_multi <- renderUI({
    req(input$uploaded_multiple_effects)
    HTML('<div class="sans-serif">You can choose whether to specify pre-calculated confidence intervals or standard errors (you do not need to specify both)<br><br></div>')
  })
  
  output$id_select_note_multi <- renderUI({
    req(input$uploaded_multiple_effects)
    HTML('<div class="sans-serif">Select all variables that were used to define the strata<br><br></div>')
  })
  
  output$se_blurb_full <- renderUI({
    req(input$uploaded_full_effects)
    HTML('<div class="sans-serif">You can choose whether to specify pre-calculated confidence intervals or standard errors (you do not need to specify both)<br><br></div>')
  })
  
  output$id_select_note_full <- renderUI({
    req(input$uploaded_multiple_effects)
    HTML('<div class="sans-serif">Select all variables that were used to define the strata<br><br></div>')
  })
  
  output$se_select_multi <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("se_selectize_multi", 
                   "S.E. Column:",
                   choices = c("", names(multi_eff_data()), "N/A"),
                   selected = "",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })
  
  output$lb_select_multi <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("lb_selectize_multi", 
                   "CI Lower Bound Column:",
                   choices = c("", names(multi_eff_data()), "N/A"),
                   selected = "",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(multiple = TRUE, maxOptions = 1000))
  })
  
  output$ub_select_multi <- renderUI({ 
    
    req(input$uploaded_multiple_effects)
    
    selectizeInput("ub_selectize_multi", 
                   "CI Upper Bound Column:",
                   choices = c("", names(multi_eff_data()), "N/A"),
                   selected = "",
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
                   choices = c("", names(full_eff_data())),
                   selected = "",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(full = TRUE, maxOptions = 1000))
  })
  
  output$se_select_full <- renderUI({ 
    
    req(input$uploaded_full_effects)
    
    selectizeInput("se_selectize_full", 
                   "S.E. Column:",
                   choices = c("", names(full_eff_data()), "N/A"),
                   selected = "",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(full = TRUE, maxOptions = 1000))
  })
  
  output$lb_select_full <- renderUI({ 
    
    req(input$uploaded_full_effects)
    
    selectizeInput("lb_selectize_full", 
                   "CI Lower Bound Column:",
                   choices = c("", names(full_eff_data()), "N/A"),
                   selected = "",
                   multiple = FALSE,  
                   width = "100%",
                   options = list(full = TRUE, maxOptions = 1000))
  })
  
  output$ub_select_full <- renderUI({ 
    
    req(input$uploaded_full_effects)
    
    selectizeInput("ub_selectize_full", 
                   "CI Upper Bound Column:",
                   choices = c("", names(full_eff_data()), "N/A"),
                   selected = "",
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

  
  id_selectize_mult_db <- debounce(
    reactive(input$id_selectize_multi),
    millis = 500
  )
    
  id_selectize_full_db <- debounce(
    reactive(input$id_selectize_full),
    millis = 500
  )

  
  
  strata_labels <- reactive({
    
    x <- get_strata_labels(
      id_select_multi = input$id_selectize_multi,
      id_select_full = input$id_selectize_full,
      multi_effs = multi_eff_data(),
      full_effs = full_eff_data()
    )
    
    x
    
  })
  
  output$strata_select <- renderUI({

    #req(input$uploaded_multiple_effects || input$uploaded_full_effects)

    selectizeInput(
      "strata_selectize",
      "Use the box below to select characteristics of the strata you want to highlight:",
      choices = strata_labels(),
      selected = NULL,
      multiple = TRUE
    )
    
  })
  
  strata_selectize_db <- debounce(
    reactive(input$strata_selectize),
    millis = 500
  )
  
  output$plot_ui <- renderUI({
    
    # req(!is.null(input$uploaded_multiple_effects) ||
    #       !is.null(input$uploaded_full_effects))
    
    tryCatch(error_handler( mult_effs = multi_eff_data(),
                            full_effs = full_eff_data(),
                            est_m = input$est_selectize_multi,
                            est_f = input$est_selectize_full,
                            se_m = input$se_selectize_multi,
                            se_f = input$se_selectize_full,
                            lb_m = input$lb_selectize_multi,
                            ub_m = input$ub_selectize_multi,
                            lb_f = input$lb_selectize_full,
                            ub_f = input$ub_selectize_full,
                            id_m = id_selectize_mult_db(),
                            id_f = id_selectize_full_db(),
                            emphasise = strata_selectize_db()),
             error = function(e) {
               validate(need(FALSE, e$message))
             })
    
    updateBox(
      id = "caterpillar_plot",
      action = "update",
      options = list(height = "700px")
    )
    
    plotlyOutput("caterpillar_plot_out", height = "650px")
  })
  
  
  output$caterpillar_plot_out <- renderPlotly({
    
    validate(need(!is.null(input$uploaded_multiple_effects) | !is.null(input$uploaded_full_effects), "Please provide either a multiplicative effects file or a full effects file (or both)"))

    updateBox(id = "caterpillar_plot", action = "update", options = list(height = "700px"))
  
    tryCatch(
      create_interactive_plot(mult_effs = multi_eff_data(), 
                              full_effs = full_eff_data(),
                              est_m = input$est_selectize_multi, 
                              est_f = input$est_selectize_full,
                              se_m = input$se_selectize_multi, 
                              se_f = input$se_selectize_full,
                              lb_m = input$lb_selectize_multi,
                              ub_m = input$ub_selectize_multi,
                              lb_f = input$lb_selectize_full,
                              ub_f = input$ub_selectize_full,
                              id_m = id_selectize_mult_db(), 
                              id_f = id_selectize_full_db(), 
                              emphasise = strata_selectize_db()),
      error = function(e) {
        validate(need(FALSE, e$message))
      })
  
    
  })
  
})
  
  