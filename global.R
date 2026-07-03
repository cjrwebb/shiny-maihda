library(shiny)
library(tidyverse)
library(lme4)
library(plotly)

#' This Shiny app is designed to create an interactive caterpillar plot
#' from exported fixed and random effects across intersectional groups
#' to allow for flexible exploration and visualisation of large numbers
#' of intersectional strata.
#' 
#' The user is required to create two csv files containing the stratum
#' multiplicative random effects and the full predictions (based on 
#' simulations)

#full_preds <- read_csv("prediction-csvs/fullprediction.csv")
#mult_preds <- read_csv("prediction-csvs/multiplicative.csv")

#' Function for creating a caterpillar plot using the multiplicative
#' predictions that can be coloured based on stratum characteristics

# Get strata names for initialising selections


tidy_mult_data <- function(mult_effs) {
  
  #' This function tidies the multiplicative differences across
  #' strata following the output csv that the tutorial provides
  #' while keeping the full range of variables that are included.
  #' The function is flexible to handle more or fewer variables
  #' that make up the strata
  
  # Use the variable names for the stratum ID and the mean values
  # for the random effects as "bookends" to extract all of the 
  # stata included in a model.
  varnames <- names(mult_effs)
  strata_limits <- match(c("stratum", "mean"), varnames)
  strata_limits[1] <- strata_limits[1]+1
  strata_limits[2] <- strata_limits[2]-1
  strata_variables <- names(mult_effs)[strata_limits[1]:strata_limits[2]]
  
  # Create strata information to be showed on popup.
  n_vars <- length(strata_variables)
  
  #' Create the mouseover popup information
  mult_effs <- mult_effs %>%
    mutate(
      mean_lwr = mean - 1.96*sd,
      mean_upr = mean + 1.96*sd
    ) %>%
    rowwise() %>%
    mutate(
      popup_text = paste0(
        "<b>Random effect:</b> ", round(mean, 3),  "<br>", 
        "<b>95% C.I.:</b> (", round(mean - 1.96*sd, 3), " &#8211; ", round(mean + 1.96*sd, 3), ")<br>",
        "<br><b>Strata Information</b>", "<br>",
        paste0(str_to_title(strata_variables), ": ", c_across(all_of((strata_variables))), "<br>", collapse = "")
      )
    ) %>%
    ungroup() %>%
    select(-sd)
  
  return(mult_effs)
  
}

#tidy_mult_data(mult_preds)


tidy_full_data <- function(full_effs) {
  
  #' This function tidies the multiplicative predictions across
  #' strata following the output csv that the tutorial provides
  #' while keeping the full range of variables that are included.
  #' The function is flexible to handle more or fewer variables
  #' that make up the strata
  
  # Set stratum to second position
  full_effs <- full_effs %>%
    relocate(stratum, .after = `...1`) %>%
    rename(mean = m1Bmfit,
           mean_lwr = m1Bmlwr,
           mean_upr = m1Bmupr)
  
  # Use the variable names for the stratum ID and the mean values
  # for the random effects as "bookends" to extract all of the 
  # stata included in a model.
  varnames <- names(full_effs)
  strata_limits <- match(c("stratum", "mean"), varnames)
  strata_limits[1] <- strata_limits[1]+1
  strata_limits[2] <- strata_limits[2]-1
  strata_variables <- names(full_effs)[strata_limits[1]:strata_limits[2]]
  
  # Create strata information to be showed on popup.
  n_vars <- length(strata_variables)
  
  #' Create the mouseover popup information
  full_effs <- full_effs %>%
    rowwise() %>%
    mutate(
      popup_text = paste0(
        "<b>Random effect:</b> ", round(mean, 2),  "<br>", 
        "<b>95% C.I.:</b> (", round(mean_lwr, 2), " &#8211; ", round(mean_upr, 2), ")<br>",
        "<br><b>Strata Information</b>", "<br>",
        paste0(str_to_title(strata_variables), ": ", c_across(all_of((strata_variables))), "<br>", collapse = "")
      )
    ) %>%
    ungroup() 
  
  return(full_effs)
  
}

#tidy_full_data(full_preds) 



create_interactive_plot <- function(mult_effs, full_effs) {
  
  #' 
  #' 
  #' 
  #' 
  
  #' Create hline tibble: the horizontal line for the multiplicative effects
  #' is set to zero, but the horizontal line for the predicted values is set
  #' to the mean of all of the predictions across strata group — which means 
  #' that all strata are weighted equally. This should be somewhat close to
  #' the Intercept/predicted value of a model where all predictors are centred,
  #' or where model predictions are centered on mean values across all 
  #' predictors, but will not be a precise match
  
  hlines_d <- tibble(
     id = factor(c("Stratum Random Effect", "Stratum Predicted Value"),
                 levels = c("Stratum Predicted Value", "Stratum Random Effect")),
     hline = c(0, mean(full_effs$mean, na.rm = TRUE))
  )
  
  
  #' Bind the rows from the multiplicative random effects and the full 
  #' predictions for each stratum, adding an id column for each set of
  #' data and then calculating the rank independently for each set of
  #' predictions using group_by(). 
  
  strata_data <- bind_rows(mult_effs, full_effs, .id = "id") %>%
    mutate(
      id = case_when(id == 1 ~ "Stratum Random Effect",
                     id == 2 ~ "Stratum Predicted Value")
    ) %>%
    mutate(
      id = factor(id, levels = c("Stratum Predicted Value", "Stratum Random Effect"))
    ) %>%
    group_by(id) %>%
    mutate(
      rank = rank(mean)
    ) %>%
    ungroup()
  
  
  #' Create the base ggplot that will be made interactive; here, the plot
  #' is faceted according to whether the value represents the full prediction
  #' or the multiplicative change. The axes are freed for both plots so that
  #' each plot is effectively independent. plotly::highlight_key() sets the 
  #' variable that acts as a way to highlight traces across both plots. 
  
  strata_plot <- strata_data %>%
    plotly::highlight_key(~stratum) %>%
    ggplot(aes(x=rank, y=mean, text = popup_text)) +
    geom_point(size=1) +
    geom_pointrange(aes(ymin=mean_lwr, ymax=mean_upr)) + 
    facet_grid(id ~ ., scales = "free") +
    geom_hline(data = hlines_d, aes(yintercept=hline), color="grey30", linewidth=0.5)+
    xlab("Stratum Rank") +
    ylab("")+
    theme_minimal() +
    theme(
      plot.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8"),
      panel.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8")
    )
  
  
  #' Convert the ggplot object to a plotly object, setting the
  #' tooltip to be the text that is shown on hover. This text is
  #' created in the data tidying functons.
  
  strata_plot <- ggplotly(strata_plot, tooltip = "text")
  
  return(
    
    #' Return the plotly plot and add conditions for the highlighting
    #' functionality — traces become highlighted on a left click and 
    #' are reset when the plot detects a double-click. Highlighting a 
    #' trace causes all other traces to be reduced to 20% opacity (
    #' though this does not affect hovertext.)
    
    strata_plot %>%
      highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.2, dynamic = FALSE)
    
  )
  
}


#create_interactive_plot(tidy_mult_data(mult_preds), tidy_full_data(full_preds))

int_selections <- c("Sex: Male", "Age: 18-29", "Race: White", "Income: Low")

create_selected_plot <- function(mult_effs, full_effs, int_selections) {
  
  # Use the variable names for the stratum ID and the mean values
  # for the random effects as "bookends" to extract all of the 
  # stata included in a model.
  varnames <- names(full_effs)
  strata_limits <- match(c("stratum", "mean"), varnames)
  strata_limits[1] <- strata_limits[1]+1
  strata_limits[2] <- strata_limits[2]-1
  strata_variables <- names(full_effs)[strata_limits[1]:strata_limits[2]]
  
  #' Argument int_selections should be a vector of strings which are
  #' then applied to each filter within an %in% statement

  #' Get unique values for each variable pasted into in a single
  #' vector
  #' Not needed for this function, needed for populating dynamic dropdown
  
  # options <- character(0)
  # for (i in 1:length(strata_variables)) {
  #   add_options <- paste0(str_to_title(strata_variables[i]), ": ", unique(mult_effs[,strata_variables[i]])[[1]])
  #   options <- c(options, add_options)
  # }
  # 
  # print(options)
  
  #' 
  #' 
  #' 
  #' One issue with this solution is that the user will need to be 
  #' prevented from selecting more than one of what should be exclusive
  #' categories, though the easiest implementation is to have all of
  #' the possible stratum selections within a single dropdown, as the 
  #' number of options within the dropdown can be generalised more
  #' easily than making the number of dropdown boxes dynamic based
  #' on the number of intersection variables
  mult_effs <- mult_effs %>%
    rowwise() %>%
    mutate(
      strata_string = paste0(str_to_title(strata_variables), ": ", c_across(all_of((strata_variables))), collapse = "")
    ) %>%
    mutate(
      emphasise = ifelse(sjmisc::str_contains(strata_string, int_selections, logic = "&"), "purple4", "grey70")
    )
  
  full_effs <- full_effs %>%
    rowwise() %>%
    mutate(
      strata_string = paste0(str_to_title(strata_variables), ": ", c_across(all_of((strata_variables))), collapse = "")
    ) %>%
    mutate(
      emphasise = ifelse(sjmisc::str_contains(strata_string, int_selections, logic = "&"), "purple4", "grey70")
    )
  
  
  hlines_d <- tibble(
    id = factor(c("Stratum Random Effect", "Stratum Predicted Value"),
                levels = c("Stratum Predicted Value", "Stratum Random Effect")),
    hline = c(0, mean(full_effs$mean, na.rm = TRUE))
  )
  
  
  #' Bind the rows from the multiplicative random effects and the full 
  #' predictions for each stratum, adding an id column for each set of
  #' data and then calculating the rank independently for each set of
  #' predictions using group_by(). 
  
  strata_data <- bind_rows(mult_effs, full_effs, .id = "id") %>%
    mutate(
      id = case_when(id == 1 ~ "Stratum Random Effect",
                     id == 2 ~ "Stratum Predicted Value")
    ) %>%
    mutate(
      id = factor(id, levels = c("Stratum Predicted Value", "Stratum Random Effect"))
    ) %>%
    group_by(id) %>%
    mutate(
      rank = rank(mean)
    ) %>%
    ungroup()
  
  
  #' Create the base ggplot that will be made interactive; here, the plot
  #' is faceted according to whether the value represents the full prediction
  #' or the multiplicative change. The axes are freed for both plots so that
  #' each plot is effectively independent. plotly::highlight_key() sets the 
  #' variable that acts as a way to highlight traces across both plots. 
  
  strata_plot <- strata_data %>%
    plotly::highlight_key(~stratum) %>%
    ggplot(aes(x=rank, y=mean, text = popup_text, colour = emphasise)) +
    geom_point(size=1) +
    geom_pointrange(aes(ymin=mean_lwr, ymax=mean_upr)) + 
    scale_colour_identity() +
    facet_grid(id ~ ., scales = "free") +
    geom_hline(data = hlines_d, aes(yintercept=hline), color="grey50", linewidth=0.5)+
    xlab("Stratum Rank") +
    ylab("")+
    theme_minimal() +
    theme(legend.position = "none") +
    theme(
      plot.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8"),
      panel.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8")
    )
  
  
  #' Convert the ggplot object to a plotly object, setting the
  #' tooltip to be the text that is shown on hover. This text is
  #' created in the data tidying functons.
  
  strata_plot <- ggplotly(strata_plot, tooltip = "text")
  
  print(strata_data)
  
  return(
    
    #' Return the plotly plot and add conditions for the highlighting
    #' functionality — traces become highlighted on a left click and 
    #' are reset when the plot detects a double-click. Highlighting a 
    #' trace causes all other traces to be reduced to 20% opacity (
    #' though this does not affect hovertext.)
    
    strata_plot %>%
      highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.2, dynamic = FALSE)
    
  )
  
}


#int_selections <- c("Sex: Male", "Age: 18-29", "Race: White", "Income: Low")

#create_selected_plot(tidy_mult_data(mult_preds), tidy_full_data(full_preds), int_selections)


get_strata_labels <- function(multi_effs) {
  
  varnames <- names(multi_effs)
  strata_limits <- match(c("stratum", "mean"), varnames)
  strata_limits[1] <- strata_limits[1]+1
  strata_limits[2] <- strata_limits[2]-1
  strata_variables <- names(multi_effs)[strata_limits[1]:strata_limits[2]]
  
  options <- character(0)
  for (i in 1:length(strata_variables)) {
    #add_options <- paste0(str_to_title(strata_variables[i]), " ", seq(1, length(unique(multi_effs[,strata_variables[i]])[[1]]), 1), ": ", unique(multi_effs[,strata_variables[i]])[[1]])
    add_options <- paste0(str_to_title(strata_variables[i]), ": ", unique(multi_effs[,strata_variables[i]])[[1]])
    options <- c(options, add_options)
  }

  return(options)
  
}


#get_strata_labels(tidy_mult_data(mult_preds))



#' Functions where only significant strata on the multiplicative
#' scale are shown


