library(shiny)
library(tidyverse)
library(lme4)
library(plotly)

options(shiny.sanitize.errors = FALSE)

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



# refactor creating the plots ---------------------------------------------

# 1. Create plot for multiplicative, full, or both
# 2. Required: Means, SE or LB UB — creates plot
# 3. Require strata labels for pop-up text
# 4. Require strata labels for highlighting

# mult_effs <- read_csv("prediction-csvs/multiplicative.csv")
# full_effs <- read_csv("prediction-csvs/fullprediction.csv")
# 
# est_m = "mean"
# est_f = "m1Bmfit"
# se_m = "sd"
# se_f = "N/A"
# lb_m = "N/A"
# lb_f = "m1Bmlwr"
# ub_m = "N/A"
# ub_f = "m1Bmupr"
# id_m = c("sex", "race", "education", "income", "age")
# id_f = c("sex", "race", "education", "income", "age")
# int_selections <- c("Sex: Male", "Age: 18-29", "Race: White", "Income: Low")

error_handler <- function(mult_effs = NULL, full_effs = NULL, 
                          est_m = NULL, est_f = NULL, 
                          se_m = NULL, lb_m = NULL, ub_m = NULL,
                          se_f = NULL, lb_f = NULL, ub_f = NULL,
                          id_m = NULL, id_f = NULL, 
                          emphasise = NULL) {
  
  if (is.null(mult_effs) && is.null(full_effs)) {
    return(stop("Please provide either a multiplicative effects file or a full effects file (or both)"))
  }
  if (!is.null(full_effs) && (is.null(est_f) || est_f == "")) {
    return(stop("Please provide the name of the point estimate column for full effects."))
  }
  if (!is.null(full_effs) && se_f %in% c("", "N/A") && (lb_f %in% c("", "N/A") || ub_f %in% c("", "N/A")) ) {
    return(stop("Please provide either the standard error column name or lower and upper bound column names for the full effects."))
  }
  if (!is.null(full_effs) && is.null(id_f)) {
    return(stop("Please provide the names of the strata ID columns for full effects."))
  }
  if (!is.null(mult_effs) && (is.null(est_m) || est_m == "")) {
    return(stop("Please provide the name of the point estimate column for multiplicative effects."))
  }
  if (!is.null(mult_effs) && se_m %in% c("", "N/A") && (lb_m %in% c("", "N/A") || ub_m %in% c("", "N/A")) ) {
    return(stop("Please provide either the standard error column name or lower and upper bound column names for the multiplicative effects."))
  }
  if (!is.null(mult_effs) && is.null(id_m)) {
    return(stop("Please provide the names of the strata ID columns for multiplicative effects."))
  }
  
}


create_interactive_plot <- function(mult_effs = NULL, full_effs = NULL, 
                                    est_m = NULL, est_f = NULL, 
                                    se_m = NULL, lb_m = NULL, ub_m = NULL,
                                    se_f = NULL, lb_f = NULL, ub_f = NULL,
                                    id_m = NULL, id_f = NULL, 
                                    emphasise = NULL) {

  if (is.null(mult_effs) && is.null(full_effs)) {
    return(stop("Please provide either a multiplicative effects file or a full effects file (or both)"))
  }
  if (!is.null(full_effs) && (is.null(est_f) || est_f == "")) {
    return(stop("Please provide the name of the point estimate column for full effects."))
  }
  if (!is.null(full_effs) && se_f %in% c("", "N/A") && (lb_f %in% c("", "N/A") || ub_f %in% c("", "N/A")) ) {
    return(stop("Please provide either the standard error column name or lower and upper bound column names for the full effects."))
  }
  if (!is.null(full_effs) && is.null(id_f)) {
    return(stop("Please provide the names of the strata ID columns for full effects."))
  }
  if (!is.null(mult_effs) && (is.null(est_m) || est_m == "")) {
    return(stop("Please provide the name of the point estimate column for multiplicative effects."))
  }
  if (!is.null(mult_effs) && se_m %in% c("", "N/A") && (lb_m %in% c("", "N/A") || ub_m %in% c("", "N/A")) ) {
    return(stop("Please provide either the standard error column name or lower and upper bound column names for the multiplicative effects."))
  }
  if (!is.null(mult_effs) && is.null(id_m)) {
    return(stop("Please provide the names of the strata ID columns for multiplicative effects."))
  }
  
  # Calculate the 95% lower and upper band for full effects if the 
  # user does not provide columns, using standard error column
  if (!is.null(full_effs) && (lb_f %in% c("", "N/A") || ub_f %in% c("", "N/A"))) {
    full_effs <- full_effs %>%
      mutate(
        est_lb = .data[[est_f]] - 1.96*.data[[se_f]],
        est_ub = .data[[est_f]] + 1.96*.data[[se_f]]
      )
    lb_f <- "est_lb"
    ub_f <- "est_ub"
    print(full_effs)
  }
  
  # Calculate the 95% lower and upper band for multiplicative effects if the 
  # user does not provide columns, using standard error column
  if (!is.null(mult_effs) && (lb_m %in% c("", "N/A") || ub_m %in% c("", "N/A"))) {
    mult_effs <- mult_effs %>%
      mutate(
        est_lb = .data[[est_m]] - 1.96*.data[[se_m]],
        est_ub = .data[[est_m]] + 1.96*.data[[se_m]]
      )
    lb_m <- "est_lb"
    ub_m <- "est_ub"
    print(mult_effs)
  }

  
  # add a column with the effect types stored (for later id)
  # Rename est, lb, ub columns so that they are harmonised across data
  if (!is.null(full_effs)) {
    full_effs <- full_effs %>%
      mutate(
        id = "Stratum Predicted Value"
      ) %>%
      rename(
        est = .data[[est_f]],
        lb = .data[[lb_f]],
        ub = .data[[ub_f]]
      )
    print(full_effs)
  }
  
  if (!is.null(mult_effs)) {
    mult_effs <- mult_effs %>%
      mutate(
        id = "Stratum Random Effect"
      ) %>%
      rename(
        est = .data[[est_m]],
        lb = .data[[lb_m]],
        ub = .data[[ub_m]]
      )
    print(mult_effs)
  }

  
  # Create a unified id columns vector (only ID columns specified in both
  # datasets)
  
  if (!is.null(id_m) && !is.null(id_f)) {
    id_cols <- intersect(id_m, id_f)
  } else if (!is.null(id_m) && is.null(id_f)) {
    id_cols <- id_m
  } else if (is.null(id_m) && !is.null(id_f)) {
    id_cols <- id_f
  }
  
  .id_cols <- rlang::syms(id_cols)
  
  d <- bind_rows(mult_effs, full_effs) %>%
    group_by(id) %>%
    mutate(
      rank = rank(est)
    ) %>%
    ungroup() %>%
    mutate(
      stratum = paste(!!!.id_cols)
    )
  
  # Create popup text
  d <- d %>%
    rowwise() %>%
    mutate(
        popup_text = paste0(
          "<b>Random effect:</b> ", round(est, 2),  "<br>", 
          "<b>95% C.I.:</b> (", round(lb, 2), " &#8211; ", round(ub, 2), ")<br>",
          "<br><b>Strata Information</b>", "<br>",
          paste0(str_to_title(id_cols), ": ", c_across(all_of((id_cols))), "<br>", collapse = "")
      )
    ) %>%
    ungroup() 
  
  #' Create the base ggplot that will be made interactive; here, the plot
  #' is faceted according to whether the value represents the full prediction
  #' or the multiplicative change. The axes are freed for both plots so that
  #' each plot is effectively independent. plotly::highlight_key() sets the
  #' variable that acts as a way to highlight traces across both plots.

  hlines <- d %>%
    group_by(id) %>%
    summarise(
      hline = mean(est, na.rm = TRUE)
    ) %>%
    mutate(
      hline = ifelse(id == "Stratum Random Effect", 0, hline)
    )
  print(hlines)
  
  if (is.null(emphasise)) {
    strata_plot <- d %>%
      plotly::highlight_key(~stratum) %>%
      ggplot(aes(x=rank, y=est, text = popup_text)) +
      geom_point(size=1) +
      geom_pointrange(aes(ymin=lb, ymax=ub)) +
      facet_grid(id ~ ., scales = "free") +
      geom_hline(data = hlines, aes(yintercept=hline), color="grey30", linewidth=0.5)+
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
  
  if (!is.null(emphasise)) {
    
    d <- d %>%
      rowwise() %>%
      mutate(
        strata_string = paste0(str_to_title(id_cols), ": ", c_across(all_of((id_cols))), " ", collapse = "")
      ) %>%
      mutate(
        emphasise = ifelse(sjmisc::str_contains(strata_string, emphasise, logic = "&"), "purple4", "grey70")
      ) %>%
      ungroup()
    
    strata_plot <- d %>%
      plotly::highlight_key(~stratum) %>%
      ggplot(aes(x=rank, y=est, text = popup_text, colour = emphasise)) +
      geom_point(size=1) +
      geom_pointrange(aes(ymin=lb, ymax=ub)) +
      scale_colour_identity() +
      facet_grid(id ~ ., scales = "free") +
      geom_hline(data = hlines, aes(yintercept=hline), color="grey30", linewidth=0.5)+
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
  
}

# create_interactive_plot(mult_effs = mult_effs, full_effs = full_effs,
#                         est_m = est_m, est_f = est_f,
#                         se_m = se_m, se_f = se_f,
#                         lb_m = lb_m, ub_m = ub_m, lb_f = lb_f, ub_f = ub_f,
#                         id_m = id_m, id_f = id_f, emphasise = int_selections
#                         )


est_m = ""
est_f = ""
se_m = ""
se_f = ""
lb_m = ""
lb_f = ""
ub_m = ""
ub_f = ""
id_m = NULL
id_f = NULL
int_selections <- NULL

# create_interactive_plot(mult_effs = mult_effs,
#                         full_effs = full_effs,
#                         est_m = if (est_m == "") {NULL} else {est_m},
#                         est_f = if (est_f == "") {NULL} else {est_f},
#                         se_m = if (se_m == "") {NULL} else {se_m},
#                         se_f = if (se_f == "") {NULL} else {se_f},
#                         lb_m = if (lb_m == "") {NULL} else {lb_m},
#                         ub_m = if (ub_m == "") {NULL} else {ub_m},
#                         lb_f = if (lb_f == "") {NULL} else {lb_f},
#                         ub_f = if (ub_f == "") {NULL} else {ub_f},
#                         id_m = id_m,
#                         id_f = id_f,
#                         emphasise = int_selections)



# old ---------------------------------------------------------------------



#' create_interactive_plot <- function(mult_effs, full_effs) {
#'   
#'   #' Create hline tibble: the horizontal line for the multiplicative effects
#'   #' is set to zero, but the horizontal line for the predicted values is set
#'   #' to the mean of all of the predictions across strata group — which means 
#'   #' that all strata are weighted equally. This should be somewhat close to
#'   #' the Intercept/predicted value of a model where all predictors are centred,
#'   #' or where model predictions are centered on mean values across all 
#'   #' predictors, but will not be a precise match
#'   
#'   hlines_d <- tibble(
#'      id = factor(c("Stratum Random Effect", "Stratum Predicted Value"),
#'                  levels = c("Stratum Predicted Value", "Stratum Random Effect")),
#'      hline = c(0, mean(full_effs$mean, na.rm = TRUE))
#'   )
#'   
#'   
#'   #' Bind the rows from the multiplicative random effects and the full 
#'   #' predictions for each stratum, adding an id column for each set of
#'   #' data and then calculating the rank independently for each set of
#'   #' predictions using group_by(). 
#'   
#'   strata_data <- bind_rows(mult_effs, full_effs, .id = "id") %>%
#'     mutate(
#'       id = case_when(id == 1 ~ "Stratum Random Effect",
#'                      id == 2 ~ "Stratum Predicted Value")
#'     ) %>%
#'     mutate(
#'       id = factor(id, levels = c("Stratum Predicted Value", "Stratum Random Effect"))
#'     ) %>%
#'     group_by(id) %>%
#'     mutate(
#'       rank = rank(mean)
#'     ) %>%
#'     ungroup()
#'   
#'   
#'   #' Create the base ggplot that will be made interactive; here, the plot
#'   #' is faceted according to whether the value represents the full prediction
#'   #' or the multiplicative change. The axes are freed for both plots so that
#'   #' each plot is effectively independent. plotly::highlight_key() sets the 
#'   #' variable that acts as a way to highlight traces across both plots. 
#'   
#'   strata_plot <- strata_data %>%
#'     plotly::highlight_key(~stratum) %>%
#'     ggplot(aes(x=rank, y=mean, text = popup_text)) +
#'     geom_point(size=1) +
#'     geom_pointrange(aes(ymin=mean_lwr, ymax=mean_upr)) + 
#'     facet_grid(id ~ ., scales = "free") +
#'     geom_hline(data = hlines_d, aes(yintercept=hline), color="grey30", linewidth=0.5)+
#'     xlab("Stratum Rank") +
#'     ylab("")+
#'     theme_minimal() +
#'     theme(
#'       plot.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8"),
#'       panel.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8")
#'     )
#'   
#'   
#'   #' Convert the ggplot object to a plotly object, setting the
#'   #' tooltip to be the text that is shown on hover. This text is
#'   #' created in the data tidying functons.
#'   
#'   strata_plot <- ggplotly(strata_plot, tooltip = "text")
#'   
#'   return(
#'     
#'     #' Return the plotly plot and add conditions for the highlighting
#'     #' functionality — traces become highlighted on a left click and 
#'     #' are reset when the plot detects a double-click. Highlighting a 
#'     #' trace causes all other traces to be reduced to 20% opacity (
#'     #' though this does not affect hovertext.)
#'     
#'     strata_plot %>%
#'       highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.2, dynamic = FALSE)
#'     
#'   )
#'   
#' }


#create_interactive_plot(tidy_mult_data(mult_preds), tidy_full_data(full_preds))
#' 
#' int_selections <- c("Sex: Male", "Age: 18-29", "Race: White", "Income: Low")
#' 
#' create_selected_plot <- function(mult_effs, full_effs, int_selections) {
#'   
#'   # Use the variable names for the stratum ID and the mean values
#'   # for the random effects as "bookends" to extract all of the 
#'   # stata included in a model.
#'   varnames <- names(full_effs)
#'   strata_limits <- match(c("stratum", "mean"), varnames)
#'   strata_limits[1] <- strata_limits[1]+1
#'   strata_limits[2] <- strata_limits[2]-1
#'   strata_variables <- names(full_effs)[strata_limits[1]:strata_limits[2]]
#'   
#'   #' Argument int_selections should be a vector of strings which are
#'   #' then applied to each filter within an %in% statement
#' 
#'   #' Get unique values for each variable pasted into in a single
#'   #' vector
#'   #' Not needed for this function, needed for populating dynamic dropdown
#'   
#'   # options <- character(0)
#'   # for (i in 1:length(strata_variables)) {
#'   #   add_options <- paste0(str_to_title(strata_variables[i]), ": ", unique(mult_effs[,strata_variables[i]])[[1]])
#'   #   options <- c(options, add_options)
#'   # }
#'   # 
#'   # print(options)
#'   
#'   #' 
#'   #' 
#'   #' 
#'   #' One issue with this solution is that the user will need to be 
#'   #' prevented from selecting more than one of what should be exclusive
#'   #' categories, though the easiest implementation is to have all of
#'   #' the possible stratum selections within a single dropdown, as the 
#'   #' number of options within the dropdown can be generalised more
#'   #' easily than making the number of dropdown boxes dynamic based
#'   #' on the number of intersection variables
#'   mult_effs <- mult_effs %>%
#'     rowwise() %>%
#'     mutate(
#'       strata_string = paste0(str_to_title(strata_variables), ": ", c_across(all_of((strata_variables))), collapse = "")
#'     ) %>%
#'     mutate(
#'       emphasise = ifelse(sjmisc::str_contains(strata_string, int_selections, logic = "&"), "purple4", "grey70")
#'     )
#'   
#'   full_effs <- full_effs %>%
#'     rowwise() %>%
#'     mutate(
#'       strata_string = paste0(str_to_title(strata_variables), ": ", c_across(all_of((strata_variables))), collapse = "")
#'     ) %>%
#'     mutate(
#'       emphasise = ifelse(sjmisc::str_contains(strata_string, int_selections, logic = "&"), "purple4", "grey70")
#'     )
#'   
#'   
#'   hlines_d <- tibble(
#'     id = factor(c("Stratum Random Effect", "Stratum Predicted Value"),
#'                 levels = c("Stratum Predicted Value", "Stratum Random Effect")),
#'     hline = c(0, mean(full_effs$mean, na.rm = TRUE))
#'   )
#'   
#'   
#'   #' Bind the rows from the multiplicative random effects and the full 
#'   #' predictions for each stratum, adding an id column for each set of
#'   #' data and then calculating the rank independently for each set of
#'   #' predictions using group_by(). 
#'   
#'   strata_data <- bind_rows(mult_effs, full_effs, .id = "id") %>%
#'     mutate(
#'       id = case_when(id == 1 ~ "Stratum Random Effect",
#'                      id == 2 ~ "Stratum Predicted Value")
#'     ) %>%
#'     mutate(
#'       id = factor(id, levels = c("Stratum Predicted Value", "Stratum Random Effect"))
#'     ) %>%
#'     group_by(id) %>%
#'     mutate(
#'       rank = rank(mean)
#'     ) %>%
#'     ungroup()
#'   
#'   
#'   #' Create the base ggplot that will be made interactive; here, the plot
#'   #' is faceted according to whether the value represents the full prediction
#'   #' or the multiplicative change. The axes are freed for both plots so that
#'   #' each plot is effectively independent. plotly::highlight_key() sets the 
#'   #' variable that acts as a way to highlight traces across both plots. 
#'   
#'   strata_plot <- strata_data %>%
#'     plotly::highlight_key(~stratum) %>%
#'     ggplot(aes(x=rank, y=mean, text = popup_text, colour = emphasise)) +
#'     geom_point(size=1) +
#'     geom_pointrange(aes(ymin=mean_lwr, ymax=mean_upr)) + 
#'     scale_colour_identity() +
#'     facet_grid(id ~ ., scales = "free") +
#'     geom_hline(data = hlines_d, aes(yintercept=hline), color="grey50", linewidth=0.5)+
#'     xlab("Stratum Rank") +
#'     ylab("")+
#'     theme_minimal() +
#'     theme(legend.position = "none") +
#'     theme(
#'       plot.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8"),
#'       panel.background = element_rect(fill = "#f8f8f8", colour = "#f8f8f8")
#'     )
#'   
#'   
#'   #' Convert the ggplot object to a plotly object, setting the
#'   #' tooltip to be the text that is shown on hover. This text is
#'   #' created in the data tidying functons.
#'   
#'   strata_plot <- ggplotly(strata_plot, tooltip = "text")
#'   
#'   print(strata_data)
#'   
#'   return(
#'     
#'     #' Return the plotly plot and add conditions for the highlighting
#'     #' functionality — traces become highlighted on a left click and 
#'     #' are reset when the plot detects a double-click. Highlighting a 
#'     #' trace causes all other traces to be reduced to 20% opacity (
#'     #' though this does not affect hovertext.)
#'     
#'     strata_plot %>%
#'       highlight(on = "plotly_click", off = "plotly_doubleclick", opacityDim = 0.2, dynamic = FALSE)
#'     
#'   )
#'   
#' }
#' 
#' 
#' #int_selections <- c("Sex: Male", "Age: 18-29", "Race: White", "Income: Low")
#' 
#' #create_selected_plot(tidy_mult_data(mult_preds), tidy_full_data(full_preds), int_selections)
#' 
#' 
get_strata_labels <- function(id_select_multi = NULL, id_select_full = NULL, multi_effs = NULL, full_effs = NULL) {

  print(paste("MULTI ID COLS", id_select_multi))
  print(paste("FULL ID COLS", id_select_full))

  print(paste("MULTI NAMES", names(multi_effs)))
  print(paste("FULL NAMES", names(full_effs)))

  if (length(id_select_multi) == 0 &&
      length(id_select_full) == 0) {
    return("Please select strata variables")
  }

  if (is.null(full_effs) & !is.null(multi_effs)) {

    options <- character(0)
    for (i in seq_along(id_select_multi)) {
      add_options <- paste0(str_to_title(id_select_multi[i]), ": ", unique(multi_effs[,id_select_multi[i]])[[1]])
      options <- c(options, add_options)
    }

  } else if (!is.null(full_effs) & is.null(multi_effs)) {

    options <- character(0)
    for (i in seq_along(id_select_full)) {
      add_options <- paste0(str_to_title(id_select_full[i]), ": ", unique(full_effs[,id_select_full[i]])[[1]])
      options <- c(options, add_options)
    }

  }

  else if (!is.null(full_effs) & !is.null(multi_effs)) {

    strata_vars <- intersect(id_select_full, id_select_multi)

    options <- character(0)
    for (i in seq_along(strata_vars)) {
      add_options <- paste0(str_to_title(strata_vars[i]), ": ", unique(multi_effs[,strata_vars[i]])[[1]]
                            )
      options <- c(options, add_options)
    }

  } else {

    options <- character(0)

  }

  print(paste("OPTIONS", options))
  return(options)

}


#get_strata_labels(tidy_mult_data(mult_preds))



#' Functions where only significant strata on the multiplicative
#' scale are shown

