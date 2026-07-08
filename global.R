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

# 1. Create plot for multiplicative, full, or both
# 2. Required: Means, SE or LB UB — creates plot
# 3. Require strata labels for pop-up text
# 4. Require strata labels for highlighting

# For testing:
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

# Create the interactive plot
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
    ) 
  
  #print(hlines)
  
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

# For testing:

# create_interactive_plot(mult_effs = mult_effs, full_effs = full_effs,
#                         est_m = est_m, est_f = est_f,
#                         se_m = se_m, se_f = se_f,
#                         lb_m = lb_m, ub_m = ub_m, lb_f = lb_f, ub_f = ub_f,
#                         id_m = id_m, id_f = id_f, emphasise = int_selections
#                         )

# est_m = ""
# est_f = ""
# se_m = ""
# se_f = ""
# lb_m = ""
# lb_f = ""
# ub_m = ""
# ub_f = ""
# id_m = NULL
# id_f = NULL
# int_selections <- NULL

# Function for tidying up and getting the strata labels, so they show variable name 
# then the variable value in title case

get_strata_labels <- function(id_select_multi = NULL, id_select_full = NULL, multi_effs = NULL, full_effs = NULL) {

  # print(paste("MULTI ID COLS", id_select_multi))
  # print(paste("FULL ID COLS", id_select_full))
  # 
  # print(paste("MULTI NAMES", names(multi_effs)))
  # print(paste("FULL NAMES", names(full_effs)))

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


