
library(shiny)
library(bs4Dash) # this is for specific themeing, but is interchangeable with shinydashboard
library(tidyverse)
library(plotly)
library(bslib)
library(fresh)

ipse_green <- "#f8f8f8"
ipse_yellow <- "#44009B"
ipse_white <- "#080808"

#' The ui.R file is used to structure the arrangement of the input and output
#' elements of the Shiny app. Generally my workflow is to build the global
#' functions first (and check they don't break when I run them in console).
#' I then like to build up the UI where I will name all of my input and output
#' fields. Finally, I can populate those fields using the server.R file.

mytheme <- create_theme(theme = "flatly", 
                        bs4dash_status(primary = ipse_yellow, secondary = ipse_white),
                        fresh::bs4dash_sidebar_light(active_color = ipse_yellow, bg = ipse_green, header_color = ipse_green, color = ipse_white, submenu_active_color = ipse_green, hover_color = ipse_green, submenu_color = ipse_white, submenu_hover_color = ipse_white),
                        fresh::bs4dash_sidebar_dark(active_color = ipse_green, bg = ipse_green, header_color = ipse_green, color = ipse_green, hover_color = ipse_white, submenu_color = ipse_white, submenu_hover_color = ipse_white),
                        fresh::bs4dash_color(olive = ipse_green, gray_900 = ipse_white, black = ipse_white, gray_800 = ipse_white, gray_x_light = ipse_white, navy = ipse_white), 
                        fresh::bs4dash_font(family_base = "Source Serif Pro", family_sans_serif = "Source Sans Pro", 
                                            weight_bold = 100, weight_normal = 200),
                        bs4dash_layout(main_bg = ipse_green),
                        #bs4dash_yiq(contrasted_threshold = 10, text_dark = ipse_white, text_light = ipse_white),
                        bs4dash_vars(
                          navbar_light_color = ipse_green,
                          navbar_light_active_color = ipse_green,
                          navbar_light_hover_color = ipse_green, 
                          navbar_dark_color = ipse_green,
                          navbar_dark_active_color = ipse_green,
                          navbar_dark_hover_color = ipse_green,
                          nav_tabs_link_active_color = ipse_green,
                          main_footer_bg = ipse_green,
                          main_footer_height = "100px",
                          link_color = ipse_green, 
                          family_sans_serif = "Source Sans Pro"
                        )
)


bs4DashPage(
  
  #' it's not necessary to do this but I am also running a custom bootstrap
  #' theme (the default shinydashboard one is just fine, but any default seen
  #' enough times starts to look awful) using the fresh package
  freshTheme = TRUE, help = NULL, 
  # turn off dark mode option
  dark = NULL,
  # The header contains the title
  title = "Cocoon", 
  header = dashboardHeader(title = "Cocoon", tags$style("
          @import url('https://fonts.googleapis.com/css2?family=Source+Sans+Pro:wght@100;200;400;500;800');
          @import url('https://fonts.googleapis.com/css2?family=Source+Serif+Pro:wght@100;200;400;500;800');
          .navbar-gray-dark { background-color: #44009B; } 
          .navbar-white { background-color: #44009B;} 
          .main-footer {
            color: #44009B;
          }
          h1 h2 h3 h4 {
            font-family: 'Source Serif Pro';
          }
          .sans-serif {
            font-family: 'Source Sans Pro';
          }
          .main-footer a {
            color: #44009B;
          }
          .nav-link.active {
                background-color: #44009B;
                color: #f8f8f8;
          }
          
          .js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #44009B}
          .js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #44009B}
          
          "
  )
  ),
  sidebar = bs4DashSidebar(
    skin = "light",
    # the sidebar contains different dashboard pages - I like to split 
    # at most 2-3 visualisations between each page as this makes the app feel
    # more responsive. Here we just have one, but I might make multiple, for 
    # example, one sidebar page per random effect caterpillar plot - or one
    # page for residual residual caterpillar REs and effects, one page for
    # slope-slope, and one for intercept-intercept
    bs4SidebarMenu(
      bs4SidebarMenuItem("Caterpillar", tabName = "caterpillar", icon = icon("grip-lines"), selected = TRUE)
      #bs4SidebarMenuItem("Maps", tabName = "maps", icon = icon("map-pin"), selected = FALSE),
      # bs4SidebarMenuItem("Age Groups", tabName = "byage", icon = icon("seedling"), selected = FALSE),
      # bs4SidebarMenuItem("Gender", tabName = "bygender", icon = icon("children"), selected = FALSE),
      # bs4SidebarMenuItem("Ethnicity", tabName = "byethnicity", icon = icon("shapes"), selected = FALSE)#,
      #bs4SidebarMenuItem("Spending", tabName = "spending", icon = icon("money-bill"), selected = FALSE)
    )
  ),
  body = bs4DashBody(
    use_googlefont("Source Sans Pro"),
    use_theme(mytheme),
    bs4TabItems(
      bs4TabItem(tabName = "caterpillar",
                 # Dashboard bodies are usually split into rows and boxes, split into
                 # widths of 12 equal parts (so a width of 12 would be the entire space)
                 fluidRow(
                   column(width = 2), # I like to centre things using some empty columns
                   column(width = 8,
                          # Title of the page
                          HTML('<h2><br><p align="center">Interactive Caterpillar Plot</p></h2><br>'),
                          HTML('<div class="sans-serif">Current bugs: If strata contain identical string patterns at the start, this will sometimes cause both strata to be selected. E.g. Income: Low and Income: Low-Mid will both be selected if Income: Low is chosen, because the string "Income: Low" is detected in the string "Income: Low-Mid". This would be solved by adding a unique strata value number, e.g. Income 1: Low, Income 2: Low-Mid, but this is currently not trivial to add to the data tidying process.<br><br></div>')
                   ),
                   column(width = 2)
                 ),
                 
                 fluidRow(
                   column(width = 2),
                   column(width = 4,
                          box(
                            width = 12,
                            background = "olive",
                            fileInput("uploaded_multiple_effects", 
                                      label = "Upload Random Intercepts on the Multiplicative Scale:", 
                                      accept = c(".csv", ".tsv")),
                            uiOutput("est_select_multi"),
                            uiOutput("se_select_multi"),
                            uiOutput("lb_select_multi"),
                            uiOutput("ub_select_multi"),
                            uiOutput("id_select_multi")
                          )
                          ),
                   column(width = 4,
                          box(
                            width = 12,
                            background = "olive",
                            fileInput("uploaded_full_effects", 
                                      label = "Upload Random Intercepts on the Full Model Prediction Scale:", 
                                      accept = c(".csv", ".tsv")),
                            uiOutput("est_select_full"),
                            uiOutput("se_select_full"),
                            uiOutput("lb_select_full"),
                            uiOutput("ub_select_full"),
                            uiOutput("id_select_full")
                            )
                          ),
                   column(width = 2)
                 ),
                 
                 
                 fluidRow(
                   column(width = 12,
                          box(
                            uiOutput("strata_select"), 
                            background = "olive", width = 12
                          )
                   )
                 ),

                 fluidRow(
                   column(width = 12,
                          box(id = "caterpillar_plot", title = "Interactive Plot", 
                              HTML('<div class="sans-serif">Click once on any stratum trace to highlight it across both full predictions and multiplicative random intercepts. Double-click anywhere else on the plot to deselect the trace.</div>'),
                              plotlyOutput("caterpillar_plot_out", height = "600px"),
                              width = 12, height = "650px", maximizable = TRUE,
                              background = "olive"
                          )
                   )
                 )
                 
      )
      
      
    ),
    
    
  ),
  footer = dashboardFooter(left = a(href = "https://calumwebb.co.uk",
                                    target = "_blank", "calumwebb.co.uk"), right = "Created by Dr. Calum Webb, University of Sheffield", fixed = TRUE))

