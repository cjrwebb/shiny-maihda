
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
                          link_color = ipse_yellow, 
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
          
          .shiny-output-error { color: #44009B; }
          .shiny-output-error:before { visibility: hidden; }
          
          .ui-row {
            min-height: 50px;
          }
      
          .ui-row-large {
            min-height: 85px;
          }
      
          .ui-row-small {
            min-height: 35px;
          }
          
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
                          HTML('<h2><br><p align="center">Interactive tool for exploring MAIHDA strata predictions</p></h2><br>'),
                          HTML('<div class="sans-serif">This tool allows you to explore estimates from a MAIHDA model — where level 2 units have been defined based on combinations of other variables. For more on how to use these models, see this <a href="https://www.sciencedirect.com/science/article/pii/S235282732400065X">tutorial paper</a>.<br><br>
                               In order to use the app, you will need two sets of level 2 estimates (as .csv files): one that are full predictions (based on both the fixed part and the level 2 random part of a MAIHDA model), and another that are the multiplicative predictions (based on the random effects estimates from MAIHDA model). The files should have a column for each strata defining variable, and either standard errors or confidence intervals for the estimates\' uncertainty.  To see some example .csv files, which you can also use to test the app, see the <a href="https://github.com/cjrwebb/shiny-maihda/tree/main/prediction-csvs">github repository</a>.<br><br>
                               This tool will then produce interactive graphs displaying those predictions, that allow you to identify particular strata, or particular combinations of strata-defining variables. Please note that no data you upload is stored in any way beyond your session.<br><br>
                               To cite the app, please use the following citation: Webb, C. and Bell, A. (2026). <em>Interactive tool for exploring MAIHDA strata predictions.</em> Available at <a href="https://webb.shinyapps.io/maihda-cocoon/">https://webb.shinyapps.io/maihda-cocoon/</a>. doi: 10.15131/shef.data.32925566. <a href="https://github.com/cjrwebb/shiny-maihda">Source code.</a><br><br></div>')
                   ),
                   column(width = 2)
                 ),
                 
                 fluidRow(
                   column(width = 2),
                   column(width = 4,
                          box(
                            width = 12,
                            background = "olive",
                            div(class="ui-row",
                            HTML("Upload full predictions from MAIHDA model:")
                            ),
                            div(class="ui-row",
                                fileInput("uploaded_full_effects", 
                                      label = "", 
                                      accept = c(".csv", ".tsv"))
                                ),
                            uiOutput("helper_full"),
                            uiOutput("est_select_full"),
                            uiOutput("se_blurb_full"),
                            uiOutput("se_select_full"),
                            uiOutput("lb_select_full"),
                            uiOutput("ub_select_full"),
                            uiOutput("id_select_note_full"),
                            uiOutput("id_select_full")
                          )
                          ),
                   column(width = 4,
                          box(
                            width = 12,
                            background = "olive",
                            div(class="ui-row",
                                HTML("Upload multiplicative effects from MAIHDA model:")
                                ),
                            div(class="ui-row",
                                fileInput("uploaded_multiple_effects", 
                                      label = "", 
                                      accept = c(".csv", ".tsv"))
                                ),
                            uiOutput("helper_multi"),
                            uiOutput("est_select_multi"),
                            uiOutput("se_blurb_multi"),
                            uiOutput("se_select_multi"),
                            uiOutput("lb_select_multi"),
                            uiOutput("ub_select_multi"),
                            uiOutput("id_select_note_multi"),
                            uiOutput("id_select_multi")
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
                              HTML('<div class="sans-serif">Click on the points on the graph to highlight that point, identify it\'s strata, and identify the same strata\'s point on the other graph.<br><br></div>'),
                              uiOutput("plot_ui"),
                              #plotlyOutput("caterpillar_plot_out", height = "650px"),
                              width = 12, height = "100px", maximizable = TRUE,
                              background = "olive"
                          )
                   )
                 )
                 
      )
      
      
    ),
    
    
  ),
  footer = dashboardFooter(left = a(href = "https://intersectionalhealth.org/",
                                    target = "_blank", HTML("<em>IntersectionalHealth.org/</em> (2026-)")), right = HTML('Created by <a href="https://calumwebb.co.uk/calum.html"><em>Dr. Calum Webb</em></a>, University of Sheffield'), fixed = TRUE))

