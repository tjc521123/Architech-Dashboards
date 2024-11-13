library(pacman)
pacman::p_load(shiny,
               bslib,
               bsicons,
               tidyverse,
               zoo,
               ggplot2,
               readxl,
               writexl,
               plotly,
               DT,
               tools,
               rmarkdown)

page_navbar(
  title = 'Monthly Reporting',
  
  window_title = 'Monthly Reporting',
  
  theme = bs_theme(preset = 'spacelab'),
  
  sidebar = sidebar(
   width = '400px',
   card(
     height = '600px',
     fileInput(
       inputId = 'test_file',
       label   = 'Select Test Data'
     ),
     selectInput(
       inputId = 'location_select',
       labe    = 'Select location to view',
       choices = ''
     ),
     selectInput(
       inputId = 'athlete_select',
       label   = 'Select athlete to view',
       choices = ''
     ),
     selectInput(
       inputId = 'test_select',
       label   = 'Select test to view',
       choices = ''
     ),
     selectInput(
       inputId = 'box_select',
       label   = 'Choose metrics for boxplots',
       choices = '',
       multiple = TRUE
     )
   )
  ),
  nav_spacer(),
  nav_panel(
   title = 'Testing',
   card(
     layout_columns(
       col_widths = c(8, 4),
       height = '32vh',
       plotlyOutput('athlete_plot'),
       card(
         uiOutput('group_avg'),
         uiOutput('athlete_z')
       )
     ),
     layout_columns(
       col_widths = c(8, 4),
       row_heights = '32vh',
       plotlyOutput('group_avg_plot'),
       card(
         uiOutput('change_young'),
         uiOutput('change_old')
       )
     ),
     layout_columns(
       row_heights = '22vh',
       plotOutput('group_box_plot')
     )
   )
  ),
  nav_panel(
    title = 'PBs and Improvements',
    selectInput(
      inputId = 'row_group_select',
      label   = 'Group by:',
      choices = c('Athlete', 'Test'),
      selected = 'Athlete'
    ),
    dataTableOutput('pb_boxes')
  ),
  nav_panel(
   title = 'Main Tests Comparison',
   layout_columns(
     col_widths = c(6, 6),
     row_heights = '32vh',
     card(
       card_header('Vertical Jump'),
       card_body(
         layout_columns(
           col_widths = c(6, 6),
           uiOutput('vert_overall_box'),
           uiOutput('vert_loc_box')
         )
       )
     ),
     card(
       card_header('Broad Jump'),
       card_body(
         layout_columns(
           col_widths = c(6, 6),
           uiOutput('broad_overall_box'),
           uiOutput('broad_loc_box')
         )
       ) 
     )
   ),
   layout_columns(
     col_widths = c(6, 6),
     row_heights = '32vh',
     card(
       card_header('Pro Agility'),
       card_body(
         layout_columns(
           uiOutput('agility_overall_box'),
           uiOutput('agility_loc_box')
         )
       )
     ),
     card(
       card_header('40-Yd Dash'),
       card_body(
         layout_columns(
           uiOutput('sprint_overall_box'),
           uiOutput('sprint_loc_box')
         )
       )
     )
   )
  )
)