library(pacman)
pacman::p_load(shiny,
               bslib,
               bsicons,
               tidyverse,
               ggplot2,
               readxl,
               writexl,
               plotly,
               DT,
               tools,
               rmarkdown)

page_sidebar(
  title = 'Speed Dashboard',
  window_title = 'Speed Dashboard',
  theme = bs_theme(preset = 'spacelab'),

  sidebar = sidebar(
    width = '500px',

    card(
      fileInput(
        inputId = 'speed_file',
        label   = 'File',
        accept  = c('.xlsx')
      ),
      fileInput(
        inputId = 'test_file',
        label   = 'Test File',
        accept  = c('.xlsx')
      ),
      
      selectInput(
        inputId = 'athlete_select',
        label   = 'Select Athlete',
        choices = ''
      ),
  
      card(
        dateInput(
          inputId = 'date',
          label   = 'Enter Date',
          value   = Sys.Date(),
          max     = Sys.Date()
        ),
        textInput(
          inputId = 'athlete',
          label   = 'Athlete Name'
        ),
        
        layout_columns(
          numericInput(
            inputId = 'run',
            label   = 'Run',
            value   = 1,
            min     = 1
          ),
          numericInput(
            inputId = 'split',
            label   = '10-Yd Split',
            value   = 0,
            min     = 0
          ),
          numericInput(
            inputId = 'sprint',
            label   = '20-Yd Sprint',
            value   = 0,
            min     = 0
          )
        ),
        layout_columns(
          actionButton(
            inputId = 'add_data',
            label   = 'Add Data'
          ),
          downloadButton(
            outputId = 'download'
          )
        ),
      )
    )
  ),
  
  textOutput(outputId = 'athlete_name'),

  layout_columns(
    row_heights = '75px',
    uiOutput('showcase_1'),
    uiOutput('showcase_2'),
    uiOutput('showcase_3')
  ),

  card(
    dataTableOutput(
      outputId = 'table'
    ),
    full_screen = TRUE
  )
)