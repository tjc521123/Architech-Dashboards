library(pacman)
pacman::p_load(shiny,
               tidyverse,
               zoo,
               ggplot2,
               readxl,
               writexl,
               plotly,
               DT,
               tools,
               rmarkdown)

min_agg <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (class(x) != "factor") {
    round(min(x, na.rm = TRUE), digits = 2)
  } else {
    return(NULL)
  }
}

mean_agg <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (class(x) != 'factor') {
    round(mean(x, na.rm = TRUE), digits = 2)
  } else {
    return(NULL)
  }
}

function(input, output, session) {
  data <- reactiveValues(curr = NULL, orig = NULL)
  
  #-----------------------------------------------------------------------------
  # Read data from the selected files
  #-----------------------------------------------------------------------------
  observeEvent(
    input$speed_file,
    {
      file <- input$speed_File
      req(input$speed_file)
      data$orig <- read_excel(path = as.character(input$speed_file$datapath)) %>%
        mutate(Date   = as.Date(Date)) %>%
        as.data.frame()

      data$curr <- data$orig
    }
  )
  
  observeEvent(
    input$test_file,
    {
      file <- input$test_file
      req(input$test_file)
      tmp <- read_excel(path = as.character(input$test_file$datapath)) %>%
        mutate(Date   = as.Date(Date),
               Split  = as.numeric(SPLIT_10),
               Sprint = as.numeric(SPRINT_20)) %>%
        select(Date, Athlete, Split, Sprint) %>%
        as.data.frame()
      
      data$curr <- plyr::rbind.fill(data$curr, tmp) %>%
        arrange(Date)
    }
  )
  
  #-----------------------------------------------------------------------------
  # Update Athlete selection widget
  #-----------------------------------------------------------------------------
  observeEvent(
    input$speed_file,
    {
      req(input$speed_file)
      
      tmp <- data$curr
      choices <- sort(unique(data$curr$Athlete))
      
      updateSelectInput(
        inputId = 'athlete_select',
        choices = choices
      )
    }
  )
  
  observeEvent(
    input$test_file,
    {
      req(input$speed_file)
      
      tmp <- data$curr
      choices <- sort(unique(data$curr$Athlete))
      
      updateSelectInput(
        inputId = 'athlete_select',
        choices = choices
      )
    }
  )
  
  output$athlete_name <- renderText({
    input$athlete_select
  })
  
  #-----------------------------------------------------------------------------
  # Add Data
  #-----------------------------------------------------------------------------
  observeEvent(
    input$add_data,
    {
      req(input$speed_file)

      tmp <- data.frame(
        Date    = as.Date(input$date),
        Athlete = input$athlete,
        Run     = as.numeric(input$run),
        Split   = as.numeric(input$split),
        Sprint  = as.numeric(input$sprint)  
      )
      print(names(data$curr))
      print(names(tmp))
      
      data$curr <- data$curr %>%
        rbind(tmp)
    }
  )
  
  #-----------------------------------------------------------------------------
  # Download Handler for data
  #-----------------------------------------------------------------------------
  output$download <- downloadHandler(
    filename = function() {
      ext  <- paste(".", file_ext(input$speed_file$name), sep = "")
      name <- paste('Speed', gsub('-', '_', Sys.Date()), sep = '_')
      paste(name, '.xlsx', sep = '')
    },
    
    content  = function(file) {
      writexl::write_xlsx(data$curr, path = file)
    }
  )
  
  #-----------------------------------------------------------------------------
  # Create showcase boxes
  #-----------------------------------------------------------------------------

  output$showcase_1 <- renderUI({
    req(input$speed_file)
    req(input$athlete_select)
    
    improvement <- min(data$curr$Sprint[data$curr$Athlete == input$athlete_select], na.rm = TRUE) - head(data$curr$Sprint[data$curr$Athlete == input$athlete_select], 1)
    improvement <- sprintf('%.2f', improvement)
    
    value_box(
      title       = 'Improvement',
      value       = improvement,
      showcase    = bs_icon('graph-up'),
      #full_screen = TRUE,
      theme       = 'success'
    )
  }) 
  
  output$showcase_2 <- renderUI({
    req(input$speed_file)
    req(input$athlete_select)
    
    plot <- data$curr[data$curr$Athlete == input$athlete_select, ] %>%
      group_by(Date) %>%
      mutate(`Avg. Split` = min_agg(Split))
    
    plot <- plot_ly(plot) %>%
      add_lines(
        x = ~Date, y = ~`Avg. Split`,
        color = I("white"), span = I(1),
        fill = 'tozeroy', alpha = 0.5
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = "", tickangle = -45),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
          el.closest('.bslib-value-box')
            .addEventListener('bslib.card', function(ev) {
              Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
            })
        }"
      )
    
    value_box(
      title       = 'Best Split',
      value       = min(data$curr$Split[data$curr$Athlete == input$athlete_select],
                        na.rm = TRUE),
      showcase    = plot,
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  output$showcase_3 <- renderUI({
    req(input$speed_file)
    req(input$athlete_select)
    
    plot <- data$curr[data$curr$Athlete == input$athlete_select, ] %>%
      group_by(Date) %>%
      mutate(`Avg. Sprint` = min_agg(Sprint))
    
    plot <- plot_ly(plot) %>%
      add_lines(
        x = ~Date, y = ~`Avg. Sprint`,
        color = I("white"), span = I(1),
        fill = 'tozeroy', alpha = 0.5
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = "", tickangle = -45),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
          el.closest('.bslib-value-box')
            .addEventListener('bslib.card', function(ev) {
              Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
            })
        }"
      )
    
    value_box(
      title       = 'Best Sprint',
      value       = min(data$curr$Sprint[data$curr$Athlete == input$athlete_select],
                        na.rm = TRUE),
      showcase    = plot,
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  output$showcase_4 <- renderUI({
    req(input$speed_file)
    
    value_box(
      title       = '',
      value       = 'Group Stats',
      showcase    = bs_icon('graph-up'),
      theme       = 'success'
    )
  })
  
  output$showcase_5 <- renderUI({
    req(input$speed_file)
    
    plot <- data$curr %>%
      group_by(Date) %>%
      mutate(`Avg. Split` = mean_agg(Split)) 
    
    plot <- plot_ly(plot) %>%
      add_lines(
        x = ~Date, y = ~`Avg. Split`,
        color = I("white"), span = I(1),
        fill = 'tozeroy', alpha = 0.5
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = "", tickangle = -45),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
          el.closest('.bslib-value-box')
            .addEventListener('bslib.card', function(ev) {
              Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
            })
        }"
      )
    
    value_box(
      title       = 'Avg. Split',
      value       = sprintf('%.2f', mean(data$curr$Split, na.rm = TRUE)),
      showcase    = plot,
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  
  output$showcase_6 <- renderUI({
    req(input$speed_file)
    
    plot <- data$curr %>%
      group_by(Date) %>%
      mutate(`Avg. Sprint` = mean_agg(Sprint)) 
    
    plot <- plot_ly(plot) %>%
      add_lines(
        x = ~Date, y = ~`Avg. Sprint`,
        color = I("white"), span = I(1),
        fill = 'tozeroy', alpha = 0.5
      ) %>%
      layout(
        xaxis = list(visible = F, showgrid = F, title = "", tickangle = -45),
        yaxis = list(visible = F, showgrid = F, title = ""),
        hovermode = "x",
        margin = list(t = 0, r = 0, l = 0, b = 0),
        font = list(color = "white"),
        paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      ) %>%
      config(displayModeBar = F) %>%
      htmlwidgets::onRender(
        "function(el) {
          el.closest('.bslib-value-box')
            .addEventListener('bslib.card', function(ev) {
              Plotly.relayout(el, {'xaxis.visible': ev.detail.fullScreen});
            })
        }"
      )
    
    value_box(
      title       = 'Avg. Sprint',
      value       = sprintf('%.2f', mean(data$curr$Sprint, na.rm = TRUE)),
      showcase    = plot,
      full_screen = TRUE,
      theme       = 'success'
    )
  })
  

  
  #-----------------------------------------------------------------------------
  # Create data table
  #-----------------------------------------------------------------------------
  
  output$table <- renderDataTable({
    req(input$speed_file)
    
    data$curr %>%
      # select(Date, Athlete, Split, Sprint) %>%
      datatable(
        rownames = FALSE,
        width      = "90%",
        filter     = "top",
        editable   = "cell",
        extensions = c('RowGroup', 'Scroller'),
        options    = list(
          columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(data$curr) - 1))),
          pageLength  = 20,
          lengthMenu  = c(5, 10, 20, 50),
          scrollX     = TRUE,
          dom         = 'Bfrtip',
          rowGroup    = list(dataSrc = which(colnames(data$curr) == "Date") - 1),
          deferRender = TRUE
        )
      )
  })
  
  observeEvent(input$table_cell_edit, {
    row  <- input$table_cell_edit$row
    clmn <- input$table_cell_edit$col + 1
    data$curr[row, clmn] <- input$table_cell_edit$value
  })
}