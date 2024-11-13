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

clean_names <- function(x) {
  xnames <- names(x)
  
  xnames <- gsub(pattern = '_', replacement = ' ', x = xnames)
  xnames <- gsub(pattern = '.', replacement = ' ', x = xnames, fixed = TRUE)
  xnames <- str_to_title(xnames)
  xnames <- gsub(pattern = 'Cmj', replacement = 'CMJ', x = xnames)
  xnames <- gsub(pattern = 'Imtp', replacement = 'IMTP', x = xnames)
  xnames <- gsub(pattern = 'Dsi', replacement = 'DSI', x = xnames)
  xnames <- gsub(pattern = 'Fbw', replacement = 'FBW', x = xnames)
  
  return(xnames)
}

conf_interval <- function(x) {
  conf_int <- 2 * sd(x, na.rm = TRUE) / sqrt(length(x) - sum(is.na(x)))
  
  return(2 * conf_int)
}

function(input, output, session) {
  data <- reactiveValues(cols = NULL, locations = NULL)

  #-----------------------------------------------------------------------------
  # Read data from the selected files
  #-----------------------------------------------------------------------------
  observeEvent(
    input$test_file,
    {
      file <- input$test_file
      req(input$test_file)
      data$locations <- setdiff(excel_sheets(input$test_file$datapath), 
                                c('DASHBOARD', 'NORMS', 'FORCE DECK DATA', 'ATHLETE INFO')) #%>%
        #str_to_title()
      
      lapply(data$locations, function(location_name) {
        data[[location_name]] <- read_excel(path = as.character(input$test_file$datapath),
                                            #sheet = str_to_upper(location_name)) %>%
                                            sheet = str_to_upper(location_name)) %>%
          mutate(DATE = as.Date(DATE)) %>%
          as.data.frame()

        names(data[[location_name]]) <- clean_names(data[[location_name]])
        cols_mutate      <- data[[location_name]] %>%
          select(where(is.numeric)) %>%
          names()
        
        data[[location_name]] <- data[[location_name]] %>%
          mutate(age_group   = ifelse(Age > 13, 1, 0),
                 `Age Group` = as.factor(ifelse(age_group, '14+', '13U')),
                 month_group = ifelse(day(Date) <= 15,
                                      ddays(as.numeric(floor_date(Date, unit = 'month'))),
                                      ddays(as.numeric(floor_date(Date + ddays(20), unit = 'month')))),
                 month_group = as.Date(as_datetime(month_group))
          ) %>%
          group_by(age_group, month_group) %>%
          mutate(across(.cols  = all_of(cols_mutate),
                        .fns   = ~ mean(.x, na.rm = TRUE),
                        .names = 'Group Mean - {.col}')) %>%
          mutate(across(.cols  = all_of(cols_mutate),
                        .fns   = ~ mean(.x, na.rm = TRUE) + conf_interval(.x),
                        .names = 'Group CI-Hi - {.col}')) %>%
          mutate(across(.cols  = all_of(cols_mutate),
                        .fns   = ~ mean(.x, na.rm = TRUE) - conf_interval(.x),
                        .names = 'Group CI-Lo - {.col}')) %>%
          ungroup()
        
        data[[location_name]] <- data[[location_name]] %>%
          group_by(Athlete) %>%
          mutate(
            count_athlete = n()
          ) %>%
          filter(
            count_athlete > 1
          ) %>%
          mutate(
            `Vertical Jump Improvement By Athlete` = max(`Vertical Jump`, na.rm = TRUE) - head(`Vertical Jump`, 1),
            `Broad Jump Improvement By Athlete` = max(`Broad Jump`, na.rm = TRUE) - head(`Broad Jump`, 1),
            `Pro Agility Improvement By Athlete` = min(`Pro Agility`, na.rm = TRUE) - head(`Pro Agility`, 1),
            `Sprint 40 Improvement By Athlete` = min(`Sprint 40`, na.rm = TRUE) - head(`Sprint 40`, 1)
          ) %>%
          ungroup() %>%
          mutate(
            `Avg. Vertical Jump Improvement` = mean(`Vertical Jump Improvement By Athlete`, na.rm = TRUE),
            `Avg. Broad Jump Improvement` = mean(`Broad Jump Improvement By Athlete`, na.rm = TRUE),
            `Avg. Agility Improvement` = mean(`Pro Agility Improvement By Athlete`, na.rm = TRUE),
            `Avg. Sprint 40 Improvement` = mean(`Sprint 40 Improvement By Athlete`, na.rm = TRUE)
          )
      })
      
      data$Improvements <- data.frame(
        `Vertical Jump` = NA,
        `Broad Jump` = NA,
        `Pro Agility` = NA,
        `Sprint 40` = NA,
        check.names = FALSE
      )
      
      lapply(data$locations, function(location_name) {
        tmp <- data[[location_name]] %>%
          group_by(Athlete) %>%
          mutate(count_athlete = n()) %>%
          filter(count_athlete > 1) %>%
          ungroup() %>%
          select(
            `Avg. Vertical Jump Improvement`,
            `Avg. Broad Jump Improvement`,
            `Avg. Agility Improvement`,
            `Avg. Sprint 40 Improvement`
          ) %>%
          mutate(
            `Vertical Jump` = mean(`Avg. Vertical Jump Improvement`, na.rm = TRUE),
            `Broad Jump` = mean(`Avg. Broad Jump Improvement`, na.rm = TRUE),
            `Pro Agility` = mean(`Avg. Agility Improvement`, na.rm = TRUE),
            `Sprint 40` = mean(`Avg. Sprint 40 Improvement`, na.rm = TRUE)
          ) %>%
          select(
            -`Avg. Vertical Jump Improvement`,
            -`Avg. Broad Jump Improvement`,
            -`Avg. Agility Improvement`,
            -`Avg. Sprint 40 Improvement`
          )
        
        data$Improvements <- rbind(data$Improvements, colMeans(tmp))
      })
      
      data$Improvements <- colMeans(data$Improvements, na.rm = TRUE) %>%
        t() %>%
        as.data.frame()
      
      #data$locations <- str_to_title(data$locations)

      updateSelectInput(
        inputId = 'location_select',
        choices = sort(data$locations),
        selected = head(data$locations, 1)
      )
      
    }
  )
  
  observeEvent(
    input$location_select,
    {
      req(input$test_file)
      req(input$location_select)

      data$curr <- data[[input$location_select]]
    }
  )

  
  #-----------------------------------------------------------------------------
  # Update Variable selection widget
  #-----------------------------------------------------------------------------
  observeEvent(
    input$test_file,
    {
      req(input$test_file)
      req(input$location_select)
      
      tmp <- data$curr
      choices <- sort(unique(tmp$Athlete)) 
      
      updateSelectInput(
        inputId = 'athlete_select',
        choices = choices,
        selected = head(choices, 1)
      )
      
      tmp <- data$curr %>%
        select(!(c(Date, Age, `Age Group`, age_group, month_group) | starts_with('Group') | starts_with('Avg') | contains('Athlete')))
      choices <- sort(names(tmp))
      
      updateSelectInput(
        inputId = 'test_select',
        choices = choices,
        selected = head(choices, 1)
      )
      
      updateSelectInput(
        inputId = 'box_select',
        choices = choices,
        selected = head(choices, 1)
      )
    }
  )
  
  observeEvent(
    input$location_select,
    {
      req(input$test_file)
      req(input$location_select)
      
      tmp <- data$curr
      choices <- sort(unique(tmp$Athlete)) 
      
      updateSelectInput(
        inputId = 'athlete_select',
        choices = choices,
        selected = head(choices, 1)
      )
      
      tmp <- data$curr %>%
        select(!(c(Date, Age, `Age Group`, age_group, month_group) | starts_with('Group') | starts_with('Avg') | contains('Athlete')))
      choices <- sort(names(tmp))
      
      updateSelectInput(
        inputId = 'test_select',
        choices = choices,
        selected = head(choices, 1)
      )
      
      updateSelectInput(
        inputId = 'box_select',
        choices = choices,
        selected = head(choices, 1)
      )
    }
  )
  
  #-----------------------------------------------------------------------------
  # Update Athlete Plot
  #-----------------------------------------------------------------------------
  output$athlete_plot <- renderPlotly({
    req(input$test_file)
    req(input$test_select)
    req(input$athlete_select)
    req(input$location_select)
    
    tmp <- data$curr
    
    tmp <- tmp[tmp$Athlete == input$athlete_select, ]
    colors <- c('13U' = 'red', '14+' = 'blue')
    
    validate(need(tmp, message = 'Loading...'))
    
    plot <- tmp %>%
      ggplot(mapping = aes(x = Date, y = .data[[!!input$test_select]], 
                           color = `Age Group`)) +
      geom_line() +
      geom_point() +
      geom_line(data = data$curr, 
                mapping = aes(x = month_group, y = .data[[!!paste('Group Mean - ', input$test_select, sep = '')]]),
                linetype = 'dashed') +
      lims(
        y = c(0, NA)
      ) +
      labs(
        x = element_blank()
      ) +
      scale_color_manual(values = colors)
    
    ggplotly(plot) %>%
      layout(
        title = list(text = paste0(input$athlete_select, ' - ',
                                   'Athlete Scores (Solid) vs Group Averages (Dashed)'),
                     y = 0.98,
                     size = 10)
      )
  }) %>% bindCache(input$athlete_select, input$test_select)
  
  output$group_avg <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$athlete_select)
    req(input$location_select)
    
    if (is.null(data$curr)) {
      return()
    }
    
    athlete_group <- max(data$curr$age_group[data$curr$Athlete == input$athlete_select], 
                         na.rm = TRUE)
    group_avg <- colMeans(data$curr[data$curr$age_group == athlete_group, input$test_select],
                          na.rm = TRUE) %>%
      round(digits = 1)

    value_box(
      title = 'Group Average',
      value = group_avg
    )
  })
  
  output$athlete_z <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$athlete_select)
    req(input$location_select)
    
    if (is.null(data$curr)) {
      return()
    }
    
    athlete_group <- max(data$curr$age_group[data$curr$Athlete == input$athlete_select], 
                         na.rm = TRUE)
    
    tmp <- data$curr[data$curr$age_group == athlete_group, c('Athlete', input$test_select)] %>%
      mutate(z_score = (.[[2]] - mean(.[[2]], na.rm = TRUE)) / sd(.[[2]], na.rm = TRUE)) %>%
      filter(
        Athlete == input$athlete_select
      )

    z_score <- round(tmp[nrow(tmp), 3], digits = 2)

    value_box(
      title = 'Athlete Z-Score',
      value = z_score
    )
  })
  
  output$group_avg_plot <- renderPlotly({
    req(input$test_file)
    req(input$test_select)
    
    if (is.null(data$curr)) {
      return()
    }
    
    conf_lo <- paste0('Group CI-Lo - ', input$test_select)
    conf_up <- paste0('Group CI-Hi - ', input$test_select)
    Average <- paste0('Group Mean - ', input$test_select)
    colors <- c('13U' = 'red', '14+' = 'blue')
    
    tmp <- data$curr %>%
      select('Age Group', 'month_group', contains(input$test_select)) %>%
      rename(average = !!Average,
             lower   = !!conf_lo,
             upper   = !!conf_up,
             Month   = month_group) %>%
      group_by(`Age Group`, Month)
    
    plot <- tmp %>%
      ggplot(mapping = aes(x = Month, y = average, color = `Age Group`)) +
      geom_point(position = position_dodge(width = 10)) +
      geom_linerange(aes(ymin = lower, ymax = upper), position = position_dodge(width = 10)) +
      labs(
        x = element_blank(),
        y = input$test_select,
        title = paste(input$test_select, '- Average with 95% CI')
      ) +
      scale_color_manual(values = colors)
  })
  
  output$change_old <- renderUI({
    req(input$test_file)
    req(input$test_select)
    
    tmp <- data$curr %>%
      group_by(month_group, `Age Group`) %>%
      summarise(avg = mean(.data[[input$test_select]], na.rm = TRUE)) %>%
      filter(`Age Group` == '14+') %>%
      ungroup() %>%
      select(avg) %>%
      tail(2)
    
    change <- round((tmp[2, 1] - tmp[1, 1]) / tmp[1, 1] * 100, digits = 1)
    
    value_box(
      title = 'Change from last month - 14+',
      value = paste0(change, '%'),
      theme = 'primary'
    )
  })
  
  output$change_young <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    tmp <- data$curr %>%
      group_by(month_group, `Age Group`) %>%
      summarise(avg = mean(.data[[input$test_select]], na.rm = TRUE)) %>%
      filter(`Age Group` == '13U') %>%
      ungroup() %>%
      select(avg) %>%
      tail(2)
    
    change <- round((tmp[2, 1] - tmp[1, 1]) / tmp[1, 1] * 100, digits = 1)
    
    value_box(
      title = 'Change from last month - 13U',
      value = paste0(change, '%'),
      theme = 'red'
    )
  })

  output$group_box_plot <- renderPlot({
    req(input$test_file)
    req(input$location_select)

    cols <- input$box_select

    tmp <- data$curr %>%
      select(`Age Group`, any_of(cols))
    
    if (ncol(tmp) == 1) {
      return()
    } else {
      tmp <- tmp %>%
        pivot_longer(cols = !c(`Age Group`))
    }
    
    colors <- c('13U' = 'red', '14+' = 'blue')

    plot <- tmp %>%
      ggplot(mapping = aes(x = 0, y = value, color = `Age Group`)) +
      geom_violin(position = position_dodge(0.9), alpha = 0.5) +
      geom_boxplot(position = position_dodge(0.9), width = 0.1) +
      labs(
        title = 'Summary and Distribution of Selected Tests',
        x = 'Test',
        y = ''
      ) +
      theme(axis.title.x = element_blank(),
            axis.text.x  = element_blank(),
            axis.ticks.x = element_blank()) +
      scale_color_manual(values = colors) +
      facet_wrap(facets = ~name, nrow = 1, scales = 'free')
    
    plot
  })
  
  #-----------------------------------------------------------------------------
  # Create PBs and Improvement Tab
  #-----------------------------------------------------------------------------
  output$pb_boxes <- renderDataTable({
    req(input$test_file)
    req(input$location_select)
    req(input$row_group_select)
    
    test_names <- data$curr %>%
      select(!(contains('Group') | contains('Improvement') | contains('Nordbord') | contains('count') | c('Athlete', 'Date', 'Age', 'age_group'))) %>%
      names()
    
    last_athletes <- sort(unique(data$curr$Athlete[data$curr$month_group == max(data$curr$month_group, na.rm = TRUE)]))

    tmp <- data$curr %>%
      select('Athlete', 'Date', 'month_group', all_of(test_names)) %>%
      filter(Athlete %in% last_athletes) %>%
      group_by(Athlete) %>%
      mutate(
        across(
          all_of(test_names),
          list(
            best = ~ ifelse(cur_column() %in% c('Split 10', 'Sprint 20', 'Sprint 40', 'Pro Agility'),
                            min(.x, na.rm = TRUE), max(.x, na.rm = TRUE)),
            last_month = ~ tail(.x, 2)[-2],
            this_month = ~ tail(.x, 1)[1],
            month_imp = ~ tail(.x, 1)[1] - tail(.x, 2)[-2],
            best_imp = ~ ifelse(cur_column() %in% c('Split 10', 'Sprint 20', 'Sprint 40', 'Pro Agility'),
                                min(.x, na.rm = TRUE) - tail(.x, 1)[1], tail(.x, 1)[1] - max(.x,na.rm = TRUE))
            )
          )
        ) %>%
      filter(month_group == max(month_group, na.rm = FALSE)) %>%
      as.data.frame()

    table_list <- lapply(test_names, function(test_name) {
      tmp %>%
        select(Athlete, contains(test_name)) %>%
        rename(Value = 2, Best = 3, `Last Month` = 4, `This Month` = 5, `Last Month Imp.` = 6, `Best Imp.` = 7) %>%
        select(-Value) %>%
        group_by(Athlete) %>%
        filter(`Last Month Imp.` >= 0) %>%
        mutate(Test = test_name) %>%
        mutate(across(where(is.numeric), ~ round(.x, digits = 1)))
    })
  
    output_table <- do.call(rbind, table_list)
    
    if (input$row_group_select == 'Athlete') {
      group = 0
      output_table <- arrange(output_table, Athlete)
    } else {
      group = c(ncol(output_table) - 1)
      output_table <- arrange(output_table, Test)
    }
    
    datatable(
      output_table,
      rownames = FALSE,
      width      = "90%",
      filter     = "top",
      extensions = c('RowGroup', 'Scroller'),
      options    = list(
        columnDefs = list(list(className = 'dt-center', targets = 0:(ncol(output_table) - 1))),
        pageLength  = 20,
        lengthMenu  = c(5, 10, 20, 50),
        scrollX     = TRUE,
        dom         = 'Bfrtip',
        rowGroup    = list(dataSrc = group),
        deferRender = TRUE
      )
    ) %>%
    formatStyle('Best Imp.',
                target = 'row',
                backgroundColor = styleEqual(c(0), c('#0f0')))
  })

  #-----------------------------------------------------------------------------
  # Create Test Comparison
  #-----------------------------------------------------------------------------
  output$vert_overall_box <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    if (length(data$Improvements$`Vertical Jump`) == 0) {
      value = 'NA'
    } else {
      value = paste(round(data$Improvements$`Vertical Jump`, digits = 2), "in", sep = '')
    }
    
    value_box(
      title = 'Overall Improvement',
      value = value,
      height = '100px',
      theme = 'text-info'
    )
  })
  
  output$vert_loc_box <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    # if (length(data$Improvements$`Avg. Vertical Jump Improvement`) == 0) {
    #   value = 'NA'
    # } else {
    # 
    # }
    
    value_box(
      title = paste('Improvement -', input$location_select),
      value = paste(round(mean(data$curr$`Avg. Vertical Jump Improvement`, na.rm = TRUE), digits = 2),
                    "in", sep = ''),
      height = '100px',
      theme = 'text-info'
    )
  })
  
  output$broad_overall_box <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    if (length(data$Improvements$`Broad Jump`) == 0) {
      value = 'NA'
    } else {
      value = paste(round(data$Improvements$`Broad Jump`, digits = 2), "in", sep = '')
    }
    
    value_box(
      title = 'Overall Improvement',
      value = value,
      height = '100px'
    )
  })
  
  output$broad_loc_box <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    # if (length(data$Improvements$`Avg. Broad Jump Improvement`) == 0) {
    #   value = 'NA'
    # } else {
    # 
    # }
    
    value_box(
      title = paste('Improvement -', input$location_select),
      value = paste(round(mean(data$curr$`Avg. Broad Jump Improvement`, na.rm = TRUE), digits = 2),
                    "in", sep = ''),
      height = '100px'
    )
  })
  
  output$agility_overall_box <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    if (length(data$Improvements$`Pro Agility`) == 0) {
      value = 'NA'
    } else {
      value = paste(round(data$Improvements$`Pro Agility`, digits = 2), "s", sep = '')
    }
    
    value_box(
      title = 'Overall Improvement',
      value = value,
      height = '100px'
    )
  })
  
  output$agility_loc_box <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    # if (length(data$Improvements$`Avg. Agility Improvement`) == 0) {
    #   value = 'NA'
    # } else {
    # 
    # }
    
    value_box(
      title = paste('Improvement -', input$location_select),
      value = paste(round(mean(data$curr$`Avg. Agility Improvement`, na.rm = TRUE), digits = 2),
                    "s", sep = ''),
      height = '100px'
    )
  })
  
  output$sprint_overall_box <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    if (length(data$Improvements$`Sprint 40`) == 0) {
      value = 'NA'
    } else {
      value = paste(round(data$Improvements$`Sprint 40`, digits = 2), 's', sep = '')
    }
    
    value_box(
      title = 'Overall Improvement',
      value = value,
      height = '100px'
    )
  })
  
  output$sprint_loc_box <- renderUI({
    req(input$test_file)
    req(input$test_select)
    req(input$location_select)
    
    # if (length(data$Improvements$`Avg. Sprint Improvement`) == 0) {
    #   value = 'NA'
    # } else {
    #   
    # }
    
    value_box(
      title = paste('Improvement -', input$location_select),
      value = paste(round(mean(data$Improvements$`Avg. Sprint Improvement`, na.rm = TRUE), digits = 2),
                    's', sep = ''),
      height = '100px'
    )
  })
}
