
#inputs being used
  #data = filtered_pop_data
  #value = Value, a column name in filtered_pop_data
  #sex = Sex, a column name in filtered_pop_data
  #area = AreaName, a column name in filterd_pop_data
  #area_name = selected_area, choice from a list of values (characters) in the shiny ui
    #which correspond to values of the AreaName column in filtered_data
  #comparator_1 = selected_region, 
  #comparator_2 = "England", fixed value
  #other arguments passed directly through to ggplot labs layer


population_pyramid <- 
  function(data,
           value,
           sex,
           age,
           area,
           area_name,
           comparator_1,
           comparator_2,
           title,
           subtitle,
           xlab) {
    
    #this logic checks whether there are three different things to plot or not - 
      #the area_name, comparator_1, and comparator_2 arguments
    #
  if (!missing(area_name) & !missing(comparator_1) & !missing(comparator_2)) {
    areas <- c(area_name, comparator_1, comparator_2)
  }
  else if (!missing(area_name) & !missing(comparator_1) & missing(comparator_2)) {
    areas <- c(area_name, comparator_1)
  }
  else if (!missing(area_name) & missing(comparator_1) & missing(comparator_2)) {
    areas <- area_name
  }
  else {
    stop("area_name must be complete for a population pyramid to be drawn")
  }
  
  filtered_data <- data %>%
    filter({{ area }} %in% areas) %>%
    group_by({{ area }}) %>%
    mutate({{ value }} := 100 * ({{ value }}) / sum({{ value }}),
           {{ value }} := ifelse({{ sex }} == "Male", -({{ value }}), {{ value }}))
  
  extremex <- scales::breaks_pretty(n = 3)(0:max(abs(pull(filtered_data, {{ value }})), na.rm = TRUE))
  
  population <- ggplot(filter(filtered_data, {{ area }} == area_name), aes(
    y = {{ value }},
    x = {{ age }},
    fill = {{ sex }}
  )) +
    geom_col(col = "black", width = 0.7) +
    coord_flip() +
    scale_y_continuous(
      breaks = c(rev(-extremex), extremex[2:length(extremex)]),
      labels = abs(c(rev(extremex), extremex[2:length(extremex)]))
    ) +
    scale_fill_manual(
      name = "",
      values = c(Male = "#5555E6", Female = "#C2CCFF"),
      breaks = c("Male", "Female"),
      labels = c(paste(area_name, "(Male)"), paste(area_name, "(Female)"))
    ) +
    labs(title = title, subtitle = subtitle, y = xlab) +
    theme(
      legend.position = "bottom",
      legend.key = element_blank(),
      axis.title.y = element_blank(),
      line = element_blank(),
      rect = element_blank(),
      panel.grid.major.x = element_line(colour = "gray80"),
      plot.title = element_text(face = "bold")
    )
  
  
  if (!missing(#this is where there was no value being passed to the missing function
    comparator_1
  )) {
    compdata1 <- filter(filtered_data, {{ area }} == comparator_1)
    print(head(compdata1))
    population <- population +
      geom_line(
        data = compdata1,
        aes(
          y = {{ value }},
          x = {{ age }},
          group = interaction(pull(compdata1, {{ sex }}), pull(compdata1, {{ area }})),
          col = {{ area }}
        ),
        linewidth = 1.5
      )
    
    if (!missing(comparator_2)) {
      compdata2 <- filter(filtered_data, {{ area }} == comparator_2)
      print(head(compdata2))
      population <- population +
        geom_line(
          data = compdata2,
          aes(
            y = {{ value }},
            x = {{ age }},
            group = interaction(pull(compdata2, {{ sex }}), pull(compdata2, {{ area }})),
            col = {{ area }}
          ),
          linewidth = 1.5
        ) +
        scale_colour_manual(
          name = "",
          breaks = c(comparator_1, comparator_2),
          limits = c(comparator_1, comparator_2),
          values = c("black", "#E563F9")
        )
    } else {
      population <- population +
        scale_colour_manual(
          name = "",
          breaks = c(comparator_1),
          limits = c(comparator_1),
          values = c("black")
        )
    }
  }
  
  return(population)
  }



population_pyramid(data=filtered_pop_data,
                   value=Value,
                   sex=Sex,
                   age=Age,
                   area=AreaName,
                   area_name=selected_area,
                   comparator_1= selected_region,
                   comparator_2 = "England",
                   title="blah",
                   subtitle="blah",
                   xlab="blah")
