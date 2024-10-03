#this population pyramid function code was taken from the FingertipsCharts package. There were issues loading the package 
#with conflicting versions of R so have just copied the code to here

generate_ggplot_chart <- function(data, value, sex, age, area, area_name, comparator_1, comparator_2, title, subtitle, xlab) {
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
  
  
  if (!missing(#fix 1####
               #this is where there was no value being passed to the missing function
               comparator_1
  )) {
    compdata1 <- filter(filtered_data, {{ area }} == comparator_1)
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
