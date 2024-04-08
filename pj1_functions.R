library(dplyr)

gni <- read.csv("D:/R/Gross National Income Per Capita.csv")

## list functions created in Rmd file and will be use in shiny
extract_year <- function(dataset, option) {
  x_values <- colnames(dataset)[8:ncol(dataset)]
  new_column_names <- sub('.*\\.(\\d+)\\..*', '\\1', x_values)
  new_data <- dataset
  colnames(new_data)[8:ncol(new_data)] <- new_column_names
  
  if("colname" %in% option) {
    return (new_column_names)
  }
  
  if ("new" %in% option) {
    return (new_data)
  }
}


country_gni <- function(country, description) {
  selected_country <- gni[gni$Country == country, ]
  
  
  if ("rank" %in% description ) {
    print(selected_country$HDI.Rank..2021.)
  }
  
  if ("group" %in% description ) {
    print(selected_country$Human.Development.Groups)
  }
  
  if ("gniData" %in% description ) {
    print(selected_country[, 8:39])
  }
}

multiple_line <- function(country_names) {
  y_values <- lapply(country_names, function(country) {
    as.numeric(country_desc(gni, country, "gniData"))
  })
  
  df <- data.frame(x = extract_year(gni, "colname"), 
                   country = rep(country_names, each = length(y_values[[1]])),
                   y = unlist(y_values))
  
  ggplot(df, aes(x, y, color = country, group = country)) +
    geom_line() +
    labs(title = "GNI Per Capita Trends in Selected Countries",
         x = "Year", y = "GNI Per Capita", color = "Country") +
    scale_color_discrete(guide = "legend") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

get_specific_summary <- function(group_column, specific) {
  
  group_summary <- no_na_gni  %>%
    filter({{ group_column }} == {{ specific }} ) %>%
    group_by({{ group_column }}, Human.Development.Groups) %>%
    summarize(country_count = n(),
              .groups = "drop") 
  
  return(group_summary)
}
