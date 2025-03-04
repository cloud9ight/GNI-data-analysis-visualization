# [My Shiny APP Youtube URL](https://www.youtube.com/watch?v=4xLDm6Taf5c&ab_channel=HazLi)

options(scipen = 999)
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(plotly)
library(scales)


# Load dataset and functions
gni <- read.csv("Gross National Income Per Capita.csv")
no_na_gni <- na.omit(gni)

# Load functions created in Rmd file and will be use in shiny
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

# Call the extract_year function to extract year
year <- extract_year(gni, "colname")


# SHINY APP

ui <- fluidPage(theme = shinytheme("cosmo"),
                
                # In the navbar, we will have FIVE tabs to present different plots with unique functions
                navbarPage(
                  "Gross National Income Per Capita",
                  
                  # Tab 1: Line & Point - Country
                  tabPanel("Line&Point-Country Trend",
                           sidebarPanel(
                             selectInput(inputId = "country_input1", 
                                         label = "Choose Country 1:",
                                         choices = unique(gni$Country)),
                             selectInput(inputId = "country_input2", 
                                         label = "Choose Country 2:",
                                         choices = unique(gni$Country)),br(),
                             selectInput(inputId = "country_input3", 
                                         label = "Country Data Details:",
                                         choices = unique(gni$Country)),
                             actionButton("update_button", "Update Plot"),
                             width = 2
                           ),
                           mainPanel(
                             fluidRow(
                               plotOutput("plot1", height = "400px", width = "900px"),
                               tags$div(style = "height: 10px;"), 
                               plotlyOutput("plot2", height = "400px", width = "800px")
                             )
                           )
                  ),
                  
                  # Tab 2: Pie - HD group
                  tabPanel("Pie-HD group",
                           sidebarPanel(
                             selectInput(inputId = "input_continent", 
                                         label = "Choose Continent:",
                                         choices = unique(gni$Continent)),
                             selectInput(inputId = "input_region", 
                                         label = "Choose Region:",
                                         choices = unique(no_na_gni$UNDP.Developing.Regions)),
                             helpText(
                               tags$p(
                                 style = "font-size: 12px;",br(),
                                 "# The Human Development Index (HDI)" ,br(),"measures human development by average achievement in three basic dimensions: standard of living, education and health, 
                                 which are assessed by three indicators: GNI per capita, expected years of schooling and life expectancy.",br(),br(),
                                 "# UNDP.Developing.Regions." ,br(),"- AS: Arab States" ,br(),"   
                                 - EAP: East Asia and the Pacific" ,br(),"  
                                 - ECA: Europe and Central Asia" ,br(),"   
                                 - LAC: Latin America and the Caribbean" ,br(),"  
                                 - SA: South Asia" ,br(),"   
                                 - SSA: Sub-Saharan Africa "
                               )
                             ),
                             width = 2
                           ),
                           mainPanel(
                             fluidRow(
                               plotOutput("plot3", height = "400px", width = "900px"),
                               tags$div(style = "height: 20px;"), 
                               plotOutput("plot4", height = "400px", width = "900px")
                             )
                           )
                  ),
                  
                  # Tab 3: Historgram - GNI 
                  tabPanel("Historgram-2021GNI",
                           sidebarPanel(
                             sliderInput("bins", "Customize the partitions:", min = 0, max = 150000, value = 5000),
                             width = 2
                           ),
                           mainPanel(
                             fluidRow(
                               plotOutput("plot5", height = "400px", width = "900px")
                             )
                           )
                  ),
                  
                  # Tab 4: Scatter - Growth Rate
                  tabPanel("Scatter-Growth Rate",
                           sidebarPanel(
                               width = 2,offset = 2,
                                      sliderInput("rate", "Select Growth Rate Range (100%):", min = -100, max = 400, value = 100),
                                      helpText(
                                        tags$p(
                                          style = "font-size: 19px;", br(), br(), br(), br(),br(),br(), br(), br(),br(),br(), br(), br(), br(),br(),
                                          "# View dispersion of data points", br(), "by Continent or Global"
                                        )
                                      ),
                              
                                      selectInput(inputId = "input_continent_scatter", 
                                                  label = "Choose Continent:",
                                                  choices = c("Global", unique(gni$Continent)))
                                  ),
                           mainPanel(
                               width = 9, 
                               plotlyOutput("plot6", height = "400px", width = "600px"),
                               tags$div(style = "height: 30px;"), 
                               plotOutput("plot7", height = "600px", width = "700px")
                             
                           )
                  ),
                  
                  # Tab 5:Box - Details
                  tabPanel("BOX-Continent Details",
                           sidebarPanel(
                             width = 2,offset = 2,
                                      checkboxGroupInput("Details", "Select Where You Want To Know:",
                                                choices = unique(gni$Continent))
                           ),
                           mainPanel(
                             fluidRow(
                               plotOutput("plot8", height = "600px", width = "900px"),br(),br(),br(), br(), br(),
                               tableOutput("table")
                             )
                           ))
))

# Define the server
server <- function(input, output, session) {
  options(warn = -1)
  output$plot1 <- renderPlot({
    
      validate(
        need(input$update_button > 0, "Please select a country. Click 'Update Plot' to generate the plot")
      )
    
    countryone <- input$country_input1
    countrytwo <- input$country_input2
    countryone_gni <- country_gni(countryone, "gniData")
    countrytwo_gni <- country_gni(countrytwo, "gniData")

    two_gni <- data.frame(x = year, y1 =as.numeric(countryone_gni[1,]), y2 =as.numeric(countrytwo_gni[1,]) )
    
    ggplot(data = two_gni, aes(x = year, y, group = 1)) +
      geom_line(aes(y = y1, color = countryone)) +
      geom_point(aes(y = y1, color = countryone)) +
      geom_line(aes(y = y2, color = countrytwo)) +
      geom_point(aes(y = y2,  color = countrytwo)) +
      labs(title =paste(countryone, "&", countrytwo, "GNI Per Capita Trend Over Years") ,
           x = "Year", y = "Gross National Income", color = "Country") +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size=16),
            axis.text.x = element_text(angle = 45, hjust = 1))

  })
  
  ## Another plot with interactive line and point plot to present selected country's GNI data at certain year
  # Using Plotly
  
  output$plot2 <- renderPlotly({
    validate(
      need(input$update_button > 0, "Please select a country. Click 'Update Plot' to generate the plot")
    )
    selected_country <- input$country_input3
    country_gni_data <- country_gni(selected_country, "gniData")
    
    country_data <- data.frame(x = year, y = as.numeric(country_gni_data[1,]))
    
    p <- plot_ly(data = country_data, x = ~x, y = ~y, type = 'scatter', mode = 'lines+markers', name = selected_country) %>%
      layout(title = paste(selected_country, "GNI Per Capita Over Years  # Hover to check specific data!!!"),
             xaxis = list(title = "Year", tickangle = -45),
             yaxis = list(title = "Gross National Income")) 
    
    p
  })
  
  ##tab 2 HD group
  output$plot3 <- renderPlot({

    hd_group <- data.frame(get_specific_summary(Continent, input$input_continent))
   
    ggplot(data = hd_group, aes(x="", y = country_count, fill = Human.Development.Groups)) +
      geom_bar(stat = "identity", width = 1) +
      geom_text(aes(label = paste(country_count, " (", scales::percent(country_count / sum(country_count)), ")")),
                position = position_stack(vjust = 0.5),
                size = 5, family = "bold") +
      coord_polar("y", start = 0) +
      labs(title = paste("HD Group Proportions in", input$input_continent),
           fill = "HD Group") +
      theme(
        plot.title = element_text(size = 16)
      )
  })
  output$plot4 <- renderPlot({
    
    hd_group <- data.frame(get_specific_summary(UNDP.Developing.Regions, input$input_region))
    
    ggplot(data = hd_group, aes(x="", y = country_count, fill = Human.Development.Groups)) +
      geom_bar(stat = "identity", width = 1) +
      coord_polar("y", start = 0) +
      geom_text(aes(label = paste(country_count, " (", scales::percent(country_count / sum(country_count)), ")")),
                position = position_stack(vjust = 0.5),
                size = 5, family = "bold") +
      labs(title = paste("HD Group Proportions in", input$input_region, " (Only Meansuring Developing Countries)"),
           fill = "HD Group") +
      theme(
        plot.title = element_text(size = 16)
      )

  })
  
  ##tab 3 GNI
  output$plot5 <- renderPlot({
    options(scipen = 999)
    
    latest_table <- data.frame(Country = no_na_gni$Country,
                               Continent = no_na_gni$Continent,
                                
                               Latest = no_na_gni$Gross.National.Income.Per.Capita..2021.)
    
    ggplot(data = latest_table, aes(x = Latest, fill = Continent)) +
      
      geom_histogram(binwidth = input$bins) + 
      
      geom_vline(aes(xintercept=mean(Latest)), color="darkblue", linetype="dashed", size=2)+
      labs(
        title = "Histogram of GNI Per Capita Data",
        x = "GNI Range",
        y = "Count of Countries"
      ) +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.4, hjust = 0.5, face = "bold", size = 14),
            axis.text.y = element_text(size = 16),
            plot.title = element_text(size = 20))
  }, height = 600, width = 600)
  
  
  ##tab4 scatter
  output$plot6 <- renderPlotly({
    
    growth_rate <- round((no_na_gni[, 39] - no_na_gni[, 8]) / no_na_gni[, 8] * 100, 0)
    growth_scatter <- data.frame(Country = no_na_gni$Country, 
                                 Continent = no_na_gni$Continent, 
                                 GrowthRate = growth_rate, 
                                 Latest = no_na_gni$Gross.National.Income.Per.Capita..2021.)
  
    
        # Set the y-axis range according to the range of the sliderInput 
    y_range <- case_when(
      input$rate <= 0 ~ c(-100, 0), input$rate > -0 && input$rate<=100 ~ c(0, 100), input$rate > 100 && input$rate<=200 ~ c(100, 200),
      input$rate > 200 && input$rate<=300 ~ c(200, 300), input$rate > 300 && input$rate<=400 ~ c(300, 400)
    )
        
    plot_ly(growth_scatter, 
            type = "scatter", mode = "markers", x = ~Latest, y = ~GrowthRate,
            color = ~Continent, size = ~GrowthRate,
            text = ~paste(Country, "<br>Grow Rate:", GrowthRate, "%<br>GNI:", Latest)
    ) %>%
      layout(
        
        title = list(text = "Latest GNI Per Capita Vs Growth Rate(1990-2021) <br>Hover to see country details!!!", 
                     size = 20),  
        xaxis = list(title = "Gross.National.Income.Per.Capita..2021"),
        yaxis = list(title = "Growth Rate %", range = y_range),
        margin = list(t = 100),
        height = 400, width = 600
      )
    
  })
  
  output$plot7 <- renderPlot({
  
    growth_rate <- round((no_na_gni[, 39] - no_na_gni[, 8]) / no_na_gni[, 8] *100,0)
   growth_table <- data.frame(Country = no_na_gni$Country, Continent = no_na_gni$Continent, GrowthRate = growth_rate, Latest = no_na_gni$Gross.National.Income.Per.Capita..2021.)
    
    selected_continent <- input$input_continent_scatter
    
    aim <- growth_table %>% filter(Continent == selected_continent)
    
    # set unique range of aimed continent
    min_gni <- min(aim$Latest)
    max_gni <-max(aim$Latest)
    min_rate <-min(aim$GrowthRate)
    max_rate <-max(aim$GrowthRate)
    
    if (selected_continent == "Global") {
      ggplot(growth_table, aes(x = Latest, y = GrowthRate)) + 
        geom_point(aes(col = Continent, size = 1.5)) + 
        geom_density_2d_filled(alpha = 0.5) +
        xlim(c(700, 67000)) +
        ylim(c(-50, 200)) +
        labs(title = paste(selected_continent, " GNI Data Vs Growth Rate(1990-2021)"), 
             x = "Gross.National.Income.Per.Capita..2021", 
             y = "Growth Rate %") +
        theme(
          plot.title = element_text(size = 16),  
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)
        )
    } else {
      ggplot(aim, 
             aes(x = Latest, y = GrowthRate)) + 
        geom_point(aes(col = Continent, size = Latest),color="blue") + 
        geom_density_2d_filled(alpha = 0.5) +
        xlim(c(min_gni, max_gni)) +
        ylim(c(min_rate, max_rate)) +
        labs(title = paste(selected_continent, " GNI Data Vs Growth Rate(1990-2021)"), 
             x = "Gross.National.Income.Per.Capita..2021", 
             y = "Growth Rate %") +
        theme(
          plot.title = element_text(size = 16),  
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 14)
        )
    }
})
  
  ## tab 5 Box Plot
  output$plot8 <- renderPlot({

    selected_continents <- input$Details
    filtered_data <- gni %>%
      filter(Continent %in% selected_continents) %>%
      mutate(`Average GNI (1990-2021)` = rowMeans(.[8:39]))
    
    continent_combined <- ggplot(filtered_data, aes(x = Continent, y = `Average GNI (1990-2021)`, fill = Continent)) +
      geom_boxplot(position = position_dodge(width = 0.8)) +
      
      stat_summary(fun = "mean", geom = "point", shape = 8,
                   size = 3, color = "white")+
      labs(title = 'Box Plot of Average GNI by Continent',
           x = 'Continent',
           y = 'Average GNI') +
      theme_minimal()+
      theme(
        plot.title = element_text(size = 16),  
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14)
      )
    
    continent_combined
  })
  
  output$table <- renderTable({
    growth_rate <- round((no_na_gni[, 39] - no_na_gni[, 8]) / no_na_gni[, 8] * 100, 0) ## drop na
    mean = round(rowMeans(no_na_gni[8:39], na.rm = TRUE), 0)
    
    ## create a new data frame and we will be using Growth Rate $ AVG as Indicator a lot later
    growth_table_new <- data.frame(
      Country = no_na_gni$Country,
      Continent = no_na_gni$Continent,
      Latest = no_na_gni$Gross.National.Income.Per.Capita..2021.,
      GrowthRate = growth_rate,
      AVG = mean
    )
    
    selected_continent <- input$Details
    
    continent_summary <- growth_table_new %>%
      group_by(Continent) %>%
      summarize(
        avg_gni = round(mean(AVG), 1),
        min_gni = round(min(Latest), 1),
        min_gni_country = Country[which.min(Latest)],
        max_gni = round(max(Latest), 1),
        max_gni_country = Country[which.max(Latest)],
        `avg_rate%` = round(mean(GrowthRate), 1),
        `min_rate%` = min(GrowthRate),
        min_rate_country = Country[which.min(GrowthRate)],
        `max_rate%` = max(GrowthRate),
        max_rate_country = Country[which.max(GrowthRate)]
      ) %>%
      filter(Continent %in% selected_continent)
  })
  
  
}

shinyApp(ui, server)

