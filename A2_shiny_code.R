# Import libraries
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(scales)
library(shiny)
library(shinydashboard)
library(plotly)
library(geojsonio)
library(sp)
library(leaflet)
library(readxl)
library(shinyWidgets)

getwd()
salary_df <- read_csv("IT_salary_Germany.csv")
salary_df$company_size <- factor(salary_df$company_size, 
                                 levels=c("UP TO 10","11-50","51-100","101-1000", "1000+"))
salary_df$experienced_year <- factor(salary_df$experienced_year, 
                                   levels=c("Less than 5","6 to 10","11 to 15",
                                            "16 to 20","21 to 25", "26 to 30"))
salary_df$Seniority_level <- factor(salary_df$Seniority_level,
                                     levels=c("NO LEVEL","JUNIOR","MIDDLE",
                                              "SENIOR","HEAD/LEAD", "PRINCIPAL"))

# Data used for draw map
map_colnames <- c('City', 'Current_Salary', 'lon','lat')
city_table <- salary_df %>% select(all_of(map_colnames)) 

# calculate average salary
city_table_1 <- city_table %>% group_by(City,lon,lat) %>% 
  summarise(mean_salary = round(mean(Current_Salary)), .groups = 'drop')

city_table_1$mean_salary[city_table_1$mean_salary < 50000] <- '< 50000'
city_table_1$mean_salary[city_table_1$mean_salary >= 50000 &
                           city_table_1$mean_salary < 60000] <- '50000 ~ 60000'
city_table_1$mean_salary[city_table_1$mean_salary >= 60000 &
                           city_table_1$mean_salary < 70000] <- '60000 ~ 70000'
city_table_1$mean_salary[city_table_1$mean_salary >= 70000 &
                           city_table_1$mean_salary < 80000] <- '70000 ~ 80000'
city_table_1$mean_salary[city_table_1$mean_salary >= 80000 &
                           city_table_1$mean_salary < 90000] <- '80000 ~ 90000'
city_table_1$mean_salary[city_table_1$mean_salary >= 90000 &
                           city_table_1$mean_salary < 99999] <- '90000 ~ 100000'

# get the Germany city shapes
spdf <- geojson_read("4_niedrig.geo1.json",  what = "sp")
# read the cost file
cost_df <- read_excel("cost_germany.xlsx")
# Change the first letter of name into upperc ase
cost_df$city <- cost_df$city %>% str_to_title()
# merge the cost with shape
spdf_1 <- merge(spdf, cost_df, sort=FALSE, by.x="NAME_2", by.y="city", all.x=TRUE)

year_color <- c('#ff9933','#009900','#0073e6')

# UI of shiny
ui <- dashboardPage(
  dashboardHeader(title = "IT Salary in Germany"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Overview Salary", tabName = "sheet1", icon = icon("server")),
      menuItem("Experienced Year", tabName = "sheet2", icon = icon("laptop")),
      menuItem("Salary on Map", tabName = "sheet3", icon = icon("laptop-house"))
      )
    ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "home",
              fluidRow( 
                box(
                  h3("Introduction"),
                  p("Salary is one of the most significant components in each individual’s life, as shown on the 
                  websites, IT relevant jobs, such as Data Scientists, Software Engineers and IT system 
                  managers, occupied a big part of the 20 Highest-Paying Jobs in the World (CareerAddict, 
                  2020). So, this report focuses on the yearly salary of IT scientists in Germany. The salary data is 
                  collected from an annually anonymous Survey from 2018 to 2020 year, and is resourced from Kaggle, 
                  which is an online community that allows users to upload and download open source data directly. We first observed the
                    overview of salary and under different factors, as the experienced year influenced salary most, we second visualized
                    the salary by different experienced year and the occupation of Gender, Seniority Level in different range of experienced
                    year. Lastly, we import the cost of living data of Germany, to draw a map, so that could visualize the incomes and outcomes
                    in Germany directly."), 
                  br(),
                  HTML('<center><img src="Germany.jpg" height="450px"></center>'),
                  br(),
                  h4("Sidebar tabs"),
                  tags$ul(
                    tags$li(tags$b("Overview Salary: "),"see the distribution of different years' salary and salary by factors."), 
                    tags$li(tags$b("Experienced Year: "),"Salary in different range of experienced year, and the occupation of Gerder and Seniority level."), 
                    tags$li(tags$b("Salary on Map: "), "Choropleths map colored by cost of living in cities of Germany, marke Salary points on it.")),
                  width = 12
                  ))),  # home tab close
      
      tabItem(tabName = "sheet1",
              fluidRow(
                box(h3("Overview of Salary in Germany"),
                    p("The distributions of Salary in different years are illustrated on the right side, use the selection bar to choose
                      one or more years for display. Average yearly salary is shown below."),
                    selectInput("Selected_Year",label = "Select Year",
                                choices = c(2018, 2019, 2020),
                                selected = NULL,
                                multiple = TRUE,
                                width = "400"),
                    valueBoxOutput("salary_box", width = 12),height = 380, width = 4),
                
                box(title = "Distrbution of Salary",
                  plotlyOutput("p_density", height = 320),height = 380,width = 8)
                ), # fluidrow close
              
              fluidRow(
                box(
                  title = "Tips", solidHeader = TRUE, status = "primary",
                  h4("- Single click on the legend to see an item"),
                  h4("- Double click on the legend to hidden an item"),
                  h4("- Move to the plots see more information"),
                  br(),
                  tags$li("The average salary in groups under each factor is significant different with each other (see bar plot)."),
                  br(),
                  br(),
                  prettyRadioButtons("Factor_choose", label = "Factor Choice",
                               choices = list("Experienced Year" = "experienced_year", 
                                              "Gender" = "Gender",
                                              "Senioriy Level" = "Seniority_level",
                                              "Company Size" = "company_size")), 
                               selected = "experienced_year", height = 380,width = 4),
                box(title = "Average Salary by Factor",
                    plotlyOutput("p_bar", height = 320), height = 380,width = 8)
                ) # fluidrow close
              ), # sheet1 tab close
      
      tabItem(tabName = "sheet2",
              fluidRow( 
                box(
                    plotlyOutput("p_line", height  = 320),height = 340,width = 6),
                box(
                  prettyRadioButtons("Exp_choose", label = "Experienced Year",
                               choices = levels(as.factor(salary_df$experienced_year)),
                               selected = "Less than 5", inline = TRUE),
                  br(),
                  infoBoxOutput("min_max_box", width = 12),
                  br(),
                  infoBoxOutput("avg_box", width = 12),height = 340,width = 6)
                ), # fluidrow close
              
              fluidRow(
                box(
                  valueBoxOutput("pie_intro",width = 12), height = 95, width = 12)
                ), # fluidrow close
              
              fluidRow(
                box(
                  title = "Gender occupation", 
                  plotlyOutput("p_pie1", height = 250), height = 310,width = 4, status = "primary"),
                box(
                  title = "Tips",
                  solidHeader = TRUE,
                  h5("- Single click on the legend to see an item"),
                  h5("- Double click on the legend to hidden an item"),
                  h5("- Move to the plots see more information"),
                  h3("Salary by experienced year", align = "center"),
                  tags$li("Experienced year influenced salary most, 21-25 years is the highest range."),
                  tags$li("After worked 21 years, there only contain males."),
                  tags$li("People worked 16 years and more, no junior level."),
                  height = 310,width = 4, status = "primary"),
                box(
                  title = "Seniority occupation",
                  plotlyOutput("p_pie2", height = 250),height = 310, width = 4, status = "primary")
                ) # fluidrow close
              ),  # sheet2 tab close
      
      tabItem(tabName = "sheet3",
              fluidRow(
                box(width = 4,
                       box(
                         h3("IT Salary on Germany Map", align = "center"),
                         p("On the right side is the Choropleths map of Germany, colored by the monthly cost 
                           of living in each city, meanwhile, the makers on the map represent the average salary range 
                           of them."),
                         tags$li("In general, a high cost of living city, has a high salary."),
                         br(),
                         h4("- Move to markers see yearly salary"),
                         h4("- Move to city see monthly cost"),
                         title = "Tips",solidHeader = TRUE, status = "primary", width = 12, height = 360),
                       box(title = "Data Source",solidHeader = TRUE, status = "primary",
                           br(),
                           div(tags$li(HTML(paste0("- IT Salary data from kaggle: ",
                                             a(href = "https://www.kaggle.com/parulpandey/2020-it-salary-survey-for-eu-region", 
                                               target='_blank', tags$b("Click here.")))))),
                           br(),
                           div(tags$li(HTML(paste0("- Germany GeoJSON: ",
                                             a(href = "https://github.com/isellsoap/deutschlandGeoJSON/blob/main/3_regierungsbezirke/4_niedrig.geo.json", 
                                               target='_blank', tags$b("Click here.")))))),
                           br(),
                           div(tags$li(HTML(paste0("- Germany cost of living: ",
                                             a(href = "https://livingcost.org/cost/germany/ni", 
                                               target='_blank', tags$b("Click here.")))))),
                           
                           width = 12, height = 360)
                       ),
                box(leafletOutput("p_map",height = 760),status = "primary", width = 8)
                ) # fluidrow close
              ))  # sheet2 tab close
  )
)

# Server of shiny
server <- function(input, output) {
  # Density plot
  output$p_density <- renderPlotly({
    # When none is selected, show all years
    if (is.null(input$Selected_Year)){
      selected_year <- salary_df
    }
    # Show the years that are selected
    else {
      print(input$Selected_Year)
      selected_year <- dplyr::filter(salary_df, Year  %in% input$Selected_Year)
    }
    ggplotly(selected_year %>%
      ggplot(aes(x = Current_Salary, group = as.factor(Year),color = as.factor(Year))) +
      geom_density(size = 1,alpha=.8) + 
      scale_color_manual(values = year_color) + 
      scale_x_continuous(labels = comma) + 
      scale_y_continuous(labels = comma) + 
      labs(col="",x = "Current Salary", y = "Density") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank()) ) %>%
      layout(legend=list(title=list(text='<b> Year </b>'), y = 0.5))
  })
  
  # Bar plot
  x_label_text <- reactive({
    sub("_", " ",input$Factor_choose)
  })
  
  output$p_bar <- renderPlotly({
    # When none is selected, show all years
    if (is.null(input$Selected_Year)){
      selected_year <- salary_df
    }
    # Show the years that are selected
    else {
      print(input$Selected_Year)
      selected_year <- dplyr::filter(salary_df, Year  %in% input$Selected_Year)
    }
    
    bar_df <- data.frame(Year = selected_year$Year, 
                         Current_Salary = selected_year$Current_Salary, 
                         a_factor = factor(selected_year[[input$Factor_choose]])
    )
    
    pp1 <- bar_df %>% 
      group_by(a_factor,Year) %>%
      summarize(mean = mean(Current_Salary),.groups = 'drop')
    
    ggplotly(ggplot(pp1, aes(x = a_factor,y = mean,fill = as.factor(Year))) +
      geom_col(position="dodge",alpha=.8) + 
      scale_fill_manual(values = year_color) +
      scale_y_continuous(labels = comma) + 
      labs(fill="",x = as.character(x_label_text()), y = "Average Salary") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank()))  %>%
      layout(legend=list(title=list(text='<b> Year </b>'), y = 0.5))
  })
  
  # Salary box
  output$salary_box <- renderValueBox({
    # When none is selected, show all years
    if (is.null(input$Selected_Year)){
      selected_year <- salary_df
    }
    # Show the years that are selected
    else {
      print(input$Selected_Year)
      selected_year <- dplyr::filter(salary_df, Year  %in% input$Selected_Year)
    }
    # calculate the average salary of selected years
    avg_salary <- selected_year %>% summarize(mean = mean(Current_Salary)) %>% round(2)
    # value box
    valueBox(
      paste0(avg_salary," €"), 
      "Yearly average Salary", 
      icon = icon("money-check"),
      color = "light-blue"
    )
  })
  
  # Line plot
  output$p_line <- renderPlotly({
    # calculate avg, min and max
    com_sum <- salary_df %>% group_by(experienced_year,Year) %>% 
      summarise(avg_salary = round(mean(Current_Salary)), 
                min_salary = round(min(Current_Salary)),
                max_salary = round(max(Current_Salary)),
                .groups = 'drop')
    
    # create the vactor of color
    line_color <- c("Avg salary" = "blue", "Min salary" = "red", "Max salary" = "darkgreen")
    
    # line plot
    ggplotly(com_sum %>% 
      ggplot(aes(x= experienced_year,group=Year)) + 
      geom_point(aes(y = avg_salary)) +
      geom_line(aes(y = avg_salary,color="Avg salary")) + 
      geom_point(aes(y = min_salary)) +
      geom_line(aes(y = min_salary,color="Min salary")) + 
      geom_point(aes(y = max_salary)) +
      geom_line(aes(y = max_salary,color="Max salary")) + 
      facet_grid(Year ~.) +
      scale_color_manual(values = line_color) + 
      scale_y_continuous(labels = comma) + 
      labs(col="",x = "", y = "") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.border = element_blank()))  %>%
      layout(legend=list(orientation = 'h', x = 0.1, y = 1.2),
             xaxis = list(title = 'Experienced Year'),
             yaxis = list(title = 'Salary'))
  })
  
  # Min max box
  output$min_max_box <- renderInfoBox({
    selected_exp <- dplyr::filter(salary_df, experienced_year  %in% input$Exp_choose)
    com_df <- selected_exp %>% group_by(experienced_year) %>% 
      summarise(min_salary = round(min(Current_Salary)),
                max_salary = round(max(Current_Salary)))
    infoBox(
      "Min & Max Salary", HTML(paste0("Min: ",
                               format(com_df$min_salary,big.mark=",",scientific=FALSE),
                               " €",
                               br(),
                               "Max: ",
                               format(com_df$max_salary,big.mark=",",scientific=FALSE),
                               " €")), 
      icon = icon("list"),
      color = "light-blue")
  })
  
  # Avg box with relative number
  output$avg_box <- renderInfoBox({
    selected_exp <- dplyr::filter(salary_df, experienced_year  %in% input$Exp_choose)
    avg_df <- selected_exp %>% group_by(experienced_year) %>% 
      summarise(avg_salary = round(mean(Current_Salary)))
    total_avg <- salary_df %>% summarize(mean = mean(Current_Salary))
    dif_per <- round((avg_df$avg_salary - total_avg) / total_avg * 100,2)
    
    infoBox(
      "Average Salary", HTML(paste0('"',as.character(input$Exp_choose),'"', 
                               " average salary: ",
                                 format(avg_df$avg_salary,big.mark=",",scientific=FALSE),
                                 " €",
                               br(),
                               dif_per,
                                 " % than total average salary")), 
      icon = icon("money-check-alt"),
      color = "light-blue")
  })
  
  # Pie introduction box
  output$pie_intro <- renderValueBox({
    valueBox(
      div(h3(paste0("Consists of (", input$Exp_choose, ") experienced year")),class = "text-center"), 
      "", 
      color = "light-blue"
    )
  })
  
  # Gender pie
  output$p_pie1 <- renderPlotly({
    # count the Gender
    pie_df <- dplyr::filter(salary_df, experienced_year  %in% input$Exp_choose) %>% count(Gender) 
    
    pie_df %>% plot_ly(labels = ~Gender, values = ~n) %>% 
      add_pie(hole = 0.4) %>% 
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # Seniority pie
  output$p_pie2 <- renderPlotly({
    # count the Seniority_level
    pie_df <- dplyr::filter(salary_df, experienced_year  %in% input$Exp_choose) %>% count(Seniority_level)
    
    pie_df %>% plot_ly(labels = ~Seniority_level, values = ~n) %>% 
      add_pie(hole = 0.4) %>% 
      layout(showlegend = T,
             xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })
  
  # Choropleths map
  output$p_map <- renderLeaflet({
    # Set Choropleths map color
    bins <- c(700,900,1100,1300,1500,1700,1900)
    pal <- colorBin("Reds", domain = spdf_1$cost_of_living, bins = bins)
    # Set salary makers color
    salary_color <- colorFactor(palette = "YlGnBu", 
                                domain = city_table_1$mean_salary) 
    # Map label
    labels <- sprintf(
      "<strong>%s</strong><br/>cost of living: %g €",
      spdf_1$NAME_2, spdf_1$cost_of_living
    ) %>% lapply(htmltools::HTML)
    
    # Pop up content
    content <- paste(sep = "<br/>",
                     "<b><a", city_table_1$City,"Yearly salary (€)","</a></b>",
                     city_table_1$mean_salary)
    
    # Draw Choropleths map
    m <- leaflet(spdf_1) %>%
      setView(10.4515, 51.1657, 6) %>% 
      addTiles() %>% addPolygons(
        fillColor = ~pal(cost_of_living),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addLegend(pal = pal, values = ~cost_of_living, opacity = 0.7, title = "Monthly Cost of Living",
                position = "topleft")
    
    # Add salary makers to the map
    m %>% addCircleMarkers(leaflet(city_table_1) %>% addTiles(), 
                           lat=~city_table_1$lat, lng = ~city_table_1$lon,
                           popup = content,
                           radius = 8,
                           color = ~salary_color(city_table_1$mean_salary),
                           stroke = TRUE,
                           fillOpacity = 0.9) %>%
      addLegend(pal = salary_color, values = ~city_table_1$mean_salary,opacity = 1,position = "bottomleft",title = "Yearly Average Salary")   %>%
      addProviderTiles(providers$Stamen.TonerLite)
    
  })
}

# Run shiny
shinyApp(ui, server)