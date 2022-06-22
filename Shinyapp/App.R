if (!require("leaflet")) install.packages("leaflet")
if (!require("semantic.dashboard")) install.packages("semantic.dashboard")
if (!require("shiny")) install.packages("shiny")
if (!require("DT")) install.packages("DT")
library(shiny)
library(semantic.dashboard)
library(leaflet)
library(DT)


data = read.csv("accident-dataV2.csv")

ui <- dashboardPage(
  dashboardHeader(dropdownMenuOutput("dropdown"),
                  dropdownMenu(type = "notifications",
                               taskItem("Project progress...", 50.777, color = "red")),
                  dropdownMenu(
                    notificationItem("This is an important notification!", color = "red"))),
  dashboardSidebar(side = "left",
                   sidebarMenu(
                     menuItem(tabName = "plot_tab", text = "Main Dashboard", icon = icon("home")),
                     menuItem(tabName = "table_tab", text = "Dataframe", icon = icon("browser")))),
  dashboardBody(
    tabItems(
      tabItem(tabName = "plot_tab",
              fluidRow(
                box(title = "Filters and Slicers", color = "orange", width = 4,
                    selectInput(inputId =  "accident_severity", choices = unique(data$accident_severity),
                                label = "Accident Severity", selected = "Fatal"),
                    selectInput(inputId =  "weather", choices = unique(data$weather_conditions),
                                label = "Weather", selected = "Fine no high winds"),
                ),
                box(title = "Filters 2",color = "orange", width = 4,
                    selectInput(inputId =  "road type", choices = unique(data$road_type),
                                label = "Road Type", selected = "Single carriageway"),
                    selectInput(inputId =  "surface condition", choices = unique(data$road_surface_conditions),
                                label = "Surface Condition", selected = "Dry")
                ),
                box(title = "Filters 3",color = "orange", width = 4,
                    selectInput(inputId =  "light_conditions", choices = unique(data$light_conditions),
                                label = "Light Condition", selected = ""),
                    selectInput(inputId =  "hazards", choices = unique(data$carriageway_hazards),
                                label = "Hazards", selected = "Select")
                ),
                box(title = "Filters 4",color = "orange", width = 4,
                    selectInput(inputId =  "urban", choices = unique(data$urban_or_rural_area),
                                label = "Urban or Rural", selected = ""),
                    selectInput(inputId =  "speed_limit", choices = unique(data$speed_limit),
                                label = "Speed Limit", selected = "70")
                ),
              ),
              fluidRow(
                box(title = "Map plot", color = "purple", width = 10,
                    leafletOutput("mymap")),
                
                tabBox(title = "Additional plots", color = "blue", width = 6,
                       collapsible = FALSE,
                       tabs = list(
                         list(menu = "First Tab", content = plotOutput("plot1")),
                         list(menu = "Second Tab", content = plotOutput("plot2"))
                       ))),
              
      ),
      tabItem(tabName = "table_tab",
              fluidRow(
                valueBox("Fatal", nrow(data[data$accident_severity=="Fatal",]),  color = "red", width = 6, size = "big"),
                valueBox("Serious", nrow(data[data$accident_severity=="Serious",]),  color = "yellow", width = 5, size = "big"),
                valueBox("Slight", nrow(data[data$accident_severity=="Slight",]), color = "green", width = 5, size = "big")
              ),
              fluidRow(
                box(title = "Data Frame", color = "blue", ribbon = FALSE,
                    title_side = "top left", width = 16,
                    tags$div(
                      dataTableOutput("table1")
                      , style = paste0("color:", semantic_palette[["blue"]], ";"))
                )
              )
      )
    )
  )
)



server <- function(input, output) {
  
  conditional <- function(condition, success) {
    if (condition) success else TRUE
  }
  
  reactiveDf <- reactive({
    data %>%
      filter(
        conditional(input$accident_severity != "", accident_severity == input$accident_severity),
        conditional(input$speed_limit != "", speed_limit == input$speed_limit),
        conditional(input$light_conditions != "", light_conditions == input$light_conditions)
      )
  })
  
  dt <- reactive({
    
    data[data$accident_severity == as.character(input$accident_severity) &
           data$weather_conditions == as.character(input$weather) &
           data$speed_limit == as.character(input$speed_limit),]
    
  })
  
  output$plot1 <- renderPlot(barplot(table(data$accident_severity, data$time),
                                     xlab = "Accidents by Time of Day",
                                     col = rainbow(3)
  ))
  output$plot2 <- renderPlot(barplot(table(data$accident_severity, data$day_of_week),
                                     xlab = "Accidents by Day of Week",
                                     col = rainbow(3)
  ))
  
  output$pie1 <- renderPlot(pie(data$number_of_casualties,
                                labels = "No. of Casualties",
                                col = rainbow(ncol(data$number_of_casualties))
  ))
  
  output$table1 <- renderDataTable(dt())
  
  output$dropdown <- renderDropdownMenu({
    dropdownMenu(messageItem("User", "Test message", color = "teal", style = "min-width: 200px"),
                 messageItem("Users", "Test message", color = "teal", icon = "users"),
                 messageItem("See this", "Another test", icon = "warning", color = "red"))
  })
  
  output$mymap <- renderLeaflet({
    df <- dt()
    leaflet() %>% setView(lng = -1.1893, lat = 52.35, zoom = 6) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addCircleMarkers(lng = df$longitude, lat = df$latitude, radius = 4) 
  })
}

shinyApp(ui, server)