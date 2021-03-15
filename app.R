library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(RColorBrewer)
library(leaflet.extras) 

df00 <- read.csv("eGRID2000_plant.csv")
df10 <- read.csv("eGRID2010_Data.csv")
df18 <- read.csv("egrid2018_data_v2.csv")

#2000
lon2000 <- as.numeric(gsub("","",df00$X.23))
lati2000 <- as.numeric(gsub("","",df00$X.22))
name2000 = df00$X.2

#2010
lon2010 <- as.numeric(gsub("","", df10$X.20))
lati2010 <- as.numeric(gsub("","",df10$X.19))
name2010 = df10$X.1

#2018 
lon2018 <- as.numeric(gsub("","",df18$Plant.longitude))
lati2018<- as.numeric(gsub("","",df18$Plant.latitude))
name2018 = df18$Plant.name

other <- data.frame(other1 = c(df18$Plant.annual.other.fossil.net.generation..MWh.), 
                    other2 = c(df18$Plant.annual.other.unknown..purchased.fuel.net.generation..MWh.) )
other$OTHER <- paste(other$other1, other$other2)

renewable <- c("HYDRO", "BIO", "WIND", "SOLAR", "GEO")
non_renewable <- c("COAL", "OIL", "GAS", "NUCLEAR", "OTHER")
energySourcesList <- c("COAL", "OIL", "GAS", "NUCLEAR", "HYDRO", "BIO", "WIND", "SOLAR", "GEO", "OTHER",
                       "RENEWABLE", "NON-RENEWABLE", "ALL")
#states
states <- c(1:50)
names(states)<- state.name
centers <- state.center

#Colors for maps
listColorsNames<- c("COAL" = "red", "GAS" = "yellow")
listColor = c("red","yellow")
pal = colorFactor(palette = listColor, levels = listColorsNames)

ui <- dashboardPage(
  dashboardHeader(title = "Raw Power"),
  dashboardSidebar(disable = FALSE, collapsed = FALSE,
                   sidebarMenu(
                     menuItem("Illinois map", tabName = "ILLMap", icon = NULL),
                     menuItem("Comparing Maps", tabName = "comMap", icon = NULL),
                     menuItem("US ViewMap", tabName = "viewMap", icon = NULL),
                     menuItem("About", tabName = "about", icon = NULL)
                   )),
  dashboardBody(
    tabItem(tabName = "ILLMap",
            fluidRow(
              column(2, checkboxGroupInput("energySources","Options:", energySourcesList, selected = "ALL")),
              column(10, box(title = "Location: ILLINOIS", solidHeader = TRUE, status = "primary", width = 12, leafletOutput("map2018", width = 600, height = 500)))
            )
          ),
    tabItem(
      tabName = "comMap",
      fluidRow(
        column(2, checkboxGroupInput("energySources","Options:", energySourcesList, selected = "ALL")),
        
        column(5, tabBox(title = "Map 1", selected = "Year: 2000"),
                tabPanel("Year: 2000", leafletOutput("opt2000", width = 500, height = 500))
               ),
        column(5, tabBox(title = "Map 2"),
               tabPanel("Year: 2018", leafletOutput("opt2018", width = 500, height = 500))
        
      ),
      column(2, checkboxGroupInput("energySources","Options:", energySourcesList, selected = "ALL")) 
    )
    ),
    tabItem(
      tabName = "viewMap" , 
      fluidRow(
        column(2, box(title = "Energy Generation", status = "primary", 
                      sliderInput("range", "Energy Generation range(energy plants)", min=0, max = 32000000, value = 32000000, width = 300),
                      leafletOutput("map", width = 800, height = 500))
               )
        #column()
      )
    ),
    tabItem(tabName = "about", h2("About"),
           # mainPanel(
            h4("This project is developed by Hiral Modi."),
            h5("The original data is available from https://www.epa.gov/egrid/download-data .")#),
            
            ),
          )
) #ui close

server <- function(input, output) {
  set.seed(122)
  
  #checkbox illinois
  # if ("ALL" %in% input$energySources){df18}
  # esle if("RENEWABLE" %in% input$energySources){df18[df18$]})
  
  initial_lat = 39.707778570701505
  initial_lon = -89.02689460317609
  initial_zoom = 6
  
  output$map2018 <- renderLeaflet({
    ILLongitude <- subset(lon2018, df18$Plant.state.abbreviation =='IL')
    ILLatitude <- subset(lati2018, df18$Plant.state.abbreviation =='IL')
    ILName <- subset(name2018, df18$Plant.state.abbreviation =='IL')
    
    leaflet(df18) %>%
      #set map to Illinois state,   ,
      setView(lng = initial_lon, lat =initial_lat, zoom =initial_zoom) %>%
      addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
      addResetMapButton()%>%      #https://rdrr.io/github/bhaskarvk/leaflet.extras/man/addResetMapButton.html
      addCircleMarkers(
        lng =~ILLongitude, lat=~ILLatitude, radius=~5,
        stroke = FALSE, 
        fillOpacity = 0.5,
        popup = paste(
          "<b>",ILName, "</b><br/>") #,
        #color =~pal()
      ) %>%
      
     addLegend("bottomleft", pal=pal, values =~energySourcesList ,opacity =1, title="Energy Sources")
  })
  
 output$opt2000 <- renderLeaflet({
   ILLon2000 <- subset(lon2000, df00$X.1 == 'IL')
   ILLati2000 <- subset(lati2000, df00$X.1 == 'IL')
   ILName2000 <- subset(name2000, df00$X.1=='IL')
   
   leaflet(df00) %>%
     setView(lng = initial_lon, lat =initial_lat, zoom =4) %>%
     addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
     addResetMapButton()%>%      #https://rdrr.io/github/bhaskarvk/leaflet.extras/man/addResetMapButton.html
     addCircleMarkers(
       lng =~ILLon2000, lat=~ILLati2000, radius=~5,
       stroke = FALSE, fillOpacity = 0.5,
       color = "blue" ,
       popup = paste(
         "<b>",ILName2000, "</b><br/>")
       
     ) 
 }) 
 output$opt2018 <- renderLeaflet({
   ILLongitude <- subset(lon2018, df18$Plant.state.abbreviation =='IL')
   ILLatitude <- subset(lati2018, df18$Plant.state.abbreviation =='IL')
   ILName <- subset(name2018, df18$Plant.state.abbreviation =='IL')
   
   leaflet(df18) %>%
     #set map to Illinois state,   ,
     setView(lng = initial_lon, lat =initial_lat, zoom =6) %>%
     addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
     addResetMapButton()%>%      #https://rdrr.io/github/bhaskarvk/leaflet.extras/man/addResetMapButton.html
     addCircleMarkers(
       lng =~ILLongitude, lat=~ILLatitude, radius=~5,
       stroke = FALSE, 
       fillOpacity = 0.5,
       popup = paste(
         "<b>",ILName, "</b><br/>") #,
       #color =~pal()
     ) %>%
     
     addLegend("bottomleft", pal=pal, values =~energySourcesList ,opacity =1, title="Energy Sources")
 })
 
 output$map <-renderLeaflet({
   ILLon2000 <- subset(lon2000, df00$X.1 == 'IL')
   ILLati2000 <- subset(lati2000, df00$X.1 == 'IL')
   ILName2000 <- subset(name2000, df00$X.1=='IL')
   
   leaflet(df00) %>%
     setView(lng = initial_lon, lat =initial_lat, zoom =4) %>%
     addProviderTiles("CartoDB.Positron", options = providerTileOptions(noWrap = TRUE)) %>%
     addResetMapButton()%>%      #https://rdrr.io/github/bhaskarvk/leaflet.extras/man/addResetMapButton.html
     addCircleMarkers(
       lng =~ILLon2000, lat=~ILLati2000, radius=~5,
       stroke = FALSE, fillOpacity = 0.5,
       color = "blue" ,
       popup = paste(
         "<b>",ILName2000, "</b><br/>")
       
     ) 
 })
}

shinyApp(ui, server)
