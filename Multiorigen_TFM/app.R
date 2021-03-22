## MULTIORIGEN - app de seguimiento y predicción de casos covid
## Raquel Colorado, Universidad Complutense de Madrid

# Actualizar datos con scripts automáticos
source("jrc_update.R")
source("oms_update.R")

# Carga e instalación de paquetes y librerías


if(!require(magrittr)) install.packages("magrittr", repos = "http://cran.us.r-project.org")
if(!require(rvest)) install.packages("rvest", repos = "http://cran.us.r-project.org")
if(!require(readxl)) install.packages("readxl", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(maps)) install.packages("maps", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(reshape2)) install.packages("reshape2", repos = "http://cran.us.r-project.org")
if(!require(ggiraph)) install.packages("ggiraph", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
if(!require(geojsonio)) install.packages("geojsonio", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(shinythemes)) install.packages("shinythemes", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")
if(!require(RMySQL)) install.packages("RMySQL", repos ="http://cran.us.r-project.org" )
if(!require(readr)) install.packages("readr", repos ="http://cran.us.r-project.org" )
if(!require(datasets)) install.packages("datasets", repos ="http://cran.us.r-project.org" )
if(!require(rgdal)) install.packages("rgdal", repos ="http://cran.us.r-project.org" )
library(Hmisc)
library(magrittr)
library(rvest)
library(readxl)
library(dplyr)
library(maps)
library(ggplot2)
library(reshape2)
library(ggiraph)
library(RColorBrewer)
library(leaflet)
library(plotly)
library(geojsonio)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinythemes)
library(RMySQL)
library(readr)
library(datasets)
library(rgdal)

# Establecer un color para cada fuente de datos
jrc_col = "#22CCC7"
oms_col = "#045a8d"

# Importar datos
jrc_cases = read.csv("input_data/jrc_data.csv")
oms_cases = read.csv("input_data/oms_data.csv")

### DATA PROCESSING: COVID-19

# última fecha actualizada
update1 = tail(jrc_cases$fecha,1) 
update2 = tail(jrc_cases$fecha, 1)


oms_cases$lat <- NA
oms_cases$lon <- NA
oms_cases$iso3 <- NA
casos <- rbind(jrc_cases, oms_cases)

# Fecha de jrc data
if (any(grepl("/", casos$date))) { 
  casos$fecha = format(as.Date(casos$fecha, format="%d/%m/%Y"),"%Y-%m-%d") 
} else { casos$fecha = as.Date(casos$fecha, format="%Y-%m-%d") }
casos$fecha = as.Date(casos$fecha)
jrc_min_date = as.Date(min(casos$fecha),"%Y-%m-%d")
current_date = as.Date(max(casos$fecha),"%Y-%m-%d")
jrc_max_date_clean = format(as.POSIXct(current_date),"%d %B %Y")

# Media casos 

casos_media =
  casos %>%
  group_by(pais, fecha) %>%                        
  summarise(media_casos = mean(casos, na.rm = TRUE), media_fallecidos = mean(fallecidos, na.rm = TRUE))

# Suma de casos por fechq
casos_aggregated = aggregate(casos$casos_acumulados, by = list(casos$fecha), FUN = sum)
names(casos_aggregated) = c("fecha", "casos")

# Casos en los últimos 7 días
for (i in 1:nrow(casos_aggregated)) { 
  if (i==1) { casos_aggregated$new[i] = 0 }
  if (i>1) { casos_aggregated$new[i] = casos_aggregated$casos[i] - casos_aggregated$casos[i-1] }
  if (i>1 && casos_aggregated$new[i]< 0 ) {casos_aggregated$new[i] = 0}
}

# Region
casos_aggregated$region = "Europa"
casos_aggregated$fecha = as.Date(casos_aggregated$fecha,"%Y-%m-%d")

### MAP FUNCTIONS ###

# Función gráficos de covid
cumulative_plot = function(casos_aggregated, plot_date) {
  plot_df = subset(casos_aggregated, fecha<=plot_date)
  g1 = ggplot(plot_df, aes(x = fecha, y = casos, color = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    ylab("Casos acumulados") +  xlab("Fecha") + theme_bw() + 
    scale_colour_manual(values=c(jrc_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 100000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

# function to plot new COVID cases by date
new_cases_plot = function(casos_aggregated, plot_date) {
  plot_df_new = subset(casos_aggregated, fecha<=plot_date)
  g1 = ggplot(plot_df_new, aes(x = fecha, y = new, colour = region)) + geom_line() + geom_point(size = 1, alpha = 0.8) +
    # geom_bar(position="stack", stat="identity") + 
    ylab("Nuevos casos semanales") + xlab("Fecha") + theme_bw() + 
    scale_colour_manual(values=c(jrc_col)) +
    scale_y_continuous(labels = function(l) {trans = l / 100000; paste0(trans, "M")}) +
    theme(legend.title = element_blank(), legend.position = "", plot.title = element_text(size=10), 
          plot.margin = margin(5, 12, 5, 5))
  g1
}

casos_today = subset(casos_media, fecha==current_date) 
current_case_count = as.numeric(sum(casos_today$media_casos))
current_death_count = as.numeric(sum(casos_today$media_fallecidos))

# Parámetros gráficos del mapa
bins = c(0,10,50,100,500,1000,Inf)
cv_pal <- colorBin("Blues", domain = bins, bins = bins)
plot_map <- Europe[Europe$adm0_a3 %in% jrc_cases$iso3, ]

# Especificamos las fuentes
fuentes <- c("Joint Research Centre", "European Centre of Disease and Control", "World Health Organization (OMS)", "JHU")

# Descargamos los datos geográficos de Europa
Europe <- rgdal::readOGR(dsn = "https://data.opendatasoft.com/explore/dataset/european-union-countries@public/download/?format=geojson&timezone=Europe/Berlin", 
                         layer = "OGRGeoJSON", 
                         stringsAsFactors = FALSE)


### SHINY UI ###

ui <- bootstrapPage(
  tags$head(includeHTML("gtag.html")),
  navbarPage(theme = shinytheme("paper"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#22CCC7 ;" class="active" href="#">MULTIORIGEN</a>'), id="nav",
             windowTitle = "Plataforma Multiorigen",
             
             tabPanel("Updating data",
                      div(class="outer",
                          mainPanel(selectInput("Cargadatos",
                                                label = h6("Selecciona la fuente de datos"),
                                                choices = fuentes,
                                                selected = fuentes[1]),
                                    actionButton("carga", "Upload", icon("paper-plane"), 
                                                 style="color: #08B2AD"),
                                    downloadButton("downloadCsv", "Download as CSV", style="color: #08B2AD"),
                                    
                                    textOutput("text"),
                                    div(dataTableOutput("mytable"), style = "font-size:100%"))
                      ) 
             ),
             tabPanel("Mapping data",
                      div(class="outer",
                          
                          
                          column(width = 10, 
                                 leaflet::leafletOutput(outputId = "mymap", height = 850)),
                          
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 250, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        span(tags$i(h6("Los casos reportados están sujetos a las políticas y estrategias de cada país.")), style="color:#045a8d"),
                                        h3(textOutput("reactive_case_count"), align = "right"),
                                        h4(textOutput("reactive_death_count"), align = "right"),
                                        h6(textOutput("clean_date_reactive"), align = "right"),
                                        h6(textOutput("reactive_country_count"), align = "right"),
                                        plotOutput("epi_curve", height="130px", width="100%"),
                                        plotOutput("cumulative_plot", height="130px", width="100%"),
                                        
                                        sliderTextInput("plot_date",
                                                        label = h6("Select mapping date"),
                                                        choices = format(unique(casos$fecha), "%d %b %y"),
                                                        selected = format(current_date, "%d %b %y"),
                                                        grid = FALSE,
                                                        animate=animationOptions(interval = 3000, loop = FALSE))
                                        
                                        
                                        
                          )
                          
                      )
                      
                      
                      
                      
                      
             ),
             tabPanel("Comparing data",
                      div(class="outer",
                          mainPanel(
                            
                            # Select 
                            selectInput(inputId = "bbdd", label = strong("Fuente de datos"),
                                        choices = unique(casos$bbdd),
                                        selected = "JRC"),
                            
                            # Select date range to be plotted
                            dateRangeInput("fecha", strong("Rango fecha"), start = "2020-03-01", end = max(casos$fecha),
                                           min = "2020-03-01", max = max(casos$fecha)),
                            
                            
                            # Select cases or deaths
                            checkboxGroupInput("checkGroup", 
                                               label = strong ("Escoge una opción"), 
                                               choices = list("Casos",
                                                              "Fallecidos"
                                               ),
                                               selected = "Casos"),
                            
                            
                            fluidRow(plotlyOutput("ts_plot", height = "500px"))
                            
                          )
                      )
                      
             ),
             tabPanel("Connecting",
                      div(class = "outer",
                          tags$br(),tags$br(),tags$h5("Pasos previos"), 
                          
                          "Para conectar los datos con el programa MySQL deberás leer la documentación correspondiente: Manual multiorigen. En este archivo encontrarás las instrucciones paso a paso para poder llevar a cabo la conexión con la base de datos. También existe la posibilidad de conectarse al programa Tableau para visualizar los datos de forma clara y dinámica.",
                          
                          actionButton("sql", "Connect with MySQL", icon("paper-plane"), 
                                       style="color: #08B2AD"),
                          dataTableOutput("maestra"), style = "font-size:100%"))
             
             
             
             
  ))





### SHINY SERVER ###

server = function(input, output, session) {
  # Upload tab
  observeEvent(input$carga,{
    if(input$Cargadatos == "Joint Research Centre"){
      source("jrc_update.R")
      jrc_cases <- read_csv("input_data/jrc_data.csv")
      output$mytable = renderDataTable({jrc_cases},  options = list(pageLength = 10, lengthChange = FALSE)
      )} 
    else if (input$Cargadatos == "World Health Organization (OMS)"){
      source("oms_update.R")
      oms_cases <- read_csv("input_data/oms_data.csv")
      output$mytable = renderDataTable({oms_cases},  options = list(pageLength = 10, lengthChange = FALSE)
      )}
    
    else {output$text <- renderText("Error: por el momento, no podemos actualizar estas fuentes de datos")
    output$mytable = NULL
    }
    
  })
  
  output$downloadCsv <- downloadHandler(
    filename = function() {
      paste("COVID_data_", jrc_cases$fecha[1], ".csv", sep="")
    },
    content = function(file) {
      if(input$Cargadatos == "Joint Research Centre"){
        write.csv(jrc_cases, file)}
      else if(input$Cargadatos == "World Health Organization (OMS)"){
        write.csv(oms_cases, file)}
      
    }
  )
  
  # Comparison tab
  
  formatted_date = reactive({
    format(as.Date(input$plot_date, format="%d %b %y"), "%Y-%m-%d")
  })
  
  output$clean_date_reactive <- renderText({
    format(as.POSIXct(formatted_date()),"%d %B %Y")
  })
  
  reactive_db = reactive({
    casos %>% filter(fecha == formatted_date())
  })
  
  reactive_db_last7d = reactive({
    casos %>% filter(fecha == formatted_date() & casos > 0)
  })
  
  reactive_db_large = reactive({
    large_countries = reactive_db() %>% filter(iso3 %in% Europe$adm0_a3)
    Europe_data_subset = Europe[Europe$adm0_a3 %in% large_countries$iso3, ]
    large_countries = large_countries[match(Europe_data_subset$ISO3, large_countries$iso3),]
    large_countries
  })
  
  reactive_db_large_last7d = reactive({
    large_countries = reactive_db_last7d() %>% filter(iso3 %in% Europe$adm0_a3)
    large_countries = large_countries[order(large_countries$iso3),]
    large_countries
  })
  
  reactive_polygons = reactive({
    Europe[Europe$adm0_a3 %in% reactive_db_large()$iso3, ]
  })
  
  reactive_polygons_last7d = reactive({
    Europe[Europe$adm0_a3 %in% reactive_db_large_last7d()$iso3, ]
  })
  
  output$reactive_case_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$casos), big.mark=","), " casos")
  })
  
  output$reactive_death_count <- renderText({
    paste0(prettyNum(sum(reactive_db()$fallecidos), big.mark=","), "fallecidos")
  })
  
  output$reactive_country_count <- renderText({
    paste0(nrow(subset(reactive_db(), pais!="Diamond Princess Cruise Ship")), " paises afectados")
  })
  
  output$reactive_new_cases_7d <- renderText({
    paste0(round((casos_aggregated %>% filter(fecha == formatted_date() & region == "Europa"))$new/7,0), " 7-day average")
  })
  
  # create foundational map
  foundational.map <- reactive({
    leaflet::leaflet(Europe) %>% 
      fitBounds(-20, 65, 20, 39) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addLayersControl(
        position = "bottomright",
        overlayGroups = c("Joint Research Centre", "OMS", "European Centre of Disease and Control", "JHU"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup(c("OMS", "European Centre of Disease and Control", "JHU")) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(~-100,-60,~60,70) %>%
      leaflet::addLegend("bottomright", pal = cv_pal, values = ~casos$fallecidos_acumulados,
                         title = "<small>Fallecidos</small>") 
    
    #addPolygons(data = Europe, 
    #layerId = Europe$name_long, 
    #color = ~cv_pal(jrc_cases$fallecidos), 
    #weight = 2, 
    #fillOpacity = 0.3, 
    #opacity = 1,
    #smoothFactor = 0.2,
    #stroke = FALSE) 
  })
  
  output$mymap <- leaflet::renderLeaflet({
    foundational.map()
  }) 
  
  # add countries to selection
  observeEvent(input$plot_date, {
    leaflet::leafletProxy("mymap") %>% 
      
      addCircleMarkers(data = casos %>% filter(bbdd == "JRC"), color = jrc_col, weight = 1, radius = ~(casos)^(1/5.5),group = "Joint Research Centre", popup = casos$fallecidos_acumulados) %>%
      addCircleMarkers(data = casos %>% filter(bbdd == "OMS"), color = oms_col,weight = 1, radius = ~(casos)^(1/5.5), group = "OMS", popup = casos$fallecidos_acumulados) %>%
      addPolygons(data = Europe, 
                  layerId = Europe$name_long, 
                  color = ~cv_pal(casos$fallecidos_acumulados), 
                  weight = 2, 
                  fillOpacity = 0.3, 
                  opacity = 1,
                  smoothFactor = 0.2,
                  stroke = FALSE
                  )
    
    
    lines.of.interest <- Europe[ which(Europe$name_long %in%casos$pais), ]
    leaflet::leafletProxy(mapId = "mymap") %>%
      addPolylines(data = lines.of.interest, 
                   layerId = lines.of.interest$ids, 
                   color = "#6cb5bc", 
                   weight = 2, 
                   opacity = 1,
                   group = "lin")
  })
  
  
  
  
  
  
  output$cumulative_plot <- renderPlot({
    cumulative_plot(casos_aggregated, formatted_date())
  })
  
  output$epi_curve <- renderPlot({
    new_cases_plot(casos_aggregated, formatted_date())
  })
  
  
  
  # Subset data
  selected_bbdd <- reactive({
    req(input$fecha)
    shiny::validate(need(!is.na(input$fecha[1]) & !is.na(input$fecha[2]), "Error: Por favor, selecciona una fecha inicial y final."))
    shiny::validate(need(input$fecha[1] < input$fecha[2], "Error: Start date should be earlier than end date."))
    casos %>%
      filter(
        pais == "Spain",
        bbdd == input$bbdd,
        fecha > (input$fecha[1]) & fecha < (input$fecha[2]),
        casos == casos,
        fallecidos == fallecidos
      )
    
  })
  
  
  output$ts_plot <- renderPlotly({
    if (input$checkGroup == "Casos"){
      ggplot(selected_bbdd(), aes(x=fecha, y=casos)) +
        geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") +
        xlab("Fecha")+
        ylab("Casos positivos") +
        theme_minimal()}
    
    else if (input$checkGroup == "Fallecidos"){
      ggplot(selected_bbdd(), aes(x=fecha, y=fallecidos)) +
        geom_area(fill="#69b3a2", alpha=0.5) +
        geom_line(color="#69b3a2") +
        xlab("Fecha")+
        ylab("Fallecidos") +
        theme_minimal()
    }
  })
  
  ## Connecting tab
  
  observeEvent(input$sql,{
    # Driver
    driver=dbDriver("MySQL")
    # Definimos la conexion y el nombre de la base de datos
    conexion = dbConnect(driver,host="localhost",user="root", db = "tfm_covid19")
    # Eliminamos tabla maestra antes de cargar la nueva
    dbRemoveTable(conexion, "maestra")
    # Cargamos los nuevos datos actualizados
    dbWriteTable(conexion, "maestra", datafinal, append = TRUE, row.names = FALSE)
    paste0("Conexión realizada con éxito")
    
    tabla = dbReadTable(conexion, "maestra")
    output$maestra = renderDataTable({tabla},  options = list(pageLength = 10, lengthChange = FALSE))
  })
  
  
  
  
  
  
  
}





runApp(shinyApp(ui, server), launch.browser = TRUE)



