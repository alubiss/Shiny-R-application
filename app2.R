library(shiny)

library(dplyr)
library("ggplot2")
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(ggmosaic)
library(shinydashboard)
library(shinyjs)
library(bubbles)
library(rgdal)
library(leaflet)
library(tidyverse)
library(ggalluvial)

setwd("/Users/alubis/Downloads")
dane=read.csv("WHO-COVID-19-global-data.csv", sep=",")
dane$Date_reported = as.Date(dane$Date_reported)
dane$Country[dane$Country == 'United States of America'] <-'United States'

# mapa
world <- ne_countries(scale = "medium", returnclass = "sf")
world$name[world$name == 'Russia'] <- 'Russian Federation'

world_spdf <- readOGR( 
    dsn = '/Users/alubis/Desktop/Visualization in R/zaj 7/Data/',
    layer ="TM_WORLD_BORDERS_SIMPL-0.3", 
    verbose = FALSE
)
world_spdf@data$NAME[world_spdf@data$NAME == 'Russia'] <- 'Russian Federation'
world <- ne_countries(scale = "medium", returnclass = "sf")


# covid data
dane3=read.csv("COVIDData.csv", sep=",")

# populacje
dane3$Age= 0
dane3$Age[dane3$Age_0.9 == 1] <- '0-9'
dane3$Age[dane3$Age_10.19 == 1] <- '10-19'
dane3$Age[dane3$Age_20.24 == 1] <- '20-24'
dane3$Age[dane3$Age_25.59 == 1] <- '25-59'
dane3$Age[dane3$Age_60. == 1] <- '> 60'

dane3$Gender= 0
dane3$Gender[dane3$Gender_Female == 1] <- 'female'
dane3$Gender[dane3$Gender_Male == 1] <- 'male'
dane3= dane3  %>% filter(Gender != 0)

dane3$Severity= 0
dane3$Severity[dane3$Severity_Mild == 1] <- 'Mild'
dane3$Severity[dane3$Severity_Moderate == 1] <- 'Moderate'
dane3$Severity[dane3$Severity_None == 1] <- 'None'
dane3$Severity[dane3$Severity_Severe == 1] <- 'Serve'

#objawy
dane3$Fever= as.factor(dane3$Fever)
dane3$Tiredness = as.factor(dane3$Tiredness)
dane3$Dry.Cough = as.factor(dane3$Dry.Cough)
dane3$Difficulty.in.Breathing = as.factor(dane3$Difficulty.in.Breathing)
dane3$Sore.Throat = as.factor(dane3$Sore.Throat)
dane3$None_Sympton = as.factor(dane3$None_Sympton)
dane3$Pains = as.factor(dane3$Pains)
dane3$Nasal.Congestion = as.factor(dane3$Nasal.Congestion)
#dane3= dane3  %>% filter(Country == 'China')
dane_3 = sample_n(dane3,100)

dane2 <- dane %>% arrange(desc(Date_reported)) %>% filter(Date_reported == max(dane$Date_reported))
new_c= sum(dane2$New_cases)
new_d= sum(dane2$New_deaths)
all_time= sum(dane2$Cumulative_cases)

# https://rstudio.github.io/shinydashboard/get_started.html

ui <- dashboardPage(
    dashboardHeader(title = "Visualization of Covid"),
    dashboardSidebar(sidebarMenu(
        menuItem("Statistics", tabName = "Statistics"),
        menuItem("Symptoms", tabName = "Symptoms"),
        menuItem("By country", tabName = "country")
    )),
    dashboardBody(
        
        tabItems(
        tabItem(tabName = "Statistics",
                
        fluidRow(
            
            box(
            tabsetPanel(
                tabPanel("Plot", htmlOutput("q"), plotOutput("distPlot")),
                tabPanel("Table", htmlOutput("q3"), tableOutput("table")))
            
              , height = 520),
            
            box(
                title = "Controls",
                dateInput('date',
                          label = 'Date input: yyyy-mm-dd',
                          value = Sys.Date()-100 ),
            selectInput('variable',
                        'Select variable:',
                        choices = c("New_cases", "New_deaths", "Cumulative_cases", "Cumulative_deaths")),
                sliderInput("number",
                            "Number of countries with the highest (selected) variable value:",
                            min = 2,
                            max = 10,
                            value = 8), height = 520)
                
            ),
        
        
        fluidRow(
            box(
                width = 6, solidHeader = TRUE,
                title = "Selected statistics for individual countries",
                bubblesOutput("bubblePlot", width = "100%", height = 520)
            ),
            
            box(
                width = 6, solidHeader = TRUE,
                title = "World map",
                textOutput("q2"),
                #plotOutput("mapa", width = "100%", height = 500),
                leafletOutput("mapa", width = "100%", height = 500)
            )
        ),
        
        # https://fontawesome.com/icons?d=gallery
        fluidRow(
            infoBox("Today's New Cases", new_c, icon = icon("book-medical"), fill = TRUE),
            infoBox("Today's New Deaths", new_d, icon = icon("book-dead"), fill = TRUE, color = "red"),
            infoBox("Cumulative cases - all time", all_time, icon = icon("briefcase-medical"), fill = TRUE, color = "purple")
        )
        
        ) 
    
    , 
        
        tabItem(tabName = "Symptoms",
                fluidRow(
                    
                    navbarPage("Symptoms",id="navbar",
                               tabPanel("Fever", plotOutput("sympt_Plot")),
                               tabPanel("Tiredness", plotOutput("sympt2_Plot")),
                               tabPanel("Dry Cough", plotOutput("sympt3_Plot")),
                               tabPanel("Breathing problems", plotOutput("sympt4_Plot")),
                               tabPanel("Sore Throat", plotOutput("sympt5_Plot")),
                               tabPanel("None Sympton", plotOutput("sympt6_Plot")),
                               tabPanel("Pains", plotOutput("sympt7_Plot")),
                               tabPanel("Nasal Congestion", plotOutput("sympt8_Plot")),
                               tags$style(HTML(".navbar-header { width:100% }
                   .navbar-brand { width: 100%; text-align: center }"))) 
                    
                )
        ),
    
        tabItem(tabName = "country",
            
            fluidRow(
                
               box( title = "Choose country:",
                        selectInput('country', "",
                                    choices = c(unique(dane$Country))))
                    
                ),
            
            
            fluidRow(
                
                box( width = 12, solidHeader = TRUE,
                     title = "Plot for the selected country:",
                     plotOutput("country_plot", width = "100%", height = 500)
                     
                     )
                
            )
            
            )
    ))
)

server <- function(input, output) { 
    
    output$q <- renderUI({
        HTML(paste("<br> Raport for", input$date, ".<br>", input$number, 'countries with the highest', input$variable, 'value'))
    })
    
    output$distPlot <- renderPlot({
        to_plot= dane %>% filter(Date_reported == input$date) %>% dplyr::arrange(dplyr::desc(!!rlang::sym(input$variable)))
        to_plot= to_plot[1:input$number,]
        to_plot = to_plot %>% select(input$variable,Country_code) %>% dplyr::arrange(!!rlang::sym(input$variable))
        counts <- c(to_plot[,1])
        barplot(counts, horiz=TRUE, names.arg = to_plot$Country_code, 
                col='brown', border='brown')
    })
    
    output$q2 <- renderText({
        paste("World map - date:", input$date)
    })
    
    output$mapa <- renderLeaflet({
        dane2= dane %>% filter(Date_reported == input$date)
        #dane2$Country[dane2$Country == 'Russian Federation'] <- 'Russian Federation'
        world = world %>% left_join(dane2, c("name"="Country"))
        
        world_spdf@data$NAME[world_spdf@data$NAME == 'Russia'] <- 'Russian Federation'
        world_spdf@data = world_spdf@data %>% left_join(world, by=c("NAME"="name"))
        world_spdf@data$POP2005 <- as.numeric(world_spdf@data$POP2005)
        world_spdf@data$Cumulative_cases=replace(world_spdf@data$Cumulative_cases,world_spdf@data$Cumulative_cases==0,NA)
        
        # Creating pallete.
        mypalette <- colorNumeric( palette="viridis", domain=world_spdf@data$Cumulative_cases, na.color="transparent")
        
        # Preparing tooltip 
        mytext <- paste(
            "Country: ", world_spdf@data$NAME,"<br/>", 
            "Cumulative cases: ", world_spdf@data$Cumulative_cases, "<br/>", 
            "POP: ", round(world_spdf@data$POP2005, 2),
            sep="") %>%
            lapply(htmltools::HTML)
        
        # Final Map
        leaflet(world_spdf) %>% 
            addTiles()  %>% 
            setView(lat=0, lng=0 ,zoom=0.5) %>%
            addPolygons( 
                fillColor =  ~mypalette(Cumulative_cases), 
                stroke=TRUE, 
                fillOpacity = 0.9, 
                color="white", 
                weight=0.3,
                label = mytext,
                labelOptions = labelOptions( 
                    style = list("font-weight" = "normal", padding = "3px 8px"), 
                    textsize = "13px", 
                    direction = "auto"
                )
            ) %>%
            addLegend(pal=mypalette, values=~Cumulative_cases, opacity=0.9, title = "Cumulative cases", position = "bottomleft" )
        
        #ggplot(data = world) +
        #    geom_sf(aes(fill = !!rlang::sym(input$variable), )) +
        #    theme(legend.position="bottom")+
         #   scale_fill_gradient2()+
         #   theme(panel.border = element_blank())
    })
    
    
    output$plot3 <- renderPlot({
        dane3= dane %>% filter(Date_reported == input$date)
    })
    
    output$q3 <- renderUI({
        HTML(paste( "<br> Table for ",input$number,'coutries with the highest amount of', input$variable, 'on', input$date, '<br>'))
    })
    
    output$table <- renderTable({
        to_plot= dane %>% filter(Date_reported == input$date) %>% dplyr::arrange(dplyr::desc(!!rlang::sym(input$variable)))
        to_plot= to_plot[1:input$number,]
        to_plot = to_plot %>% select(Country, New_cases, New_deaths, Cumulative_cases, Cumulative_deaths) %>% dplyr::arrange(dplyr::desc(!!rlang::sym(input$variable)))
        
    })
    
    output$sympt_Plot <- renderPlot({
        
        ggplot(data = dane_3,
               aes(axis1 = Age, axis2 = Gender, axis3 = Severity)) +
            scale_x_discrete(limits = c('Age', "Gender", 'Severity'), expand = c(.1, .05)) +
            geom_alluvium(aes(fill = Fever)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
            theme_minimal()
    })
    
    output$sympt2_Plot <- renderPlot({
        
        ggplot(data = dane_3,
               aes(axis1 = Age, axis2 = Gender, axis3 = Severity)) +
            scale_x_discrete(limits = c('Age', "Gender", 'Severity'), expand = c(.1, .05)) +
            geom_alluvium(aes(fill = Tiredness)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
            theme_minimal()
    })
    
    output$sympt3_Plot <- renderPlot({
        
        ggplot(data = dane_3,
               aes(axis1 = Age, axis2 = Gender, axis3 = Severity)) +
            scale_x_discrete(limits = c('Age', "Gender", 'Severity'), expand = c(.1, .05)) +
            geom_alluvium(aes(fill = Dry.Cough)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
            theme_minimal()
    })
    
    output$sympt4_Plot <- renderPlot({
        
        ggplot(data = dane_3,
               aes(axis1 = Age, axis2 = Gender, axis3 = Severity)) +
            scale_x_discrete(limits = c('Age', "Gender", 'Severity'), expand = c(.1, .05)) +
            geom_alluvium(aes(fill = Difficulty.in.Breathing)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
            theme_minimal()
    })
    
    output$sympt5_Plot <- renderPlot({
        
        ggplot(data = dane_3,
               aes(axis1 = Age, axis2 = Gender, axis3 = Severity)) +
            scale_x_discrete(limits = c('Age', "Gender", 'Severity'), expand = c(.1, .05)) +
            geom_alluvium(aes(fill = Sore.Throat)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
            theme_minimal()
    })
    
    output$sympt6_Plot <- renderPlot({
        
        ggplot(data = dane_3,
               aes(axis1 = Age, axis2 = Gender, axis3 = Severity)) +
            scale_x_discrete(limits = c('Age', "Gender", 'Severity'), expand = c(.1, .05)) +
            geom_alluvium(aes(fill = None_Sympton)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
            theme_minimal()
    })
    
    output$sympt7_Plot <- renderPlot({
        
        ggplot(data = dane_3,
               aes(axis1 = Age, axis2 = Gender, axis3 = Severity)) +
            scale_x_discrete(limits = c('Age', "Gender", 'Severity'), expand = c(.1, .05)) +
            geom_alluvium(aes(fill = Pains)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
            theme_minimal()
    })
    
    output$sympt8_Plot <- renderPlot({
        
        ggplot(data = dane_3,
               aes(axis1 = Age, axis2 = Gender, axis3 = Severity)) +
            scale_x_discrete(limits = c('Age', "Gender", 'Severity'), expand = c(.1, .05)) +
            geom_alluvium(aes(fill = Nasal.Congestion)) +
            geom_stratum() + geom_text(stat = "stratum", infer.label = TRUE) +
            theme_minimal()
    })
    
    output$bubblePlot <- renderBubbles({
        
        to_plot= dane %>% filter(Date_reported == input$date) %>% dplyr::arrange(dplyr::desc(!!rlang::sym(input$variable)))
        to_plot= to_plot[1:input$number,]
        to_plot = to_plot %>% select(input$variable,Country_code) %>% dplyr::arrange(!!rlang::sym(input$variable))
        bubbles(to_plot[,1], to_plot$Country_code, key = to_plot$Country_code)
    })
    
    output$country_plot <- renderPlot({
        dane4= dane %>% filter(Country == input$country)
        ggplot(data = dane4, aes(x = Date_reported)) +
            geom_line(aes(y = New_cases)) +
            geom_line(aes(y = New_deaths*10/1), color='red',linetype = "dashed")+
            scale_y_continuous(sec.axis = sec_axis(~ . *1/10, name = "New_deaths")) +
            theme_minimal()
    })
    
    
    
    
    }

shinyApp(ui, server)


