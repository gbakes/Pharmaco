library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(shinyWidgets)
library(RMySQL)
library(DBI)
library(tidyverse)
library(plotly)
library(leaflet)
library(readxl)
library(C3)
library(rintrojs)
library(shinyjs)
library(shinycssloaders)
library(hrbrthemes)
library(NPS)


ui <-dashboardPage(title="Pharmaco",
                   dashboardHeaderPlus(title = span(img(src="logo.png", height = "30px"), "Pharmaco"),
                                       tags$li(actionLink("help", label = "", icon = icon("question")),
                                               class = "dropdown")),
                   dashboardSidebar( 
                     introBox( 
                     sidebarMenu(id = "tabs",
                                 menuItem("Overview", tabName = "overview", icon = icon("chart-bar")),
                                 menuItem("Data", tabName = "stage1", icon = icon("cart-arrow-down"))
                     ),
                     data.step = 1,
                     data.intro = "Toggle between the dashboard overview page and the raw data from these two tabs"
                     ),
                    introBox( 
                     uiOutput(outputId = "prog"),
                     data.step = 2,
                     data.intro = "Use this dropdown to segment the data by Program"
                    ),
                    introBox( 
                     uiOutput(outputId = "reg"),
                     data.step = 3,
                     data.intro = "Use these checkboxes to segment the data by region"
                    ),
                    introBox(
                     downloadButton('downloadData', 'Download'),
                     data.step = 4,
                     data.intro = "Click here to download a csv of all the data. This will be limited to your previous segmentations."
                   )
                ),
                   dashboardBody(introjsUI(),
                     div(
                       id = "main_content",
                       tags$head(
                         tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
                         tags$link(rel="shortcut icon", href="favicon.png")
                         
                       ),
                       tabItems(
                         tabItem(tabName = "overview", introBox(
                                 fluidRow(
                                   column(
                                     width = 3,
                                     descriptionBlock(
                                       header =  textOutput("avComp"), 
                                       text = "Overall Compliance", 
                                       right_border = TRUE,
                                       margin_bottom = FALSE
                                     )
                                   ),
                                   column(
                                     width = 3,
                                     descriptionBlock(
                                       header = textOutput("act"), 
                                       text = "Total ToDos", 
                                       right_border = T,
                                       margin_bottom = F
                                     )
                                   ),column(
                                     width = 3,
                                     descriptionBlock(
                                       header = textOutput("NPS"), 
                                       text = "NPS", 
                                       right_border = T,
                                       margin_bottom = F
                                     )
                                   ),
                                   column(
                                     width = 3,
                                     descriptionBlock(
                                       header = textOutput("rating"),
                                       text = "Average content rating",
                                       right_border = F
                                     )
                                     
                                     
                                   )),data.step = 5,
                                   data.intro = paste0("Here are some quick stats, the values are dynamic and update based off your program and region selection:", 
                                                       tags$p(tags$b("Overall Compliance"), "shows the average compliance of all employees over all the training experience."),
                                                       tags$p(tags$b("Total Commitments"), "is the sum of all ToDos created."),
                                                       tags$p(tags$b("NPS"), " is the collective NPS calculated as described ",
                                                              tags$a(href="https://customergauge.com/blog/how-to-calculate-the-net-promoter-score/", target = "blank", " here"), "for the training."),
                                                       tags$p(tags$b("Average content"), " rating is the average content rating out of 5 of all the content material covered during the training engagement.")
                                                       
                                   )
                                 ),
                                 
                                 fluidRow(
                                   box(width = 12,
                                       column(width = 6, introBox(
                                         leafletOutput("leaflet_map"),
                                         data.step = 6,
                                         data.intro = "Here's a map showing all the places where the training sessions took place. Click on the map markers to bring up key stats. The values are dynamic and update based off your program and region selection."
                                       
                                       )),
                                       column(width = 6,introBox(fluidRow(
                                         column(width = 10,plotlyOutput("trained")),
                                         column(width = 2,uiOutput(outputId = "projections"))),
                                         data.step = 7,
                                         data.intro = "This graph shows the employees trained over time. It can be segmented by the Program and Region selections made from the sidebar. The projection shows the estimated employees trained in 6 months.")
                                       )
                                   )
                                 ),
                                 fluidRow(
                                   box(width = 12, 
                                       column(width = 6,introBox( 
                                              plotlyOutput("completion"),
                                              data.step = 8,
                                              data.intro = "This graph shows the amount of content employees have completed over each stage"
                                              
                                       )),
                                       column(width = 6,introBox( 
                                              plotlyOutput("strategies"),
                                              data.step = 9,
                                              data.intro = "This graah shows the amount of ToDos created aligned to each to each overarching goal of the training enagagement"
                                              
                                       ))
                                   )
                                 )
                         ),
                         tabItem(tabName = "stage1",
                                 fluidRow(DT::dataTableOutput("ziptable"))
                         )
                       )
                     )
                   )
)

load_data <- function() {
  Sys.sleep(2)
  hide("loading_page")
  show("main_content")
}

server <- function(input, output, session) {
  
  seq(Sys.Date(), length = 6, by = "-1 months")
  
  set.seed(123)
  id <- seq(333,4332,1)
  program <- paste(rep("Program",4000), rep(seq(1:6),666))
  regionSample <- rep(c("West Europe","East Europe","Latin America", "US", "Asia", "Africa"),10000)
  region <- sample(regionSample,4000)
  stage1 <- as.numeric(format(runif(4000, min=70, max=100),digits=1))
  stage2 <- as.numeric(format(runif(4000, min=60, max=100),digits=1))
  stage3 <- as.numeric(format(runif(4000, min=50, max=100),digits=1))
  stage4 <- as.numeric(format(runif(4000, min=40, max=100),digits=1))
  stage5 <- as.numeric(format(runif(4000, min=30, max=100),digits=1))
  Training <- as.Date(rep(seq(Sys.Date(), length = 12, by = "-1 months"),5000))
  Training <- sample(Training,4000)
  nps <- as.numeric(format(runif(4000, min=1, max=10),digits=1))
  ToDo <- as.numeric(format(runif(4000, min=0, max=1),digits=0))
  Goal <- rep(c(rep("Leading<br>Self",12),rep("Developing<br>Others",10),rep("Driving<br>Change",8),rep("Engaging<br>Teams",6),rep("Top Line<br>Growth",4)),100)
  rating <- as.numeric(format(runif(4000, min=1, max=5),digits=1))
  
  dataset <- tibble(id,program,region,stage1,stage2,stage3,stage4,stage5,Training,nps,ToDo,Goal,rating)
  
  
  countries <- read_csv("data/countries.csv")
  colnames(countries)[colnames(countries)=="en"] <- "country"
  
  Asia <- countries %>% 
    filter(region == "Asia") %>% 
    select(country)
  
  Africa <- countries %>% 
    filter(region == "Africa") %>% 
    select(country)
  
  Easteurope <- countries %>% 
    filter(region == "East Europe") %>% 
    select(country)
  
  Latinamerica <- countries %>% 
    filter(region == "Latin America") %>% 
    select(country)
  
  US <- countries %>% 
    filter(region == "US") %>% 
    select(country)
  
  Westeurope <- countries %>% 
    filter(region == "West Europe") %>% 
    select(country)
  
  country <- rep("",4000)
  
  df2 <- data.frame(dataset,country,stringsAsFactors = FALSE) 
  
  
  for(i in 1:4000){
    if(df2$region[i] == "Asia"){
      df2$country[i] <- sample(Asia$country,1)
    } else if (df2$region[i] == "Africa"){
      df2$country[i] <- sample(Africa$country,1)
    } else if (df2$region[i] == "East Europe"){
      df2$country[i] <- sample(Easteurope$country,1)
    } else if (df2$region[i] == "Latin America"){
      df2$country[i] <- sample(Latinamerica$country,1)
    } else if (df2$region[i] == "US"){
      df2$country[i] <- sample(US$country,1)
    } else if (df2$region[i] == "West Europe"){
      df2$country[i] <- sample(Westeurope$country,1)
    } else{df2$country[i] <- "My Balls"}
  }
  
  df2 <-  select(df2,-region)
  
  
  dataset <- left_join(df2,countries, by = c("country"))
  
  
  colnames(dataset)[colnames(dataset)=="stage1"] <- "Stage 1"
  colnames(dataset)[colnames(dataset)=="stage2"] <- "Stage 2"
  colnames(dataset)[colnames(dataset)=="stage3"] <- "Stage 3"
  colnames(dataset)[colnames(dataset)=="stage4"] <- "Stage 4"
  colnames(dataset)[colnames(dataset)=="stage5"] <- "Stage 5"
  
  nms <- sort(as.character(unique(dataset$program)))
  nms <- c("All",nms)
  
  
  output$prog <- renderUI({
    selectInput("prog", "Programs",nms, selected = "All")
    
  })
  
  output$reg <- renderUI({
    awesomeCheckboxGroup(inputId = "reg",
                         label = "Region:",
                         choices = c("West Europe" = "West Europe",
                                     "East Europe" = "East Europe",
                                     "Latin America" = "Latin America", 
                                     "US" = "US", 
                                     "Asia" = "Asia", 
                                     "Africa" = "Africa"
                         ),
                         selected = c("West Europe","East Europe","Latin America", "US", "Asia", "Africa"),
                         status = "danger"
                         
    )
    
  })
  
  output$projections <- renderUI({
    prettyRadioButtons("projections", "Projections",
                 choices = list("auto" = "auto", 
                                "lm" = "lm",
                                "glm" = "glm",
                                "gam" = "gam",
                                "loess" = "loess"),selected = "lm", status = "danger")
  })
  
  
  df <- reactive({
    
    if(input$prog == "All"){filter(dataset, region %in% c(input$reg))} else{ 
      filter(dataset, program %in%(input$prog) & region %in% c(input$reg))
    }
  })
  
  
  output$trained <- renderPlotly({ 
    train <- df() %>% 
      group_by(Training) %>% 
      count(Training) %>% 
      as.data.frame() %>% 
      ggplot(aes(Training, n)) +
      
      geom_path(color = "#d90916") +
      expand_limits(x = as.Date(seq(Sys.Date(), length = 2, by = "6 months"))) +
      stat_smooth(method = input$projections, fullrange = T) +
    
      theme_ipsum() +
      theme(panel.grid.major.x = element_blank(),
            axis.text = element_text(size = 12),
            panel.background = element_rect(fill = "#E9EEF4",
                                            colour = "#E9EEF4"),
            plot.background = element_rect(fill = "#E9EEF4"))
    
    ggplotly(train, tooltip = c("x","y")) %>% 
      layout(title = "Participants Trained",
             yaxis = list(title = ""),
             xaxis = list(title = "")) 
    
  })
  output$completion <- renderPlotly({
    
    completion <- df() %>% 
      select(`Stage 1`,`Stage 2`,`Stage 3`,`Stage 4`,`Stage 5`) %>% 
      gather() %>% 
      group_by(key) %>% 
      summarize(Completion = mean(value)) %>% 
      ggplot(aes(x = key,y = Completion))+
      geom_bar(stat="identity",width = 0.3 ,fill = "#d90916") +
      #scale_y_continuous(limits = c(0,100)) + 
      theme_ipsum() +
      theme(panel.grid.major.x = element_blank(),
            axis.text = element_text(size = 12),
            panel.background = element_rect(fill = "#E9EEF4",
                                            colour = "#E9EEF4"),
            plot.background = element_rect(fill = "#E9EEF4"))
    
    ggplotly(completion, tooltip = c("x","y")) %>% 
      layout(title = "Completion vs Stage",
             xaxis = list(title = ""),
             yaxis = list(title = "")) 
    
  })
  
  output$strategies <- renderPlotly({
    
    strategies <- df() %>% 
      filter(ToDo == 1) %>% 
      select(Goal) %>% 
      count(Goal) %>% 
      ggplot(aes(x = reorder(Goal,n),y = n))+
      geom_bar(stat="identity", width = 0.3, fill = "#d90916") +
      geom_path()+
      #scale_y_continuous(limits = c(0,100)) + 
      theme_ipsum() +
      theme(panel.grid.major.x = element_blank(),
            axis.text = element_text(size = 12),
            panel.background = element_rect(fill = "#E9EEF4",
                                            colour = "#E9EEF4"),
            plot.background = element_rect(fill = "#E9EEF4"))
    
    ggplotly(strategies,tooltip = c("y")) %>% 
      layout(title = "ToDo vs Training Goals",
             xaxis = list(title = ""),
             yaxis = list(title = ""))
    
  })
  
  output$avComp <- renderText({
    x <-  df()%>% 
      select(`Stage 1`,`Stage 2`,`Stage 3`,`Stage 4`,`Stage 5`) %>% 
      gather() %>% 
      summarize(av = as.integer(mean(value)))
    paste0(x, "%")
  })
  
  output$act <- renderText({
    a <-  df()%>% 
      select(ToDo) %>% 
      summarize(total = sum(ToDo))
    paste0(a)
  })
  
  output$NPS <- renderText({ 
    nps <- df() %>% 
      select(nps) %>% 
      mutate(nps = npc(nps, breaks = list(0:6, 7:8, 9:10))) %>% 
      count(nps) %>% 
      mutate(n=n/40)
    
    NPS <- format(round(nps$n[3] - nps$n[1], 2), nsmall = 2)
    paste0(NPS)
  })
  
  output$rating <- renderText({ 
    r <- df() %>% 
      select(rating) %>% 
      summarize(format(round(mean(rating), 2), nsmall = 2))
    paste0(
      r
    )
  })
  
  tagScrub <- function(htmlString) {
    return(gsub("<.*?>", " ", htmlString))
  }
  output$ziptable <- DT::renderDataTable({
    df() %>% 
      mutate(Goal = tagScrub(Goal)) %>% 
      select(-latitude,-longitude,-code)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(),"-",input$prog,".csv", sep="")
    },
    content = function(file) {
      write.csv(df()%>% 
                  mutate(Goal = tagScrub(Goal)), file)
    }
  )
  
  greenLeafIcon <- makeIcon(
    iconUrl = "www/pin.png",
    iconWidth = 15, iconHeight = 15,
    iconAnchorX = 0, iconAnchorY = 0,
    shadowWidth = 10, shadowHeight = 10,
    shadowAnchorX = 0, shadowAnchorY = 0
  )
  
  pal <- c("white", "navy")
  
  output$leaflet_map <- renderLeaflet({
    map <- df() %>%
      group_by(country) %>% 
      summarize(longitude = longitude[1],
                latitude = latitude[1],
                population = n(),
                nps = format(round(mean(nps), 2), nsmall = 2),
                rating = format(round(mean(rating), 2), nsmall = 2),
                ToDo = sum(ToDo)) 
    leaflet(map) %>%
      addTiles(attribution = '') %>% 
      addMarkers(~longitude, ~latitude,popup = paste("Country: ",map$country,"<br>",
                                                     "People: ", map$population ,"<br>",
                                                     "NPS: ", map$nps ,"<br>",
                                                     "Rating: ", map$rating ,"<br>",
                                                     "ToDo: ", map$ToDo
                                                  
      ),
      
      icon = greenLeafIcon)
  })
  observeEvent(input$help,
               introjs(session, options = list("nextLabel"="Next",
                                               "prevLabel"="Back",
                                               "skipLabel"="Done"),
               )
  )
  load_data()
}

shinyApp(ui,server)