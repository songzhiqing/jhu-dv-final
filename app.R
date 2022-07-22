#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(shiny)
library(shinydashboard) 
library(leaflet)
library(DT)
library(plotly)

game1 = read_csv("game.csv")
game1$na = is.na(game1$Genre)
game1 = game1[game1$na == "FALSE",]
game1$Year_of_Release <- as.numeric(game1$Year_of_Release)
attach(game1)
min_critic_score <- min(game1$Critic_Score,na.rm = TRUE)
max_critic_score <- max(game1$Critic_Score,na.rm = TRUE)

all_genres <- game1 %>%
  distinct(Genre)%>%
  pull(Genre)

all_games = game1 %>%
  distinct(Name) %>%
  pull(Name)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Game Visualization"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Sales Overview", tabName = "page1", icon = icon("info-circle")),
      menuItem("Quantity of Games by Genre", tabName = "page2", icon = icon("chart-bar")),
      menuItem("Compare", tabName = "page3", icon = icon("trophy")),
      menuItem("Top 10 Charts", tabName = "page4", icon = icon("database")),
      menuItem("Build Your Own List", tabName = "page5", icon = icon("table"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "page1",
              h3("Game Sales on Different Platforms Based on Genre till 2016"),
              br(),
              fluidRow(
                column(
                  2,
                  checkboxGroupInput("GenreInput",label = h4("Show Genres"),
                                    choices = list("Action"="Action","Adventure"="Adventure","Fighting"="Fighting","Misc"="Misc",
                                                  "Platform"="Platform","Puzzle"="Puzzle","Racing"="Racing","Role-Playing"="Role-Playing",
                                                 "Shooter"="Shooter","Simulation"="Simulation","Sports"="Sports","Strategy"="Strategy"),
                                 selected = c("Action","Adventure","Fighting","Misc",
                                             "Platform","Puzzle","Racing","Role-Playing",
                                            "Shooter","Simulation","Sports","Strategy")
                            ),
                  actionButton("Button1", "Choose")

                ),
                column(
                  10,
                  fluidRow(
                    column(
                      6,
                      plotlyOutput("plot1", height = 500)
                    ),
                    column(
                      6,
                      plotlyOutput("plot2", height = 500)
                    ),
                  ),
                  br(),
                  fluidRow(
                    column(
                      6,
                      plotlyOutput("plot3", height = 500)
                    ),
                    column(
                      6,
                      plotlyOutput("plot4", height = 500)
                    ),
                )
                )
             
              )
      ),
      
      tabItem(tabName = "page2",
              sliderInput(
                "year",
                "Year:",
                min = 1997,
                max = 2016,
                value = 1,
                step = 1,
                animate = animationOptions(interval = 2000, loop = FALSE)
              ),
              plotlyOutput("plot5", height = 500)
      ),
      
      tabItem(tabName = "page3",
              fluidPage(
                  titlePanel("Compare Your Selection"),
                  fluidRow(column(4,
                                  selectInput(
                                    "genre1",
                                    "Select Genre for Game 1",
                                    choices = all_genres
                                  ),
                                  selectInput(
                                    "game1",
                                    "Select Game 1",
                                    choices = ""
                                  )
                  ),
                  column(4,
                         selectInput(
                           "genre2",
                           "Select Genre for Game 2",
                           choices = all_genres
                         ),
                         selectInput(
                           "game2",
                           "Select Game 2",
                           choices = ""
                         )
                  ),
                  column(4,
                         radioButtons("measure",
                                      h3("Compare by Sales or Scores"), 
                                      choices = list("Sales" = "Sales", "Scores" = "Scores")
                         )
                  )
                  ),
                  br(),
                  fluidRow(column(12,
                                  actionButton("CompareButton", "Compare")
                  )
                  ),
                  br(),
                  br(),
                  fluidRow(column(12,
                                  strong(textOutput("comparison")),
                                  br(),
                                  plotOutput("CompareChart")
                  )
              )
              )
      ),
      
      tabItem(tabName = "page4",
              h4("1. By Platform: Top 10 Sales"),
              br(),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "Id041",
                         label = h4("Select Platform:"), 
                         choices = c("2600","3DO","3DS","DC","DS","GB","GBA","GC","GEN","GG","N64","NES","NG","PC","PCFX","PS",
                                     "PS2","PS3","PS4","PSP","PSV","SAT","SCD","SNES","TG16","Wii","WiiU","WS","X360","XB","XOne")
                         )
                       ),
                column(10,
                       dataTableOutput("myTable1")
                )
                ),
              br(),
              h4("2. By Genre: Top 10 Sales"),
              br(),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "Id042",
                         label = h4("Select Genre:"), 
                         choices = c("Action","Adventure","Fighting","Misc","Platform","Puzzle","Racing","Role-Playing",
                                     "Shooter","Simulation","Sports","Strategy")
                       )
                ),
                column(10,
                       dataTableOutput("myTable2")
                )
              ),
              br(),
              h4("3. By Platform: Top 10 Critic Score"),
              br(),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "Id043",
                         label = h4("Select Platform:"), 
                         choices = c("3DS","DC","DS","GBA","GC","PC","PS","PS2","PS3","PS4","PSP","PSV",
                                     "Wii","WiiU","X360","XB","XOne")
                       )
                ),
                column(10,
                       dataTableOutput("myTable3")
                )
              ),
     
              br(),
              h4("4. By Genre: Top 10 Critic Score"),
              br(),
              fluidRow(
                column(2,
                       selectInput(
                         inputId = "Id044",
                         label = h4("Select Genre:"), 
                         choices = c("Action","Adventure","Fighting","Misc","Platform","Puzzle","Racing","Role-Playing",
                                     "Shooter","Simulation","Sports","Strategy")
                       )
                ),
                column(10,
                       dataTableOutput("myTable4")
                )
              )
              
      ),
      tabItem(tabName = "page5",
              fluidPage(
                h4("Build your own list"),
                sidebarLayout(
                  sidebarPanel(width = 2,
                               selectInput(inputId = "I_Platform",
                                           label = "Platform Selection",
                                           choices = c("All", unique(as.character(Platform))),
                                           selected = "All"
                               ),
                               selectInput(inputId = "I_Genre",
                                           label = "Genre Selection",
                                           choices = c("All", unique(as.character(Genre))),
                                           selected = "All"
                               ),
                               selectInput(inputId = "I_Rating",
                                           label = "Rating Selection",
                                           choices = c("All", unique(as.character(Rating))),
                                           selected = "All"
                               ),
                               sliderInput(
                                 inputId = "I_Critic_Score",
                                 label = "Critic Score", min = min_critic_score, max = max_critic_score,
                                 sep = "",
                                 value = c(13, 98)
                               ),
                  ),      
                  mainPanel(width = 8,
                            dataTableOutput(outputId = "Table1"))
                    
                  )
                )
              )
      )
    )
  )




# Define server logic required to draw a histogram
server <- function(input, output, session) {
game = reactive({
  input$Button1
  game1 %>%
    group_by(Genre,Platform) %>% 
    summarise(NA_Sales = sum(NA_Sales),EU_Sales = sum(EU_Sales), JP_Sales = sum(JP_Sales), Global_Sales = sum(Global_Sales)) %>%
    filter(Genre %in% isolate(input$GenreInput))
  
})

  output$plot1 = renderPlotly({
    f1=ggplot(data = game(), mapping = aes(x = Platform, fill = Genre, y = NA_Sales))+
      geom_bar(stat = "identity", position = "stack")+
      ylim(0,1300)+
      labs(
        title = "Sales in North America" ,
        x = "Platform",
        y = "Sales",
      )+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
    ggplotly(f1)
  })
  output$plot2 = renderPlotly({
    f2=ggplot(data = game(), mapping = aes(x = Platform, fill = Genre, y = EU_Sales))+
      geom_bar(stat = "identity", position = "stack")+
      ylim(0,1300)+
      labs(
        title = "Sales in Europe" ,
        x = "Platform",
        y = "Sales",
      )+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
    ggplotly(f2)
  })
  output$plot3 = renderPlotly({
    f3=ggplot(data = game(), mapping = aes(x = Platform, fill = Genre, y = JP_Sales))+
      geom_bar(stat = "identity", position = "stack")+
      ylim(0,1300)+
      labs(
        title = "Sales in Japan" ,
        x = "Platform",
        y = "Sales",
      )+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
    ggplotly(f3)
  })
  output$plot4 = renderPlotly({
    f4=ggplot(data = game(), mapping = aes(x = Platform, fill = Genre, y = Global_Sales))+
      geom_bar(stat = "identity", position = "stack")+
      ylim(0,1300)+
      labs(
        title = "Sales in Global" ,
        x = "Platform",
        y = "Sales",
      )+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))
    ggplotly(f4)
  })
  
  output$plot5 = renderPlotly({
    data_subset = game1 %>%
      filter(Year_of_Release == input$year)%>%
      group_by(Genre, Rating)%>%
      summarise(Count = n())
    gl <- ggplot(data_subset, aes(Genre,Count),width=0.5)+
      theme(axis.text.x = element_text(angle=65, vjust=0.6))+
      geom_bar(stat='identity',aes(fill=Rating))+
      labs(title = 'Global Game Quantity by Genre',
           x='Genre',
           y='Quantity')+
      ylim(0,250)
    return(gl)
  })
  
  output$myTable1 = renderDataTable({
    #(1) Sales By Platforms
    sales_platforms <- game1 %>%
      filter(Platform == input$Id041)%>%
      group_by(Platform)%>%
      slice_max(order_by = Global_Sales, n =10)
    
    sales_platforms = sales_platforms%>%
      select("Platform", "Name", "Global_Sales")
    
    return(datatable(sales_platforms, 
                     options = list(lengthChange = FALSE),
                     rownames= FALSE))
  })
  
  output$myTable2 = renderDataTable({
    #(2) Sales By Genre
    sales_genre <- game1 %>%
      filter(Genre == input$Id042)%>%
      group_by(Genre)%>%
      slice_max(order_by = Global_Sales, n =10)
    
    sales_genre = sales_genre%>%
      select("Genre", "Name", "Global_Sales")
    
    return(datatable(sales_genre, 
                     options = list(lengthChange = FALSE),
                     rownames= FALSE))
  })
  
  output$myTable3 = renderDataTable({
    #(3) Critic Score by Platforms
    score_platforms <- game1 %>%
      filter(Platform == input$Id043)%>%
      group_by(Platform)%>%
      slice_max(order_by = Critic_Score, n =10)
    
    score_platforms = score_platforms%>%
      select("Platform", "Name", "Critic_Score")
    
    return(datatable(score_platforms, 
                     options = list(lengthChange = FALSE),
                     rownames= FALSE))
  })
  
  output$myTable4 = renderDataTable({
    #(4) Critic Score by Genre
    score_genre <- game1 %>%
      filter(Genre == input$Id044)%>%
      group_by(Genre)%>%
      slice_max(order_by = Critic_Score, n =10)
    
    score_genre = score_genre%>%
      select("Genre", "Name", "Critic_Score")
    return(datatable(score_genre, 
                     options = list(lengthChange = FALSE),
                     rownames= FALSE))
  })
  
  observe({
    games_genre1 = game1 %>%
      filter(Genre == input$genre1) %>%
      distinct(Name) %>%
      pull (Name)
    
    updateSelectInput(session = session,
                      inputId = "game1",
                      choices = games_genre1)
  })
  
  observe({
    games_genre2 = game1 %>%
      filter(Genre == input$genre2) %>%
      distinct(Name) %>%
      pull (Name)
    
    updateSelectInput(session = session,
                      inputId = "game2",
                      choices = games_genre2)
  })
  
  games_sale <- game1 %>%
    pivot_longer(c(6:9), names_to = "Region", values_to = "Sales") 
  games_score <- game1 %>%
    pivot_longer(c(11,13), names_to = "Type", values_to = "Score")
  
  ShowChart1 = eventReactive(
    c(input$CompareButton,input$measure), 
    {
      if(input$measure=="Sales"){
        game1_sale <- games_sale %>%
          filter(Name == (input$game1))
        game2_sale <- games_sale %>%
          filter(Name == (input$game2))
        sales_selected <- rbind(game1_sale, game2_sale)
        
        saleschart <- 
          ggplot(sales_selected, aes(x= Name, y = Sales, fill = Region)) +
          geom_bar(stat = "identity", width = 0.3) +
          scale_fill_manual(values=c("#FFECB3",
                                     "#FFDB6D",
                                     "#EDAE49",
                                     "#D16103")) +
          theme_light()
        return(saleschart)
      }
      if(input$measure=="Scores"){ 
        game1_score <- games_score %>%
          filter(Name == (input$game1))
        game2_score <- games_score %>%
          filter(Name == (input$game2))
        scores_selected <- rbind(game1_score, game2_score)
        
        scorechart <- 
          ggplot(scores_selected, aes(x= Name, y = Score, fill = Type)) +
          geom_bar(stat = "identity", position="dodge", width = 0.3) +
          scale_fill_manual(values=c("#C3D7A4",
                                     "#52854C")) +
          theme_light()
        return(scorechart)
      }
      
    })
  
  
  ShowText = eventReactive(
    c(input$CompareButton,input$measure), 
    {text = paste0(input$measure,
                   " Comparison"
    )
    }
  )
  
  
  output$comparison =  renderText({
    return(ShowText())
  })
  
  output$CompareChart =  renderPlot({
    
    return(ShowChart1())
  }
  )
  
  
  #part5-1
  output$Table1 <- renderDataTable({
    data = game1 
    if(input$I_Platform != "All"){
      data =  data %>%
        filter(Platform == input$I_Platform)
    }
    
    if(input$I_Genre != "All"){
      data = data %>% 
        filter(Genre == input$I_Genre)
    }
    
    if(input$I_Rating != "All"){
      data = data %>% 
        filter(Rating == input$I_Rating)
    }
    data = data %>%
      filter(!is.na(Critic_Score)) %>%
      filter(Critic_Score>=input$I_Critic_Score[1] & Critic_Score<=input$I_Critic_Score[2]) %>%
      select(Name:Rating)
    
    
    datatable(data = data, options = list(pageLength = 10),
              rownames = FALSE, class = 'display', escape = FALSE)
    
  })


  
}

# Run the application 
shinyApp(ui = ui, server = server)
