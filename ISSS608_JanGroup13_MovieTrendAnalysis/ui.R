#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, tidyverse, plotly, shinythemes)

genre_list <- c("Action"="Action",
                "Adventure"="Adventure",
                "Animation"="Animation",
                "Comedy"="Comedy",
                "Crime"="Crime",
                "Documentary"="Documentary",
                "Drama"="Drama",
                "Family"="Family",
                "Fantasy"="Fantasy",
                "History"="History",
                "Horror"="Horror",
                "Music"="Music",
                "Mystery"="Mystery",
                "Romance"="Romance",
                "Science Fiction"="Science Fiction",
                "Thriller"="Thriller",
                "TV Movie"="TV Movie",
                "War"="War",
                "Western"="Western")

#productionCompanies_list <- read_csv('data/ProductionCompanies.csv')

# Define UI for application that draws a histogram
fluidPage(
  theme = shinytheme("cosmo"),
  tags$head(
    tags$style(HTML("
      .navbar{
          margin-bottom: -15px;
          margin-left: -15px;
          margin-right: -15px;
      }
    "))
  ),
navbarPage("👨🏻‍💻 ISSS608_Jan2023_Group13-Movie Analysis and Prediction 🎬🍿",
           tabPanel("🔭 Exploratory Data Analysis",           
    titlePanel(h3("🔭 Exploratory Data Analysis 🔭", align = "center")),

    # Sidebar with a slider input for number of bins
    fluidRow(
      column(12, 
       sidebarLayout(
         sidebarPanel(
           selectizeInput("genreSelected", label = h4('Genres'), selected = NULL, choices = genre_list, width = NULL, options = list(maxItems = 5)),
           sliderInput("DateRangeSelected", label = h4("Year Range"), min = 1980, max = 2022, value = c(2013, 2022))
         ),
         
         # Show a plot of the generated distribution
         mainPanel(
           tabsetPanel(type = "tabs",
                       tabPanel("Box Office Revenue", plotlyOutput('EDA_Revenue')),
                       tabPanel("Budget", plotlyOutput('EDA_Budget')),
                       tabPanel("Gross Profit", plotlyOutput('EDA_GrossProfit'))
           )
         )
       )
     )
    ),
    hr(),
    fluidRow(
      column(12, 
         sidebarLayout(
           sidebarPanel(
             actionButton("temp", label = "Movies Released in a Year ↓"),
             selectInput("DateSelected", label = h4("Select Year"), choices = seq.int(2022, 1980, by = -1), selected = 1),
             actionButton("temp", label = "Revenue to Budget Percentage for a Genre ↓"),
             selectizeInput("genreSelected2", label = h4('Genres'), selected = NULL, choices = genre_list, width = NULL, options = list(maxItems = 1)),
             sliderInput("DateRangeSelected2", label = h4("Year Range"), min = 1980, max = 2022, value = c(2013, 2022))
             )
           ,
           mainPanel(
             tabsetPanel(type = "tabs",
                         tabPanel("Movies Released in a Year", plotlyOutput('EDA_MoviesInYear')),
                         tabPanel("Revenue to Budget Percentage for a Genre", plotlyOutput('EDA_Genre'))
             )
           )
         )
      )
    )
),
tabPanel("📈 Analysis",
         titlePanel(h3("📈 Analysis 📈", align = "center")),
         fluidRow(
           column(12, 
              sidebarLayout(
                sidebarPanel(
                  actionButton("temp", label = "Genre Comparisons ↓"),
                  selectizeInput("analyticsGenreSelected", label = h4('Genres'), selected = NULL, choices = genre_list, width = NULL, options = list(maxItems = 2)),
                  actionButton("temp", label = "Production Company Comparisons ↓"),
                  selectizeInput("analyticsProductionCompaniesSelected", label = h4('Production Company'), selected = NULL, choices = c(), width = NULL, options = list(maxItems = 2)),
                ),
                mainPanel(
                  tabsetPanel(type = "tabs",
                              tabPanel("Budget VS Genre", plotlyOutput('Analytics_Genres_Budget')),
                              tabPanel("Revenue VS Genre", plotlyOutput('Analytics_Genres_Revenue')),
                              tabPanel("Production Companies VS Budget", plotlyOutput('Analytics_Prod_Budget')),
                              tabPanel("Production Companies VS Revenue", plotlyOutput('Analytics_Prod_Revenue')),
                  )
                )
              )
            )
          ),
         fluidRow(
           column(width = 6, offset = 5,
                  plotlyOutput('Analytics_CorrMatrix'))
         )
),
tabPanel("🔮 Prediction",
         titlePanel(h3("🔮 Prediction 🔮", align = "center")),
         fluidRow(
           column(12, 
              sidebarLayout(
                sidebarPanel(
                  #actionButton("temp", label = "Genre Comparisons ↓"),
                  selectizeInput("predictionGenreSelected", label = h4('Genres'), selected = NULL, choices = genre_list, width = NULL, options = list(maxItems = 5)),
                  #actionButton("temp", label = "Production Company Comparisons ↓"),
                  numericInput("predictionBudgetSelected", label = h3("Budget (in millions)"), value = 250),
                ),
                mainPanel(
                  fluidRow(
                    column(6, plotlyOutput('Prediction_BudgetRevenuePrediction')),
                    column(6, plotlyOutput('Prediction_GenreRevenuePrediction'))
                  ),
                  fluidRow(
                    column(6, verbatimTextOutput("predictionResult"))
                  )
                )
              )
           )
         )
),
tabPanel("🕵 Data",
         titlePanel(h3("🕵 Data 🕵", align = "center")),
         fluidRow(
           column(12, 
                  DT::dataTableOutput("dataTable")
           )
         )
),
)
)
