#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

pacman::p_load(shiny, tidyverse, plotly, DT, gridExtra, ggstatsplot, stringr, ggcorrplot, tidyr, dplyr)
movietidied <- read_rds("data/moviestidied.rds")

genre_train_movie_data <- movietidied %>% 
  unnest(genres)%>%
  mutate(genres = as.character(genres))


genre_train_movie_data <- subset(genre_train_movie_data, select = c("id","title","genres","original_language","popularity","release_date","budget","revenue","runtime","status","vote_average","vote_count","release_year"))

prod_train_movie_data <- movietidied %>% 
  unnest(production_companies)%>%
  mutate(production_companies = as.character(production_companies))

production_companies <- unlist(lapply(prod_train_movie_data$production_companies, str_to_title))

productionCompanies <- prod_train_movie_data$production_companies
productionCompanies <- lapply(productionCompanies, str_trim)
productionCompanies <- unique(productionCompanies)

prod_train_movie_data$production_companies <- production_companies

prod_train_movie_data <- subset(prod_train_movie_data, select = c("id","title","production_companies","original_language","popularity","release_date","budget","revenue","runtime","status","vote_average","vote_count","release_year"))

# convert list of strings to separate rows
movie_data <- movietidied %>%
  select(id, budget, genres, revenue) %>%
  unnest(genres) %>%
  mutate(genres = as.character(genres))
# pivot wider to create dummy variables for each genre
movie_data <- movie_data %>%
  pivot_wider(names_from = genres, values_from = genres, values_fn = length, values_fill = 0)
# fit linear regression model
model <- lm(revenue ~ budget + ., data = movie_data)
# print model summary
summary(model)

# Define server logic required to draw a histogram
function(input, output, session) {
    output$EDA_Revenue <- renderPlotly({
      # Filter the dataset to only include the desired genre
      filtercriteria <- input$genreSelected
      
      #unnest the data for filtering
      movieEDA <- movietidied %>% 
        unnest(genres)%>%
        mutate(genres = as.character(genres))
      
      #filter requiresd data
      movieEDA <- movieEDA %>%
        filter(genres %in% filtercriteria) %>% #remove this if filtering everything
        group_by(genres, release_year) %>% 
        summarize(total_revenue = sum(revenue)) %>%
        filter(release_year <= 2022)  # Filter data up to 2022
      
      # Create a line chart
      prevenue <- ggplot(movieEDA, aes(x = release_year, y = total_revenue, color = genres)) +
        geom_line(size = 1.5) +
        scale_x_continuous(limits = c(input$DateRangeSelected[1], input$DateRangeSelected[2]), breaks = seq(input$DateRangeSelected[1], input$DateRangeSelected[2], by = 1)) +
        labs(title = "Box Office Earnings by Genre and Year",
             subtitle = "Showing trends for Action, Comedy, and Drama genres",
             x = "Release Year",
             y = "Total Revenue",
             color = "Genres") +
        theme(plot.title = element_text(size = 20),
              plot.subtitle = element_text(size = 15),
              plot.caption = element_text(size = 10),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10)) +
        scale_color_brewer(palette = "Set1") #remove this for more colours
      ggplotly(prevenue)
    })
    
    
    
    output$EDA_Budget <- renderPlotly({
      filtercriteria <- input$genreSelected
      
      #unnest the data for filtering
      movieEDA <- movietidied %>% 
        unnest(genres)%>%
        mutate(genres = as.character(genres))
      
      #filter requiresd data
      movieEDA <- movieEDA %>%
        filter(genres %in% filtercriteria) %>% #remove this if filtering everything
        group_by(genres, release_year) %>% 
        summarize(total_budget = sum(budget)) %>%
        filter(release_year <= 2022)  # Filter data up to 2022
      
      # Create a line chart
      pbudget <- ggplot(movieEDA, aes(x = release_year, y = total_budget, color = genres)) +
        geom_line(size = 1.5) +
        scale_x_continuous(limits = c(input$DateRangeSelected[1], input$DateRangeSelected[2]), breaks = seq(input$DateRangeSelected[1], input$DateRangeSelected[2], by = 1)) +
        labs(title = "Budget by Genre and Year",
             subtitle = "Showing trends for Action, Comedy, and Drama genres",
             x = "Release Year",
             y = "Total Budget",
             color = "Genres") +
        theme(plot.title = element_text(size = 20),
              plot.subtitle = element_text(size = 15),
              plot.caption = element_text(size = 10),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10)) +
        scale_color_brewer(palette = "Set1") #remove this for more colours
      ggplotly(pbudget)
    })
    
    
    output$EDA_GrossProfit <- renderPlotly({
      filtercriteria <- input$genreSelected
      
      #unnest the data for filtering
      movieEDA <- movietidied %>% 
        unnest(genres)%>%
        mutate(genres = as.character(genres))
      
      #filter requiresd data
      movieEDA <- movieEDA %>%
        filter(genres %in% filtercriteria) %>%
        group_by(genres, release_year) %>% 
        summarize(total_revenue = sum(revenue),
                  total_budget = sum(budget),
                  profit_pct = round((total_revenue - total_budget) / total_budget * 100)) %>%
        filter(release_year <= 2022)  # Filter data up to 2022
      
      # Create a line chart
      pgrossprofit <- ggplot(movieEDA, aes(x = release_year, y = profit_pct, color = genres)) +
        geom_line(size = 1.5) +
        scale_x_continuous(limits = c(input$DateRangeSelected[1], input$DateRangeSelected[2]), breaks = seq(input$DateRangeSelected[1], input$DateRangeSelected[2], by = 1)) +
        scale_y_continuous(limits = c(-100, 400), breaks = seq(-100, 400, by = 100)) +
        labs(title = "Gross Profit Percentage by Genre and Year",
             subtitle = "Showing trends for Action, Comedy, and Drama genres",
             x = "Release Year",
             y = "Gross Profit Percentage",
             color = "Genres") +
        theme(plot.title = element_text(size = 20),
              plot.subtitle = element_text(size = 15),
              plot.caption = element_text(size = 10),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10)) +
        scale_color_brewer(palette = "Set1")
      ggplotly(pgrossprofit)
    })
    
    
    output$EDA_Genre <- renderPlotly({
      # Filter the dataset to only include the desired genre
      filtercriteria <- input$genreSelected2
      
      # Unnest the data for filtering
      movieEDA <- movietidied %>% 
        unnest(genres) %>%
        mutate(genres = as.character(genres))
      
      # Filter required data
      movieEDA <- movieEDA %>%
        filter(genres %in% filtercriteria) %>% 
        group_by(genres, release_year) %>% 
        summarize(total_revenue = sum(revenue), total_budget = sum(budget)) %>%
        filter(release_year <= 2022) %>% # Filter data up to 2022
        mutate(percent_revenue_to_budget = round(total_revenue / total_budget * 100, 0)) # Compute percentage of revenue to budget and round to nearest integer
      
      # Create a line chart
      p <- ggplot(movieEDA, aes(x = release_year)) +
        geom_line(aes(y = total_budget, color = genres), size = 1.5) +
        geom_line(aes(y = total_revenue, color = genres), size = 1.5, linetype = 2) +
        scale_x_continuous(limits = c(input$DateRangeSelected2[1], input$DateRangeSelected2[2]), breaks = seq(input$DateRangeSelected2[1], input$DateRangeSelected2[2], by = 1)) +
        labs(title = "Budget and Revenue by Genre and Year",
             subtitle = "Showing trends for the Action genre",
             x = "Release Year",
             y = "Amount",
             color = "Genres",
             linetype = "Revenue") +
        theme(plot.title = element_text(size = 20),
              plot.subtitle = element_text(size = 15),
              plot.caption = element_text(size = 10),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10)) +
        scale_color_manual(values = "red") # Set the color for the Action genre to blue
      
      # Create a line chart for percentage of revenue to budget
      pgrossprofit <- ggplot(movieEDA, aes(x = release_year, y = percent_revenue_to_budget, color = genres)) +
        geom_line(size = 1.5) +
        scale_x_continuous(limits = c(input$DateRangeSelected2[1], input$DateRangeSelected2[2]), breaks = seq(input$DateRangeSelected2[1], input$DateRangeSelected2[2], by = 1)) +
        scale_y_continuous(limits = c(-50, 500), breaks = seq(-50, 500, by = 100)) +
        labs(title = "Percentage of Revenue to Budget by Genre and Year",
             subtitle = "Showing trends for the Action genre",
             x = "Release Year",
             y = "Percentage of Revenue to Budget",
             color = "Genres") +
        theme(plot.title = element_text(size = 20),
              plot.subtitle = element_text(size = 15),
              plot.caption = element_text(size = 10),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10)) +
        scale_color_manual(values = "green") # Set the color for the Action genre to blue
      
      # Combine the two plots into one using plotly
      subplot(p, pgrossprofit, nrows = 2, titleY = TRUE, heights = c(0.7, 0.3))
    })
    
    
    output$EDA_MoviesInYear <- renderPlotly({
      #FILTER FOR YEAR
      year <- input$DateSelected
      
      # List of genres to include
      genre_list <- c("Action", "Comedy", "Drama", "Thriller", "Horror", "Adventure", "Science Fiction", "Mystery", "Fantasy", "Animation")
      
      # Get the count of movies released per year for each genre
      movieEDA <- movietidied %>%
        unnest(genres) %>%
        mutate(genres = as.character(genres)) %>%
        filter(release_year == year & genres %in% genre_list) %>%
        group_by(genres) %>%
        summarize(count = n()) %>%
        arrange(desc(count))  # reorder the genres by count in descending order
      
      # Create a horizontal bar chart with a color palette
      ggplot(movieEDA, aes(x = reorder(genres, count), y = count, fill = genres)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_brewer(palette = "Set3") +  # set the color palette
        labs(title = paste0("Number of Movies Released in ", year),
             x = "Count",
             y = "Genres") +
        theme(plot.title = element_text(size = 20),
              axis.title = element_text(size = 12, face = "bold"),
              axis.text = element_text(size = 10),
              legend.title = element_blank(),
              legend.position = "none") +
        coord_flip()  # flip the chart
    })
    output$value <- renderPrint({ input$DateRangeSelected[2] })
    output$genreSelectedValue <- renderPrint({ input$DateRangeSelected })
    output$dataTable = DT::renderDataTable({
      temp <- subset(movietidied, select = c("title","genres","original_language","release_date","budget","revenue","runtime","status", "credits", "keywords"))
      temp 
    })
    
    output$Analytics_Genres_Budget <- renderPlotly({
      
      anova_movie_df <- subset(genre_train_movie_data, genres %in% input$analyticsGenreSelected)
      ggbetweenstats(
        data = anova_movie_df,
        x = genres,
        y = budget, 
        type = "p",
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE
      ) +
      labs(title = "Budget VS Genre")
    })
    output$Analytics_Genres_Revenue <- renderPlotly({
      
      anova_movie_df <- subset(genre_train_movie_data, genres %in% input$analyticsGenreSelected)
      ggbetweenstats(
        data = anova_movie_df,
        x = genres,
        y = revenue, 
        type = "p",
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE
      ) + 
        labs(title = "Revenue VS Genre")
    })
    
    output$Analytics_Prod_Budget <- renderPlotly({
      anova_prod_movie_df <- subset(prod_train_movie_data, production_companies %in% input$analyticsProductionCompaniesSelected)
      ggbetweenstats(
        data = anova_prod_movie_df,
        x = production_companies,
        y = budget, 
        type = "p",
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE
      ) +
        labs(title = "Production Companies VS Budget")
    })
    
    output$Analytics_Prod_Revenue <- renderPlotly({
      anova_prod_movie_df <- subset(prod_train_movie_data, production_companies %in% input$analyticsProductionCompaniesSelected)
      ggbetweenstats(
        data = anova_prod_movie_df,
        x = production_companies,
        y = revenue, 
        type = "p",
        mean.ci = TRUE, 
        pairwise.comparisons = TRUE, 
        pairwise.display = "s",
        p.adjust.method = "fdr",
        messages = FALSE
      ) + 
        labs(title = "Production Companies VS Revenue")
    })
    
    observeEvent(input$analyticsGenreSelected, {
      #productionCompanies <- sort(productionCompanies)
      #write.csv(productionCompanies, "mydata.csv", row.names = FALSE, sep = ",")
      updateSelectizeInput(session, "analyticsProductionCompaniesSelected", choices = productionCompanies, selected = "Marvel Studios", server = TRUE)
    })
    
    output$Analytics_CorrMatrix <- renderPlotly({
      ggstatsplot::ggcorrmat(
        data = genre_train_movie_data, 
        cor.vars = c("popularity","budget","revenue","runtime","vote_average","vote_count"))
    })
    
    output$Prediction_BudgetRevenuePrediction <- renderPlotly({
      
      new_movie <- data.frame(budget = input$predictionBudgetSelected, 
                              Action = 0, 
                              Adventure = 0,
                              `Science Fiction` = 0,
                              Comedy = 0, 
                              Drama = 0, 
                              Romance = 0,
                              Animation = 0,
                              Thriller = 0,
                              Horror = 0,
                              Mystery = 0,
                              Fantasy = 0,
                              War = 0,
                              History = 0,
                              Family = 0,
                              Crime =0,
                              Music = 0,
                              Western = 0,
                              `TV Movie` = 0,
                              Documentary = 0,
                              id = 12345
      )
      
      for (genreInput in input$predictionGenreSelected) {
        new_movie[genreInput] = 1
      }
      names(new_movie)[4] <- "Science Fiction"
      names(new_movie)[19] <- "TV Movie"
      # make prediction with fitted model
      new_revenue <- predict(model, new_movie)
      
      genre_revenue <- movie_data %>%
        pivot_longer(cols = Action:Documentary, names_to = "genre", values_to = "is_genre") %>%
        filter(is_genre == 1) %>%
        group_by(genre) %>%
        summarize(mean_revenue = mean(revenue, na.rm = TRUE))
      genre_revenue_plot <- ggplot(genre_revenue, aes(x = genre, y = mean_revenue, fill = genre)) +
        geom_bar(stat = "identity") +
        labs(x = "Genre", y = "Mean Revenue", title = "Mean Revenue by Genre") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_minimal() +
        theme(legend.position = "none") +
        coord_flip()
      ggplotly(genre_revenue_plot)
      # Scatter plot of budget vs revenue
      budget_revenue_plot <- ggplot(movie_data, aes(x = budget, y = revenue, color = factor(Action))) +
        geom_point(alpha = 0.5) +
        labs(x = "Budget", y = "Revenue", title = "Budget vs. Revenue by Action Genre") +
        scale_color_discrete(name = "Action Genre") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_minimal() +
      # Add a point for the new movie
      geom_point(aes(x = 50, y = new_revenue), color = "red", size = 3)
      ggplotly(budget_revenue_plot)
    })
    
    output$Prediction_GenreRevenuePrediction <- renderPlotly({
      genre_revenue <- movie_data %>%
        pivot_longer(cols = Action:Documentary, names_to = "genre", values_to = "is_genre") %>%
        filter(is_genre == 1) %>%
        group_by(genre) %>%
        summarize(mean_revenue = mean(revenue, na.rm = TRUE))
      genre_revenue_plot <- ggplot(genre_revenue, aes(x = genre, y = mean_revenue, fill = genre)) +
        geom_bar(stat = "identity") +
        labs(x = "Genre", y = "Mean Revenue", title = "Mean Revenue by Genre") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_minimal() +
        theme(legend.position = "none") +
        coord_flip()
      ggplotly(genre_revenue_plot)
    })
    
    output$predictionResult <- renderText({
      new_movie <- data.frame(budget = input$predictionBudgetSelected, 
                              Action = 0, 
                              Adventure = 0,
                              `Science Fiction` = 0,
                              Comedy = 0, 
                              Drama = 0, 
                              Romance = 0,
                              Animation = 0,
                              Thriller = 0,
                              Horror = 0,
                              Mystery = 0,
                              Fantasy = 0,
                              War = 0,
                              History = 0,
                              Family = 0,
                              Crime =0,
                              Music = 0,
                              Western = 0,
                              `TV Movie` = 0,
                              Documentary = 0,
                              id = 12345
      )
      
      for (genreInput in input$predictionGenreSelected) {
        new_movie[genreInput] = 1
      }
      names(new_movie)[4] <- "Science Fiction"
      names(new_movie)[19] <- "TV Movie"
      # make prediction with fitted model
      new_revenue <- predict(model, new_movie)
      
      genre_revenue <- movie_data %>%
        pivot_longer(cols = Action:Documentary, names_to = "genre", values_to = "is_genre") %>%
        filter(is_genre == 1) %>%
        group_by(genre) %>%
        summarize(mean_revenue = mean(revenue, na.rm = TRUE))
      genre_revenue_plot <- ggplot(genre_revenue, aes(x = genre, y = mean_revenue, fill = genre)) +
        geom_bar(stat = "identity") +
        labs(x = "Genre", y = "Mean Revenue", title = "Mean Revenue by Genre") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_minimal() +
        theme(legend.position = "none") +
        coord_flip()
      ggplotly(genre_revenue_plot)
      # Scatter plot of budget vs revenue
      budget_revenue_plot <- ggplot(movie_data, aes(x = budget, y = revenue, color = factor(Action))) +
        geom_point(alpha = 0.5) +
        labs(x = "Budget", y = "Revenue", title = "Budget vs. Revenue by Action Genre") +
        scale_color_discrete(name = "Action Genre") +
        theme(plot.title = element_text(hjust = 0.5)) +
        theme_minimal() +
        # Add a point for the new movie
        geom_point(aes(x = 50, y = new_revenue), color = "red", size = 3)
      print(new_revenue)
      paste0("🔮 Projected Revenue : ", as.character(round(new_revenue, 2)), " millions")
    })

}
