#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2movies)

# Set randomness seed
set.seed(61)
# Prepare data
shiny_movie_set <- 
    movies %>% 
    filter(year >= 2000) %>%
    select(title,year,length,rating,votes,Action:Short) %>% 
    gather(genre,value,Action:Short) %>% 
    filter(value == 1) %>% 
    select(-value)

#Inputs for sliderInput('vote')
votemin = min(shiny_movie_set$votes)
votemax = max(shiny_movie_set$votes)
votemedian = median(shiny_movie_set$votes)

# Get genre list
genres <- 
    shiny_movie_set %>% 
    distinct(genre) %>% 
    unlist(.)

names(genres) <- NULL

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel("Movie Length and IMDB Scores"),
    sidebarLayout(
        sidebarPanel(
            sliderInput('year', 'Years', 2000, 2005, c(2002, 2003), sep = ''),
            selectInput('genre', 'Genre', choices = c('All', genres), selected = 'All'),
            sliderInput('minvote', 'At Least X Votes', votemin, votemax, votemedian, sep = '')
        ),
        mainPanel(
            plotOutput('plot')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$plot = renderPlot({
        data_plot = shiny_movie_set %>%
            filter(genre == input$genre | 'All' %in% input$genre) %>%
            filter(votes>input$minvote) %>%
            filter(year >= input$year[1], year <= input$year[2])

        ggplot(data_plot, aes(x = length, y = rating, color = genre)) +
            geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
