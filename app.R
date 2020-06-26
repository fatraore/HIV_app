
# Setup -----------------------------------------------------------------------------

# Package

library(shiny)
library(tidyverse)
library(DT)


# Function --------------------------------------------------------------------------

# returndt : Return the data base
returndt <- function(table, type) eval(parse(text = paste(table, type, sep = "_")))

# Data ------------------------------------------------------------------------------

# Number of people (all ages) living with HIV
living_country <- read_csv('data/Number of people (all ages) living with HIV country.csv',
                           skip = 1, col_types = cols(.default = 'c'))
living_region <- read_csv('data/Number of people (all ages) living with HIV region.csv',
                          skip = 1, col_types = cols(.default = 'c'))
# Number of deaths due to HIV/AIDS
death_country <- read_csv('data/Number of deaths due to HIV-AIDS country.csv', skip = 1,
                          col_types = cols(.default = 'c'))
death_region  <- read_csv('data/Number of deaths due to HIV-AIDS region.csv', skip = 1,
                          col_types = cols(.default = 'c'))
# Number of new HIV infections
new_country   <- read_csv('data/Number of new HIV infections country.csv', skip = 1,
                          col_types = cols(.default = 'c'), col_names = FALSE)
new_region    <- read_csv('data/Number of new HIV infections region.csv', skip = 1,
                          col_types = cols(.default = 'c'), col_names = FALSE)


# Transformation --------------------------------------------------------------------

# Number of people (all ages) living with HIV
living_country <- living_country %>%
    pivot_longer(-Country, names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)')

living_region <- living_region %>%
    pivot_longer(-`WHO region`, names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') 

death_country <-   death_country %>%
    pivot_longer(-Country,  names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%  
    map_df(str_replace_all, pattern = '&lt;', replacement = '')

death_region <-   death_region %>%
    pivot_longer(-`WHO region`,  names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%  
    map_df(str_replace_all, pattern = '&lt;', replacement = '')

new_country <-   new_country %>%
    mutate(X1 = replace_na(X1, replace = ''),
           X2 = replace_na(X2, replace = ''))
new_country[1,] <- str_c(new_country[1,], new_country[2,])
names(new_country) <- as.character(new_country[1,]) 
new_country <- new_country %>%
    slice(-(1:2)) %>%
    pivot_longer(-Country,  names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%  
    map_df(str_replace_all, pattern = '&lt;', replacement = '')
new_country_2018 <- new_country %>%
    filter(years == '2018')
new_country_percapita <- anti_join(new_country, new_country_2018) %>%
    mutate(sexe = str_extract(years, pattern = '(?<=\\d)[:alpha:].+'),
           years = str_extract(years, pattern = '\\d+'))

new_region <-   new_region %>%
    mutate(X1 = replace_na(X1, replace = ''),
           X2 = replace_na(X2, replace = ''))
new_region[1,] <- str_c(new_region[1,], new_region[2,])
names(new_region) <- as.character(new_region[1,]) 
new_region <- new_region %>%
    slice(-(1:2)) %>%
    pivot_longer(-`WHO region`,  names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%  
    map_df(str_replace_all, pattern = '&lt;', replacement = '')
new_region_2018 <- new_region %>%
    filter(years == '2018')
new_region_percapita <- anti_join(new_region, new_region_2018) %>%
    mutate(sexe = str_extract(years, pattern = '(?<=\\d)[:alpha:].+'),
           years = str_extract(years, pattern = '\\d+'))

# Define UI for application that draws a histogram
ui <- navbarPage(title = 'HIV app',
                 
                 tabPanel('Map'),
                 tabPanel('Graphic'),
                 
                 # show data
                 tabPanel('Data',
                          fluidPage(
                              
                              # Sidebar
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput(inputId = 'table', 'Choose a data:',
                                                  c('Number of people (all ages) living with HIV' = 'living',
                                                    'Number of new HIV infections' = 'new',
                                                    'Number of deaths due to HIV/AIDS' = 'death')
                                                  ),
                                      radioButtons(inputId = 'type', 'by:',
                                                   c('Country' = 'country',
                                                     'Region' = 'region')
                                                   )
                                                  
                                  ),
                                  
                                  # Show database
                                  mainPanel(
                                      DTOutput('hiv')
                                  )
                              )
                          )
                 ),
                 
                 # Show About
                 tabPanel('About',
                          includeMarkdown('about.md')
                          )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Panel data
output$hiv <- renderDT({
    hiv <- returndt(table = input$table, type = input$type)
    datatable(hiv, rownames = FALSE, filter = 'top', options = list(autoWidth = TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
