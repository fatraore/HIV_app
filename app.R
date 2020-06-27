
# Setup -----------------------------------------------------------------------------

# Package

library(shiny)
library(shinyWidgets)
library(tidyverse)
library(DT)
library(plotly)
library(scales)
library(maps)


# Function --------------------------------------------------------------------------

# returndt : Return the data base
returndt <- function(table, type) eval(parse(text = paste(table, type, sep = '_')))

# Data ------------------------------------------------------------------------------

# Number of people (all ages) living with HIV
living_country <- read_csv('data/Number of people (all ages) living with HIV country.csv',
                           skip = 1, col_types = cols(.default = 'c'), na = 'No data')
living_region <- read_csv('data/Number of people (all ages) living with HIV region.csv',
                          skip = 1, col_types = cols(.default = 'c'), na = 'No data')
# Number of deaths due to HIV/AIDS
death_country <- read_csv('data/Number of deaths due to HIV-AIDS country.csv', skip = 1,
                          col_types = cols(.default = 'c'), na = 'No data')
death_region  <- read_csv('data/Number of deaths due to HIV-AIDS region.csv', skip = 1,
                          col_types = cols(.default = 'c'), na = 'No data')
# Number of new HIV infections
new_country   <- read_csv('data/Number of new HIV infections country.csv', skip = 1,
                          col_types = cols(.default = 'c'), col_names = FALSE, na = 'No data')
new_region    <- read_csv('data/Number of new HIV infections region.csv', skip = 1,
                          col_types = cols(.default = 'c'), col_names = FALSE, na = 'No data')


# Transformation --------------------------------------------------------------------

# Number of people (all ages) living with HIV
living_country <- living_country %>%
    pivot_longer(-Country, names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%
    separate(interval, c('imin', 'imax'), sep = '–') %>%
    mutate(years = as.numeric(years),
           count = as.numeric(str_replace_all(count, '\\s+', '')),
           imin = as.numeric(str_replace_all(imin, '\\[|\\s+', '')),
           imax = as.numeric(str_replace_all(imax, '\\]|\\s+', ''))
    )

living_region <- living_region %>%
    pivot_longer(-`WHO region`, names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%
    separate(interval, c('imin', 'imax'), sep = ' - ') %>%
    mutate(`WHO region` = factor(`WHO region`, levels = as.character(unique(`WHO region`))),
           years = as.numeric(years),
           count = as.numeric(str_replace_all(count, '\\s+', '')),
           imin = as.numeric(str_replace_all(imin, '\\[|\\s+', '')),
           imax = as.numeric(str_replace_all(imax, '\\]|\\s+', ''))
    )

death_country <-   death_country %>%
    pivot_longer(-Country,  names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%  
    map_df(str_replace_all, pattern = '&lt;', replacement = '') %>%
    separate(interval, c('imin', 'imax'), sep = '–') %>%
    mutate(years = as.numeric(years),
           count = as.numeric(str_replace_all(count, '\\s+', '')),
           imin = as.numeric(str_replace_all(imin, '\\[|\\s+', '')),
           imax = as.numeric(str_replace_all(imax, '\\]|\\s+', ''))
    )

death_region <- death_region %>%
    pivot_longer(-`WHO region`,  names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%  
    map_df(str_replace_all, pattern = '&lt;', replacement = '') %>%
    separate(interval, c('imin', 'imax'), sep = ' - ') %>%
    mutate(`WHO region` = factor(`WHO region`, levels = as.character(unique(`WHO region`))),
           years = as.numeric(years),
           count = as.numeric(str_replace_all(count, '\\s+', '')),
           imin = as.numeric(str_replace_all(imin, '\\[|\\s+', '')),
           imax = as.numeric(str_replace_all(imax, '\\]|\\s+', ''))
    )

new_country <- new_country %>%
    set_names(str_c(new_country[1,], new_country[2,])) %>%
    slice(-(1:2)) %>%
    pivot_longer(-Country,  names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%  
    map_df(str_replace_all, pattern = '&lt;', replacement = '')

new_country_2018 <- new_country %>%
    filter(years == '2018') %>%
    separate(interval, c('imin', 'imax'), sep = '–') %>%
    mutate(years = as.numeric(years),
           count = as.numeric(str_replace_all(count, '\\s+', '')),
           imin = as.numeric(str_replace_all(imin, '\\[|\\s+', '')),
           imax = as.numeric(str_replace_all(imax, '\\]|\\s+', ''))
    )
new_country_percapita <- new_country %>%
    filter(years != '2018') %>%
    mutate(sexe = factor(str_extract(years, pattern = '(?<=\\d)[:alpha:].+')),
           years = as.numeric(str_extract(years, pattern = '\\d+'))) %>%
    separate(interval, c('imin', 'imax'), sep = ' - ') %>%
    mutate(count = as.numeric(str_replace_all(count, '\\s+', '')),
           imin = as.numeric(str_replace_all(imin, '\\[|\\s+', '')),
           imax = as.numeric(str_replace_all(imax, '\\]|\\s+', ''))
    )

new_region <- new_region %>%
    set_names(str_c(new_region[1,], new_region[2,])) %>%
    slice(-(1:2)) %>%
    pivot_longer(-`WHO region`,  names_to = 'years', values_to = 'count') %>%
    separate(col = count, into = c('count', 'interval'), sep = '\\s(?=\\[)') %>%  
    map_df(str_replace_all, pattern = '&lt;', replacement = '') %>%
    separate(interval, c('imin', 'imax'), sep = ' - ') %>%
    mutate(`WHO region` = factor(`WHO region`, levels = as.character(unique(`WHO region`))),
           count = as.numeric(str_replace_all(count, '\\s+', '')),
           imin = as.numeric(str_replace_all(imin, '\\[|\\s+', '')),
           imax = as.numeric(str_replace_all(imax, '\\]|\\s+', ''))
    )
new_region_2018 <- new_region %>%
    filter(years == '2018') %>%
    mutate(years = as.numeric(years))
new_region_percapita <- new_region %>%
    filter(years != '2018') %>%
    mutate(sexe = factor(str_extract(years, pattern = '(?<=\\d)[:alpha:].+')),
           years = as.numeric(str_extract(years, pattern = '\\d+')))

# Define UI for application that draws a histogram
ui <- navbarPage(title = 'HIV app',
                 
                 tabPanel('Map'),
                 
                 # Panel Graphic
                 tabPanel('Graphic',
                          fluidPage(
                              
                              # Sidebar
                              sidebarLayout(
                                  sidebarPanel(
                                      selectInput(inputId = 'gtable', 'Choose a data:',
                                                  c('Number of people (all ages) living with HIV' = 'living',
                                                    'Number of new HIV infections' = 'new',
                                                    'Number of deaths due to HIV/AIDS' = 'death')
                                      ),
                                      conditionalPanel(
                                          condition = 'input.gtable == "new"',
                                          materialSwitch(inputId = 'gswitch',
                                                         label = 'Per capita',
                                                         status = 'primary',
                                                         right = TRUE)
                                      ),
                                      conditionalPanel(
                                          condition = 'input.gswitch && input.gtable == "new"',
                                          sliderInput('year','Time',
                                                      min = min(new_country_percapita$years, na.rm = TRUE),
                                                      max = max(new_country_percapita$years, na.rm = TRUE),
                                                      value = min(new_country_percapita$years, na.rm = TRUE),
                                                      step = 1,
                                                      animate = TRUE)
                                      ),
                                      multiInput(
                                          inputId = 'countries',
                                          label = 'Countries :', 
                                          choices = NULL,
                                          choiceNames = sort(unique(living_country$Country)),
                                          choiceValues = sort(unique(living_country$Country))
                                      )
                                  ),
                                  
                                  # Show plot
                                  mainPanel(
                                      plotOutput('plot1'),
                                      plotOutput('plot2')
                                  )
                              )
                          )
                 ),
                 
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
                                      ),
                                      conditionalPanel(
                                          condition = 'input.table == "new"',
                                          materialSwitch(inputId = 'switch',
                                                         label = 'Per capita',
                                                         status = 'primary',
                                                         right = TRUE)
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
                 ),
                 # Show License
                 tabPanel('License'#,
                          #includeMarkdown('license.md')
                 )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #panel Graphic
    output$plot1<-renderPlot({
        db <- returndt(table = input$gtable, type = 'country')
        
        if (isFALSE(input$gswitch) & input$gtable == 'new') {
            db <- new_country_2018
        } else if (isTRUE(input$gswitch) & input$gtable == 'new') {
            db <- new_country_percapita %>%
                filter(years == input$year)
        }
        
        if (is.null(input$countries)) {
            db <- db %>%
                filter(Country %in% c('France', 'Niger', 'Uganda') & !is.na(count))
        } else {
            db <- db %>%
                filter(Country %in% input$countries & !is.na(count))
        }
        
        if (isFALSE(input$gswitch) & input$gtable == 'new') {
            db %>%
                ggplot(aes(x = Country, y = count, fill = Country)) +
                geom_col() +
                scale_fill_viridis_d() +
                theme_linedraw() +
                geom_errorbar(aes(ymin=imin, ymax=imax), width=.2,
                              position=position_dodge(.9)) +
                theme(legend.position = 'none') +
                labs(x = 'Country', y = 'Count',
                     caption = 'Source: WHO (https://apps.who.int/gho/data/node.main.618?lang=en)',
                     subtitle = str_c('Years : ', str_c(sort(unique(db$years)), collapse = '-')),
                     title = case_when(
                         input$gtable == 'living' ~ 'Number of people (all ages) living with HIV',
                         input$gtable  == 'new' ~ 'Number of new HIV infections',
                         input$gtable  == 'death' ~ 'Number of deaths due to HIV/AIDS')
                )
        } else if (isTRUE(input$gswitch) & input$gtable == 'new') {
            db %>%
                ggplot(aes(x = Country, y = count, fill = sexe)) +
                geom_col(position = position_dodge()) +
                scale_fill_viridis_d() +
                theme_linedraw() +
                geom_errorbar(aes(ymin=imin, ymax=imax), width=.2,
                              position=position_dodge(.9)) +
                labs(x = 'Country', y = 'Count',
                     caption = 'Source: WHO (https://apps.who.int/gho/data/node.main.618?lang=en)',
                     subtitle = str_c('Years : ', input$year),
                     title = case_when(
                         input$gtable == 'living' ~ 'Number of people (all ages) living with HIV',
                         input$gtable  == 'new' ~ 'Number of new HIV infections',
                         input$gtable  == 'death' ~ 'Number of deaths due to HIV/AIDS')
                )
        } else {
            db %>%
                ggplot(aes(x = years,y = count, group = Country, color = Country)) +
                geom_line() +
                geom_point() +
                theme_linedraw() +
                labs(x = 'Years', y = 'Count',
                     caption = 'Source: WHO (https://apps.who.int/gho/data/node.main.618?lang=en)',
                     subtitle = str_c('Years : ', str_c(sort(unique(db$years)), collapse = '-')),
                     title = case_when(
                         input$gtable == 'living' ~ 'Number of people (all ages) living with HIV',
                         input$gtable  == 'new' ~ 'Number of new HIV infections',
                         input$gtable  == 'death' ~ 'Number of deaths due to HIV/AIDS')
                ) +
                scale_fill_viridis_d() +
                geom_errorbar(aes(ymin=imin, ymax=imax), width=.2)
        }
        
    })
    output$plot2<-renderPlot({
        db <- returndt(table = input$gtable, type = 'region')
        
        if (isFALSE(input$gswitch) & input$gtable == 'new') {
            db <- new_region_2018
        } else if (isTRUE(input$gswitch) & input$gtable == 'new') {
            db <- new_region_percapita %>%
                filter(years == input$year)
        }
        if (isFALSE(input$gswitch) & input$gtable == 'new') {
            db %>%
                ggplot(aes(x = `WHO region`, y = count, fill = `WHO region`)) +
                geom_col() +
                scale_fill_viridis_d() +
                theme_linedraw() +
                geom_errorbar(aes(ymin=imin, ymax=imax), width=.2,
                              position=position_dodge(.9)) +
                theme(legend.position = 'none') +
                labs(x = '`WHO region`', y = 'Count',
                     caption = 'Source: WHO (https://apps.who.int/gho/data/node.main.618?lang=en)',
                     subtitle = str_c('Years : ', str_c(sort(unique(db$years)), collapse = '-')),
                     title = case_when(
                         input$gtable == 'living' ~ 'Number of people (all ages) living with HIV',
                         input$gtable  == 'new' ~ 'Number of new HIV infections',
                         input$gtable  == 'death' ~ 'Number of deaths due to HIV/AIDS')
                )
        } else if (isTRUE(input$gswitch) & input$gtable == 'new') {
            db %>%
                ggplot(aes(x = `WHO region`, y = count, fill = sexe)) +
                geom_col(position = position_dodge()) +
                scale_fill_viridis_d() +
                theme_linedraw() +
                geom_errorbar(aes(ymin=imin, ymax=imax), width=.2,
                              position=position_dodge(.9)) +
                labs(x = 'Country', y = 'Count',
                     caption = 'Source: WHO (https://apps.who.int/gho/data/node.main.618?lang=en)',
                     subtitle = str_c('Years : ', input$year),
                     title = case_when(
                         input$gtable == 'living' ~ 'Number of people (all ages) living with HIV',
                         input$gtable  == 'new' ~ 'Number of new HIV infections',
                         input$gtable  == 'death' ~ 'Number of deaths due to HIV/AIDS')
                )
        } else {
            db %>%
                ggplot(aes(x = `WHO region`, y = count, fill = `WHO region`)) +
                geom_col() +
                geom_errorbar(aes(ymin=imin, ymax=imax), width=.2,
                              position=position_dodge(.9)) +
                scale_fill_viridis_d() +
                # coord_flip() +
                theme_linedraw() +
                labs(x = 'WHO region', y = 'Count',
                     caption = 'Source: WHO (https://apps.who.int/gho/data/node.main.618?lang=en)',
                     subtitle = 'Year : 2018',
                     title = case_when(
                         input$gtable == 'living' ~ 'Number of people (all ages) living with HIV',
                         input$gtable  == 'new' ~ 'Number of new HIV infections',
                         input$gtable  == 'death' ~ 'Number of deaths due to HIV/AIDS')
                )
        }
    })
    
    # Panel data
    output$hiv <- renderDT({
        db <- returndt(table = input$table, type = input$type)
        if (input$table == 'new' & input$type == 'country' & isFALSE(input$switch)) {
            db <- new_country_2018
        } else if (input$table == 'new' & input$type == 'country' & isTRUE(input$switch)) {
            db <- new_country_percapita
        } else if (input$table == 'new' & input$type == 'region' & isFALSE(input$switch)) {
            db <- new_region_2018
        } else if (input$table == 'new' & input$type == 'region' & isTRUE(input$switch)) {
            db <- new_region_percapita
        }
        datatable(db, rownames = FALSE, filter = 'top', caption = 'Source: WHO (https://apps.who.int/gho/data/node.main.618?lang=en)',
                  options = list(autoWidth = TRUE))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
