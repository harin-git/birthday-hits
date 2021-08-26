library(shiny)
library(dplyr)
library(shinythemes)
library(rsconnect)
rsconnect::setAccountInfo('XXXX')

###### Initialize ######
number_ones <- read.csv('number_ones_11012021.csv') %>%
    select(Country, Country_code, Year, Week_num, Song, Artist)

update_date <- function(date, add_years){
    year <- substr(date,0,4) %>% as.numeric() + add_years
    rest <- substring(date, 5, )
    new_date <- paste0(year, rest)
    print(new_date)
}
chart_table <- function(date, add_years){
    new_date <- update_date(date, add_years)
    year <- substr(new_date,0,4) %>% as.numeric()
    week <- strftime(new_date,'%V') %>% as.numeric()
    
    all_bday <- dplyr::filter(number_ones, Year==year & Week_num==week) %>%
        select(Country, Song, Artist)
}
mega_hits <- function(date, add_years){
  new_date <- update_date(date, add_years)
  year <- substr(new_date,0,4) %>% as.numeric()
  week <- strftime(new_date,'%V') %>% as.numeric()
  
  df <- dplyr::filter(number_ones, Year==year & Week_num==week)
  
  freq <- table(df$Song) %>% as.data.frame()
  mx <- freq$Freq %>% max()
  top_song <- filter(freq, Freq==mx)$Var1[1] %>% as.character()
  
  global_hit <- filter(df, Song == top_song)
  global_hit_track <- paste(global_hit$Song[1], 'by', global_hit$Artist[1])
  
  top_countries <- global_hit$Country %>% unlist()
  
  if (length(top_countries) > 1){
    top_countries <- paste(top_countries, collapse = ', ')
    paste('<strong>', global_hit_track,'</strong>', 'was the number one single in', 
          mx, 'countries:', '<br/>', top_countries)
  } else {
    print("In this week, there was no global hit. 
              The number one singles chart around the world were rather diverse.")
  }
}

# get_country <- function(date, add_years){
#   new_date <- update_date(date, add_years)
#   year <- substr(new_date,0,4) %>% as.numeric()
#   week <- strftime(new_date,'%V') %>% as.numeric()
#   
#   country <- dplyr::filter(number_ones, Year==year & Week_num==week) %>%
#     select(Country_code, Song, Artist)
#   rename(country, 'iso-a3' = Country_code)
# }


###### UI ######
ui <- fluidPage(theme = shinytheme('united'),

    # Menu on top
    navbarPage(
        "Music Around the Globe",
        tabPanel("Birthday",
                 h3("Discover the No.1 Pop Songs On Your Birthday")
        )
    ),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("name", label = h4("Your name:"), value = "Kurt Cobain", width = '200px'), #value argument is initial argument
            dateInput("date", label = h4("Date of birth:"), value = "1967-02-20", width = '200px'),
            actionButton('reset', "reset", height = '30px'),
            hr(),
            
            h4('Jump in time:'),
            actionButton("teen", "When I was a teenage rebel"),
            p(''),
            actionButton("age18", "When I became old enough to drink"),
            p(''),
            actionButton("age30", "When I turned 30"),
            p(''),
            actionButton("age50", "When I began making retirement plans"),
            p(''),
            hr(),
            
            h4('Make it a playlist'),
            helpText("Generate Spotify or Youtube playlist based on displayed songs."),
            actionLink("spotify", "Create Spotify Playlist"),
            p(''),
            actionLink("youtube", "Create Youtube Playlist"),
            helpText("prototype.v1")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            hr(),
            htmlOutput("introText"),
            hr(),
            h4("Hits Around the World"),
            tableOutput("chartTable"),
            hr(),
            h4("MEGA HIT"),
            htmlOutput("megaText"),
            hr()
        )
    )
)


###### SERVER ######
server <- function(input, output) {
    
    ## Observe when one of jump in time button is clicked
    t <- reactiveValues(date = 0)
    
    observeEvent(input$date, {
        t$date <- 0
    })
    
    observeEvent(input$reset, {
        t$date <- 0
    })
    
    observeEvent(input$teen, {
        t$date <- 14
    })
    
    observeEvent(input$age18, {
        t$date <- 18
    })
    
    observeEvent(input$age18, {
        t$date <- 18
    })
    
    observeEvent(input$age30, {
        t$date <- 30
    })
    
    observeEvent(input$age50, {
        t$date <- 50
    })
    
    ## Outputs
    output$introText <- renderUI({
        if (t$date == 0){
            HTML(paste0(
                'On <strong>', input$date, '</strong>,', '<strong> ', input$name, 
                '</strong> was born.', '<br/>',
                'On this glorious day, these songs were rocking the charts around the world.'
            ))
        } else if (t$date == 14){
            HTML(paste0(
                'On <strong>', update_date(input$date, t$date), '</strong>,', '<strong> ', 
                input$name, "</strong>'s 14th birthday, these songs were popular and you might remember some of them."
            ))
        } else if (t$date == 18){
            if (as.numeric(substring(input$date,0,4)) + t$date < 2020){
                HTML(paste0(
                    'On <strong>', update_date(input$date, t$date), '</strong>,', '<strong> ',
                    input$name, '</strong> just turned 18 years old.', '<br/>',
                    'Now drinking is legal and the party is on. Some of these songs were being blasted on the dance floor.'
                ))
            } else {
                HTML(paste0(
                    'Hang on... You are not ', t$date, " years old yet. You shouldn't be drinking!", '<br/>',
                    'But we have an award winning algorithm that predicts which songs will become hits in year <strong>',
                    as.numeric(substring(input$date,0,4)) + t$date, '</strong>.'
                ))
            }
        } else if (t$date == 30){
            if (as.numeric(substring(input$date,0,4)) + t$date < 2020){
                HTML(paste0(
                    'On <strong>', update_date(input$date, t$date), '</strong>,', '<strong> ',
                    input$name, '</strong> turned 30 years old.', '<br/>',
                    "Do you recognise some of these tunes? You probably heard them on your friends' weddings."
                ))
            } else {
                HTML(paste0(
                    'Hang on... You are not ', t$date, ' years old yet!', '<br/>',
                    'But we have an award winning algorithm that predicts which songs will become hits in year <strong>',
                    as.numeric(substring(input$date,0,4)) + t$date, '</strong>.'
                ))
            }
        } else if (t$date == 50){
            if (as.numeric(substring(input$date,0,4)) + t$date < 2020){
                HTML(paste0(
                    'On <strong>', update_date(input$date, t$date), '</strong>,', '<strong> ',
                    input$name, '</strong> turned 50 years old.', '<br/>',
                    "Your grandchildren would play some of these songs and you probably hated it."
                ))
            } else {
                HTML(paste0(
                    'Hang on... You are not old enough to be thinking about retirement yet!', '<br/>',
                    'But we have an award winning algorithm that predicts which songs will become hits in year <strong>',
                    as.numeric(substring(input$date,0,4)) + t$date, '</strong>.'
                ))
            }
        }
    })
    
    output$chartTable <- renderTable({
        if (as.numeric(substring(input$date,0,4)) + t$date < 2020){
            chart_table(input$date, t$date)
        } else {
            data_frame(Country = c('Argentina', 'Germany', 'Spain', 'Japan', 'Korea', 'Uruguay', 'US', 'UK'),
                       Song = 'Love Song From the Future', Artist = input$name)
        }
    })
    
    output$megaText <- renderUI({
        if (as.numeric(substring(input$date,0,4)) + t$date < 2020){
            HTML(mega_hits(input$date, t$date))
        } else {
            HTML(paste0(
                "In this year, the algorithm predicts that <strong> Love Song From the Future by ",
                input$name, "</strong> will completely dominate all charts around the world. <br/><p> </p>
                Wait a minute...<strong><h2> ARE YOU THE NEXT POP SUPERSTAR?!?! </h2></strong>"
            ))
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
