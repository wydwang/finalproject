library(shiny)
library(tidyverse)
library(rvest)
library(httr)
library(stringr)
library(pool)


host <- "ruby.db.elephantsql.com"

con <- dbPool(
    RPostgres::Postgres(),
    dbname = "avlkfgtq",
    user = "avlkfgtq", password = Sys.getenv("mykey"), host = host
)

mychamp <- con %>% tbl("specinfo") %>% collect()

html <- read_html("https://www.op.gg/champion/statistics")
# read the info from the websit



champname <- html %>% 
    html_nodes("a") %>%
    html_nodes("div.champion-index__champion-item__name") %>% 
    html_text()
# scrap all the campion names


champurl <- html %>% 
    html_nodes("div.champion-index__champion-list") %>% 
    html_nodes("a") %>% 
    html_attr("href")
# scrap all the corresponding urls of each champion


position <- html %>% 
    html_nodes("a") %>%
    html_nodes("div.champion-index__champion-item__positions") %>% 
    html_text() 
# scrap champions' available positions

champnum <- c(1:length(champname))


i = 1
while (i <= length(champname)){
    position[i] <-  position[i] %>% 
        str_match_all("[A-Z][a-z]+")
    position[[i]] = replace(position[[i]], position[[i]] == "Middle", "Mid")
    # replace "Middle" with "Mid" for better future use
    position[[i]] = replace(position[[i]], position[[i]] == "Bottom", "Bot")
    # replace "Bottom" with "Bot" for better future use
    i = i+1
}
# Separate the positions


champinfo <- tibble(name = champname, url = champurl, num = champnum, posi = position) 
# combine the info into a tibble

# start of the real shiny app 
ui <- navbarPage(title = "LOL Stat",
    # used navbar to make tabs clearer
    tabPanel(title = "Search Stats by Name",
        # first tab panel, set for search champion stats by name and position
        sidebarLayout(
            sidebarPanel(
                textInput("cname", "Champion Name"),
                    # ask for champion name, not necessary
                selectInput("clist", "Select the Champion (required)", c("-",champname)),
                    # ask for the selected champion, mandatory
                selectInput("pos", "Champion Position (required)", "-"),
                    # ask for the desired position, mandatory
                actionButton("cstart", "Start")
                    # an action button to save memory when user is changing inputs
            ),
                              
            mainPanel(
                textOutput("winrate"),
                textOutput("pickrate")
                    # 2 outputs, only show up when user clicks search button
            )
        )
    ),
                 
    tabPanel(title = "Search Champion by Condition",
        # second tab panel, set for search champion by some conditions
        sidebarLayout(
            sidebarPanel(
                radioButtons("posbox", "Choose the Position", 
                            c("Top","Jungle", "Mid", "Bot", "Support")),
                    # a radio button function to get desired position
                sliderInput("cslide", "Win Rate Range", 
                            round(min(as.numeric(mychamp$win_rate))-1), 
                            round(max(as.numeric(mychamp$win_rate))+1), 
                            c(49,50)),
                    # a slider to get the desired win rate
                actionButton("rstart", "Start")
                    # also an action button to suppress output
            ),
                              
            mainPanel(
                textOutput("champ"),
                tableOutput("champtable")
            )
        )
    ),
                 
    tabPanel(title = "Some Other Stats",
        # last tab panel, set for overall stat graphs
        sidebarLayout(
            sidebarPanel(
                radioButtons("lastbox", "Choose the Position", 
                            c("Top","Jungle", "Mid", "Bot", "Support")),
                    # a radiobutton similar to the previous one
                actionButton("gstart", "Start")
            ),
                              
            mainPanel(
                textOutput("topf"),
                tableOutput("toptable"),
                textOutput("botf"),
                tableOutput("bottable"),
                plotOutput("topgraph")
            )
        )
    )
)




server <- function(input, output, session) {
    
    
    observeEvent(input$cname,{
        newlist <- champname %>% 
            str_subset(paste("(?i)^",input$cname, sep = ""))
        updateSelectInput(session, "clist","Select the Champion (required)", 
                          c("-", newlist))
    })
        # use observeEvent function to change champion list when there's input in names
    
    
    observeEvent(input$clist,{
        if (input$clist != "-"){
            n <- champinfo[champinfo$name == input$clist,]$num
            updateSelectInput(session, "pos","Champion Position (required)", 
                              c("-", position[[n]]))
        } else {
            updateSelectInput(session, "pos","Champion Position (required)", 
                              "-")
        }
    })
        # change champion position basse on the selected champion
        # use if-else for different situations
    
    
    
    winpick <- reactive({
        if (input$clist != "-"){
            mychamp %>% 
                filter(name == input$clist) %>% 
                filter(position == input$pos)
        }
    })
        # a reactive value that filter mychamp base on inputs. 
    
    
    
    
    firstout <- observeEvent(input$cstart, {
            # the outputs of first panel
        
        output$winrate <- renderText({
            req(input$pos != "-")
            paste(input$pos, input$clist, "'s Current Win Rate:", winpick()$win_rate,"%")
        })
            # output the win rate of chosen champion in chosen position
        

        
        output$pickrate <- renderText({
            req(input$pos != "-")
            paste(input$pos, input$clist, "'s Current Pick Rate:", winpick()$pick_rate,"%")
        })
            # output the pick rate of chosen champion in chosen position
    })

    
    
    
    secondout <- observeEvent(input$rstart, {
            # the outputs of second panel
        
        champfilter <- reactive({
            mychamp %>% 
                filter(position == input$posbox) %>% 
                filter(win_rate >= input$cslide[1]) %>% 
                filter(win_rate <= input$cslide[2]) %>% 
                select(name, win_rate) %>% 
                arrange(desc(as.numeric(win_rate))) %>% 
                mutate(win_rate = paste(win_rate, "%", sep = ""))
        })
            # get the champions by setted condition in the dataframe
        
        cnum <- reactive({
            champfilter() %>% 
                summarise(n = n())
        })
            # save the number of champions after filtration
        
        output$champ <- renderText({
            paste("There are", cnum()$n, 
                  "Champions that fits your choice:", sep = " ")
        }) 
            # output a line with number of champions
        
        output$champtable <- renderTable({
            if (cnum()$n == 0){
                paste("Please Choose Another Win Rate Range")
            } else {
                champfilter()
            }
        })
            # simply output the data in champfilter dataframe
    })
    
    
    botplot  <- reactive({
        req(input$lastbox)
        as.numeric(mychamp[mychamp$position == input$lastbox,]$win_rate)
    })
        # save the win rate info as numerics
    
    lastout <- observeEvent(input$gstart, {
            # outputs of the last panel
        
        output$topf <- renderText({
            paste("Best 5", input$lastbox, "Champions")
        })
            # a line containing the inpus position
        
        output$toptable <- renderTable({
            mychamp %>% 
                filter(position == input$lastbox) %>% 
                arrange(desc(win_rate)) %>% 
                head(5) %>% 
                select(name, win_rate) %>% 
                mutate(win_rate = paste(win_rate, "%", sep = ""))
        })
            # output the 5 champions in designated position with highest win rate
        
        
        output$botf <- renderText({
            paste("Worst 5", input$lastbox, "Champions")
        })
            # a line containing the inpus position
        
        output$bottable <- renderTable({
            mychamp %>% 
                filter(position == input$lastbox) %>% 
                arrange(win_rate) %>% 
                head(5) %>% 
                select(name, win_rate) %>% 
                mutate(win_rate = paste(win_rate, "%", sep = ""))
        })
            # output the 5 champions in designated position with lowest win rate
        
        output$topgraph <- renderPlot({
            hist(botplot(), 
                 main = paste(input$lastbox, "Champions Win Rate Trend", sep = " "),
                 xlab = "Win Rate")
        })
            # output the histogram of champions' win rate in that position
    })
    
    
    

    onStop(function(){
        poolClose(con)
    })
        # close the database connection when user closed the app
        # learned from stackoverflow
    
}


# Run the application 
shinyApp(ui = ui, server = server)
