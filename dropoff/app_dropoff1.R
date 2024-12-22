library(tidyverse)
library(plotly)

# Replace 'ui <- fluidPage('...')' with
#       'shinyUI(fluidPage('...'))' if in separate ui.R file
ui <- fluidPage(
    titlePanel("Drop-Off Votes"),
    sidebarLayout(
        sidebarPanel(
            width = 2,
            checkboxInput("se_non_swing", "SE non-swing", value = FALSE),
            checkboxInput("use_statename", "Use state name", value = FALSE),
            selectInput("xgroup","Group by",
                        choices = c("state","swing/dem/rep","swing/non-swing"),
                        selected = "state"),
            selectInput("xunits","Units",
                        choices = c("count","percent"),
                        selected = "percent"),
            checkboxInput("xsenator", "Senator", value = TRUE),
            checkboxInput("xgovernor","Governor",value = TRUE),
            checkboxInput("xgov_de",  "Gov(DE)", value = TRUE),
            checkboxInput("xag_nc",   "AG(NC)",  value = TRUE),
            checkboxInput("skip_me",  "Skip(ME)",value = TRUE),
            checkboxInput("skip_vt",  "Skip(VT)",value = TRUE),
            checkboxInput("xhouse",   "House",   value = FALSE),
            numericInput("nudge_x","Nudge X",0),
            numericInput("nudge_y","Nudge Y",1)
        ),
        mainPanel(
            width = 10,
            tabsetPanel(
                type = "tabs",
                tabPanel("Plotly", plotlyOutput(outputId = "myPlotly", width = "1200px", height = "800px")),
                tabPanel("Plot", plotOutput(outputId = "myPlot", width = "1200px", height = "800px")),
                tabPanel("Table", tableOutput("myTable")),
                tabPanel("Data", verbatimTextOutput("myData")),
                tabPanel("Candidates", verbatimTextOutput("myCandidates")),
                tabPanel("Usage", htmlOutput(outputId = "myUsage"))
            )
        )
    )
)

# Replace 'server <- function(input, output) {'...'}' with
#       'shinyServer(function(input, output) {'...'})' if in separate server.R file
server <- function(input, output) {
    options(width = 999, readr.show_progress = FALSE)
    options(max.print=999999)
    sparty <- c("REP","REP","SWING","REP","DEM",
                "DEM","DEM","DEM","DC","REP",
                "SWING","DEM","REP","DEM","REP",
                "REP","REP","REP","REP","DEM",
                "DEM","DEM","SWING","DEM","REP",
                "REP","REP","REP","SWING","DEM",
                "DEM","DEM","DEM","SWING","REP",
                "REP","REP","DEM","SWING","DEM",
                "REP","REP","REP","REP","REP",
                "DEM","DEM","DEM","REP","SWING",
                "REP")
    state <-  c("Alabama","Alaska","Arizona","Arkansas","California",
                "Colorado","Connecticut","Delaware","DC","Florida",
                "Georgia","Hawaii","Idaho","Illinois","Indiana",
                "Iowa","Kansas","Kentucky","Louisiana","Maine",
                "Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                "South Carolina","South Dakota","Tennessee","Texas","Utah",
                "Vermont","Virginia","Washington","West Virginia","Wisconsin",
                "Wyoming")
    state2 <- c("AL","AK","AZ","AR","CA",
                "CO","CT","DE","DC","FL",
                "GA","HI","ID","IL","IN",
                "IA","KS","KY","LA","ME",
                "MD","MA","MI","MN","MS",
                "MO","MT","NE","NV","NH",
                "NJ","NM","NY","NC","ND",
                "OH","OK","OR","PA","RI",
                "SC","SD","TN","TX","UT",
                "VT","VA","WA","WV","WI","WY")
    zstate <-  c("Alaska","Alabama","Arkansas","Arizona","California",
                 "Colorado","Connecticut","DC","Delaware","Florida",
                 "Georgia","Hawaii","Iowa","Idaho","Illinois",
                 "Indiana","Kansas","Kentucky","Louisiana","Massachusetts",
                 "Maryland","Maine","Michigan","Minnesota","Missouri",
                 "Mississippi","Montana","North Carolina","North Dakota","Nebraska",
                 "New Hampshire","New Jersey","New Mexico","Nevada","New York",
                 "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                 "South Carolina","South Dakota","Tennessee","Texas","Utah",
                 "Virginia","Vermont","Washington","Wisconsin","West Virginia",
                 "Wyoming")
    zstate2 <- c("AK","AL","AR","AZ","CA",
                 "CO","CT","DC","DE","FL",
                 "GA","HI","IA","ID","IL",
                 "IN","KS","KY","LA","MA",
                 "MD","ME","MI","MN","MO",
                 "MS","MT","NC","ND","NE",
                 "NH","NJ","NM","NV","NY",
                 "OH","OK","OR","PA","RI",
                 "SC","SD","TN","TX","UT",
                 "VA","VT","WA","WI","WV","WY")
    stinfo <- data.frame(state,state2)
    
    read_president <- function(){
        con = file("president_cbs24.txt", "r")
        line = readLines(con, n = 1)
        while(startsWith(line, "#")){
            line = readLines(con, n = 1)
        }
        if(startsWith(line, "Presidential")){
            line = readLines(con, n = 1)
        }else{
            print(paste0("***** Expected Presidential, found ",line))
        }
        ee <- NULL
        while(length(line) > 0){
            state <- line
            line = readLines(con, n = 1)
            if(startsWith(line, "Electoral Votes:")){
                line = readLines(con, n = 1)
            }else{
                print(paste0("***** Expected Electoral Votes:, found ",line))
            }
            if(startsWith(line, "Updated:")){
                line = readLines(con, n = 1)
            }else{
                print(paste0("***** Expected Updated:, found ",line))
            }
            while(!startsWith(line,"State Results")){
                ss <- unlist(strsplit(line,"\t"))
                mm <- str_match(ss[1],"([A-Za-z ]+) \\(([A-Z])\\)")
                precinct <- "ALL"
                office <- "President of the US"
                district <- ""
                candidate <- mm[2]
                party <- mm[3]
                if (is.na(party))party <- "NA"
                else if (party == "D") party <- "DEM"
                else if (party == "R") party <- "REP"
                else if (party == "L") party <- "LIB"
                else if (party == "G") party <- "GRN"
                else if (party == "I") party <- "IND"
                votes <- as.numeric(gsub(",","",ss[2]))
                dd <- data.frame(state,precinct,office,district,party,candidate,votes)
                ee <- rbind(ee,dd)
                line = readLines(con, n = 1)
            }
            line = readLines(con, n = 1)
        }
        close(con)
        return(ee)
    }
    read_senate <- function(){
        con = file("senate_cbs24.txt", "r")
        line = readLines(con, n = 1)
        while(startsWith(line, "#")){
            line = readLines(con, n = 1)
        }
        if(startsWith(line, "Senate")){
            line = readLines(con, n = 1)
        }else{
            print(paste0("***** Expected Senate, found ",line))
        }
        ee <- NULL
        while(length(line) > 0){
            state <- line
            line = readLines(con, n = 1)
            if(startsWith(line, "Updated:")){
                line = readLines(con, n = 1)
            }else{
                print(paste0("***** Expected Updated:, found ",line))
            }
            while(!startsWith(line,"State Results")){
                ss <- unlist(strsplit(line,"\t"))
                mm <- str_match(ss[1],"([A-Za-z ]+) \\(([A-Z])\\)")
                precinct <- "ALL"
                office <- "US Senator"
                district <- ""
                candidate <- mm[2]
                party <- mm[3]
                if (is.na(party))party <- "NA"
                else if (party == "D") party <- "DEM"
                else if (party == "R") party <- "REP"
                else if (party == "L") party <- "LIB"
                else if (party == "G") party <- "GRN"
                else if (party == "I") party <- "IND"
                votes <- as.numeric(gsub(",","",ss[2]))
                dd <- data.frame(state,precinct,office,district,party,candidate,votes)
                ee <- rbind(ee,dd)
                line = readLines(con, n = 1)
            }
            line = readLines(con, n = 1)
        }
        close(con)
        return(ee)    
    }
    read_governor <- function(){
        con = file("governor_cbs24.txt", "r")
        line = readLines(con, n = 1)
        while(startsWith(line, "#")){
            line = readLines(con, n = 1)
        }
        if(startsWith(line, "Governor")){
            line = readLines(con, n = 1)
        }else{
            print(paste0("***** Expected Governor, found ",line))
        }
        ee <- NULL
        while(length(line) > 0){
            line <- gsub(" Governor's Race","",line)
            state <- line
            line = readLines(con, n = 1)
            if(startsWith(line, "Updated:")){
                line = readLines(con, n = 1)
            }else{
                print(paste0("***** Expected Updated:, found ",line))
            }
            while(!startsWith(line,"State Results")){
                ss <- unlist(strsplit(line,"\t"))
                mm <- str_match(ss[1],"([A-Za-z ]+) \\(([A-Z])\\)")
                precinct <- "ALL"
                office <- "Governor"
                district <- ""
                candidate <- mm[2]
                party <- mm[3]
                if (is.na(party))party <- "NA"
                else if (party == "D") party <- "DEM"
                else if (party == "R") party <- "REP"
                else if (party == "L") party <- "LIB"
                else if (party == "G") party <- "GRN"
                else if (party == "I") party <- "IND"
                else if (party == "P") party <- "GMP"
                votes <- as.numeric(gsub(",","",ss[2]))
                dd <- data.frame(state,precinct,office,district,party,candidate,votes)
                ee <- rbind(ee,dd)
                line = readLines(con, n = 1)
            }
            line = readLines(con, n = 1)
        }
        close(con)
        return(ee)
    }
    read_house <- function(){
        con = file("house_cbs24.txt", "r")
        line = readLines(con, n = 1)
        while(startsWith(line, "#")){
            line = readLines(con, n = 1)
        }
        if(startsWith(line, "House")){
            line = readLines(con, n = 1)
        }else{
            print(paste0("***** Expected House, found ",line))
        }
        ee <- NULL
        while(length(line) > 0){
            mm <- str_match(line,"([A-Za-z ]+) ([0-9]+)")
            state <- mm[2]
            district <- as.numeric(mm[3])
            line = readLines(con, n = 1)
            if(startsWith(line, "Updated:")){
                line = readLines(con, n = 1)
            }else{
                print(paste0("***** Expected Updated:, found ",line))
            }
            while(!startsWith(line,"State Results")){
                ss <- unlist(strsplit(line,"\t"))
                mm <- str_match(ss[1],"([A-Za-z ]+) \\(([A-Z])\\)")
                precinct <- "ALL"
                office <- "US House"
                candidate <- mm[2]
                party <- mm[3]
                if (is.na(party))party <- "NA"
                else if (party == "D") party <- "DEM"
                else if (party == "R") party <- "REP"
                else if (party == "L") party <- "LIB"
                else if (party == "G") party <- "GRN"
                else if (party == "I") party <- "IND"
                votes <- as.numeric(gsub(",","",ss[2]))
                dd <- data.frame(state,precinct,office,district,party,candidate,votes)
                ee <- rbind(ee,dd)
                line = readLines(con, n = 1)
            }
            line = readLines(con, n = 1)
        }
        close(con)
        return(ee)
    }
    getcandidates <- reactive({
        ee <- read_president()
        dd <- read_senate()
        ee <- rbind(ee,dd)
        dd <- read_governor()
        ee <- rbind(ee,dd)
        dd <- read_house()
        ee <- rbind(ee,dd)
        ee$state[ee$state == "Washington D.C."] <- "DC"
        if (input$xag_nc){
            ee$votes[ee$state == "North Carolina" & ee$party == "DEM" & ee$office == "Governor"] <- 2874960
            ee$votes[ee$state == "North Carolina" & ee$party == "REP" & ee$office == "Governor"] <- 2715411
        }
        write_csv(ee,"federal2024.csv")
        if (input$se_non_swing){
            ee <- ee[ee$state %in% c("Arizona","Georgia","Michigan","Nevada","North Carolina",
                                     "Pennsylvania","Wisconsin","Ohio",
                                     "California","Connecticut","Delaware","Florida","Hawaii",
                                     "Indiana","Maryland","Montana","New York","North Dakota"),]
        }
        return(ee)
    })
    getdata <- reactive({
        ee <- getcandidates()
        ff <- data.frame(state,state2,sparty)
        ff$dem1 <- 0
        ff$rep1 <- 0
        ff$dem2 <- 0
        ff$rep2 <- 0
        ff$office2 <- ""
        for (i in 1:NROW(ff)){
            ff$office2[i] <- ""
            if (input$xsenator){
                if ("US Senator" %in% unique(ee$office[ee$state == ff$state[i] & ee$party == "DEM"])){
                    if (!input$xgov_de | ff$state2[i] != "DE"){
                        ff$dem1[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "DEM" & ee$office == "President of the US"]
                        ff$dem2[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "DEM" & ee$office == "US Senator"]
                        ff$office2[i] <- "US Senator"
                    }
                }
            }
            if (input$xgovernor){
                if (ff$office2[i] == ""){
                    if ("Governor" %in% unique(ee$office[ee$state == ff$state[i] & ee$party == "DEM"])){
                        ff$dem1[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "DEM" & ee$office == "President of the US"]
                        ff$dem2[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "DEM" & ee$office == "Governor"]
                        ff$office2[i] <- "Governor"
                    }
                }
            }
            if (input$xhouse){
                if (ff$office2[i] == ""){
                    if ("US House" %in% unique(ee$office[ee$state == ff$state[i] & ee$party == "DEM"])){
                        ff$dem1[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "DEM" & ee$office == "President of the US"]
                        ff$dem2[i] <- sum(ee$votes[ee$state == ff$state[i] & ee$party == "DEM" & ee$office == "US House"])
                        ff$office2[i] <- "US House"
                    }
                }
            }
            ff$office2[i] <- ""
            if (input$xsenator){
                if (!input$xgov_de | ff$state2[i] != "DE"){
                    if ("US Senator" %in% unique(ee$office[ee$state == ff$state[i] & ee$party == "REP"])){
                        ff$rep1[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "REP" & ee$office == "President of the US"]
                        ff$rep2[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "REP" & ee$office == "US Senator"]
                        ff$office2[i] <- "US Senator"
                    }
                }
            }
            if (input$xgovernor){
                if (ff$office2[i] == ""){
                    if ("Governor" %in% unique(ee$office[ee$state == ff$state[i] & ee$party == "REP"])){
                        ff$rep1[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "REP" & ee$office == "President of the US"]
                        ff$rep2[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "REP" & ee$office == "Governor"]
                        ff$office2[i] <- "Governor"
                    }
                }
            }
            if (input$xhouse){
                if (ff$office2[i] == ""){
                    if ("US House" %in% unique(ee$office[ee$state == ff$state[i] & ee$party == "REP"])){
                        ff$rep1[i] <- ee$votes[ee$state == ff$state[i] & ee$party == "REP" & ee$office == "President of the US"]
                        ff$rep2[i] <- sum(ee$votes[ee$state == ff$state[i] & ee$party == "REP" & ee$office == "US House"])
                        ff$office2[i] <- "US House"
                    }
                }
            }
        }
        ff <- ff[ff$state != "DC",]
        if (input$xag_nc ){
            ff$office2[ff$state == "North Carolina" & ff$office2 == "Governor"] <- "AG"
        }
        if (input$skip_me) ff <- ff[ff$state != "Maine",]
        if (input$skip_vt) ff <- ff[ff$state != "Vermont",]
        if (input$xgroup %in% c("swing/dem/rep","swing/non-swing")){
            if (input$xgroup == "swing/non-swing"){
                ff$sparty[ff$sparty == "DEM"] <- "1-NON-SWING"
                ff$sparty[ff$sparty == "REP"] <- "1-NON-SWING"
            }
            save_names <- names(ff)
            ff <- ff %>% group_by(sparty) %>% summarize(
                state  <- min(sparty),
                state2 <- min(sparty),
                dem1 <- sum(dem1),
                rep1 <- sum(rep1),
                dem2 <- sum(dem2),
                rep2 <- sum(rep2)
            )
            names(ff) <- save_names
        }
        ff <- as.data.frame(ff)
        ff$dem_dropoff <- 0
        ff$rep_dropoff <- 0
        for (i in 1:NROW(ff)){
            if (ff$dem1[i] == 0 & ff$dem2[i] == 0){
                ff$dem_dropoff[i] <- 0
            }
            else{
                if (input$xunits == "count"){
                    ff$dem_dropoff[i] <- ff$dem1[i] - ff$dem2[i]
                }
                else{
                    ff$dem_dropoff[i] <- 100 * (ff$dem1[i] - ff$dem2[i]) / ff$dem1[i]
                }
            }
            if (ff$rep1[i] == 0 & ff$rep2[i] == 0){
                ff$rep_dropoff[i] <- 0
            }
            else{
                if (input$xunits == "count"){
                    ff$rep_dropoff[i] <- ff$rep1[i] - ff$rep2[i]
                }
                else{
                    ff$rep_dropoff[i] <- 100 * (ff$rep1[i] - ff$rep2[i]) / ff$rep1[i]
                }
            }
            
        }
        ff <- ff[ff$dem_dropoff != 0,]
        ff$sparty[ff$sparty == "REP"] <- "1-REP"
        ff$sparty[ff$sparty == "SWING"] <- "2-SWING"
        ff$sparty[ff$sparty == "DEM"] <- "3-DEM"
        ff$sparty[ff$sparty == "NON-SWING"] <- "1-NON-SWING"
        return(ff)
    })
    output$myPlot <- renderPlot({
        ff <- getdata()
        stitle <- "Comparison of Drop-Off Votes from President to Next Major Down-Ballot Race, GE 2024"
        if (input$use_statename){
            gg <- ggplot(data=ff, aes_string(x="dem_dropoff",y="rep_dropoff",label="state"))
            gg <- gg + annotate("text", x = ff$dem_dropoff, y =ff$rep_dropoff, label = ff$state,
                                color="black", hjust = 0, vjust = 0)
        }
        else{
            gg <- ggplot(data=ff, aes_string(x="dem_dropoff",y="rep_dropoff",label="state2"))
            gg <- gg + annotate("text", x = ff$dem_dropoff, y =ff$rep_dropoff, label = ff$state2,
                                color="black", hjust = 0, vjust = 0)
        }
        gg <- gg + geom_point(aes_string(color="sparty",size=2,alpha=1.0))
        gg <- gg + ggtitle(stitle)
        gg <- gg + xlab("Democrat Drop-off (percent)")
        gg <- gg + ylab("Republican Drop-off (percent)")
        gg
    })
    output$myPlotly <- renderPlotly({
        ff <- getdata()
        stitle <- "Comparison of Drop-Off Votes from President to Next Major Down-Ballot Race, GE 2024"
        if (input$use_statename){
            gg <- ggplot(data=ff, aes_string(x="dem_dropoff",y="rep_dropoff",label="state"))
        }
        else{
            gg <- ggplot(data=ff, aes_string(x="dem_dropoff",y="rep_dropoff",label="state2"))
        }
        gg <- gg + geom_point(aes_string(color="sparty",size=2,alpha=1.0))
        gg <- gg + geom_text(nudge_x = input$nudge_x,nudge_y = input$nudge_y,size.unit = 1)
        gg <- gg + ggtitle(stitle)
        gg <- gg + xlab("Democrat Drop-off (percent)")
        gg <- gg + ylab("Republican Drop-off (percent)")
        ggplotly(gg)
    })
    output$myTable <- renderTable({
        getdata()
    })
    output$myData <- renderPrint({
        getdata()
    })
    output$myCandidates <- renderPrint({
        getcandidates()
    })
    output$myUsage <- renderUI({
        includeHTML("http://econdataus.com/GE24Pres_dropoff.htm")
    })
}
    

# Create Shiny app ----
shinyApp(ui = ui, server = server)