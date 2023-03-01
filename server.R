library(tidyverse)
library(readxl)
library(ggplot2)
library(reshape2)

library(tigris)
library(leaflet)
library(htmltools)
library(xlsx)
library(RcppRoll)
library(cowplot)
library(jsonlite)
library(leaflet)
library(sf)
library(rgeos)
library(rvest)
library(plotly)

areaWidth <- 900
areaHeight <- 600
input_dir <- "input/"
data_dir  <- "data/"
#cfiles <- NULL
rdata <- read_csv("rdata_init.csv") # should match race values initialized in ui.R
catlog <- "catlog.txt"
#file.remove(catlog)

shinyServer(
    function(session,input, output) {
        options(width = 999, readr.show_progress = FALSE)
        options(max.print=999999)

        catmsg <- function(msg){
            line <- paste0(msg,"\n")
            cat(file = catlog, append = TRUE, line)
            cat(file = stderr(), line)
        }
        errmsg <- function(msg){
            line <- paste0("##### ",msg,"\n")
            cat(file = catlog, append = TRUE, line)
            cat(file = stderr(), line)
        }
        warnmsg <- function(msg){
            line <- paste0("===== ",msg,"\n")
            cat(file = catlog, append = TRUE, line)
            cat(file = stderr(), line)
        }
        catfile <- function(ff,msg){
            line <- paste0(msg,"\n")
            cat(file = ff, append = TRUE, line)
            cat(file = stderr(), line)
        }
        states <- c("Alabama","Alaska","Arizona","Arkansas","California",
                    "Colorado","Connecticut","Delaware","District of Columbia","Florida",
                    "Georgia","Hawaii","Idaho","Illinois","Indiana",
                    "Iowa","Kansas","Kentucky","Louisiana","Maine",
                    "Maryland","Massachusetts","Michigan","Minnesota","Mississippi",
                    "Missouri","Montana","Nebraska","Nevada","New Hampshire",
                    "New Jersey","New Mexico","New York","North Carolina","North Dakota",
                    "Ohio","Oklahoma","Oregon","Pennsylvania","Rhode Island",
                    "South Carolina","South Dakota","Tennessee","Texas","Utah",
                    "Vermont","Virginia","Washington","West Virginia","Wisconsin",
                    "Wyoming")
        state2s = c("AK","AL","AR","AZ","CA","CO","CT","DC","DE","FL",
                    "GA","HI","IA","ID","IL","IN","KS","KY","LA","MA",
                    "MD","ME","MI","MN","MO","MS","MT","NC","ND","NE",
                    "NH","NJ","NM","NV","NY","OH","OK","OR","PA","RI",
                    "SC","SD","TN","TX","UT","VA","VT","WA","WI","WV","WY")
        stabbr <- c("AL","AK","AZ","AR","CA",
                    "CO","CT","DE","DC","FL",
                    "GA","HI","ID","IL","IN",
                    "IA","KS","KY","LA","ME",
                    "MD","MA","MI","MN","MS",
                    "MO","MT","NE","NV","NH",
                    "NJ","NM","NY","NC","ND",
                    "OH","OK","OR","PA","RI",
                    "SC","SD","TN","TX","UT",
                    "VT","VA","WA","WV","WI","WY")
        statid <- c(  1 ,  2 ,  4 ,  5 ,  6 ,
                      8 ,  9 , 10 , 11 , 12 ,
                      13 , 15 , 16 , 17 , 18 ,
                      19 , 20 , 21 , 22 , 23 ,
                      24 , 25 , 26 , 27 , 28 ,
                      29 , 30 , 31 , 32 , 33 ,
                      34 , 35 , 36 , 37 , 38 ,
                      39 , 40 , 41 , 42 , 44 ,
                      45 , 46 , 47 , 48 , 49 ,
                      50 , 51 , 53 , 54 , 55 , 56 , 11 )
        statsd <- c("01","02","04","05","06",
                    "08","09","10","11","12",
                    "13","15","16","17","18",
                    "19","20","21","22","23",
                    "24","25","26","27","28",
                    "29","30","31","32","33",
                    "34","35","36","37","38",
                    "39","40","41","42","44",
                    "45","46","47","48","49",
                    "50","51","53","54","55","56","11")
        source("createfiles.R", local = TRUE)
        addScales <- function(gg, xscale, yscale){
            xx <- NULL
            yy <- NULL
            if(xscale != ""){
                sxx <- unlist(strsplit(xscale, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(yscale != ""){
                syy <- unlist(strsplit(yscale, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }
        orderdf <- function(dd, sortcol, sortdesc){
            if (sortcol != 0){
                if (!sortdesc){
                    dd <- dd[order(dd[sortcol]),]
                }
                else{
                    if (class(dd[sortcol]) == "numeric"){
                        dd <- dd[order(-dd[sortcol]),]
                    }
                    else{
                        dd <- dd[order(dd[sortcol]),]
                        dd <- dd %>% arrange(desc(row_number()))
                    }
                }
            }
            return(dd)
        }
        getlabels <- function(type, xcounty, xtype){
            tloc <- input$state2
            if (tloc == ""){
                tloc <- "U.S."
            }
            if (xcounty != "" & xcounty != "(all)"){
                if (substring(xcounty,1,1) == "-"){
                    tloc <- substring(xcounty,2)
                }
                else{
                    tloc <- paste0(xcounty,input$areaname,", ",tloc)
                }
            }
            if (input$dist != ""){
                tloc <- paste0("District ",input$dist,", ",tloc)
            }
            if (tloc != ""){
                tloc <- paste0(tloc,":")
            }
            if (input$units == "Percent ratio"){
                tshiftfor <- "Percent of"
                tshiftin <- "Percent of"
            }
            else{
                tshiftfor <- "Shift for"
                tshiftin <- "Shift in"
            }
            if (input$units == "Percent"){
                tunits <- "Vote Share"
            }
            else{
                tunits <- "Votes"
            }
            if (type != "plot" & type != "plot2b"){
                tnote <- paste0("(",input$units,")")
            }
            else if (input$party == "Margin"){
                if (input$plusnote != ""){
                    plusnote <- input$plusnote
                }
                else if (xcounty == "" | xcounty == "(all)"){
                    plusnote <- "(positive direction is more Democratic)"
                }
                else{
                    #plusnote <- "(+ = Dem)"
                    plusnote <- "(positive direction is more Democratic)"
                }
                tnote <- paste("in areas with",input$minvotes,"or more votes",plusnote)
            }
            else{
                tnote <- paste("in areas with",input$minvotes,"or more votes")
            }
            office1 <- substring(input$xraces[1],11)
            office2 <- substring(input$xraces[2],11)
            yr1 <- substring(input$xraces[1],4,5)
            yr2 <- substring(input$xraces[2],4,5)
            racex <- paste0("20",yr1," ",office1)
            racey <- paste0("20",yr2," ",office2)
            if (type == "plotn"){
                if (xcounty == "" | xcounty == "(all)"){
                    xarea <- "County"
                }
                else if (input$state2 == "WI"){
                    xarea <- "Voting Area"
                }
                else{
                    xarea <- "Precinct"
                }
                title <- paste0(tloc," Margin Vote Share by Race and ",xarea,
                                " (",input$units,")")
            } 
            else if (input$xdxplot2){
                if (input$units == "Percent ratio"){
                    if (xtype >= 2){
                        title <- paste(tloc,"Voter Turnout")
                    }
                    else{
                        title <- paste(tloc,"Voter Turnout by", input$party,
                                       "Vote", tnote)
                    }
                }
                else{
                    if (xtype == 2){
                        title <- paste(tloc,tshiftin, input$party, tunits)
                    }
                    else if (xtype >= 3){
                        title <- paste(tloc,tshiftin, input$party, "-",
                                       substring(racex,4), "to", substring(racey,4))
                    }
                    else{
                        title <- paste(tloc,tshiftin, input$party, tunits, "from",
                                       racex, "to", racey, tnote)
                    }
                }
            }
            else{
                title <- paste(tloc, input$party, tunits, "for",
                               racex, "and", racey, tnote)
            }
            if (type == "plot2b"){
                #START areaPlot2b code
                ylabel <- paste("Areas ordered by ",input$party, tunits, "for", racex)
                xlabel <- paste0(input$party," ",tunits," for ", racex," and ",racey,
                                 "\nSources: see http://econdataus.com/voting_oe.htm")
                #STOP areaPlot2b code
            }
            else{
                if (input$xdxplot2){
                    if (xtype >= 2){
                        ylabel <- paste(tshiftfor, racey)
                    }
                    else{
                        ylabel <- paste(tshiftin, input$party, tunits, "for", racey)
                    }
                }
                else{
                    ylabel <- paste(input$party, tunits, "for", racey)
                }
                xlabel <- paste0(input$party," ",tunits," for ", racex,
                                 "\nSources: see http://econdataus.com/voting_oe.htm")
            }
            labels <- c(title, xlabel, ylabel)
            return(labels)
        }
        output$myUsage <- renderUI({
            includeHTML("http://econdataus.com/voting_oe.htm")
        })
        output$areaPlot <- renderPlotly({
            areaWidth <<- input$areaWidth
            areaHeight <<- input$areaHeight
            dd <- getdata()
            dd <- dd[is.na(dd$TOTAL) | dd$TOTAL >= input$minvotes,]
            dd <- dd[dd$AREA != "TOTAL",]
            row.names(dd) <- seq(1:NROW(dd))
            dd <- orderdf(dd,input$xsortcol,input$xsortdesc)
            row.names(dd) <- seq(1,NROW(dd)) # set before removing votes == 0
            if (input$area_x0vote){
                dd <- dd[dd[4] > 0 & dd[5] > 0,] # delete if DEM or REP votes == 0 
            }
            xx <- dd
            if (input$top2){
                gg <- top2plot(xx)
            }
            else{
                xx$Margin <- 100 * (xx[,4] - xx[,5]) / xx$TOTAL
                
                xx$Party <- ""
                if (input$xlimit != ""){
                    vlimit <- as.numeric(unlist(strsplit(input$xlimit, ",")))
                    vparty <- unlist(strsplit(input$xparty, ","))
                    xx$Party <- vparty[length(vparty)]
                    xx$Party[xx[["Margin"]] < vlimit[1]] <- vparty[1]
                    for (i in 1:length(vlimit)){
                        xx$Party[xx[["Margin"]] >= vlimit[i] & xx[["Margin"]] < vlimit[i+1]] <- vparty[i+1]
                    }
                }
                
                xx$POS   <- 0
                if (input$showall1){
                    xx$POS <- 2 # default to right
                }
                xx$LABEL <- row.names(xx)
                spos1 <- unlist(strsplit(input$pos1_1, ","))
                xx$POS[xx$LABEL %in% spos1] <- 1
                spos2 <- unlist(strsplit(input$pos2_1, ","))
                xx$POS[xx$LABEL %in% spos2] <- 2
                spos3 <- unlist(strsplit(input$pos3_1, ","))
                xx$POS[xx$LABEL %in% spos3] <- 3
                xx$VJUST <- 0.5
                xx$VJUST[xx$POS == 1] <- -1
                xx$VJUST[xx$POS == 3] <- 2
                xx$PREPEND <- ""
                xx$PREPEND[xx$POS == 2] <- "  "
                xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
                xx$LABEL[xx$POS == 0] <- ""
                title <- paste0(input$xcounty,input$areaname,", ",input$xoffice[1]," - Total Votes and Vote Margin by Area")
                xlabel <- "Total Votes"
                ylabel <- "Vote Margin"
                
                gg <- ggplot(xx, aes(x = TOTAL, y = Margin))
                gg <- gg + geom_point(aes(color=Party))
                gg <- gg + ggtitle(title)
                gg <- gg + xlab(xlabel) + ylab(ylabel)
                gg <- gg + annotate("text", x = xx$TOTAL, y =xx$Margin, label = xx$LABEL,
                                    color="red", hjust = 0, vjust = xx$VJUST)
                isParty <- NULL
                for (i in 1:length(vparty)){
                    isParty <- c(isParty, any(xx$Party == vparty[i]))
                }
                vcolor <- unlist(strsplit(input$areaColor, ","))
                vcolor <- vcolor[isParty]
                if (length(vcolor) > 1){
                    gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                    gg <- gg + scale_color_manual(values = vcolor) # Line Graph
                }
                gg <- addScales(gg,input$xscale1,input$yscale1)
            }
            ggplotly(gg) %>% layout(height=input$areaHeight, width=input$areaWidth)
        })
        top2plot <- function(dd){
            tt <- as.numeric(dd[dd$AREA == "TOTAL",])
            tt <- tt[4:length(tt)]
            idesc <- order(tt, decreasing = TRUE)
            idesc <- c(1,2,3,idesc+3)
            xx <- dd[,idesc]
            namesxx <- names(xx)
            namesxx <- gsub("'","",namesxx)
            names(xx) <- namesxx
            if (input$units == "Percent"){
                for (i in 4:NCOL(xx)){
                    xx[,i] <- 100 * xx[,i] / xx[,3]
                }
            }
            xx <- xx[xx$COUNTY != "",]
            names(xx)[3] <- "Votes"
            names(xx)[4] <- "MARGIN1"
            names(xx)[5] <- "MARGIN2" #DEBUG-ADD
            xcounty <- input$xcounty
            if (input$xdxplot1){
                xx$MAR_SH <- xx$MARGIN1 - xx$MARGIN2
                #gg <- ggplot(xx, aes_string(x = "MARGIN1", y = "MAR_SH"))
                gg <- ggplot(xx, aes_string(x = "MAR_SH", y = "Votes")) #DEBUG-CHANGE
                ylabel <- paste0("Shift in Margin Vote Share from ",namesxx[5])
                xlabel <- paste0("Vote Share of ",namesxx[4])
                xlabel <- paste0(xlabel,"\nSources: see http://econdataus.com/voting_oe.htm")
                title <- paste0("Shift in Margins from ",namesxx[5]," to ",namesxx[4],
                                " in ",input$xraces[1])
                gg <- gg + ggtitle(title)
                gg <- gg + xlab(xlabel) + ylab(ylabel)
            }
            else{
                gg <- ggplot(xx, aes_string(x = "MARGIN1", y = "MARGIN2"))
                ncolor1 <- "black"
                gg <- gg + geom_abline(intercept=0, slope=1, color=ncolor1, linetype="dashed")
                if (min(xx$MARGIN1) <= 50 & max(xx$MARGIN1) >= 50){
                    gg <- gg + geom_vline(xintercept=50, color=ncolor1, linetype="dotted")
                }
                if (min(xx$MARGIN2) <= 50 & max(xx$MARGIN2) >= 50){
                    gg <- gg + geom_hline(yintercept=50, color=ncolor1, linetype="dotted")
                }
                if (length(xcounty) > 1){
                    title <- paste0("Selected counties, ",toupper(input$state2),
                                    ": Margins of Top 2 Candidates in ",input$xraces[1])
                }
                else if (xcounty == "(all)"){
                    title <- paste0("Selected counties, ",toupper(input$state2),
                                    ": Margins of Top 2 Candidates in ",input$xraces[1]) # change to All counties once all are present
                }
                else{
                    title <- paste0(xcounty," County, ",toupper(input$state2),
                                    ": Margins of Top 2 Candidates in ",input$xraces[1])
                }
                gg <- gg + ggtitle(title)
                ylabel <- namesxx[5]
                xlabel <- namesxx[4]
                xlabel <- paste0(xlabel,"\nSources: see http://econdataus.com/voting_oe.htm")
                gg <- gg + xlab(xlabel) + ylab(ylabel)
            }
            gg <- gg + geom_point(aes_string(color="COUNTY"), size=as.numeric(input$xsize1), alpha=as.numeric(input$xalpha1))
            # gg <- gg + ggtitle(title)
            # gg <- gg + xlab(xlabel) + ylab(ylabel)
            # gg <- gg + annotate("text", x = xx$TOTAL, y =xx$Margin, label = xx$LABEL,
            #                     color="red", hjust = 0, vjust = xx$VJUST)
            # code modified from doAreaPlot2
            xx$POS   <- 0
            if (input$showall1){
                xx$POS <- 2 # default to right
            }
            if (input$label1 == "Index"){
                xx$LABEL <- row.names(xx)
            }
            else if (input$label1 == "County"){
                xx$LABEL <- str_to_title(xx$COUNTY)
            }
            else if (input$label1 == "CountyID"){
                xx$LABEL <- xx$COUNTY
                # if (input$state2 == "TX"){
                #     xx$LABEL <- sub("^0+", "", substring(xx$AREA,1,3))
                # }
            }
            else if (input$label1 == "Area"){
                xx$LABEL <- str_to_title(xx$AREA)
                # if (input$state2 == "TX"){
                #     xx$LABEL <- sub("^0+", "", substring(xx$AREA,4))
                # }
            }
            else if (input$label1 == "CNTYVTD"){
                xx$LABEL <- xx$AREA
            }
            spos1_1 <- unlist(strsplit(input$pos1_1, ","))
            xx$POS[xx$AREA %in% spos1_1] <- 1
            xx$POS[row.names(xx) %in% spos1_1] <- 1
            spos2_1 <- unlist(strsplit(input$pos2_1, ","))
            xx$POS[xx$AREA %in% spos2_1] <- 2
            xx$POS[row.names(xx) %in% spos2_1] <- 2
            spos3_1 <- unlist(strsplit(input$pos3_1, ","))
            xx$POS[xx$AREA %in% spos3_1] <- 3
            xx$POS[row.names(xx) %in% spos3_1] <- 3
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            xx$LABEL[xx$POS == 0] <- ""
            if (input$xdxplot1){
                gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MAR_SH, label = xx$LABEL,
                                    hjust = 0, vjust = xx$VJUST)
            }
            else{
                gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MARGIN2, label = xx$LABEL,
                                    hjust = 0, vjust = xx$VJUST)
            }
            xx <- NULL
            yy <- NULL
            if(input$xscale1 != ""){
                sxx <- unlist(strsplit(input$xscale1, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(input$yscale1 != ""){
                syy <- unlist(strsplit(input$yscale1, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }
        doCvtPlot <- function(iraces, xcounty, xtype, xparty){
            races <- input$xraces
            nraces <- length(iraces)
            zz <- NULL
            for (i in 1:nraces){
                ii <- iraces[i]
                xx <- getrace(races[ii])
                if (is.null(xx)){
                    next
                }
                xx <- xx[is.na(xx$TOTAL) | xx$TOTAL >= input$minvotes,]
                if (is.null(xx)){
                    next
                }
                row.names(xx) <- seq(1:NROW(xx))
                if (input$cvt_x0vote){
                    xx <- xx[xx[4] > 0 & xx[5] > 0,] # delete if DEM or REP votes == 0 
                }
                if (xcounty != "" & xcounty != "(all)"){
                    xx <- xx[xx$COUNTY == xcounty,]
                }
                else{
                    xx <- xx[xx$COUNTY != "" & !is.na(xx$COUNTY),] # removes TOTAL
                }
                votesM <- getDeltaM(xx, xcounty)[2]
                if (NROW(xx) == 0){
                    next
                }
                yy <- xx
                yy <- orderdf(yy,input$xsortcol,input$xsortdesc)
                names(yy)[3] <- "Votes"
                if (input$cvt_window > 0){
                    nn <- input$cvt_window
                    totsum <- roll_sum(unlist(yy[3]),nn)
                    demsum <- roll_sum(unlist(yy[4]),nn)
                    repsum <- roll_sum(unlist(yy[5]),nn)
                }
                yy[3] <- cumsum(yy[3])
                for (i in 4:NCOL(yy)){
                    yy[i] <- 100 * cumsum(yy[i]) / yy[3]
                }
                if (input$plotbyarea){
                    len <- length(unlist(yy[3]))
                    yy[3] <- seq(1:len)
                    votesM <- median(seq(1:len))
                }
                if (input$cvt_window > 0){
                    yy$Dem_SMA <- NA
                    yy$Rep_SMA <- NA
                    yy$Dem_SMA[nn:NROW(yy)] <- 100 * demsum / totsum
                    yy$Rep_SMA[nn:NROW(yy)] <- 100 * repsum / totsum
                }
                if (nraces > 1){
                    office1 <- substring(races[ii],11)
                    office1 <- gsub(" ","_",office1)
                    yr1 <- substring(races[ii],4,5)
                    if (input$cvt_party == "DEM"){
                        yy <- yy[,1:4]
                        names(yy)[4] <- paste0(yr1,"_",office1,"_D")
                    }
                    else if (input$cvt_party == "REP"){
                        yy <- yy[,c(1,2,3,5)]
                        names(yy)[4] <- paste0(yr1,"_",office1,"_R")
                    }
                    else{
                        yy <- yy[,1:5]
                        names(yy)[4] <- paste0(yr1,"_",office1,"_D")
                        names(yy)[5] <- paste0(yr1,"_",office1,"_R")
                    }
                    aa <- gather(yy, "Candidate", "Share", 4:NCOL(yy))
                    aa$Candidate <- factor(aa$Candidate, levels = names(yy)[4:NCOL(yy)])
                    zz <- rbind(zz,aa)
                }
                else{
                    zz <- gather(yy, "Candidate", "Share", 4:NCOL(yy))
                    zz <- zz[!startsWith(zz$Candidate,"Write.in") & !startsWith(zz$Candidate,"IND") &
                                 !startsWith(zz$Candidate,"WRI") & zz$Candidate != "SCATTERING",]
                    zz$Candidate <- factor(zz$Candidate, levels = names(yy)[4:NCOL(yy)])
                }
            }
            if (nraces > 1){
                race1 <- "Selected Races"
            }
            else{
                office1 <- substring(input$xraces[iraces[1]],11)
                yr1 <- substring(input$xraces[iraces[1]],4,5)
                race1 <- paste0("20",yr1," ",office1)
            }
            if (xtype >= 2){
                title <- paste0(xcounty,input$areaname,", ",race1)
            }
            else{
                xsortdir <- "Ascending"
                if (input$xsortdesc) xsortdir <- "Desc"
                title <- paste0(xcounty,input$areaname,", ",race1," - Cumulative Vote Tally, ordered by ",names(zz)[input$xsortcol],", ",xsortdir)
            }
            xlabel <- "Votes"
            ylabel <- "Percent of Votes"
            if (input$plotbyarea){
                xlabel <- "Number of Areas"
            }
            else if (input$votes1000){
                votesM <- votesM/1000
                zz$Votes <- zz$Votes/1000
                xlabel <- "Votes (thousands)"
            }
            gg <- ggplot(zz, aes(x = Votes, y = Share))
            gg <- gg + geom_point(aes_string(color="Candidate",shape="Candidate"),
                                  size=as.numeric(input$xpsize_cvt), alpha=as.numeric(input$xalpha_cvt))
            gg <- gg + geom_line(aes_string(color="Candidate"),
                                 size=as.numeric(input$xlsize_cvt), alpha=as.numeric(input$xalpha_cvt))
            gg <- gg + geom_vline(aes(xintercept = votesM))
            gg <- gg + ggtitle(title)
            gg <- gg + xlab(xlabel) + ylab(ylabel)
            vcolor <- unlist(strsplit(input$xcolor, ","))
            if (length(vcolor) > 1){
                ncand <- (NCOL(yy)-3)*nraces #fix for multiple races
                vcolor <- rep(vcolor, length.out=ncand)
                if (input$cvt_window > 0){
                    vcolor[ncand-1] <- "lightblue"
                    vcolor[ncand] <- "pink"
                }
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            else{
                colorCount = NCOL(yy)-3
                if (colorCount > 12){
                    getPalette = colorRampPalette(brewer.pal(
                        brewer.pal.info[vcolor[[1]],]$maxcolors, vcolor[[1]]))
                    gg <- gg + scale_colour_manual(values = getPalette(colorCount))
                    gg <- gg + scale_fill_manual(values = getPalette(colorCount))
                }
                else{
                    gg <- gg + scale_colour_brewer(palette = vcolor[[1]])
                    gg <- gg + scale_fill_brewer(palette = vcolor[[1]])
                }
            }
            vshape <- as.numeric(unlist(strsplit(input$xshape, ",")))
            if (length(vshape) > 1){
                ncand <- length(unique(zz$Candidate))
                len <- length(vshape)
                if (len < ncand){
                    vshape <- c(vshape, rep(16,(ncand-len)))
                }
                vshape[ncand-1] <- 20
                vshape[ncand] <- 20
                gg <- gg + scale_shape_manual(values = vshape) # Line Graph
            }
            gg <- addScales(gg,input$xscale,input$yscale)
            gg
        }
        output$cvtPlot <- renderPlot({
            xx <- getdata()
            vindices <- as.numeric(unlist(strsplit(input$race_indices, ",")))
            doCvtPlot(vindices, input$xcounty, 1, input$cvt_party)
        })
        output$cvtPlots <- renderPlot({
            xx <- getdata()
            cc <- getCounties()
            nn <- input$cvt_cols * input$cvt_rows
            ist <- input$cvt_start
            imx <- min(nn, (1+nrow(cc)-ist))
            pp <- NULL
            for (i in 1:nn) pp[[i]] <- ggplot() + theme_void()
            vindices <- as.numeric(unlist(strsplit(input$race_indices, ",")))
            for (i in 1:imx){
                catmsg(paste0("##### COUNTY=",cc[(ist+i-1),"COUNTY"]," #####"))
                pp[[i]] <- doCvtPlot(vindices, cc[(ist+i-1),"COUNTY"], 2, input$cvt_party)
            }
            plot_grid(plotlist = pp, ncol = input$cvt_cols)
        #})
        #}, height = 9000, width = 1000) #45 rows
        #}, height = 600, width = 1000)
        }, height = 800, width = 1200)
        doAreaPlot2 <- function(xx, xcounty, xtype){
            if (xcounty != "" & xcounty != "(all)"){
                xx <- xx[toupper(xx$COUNTY) == toupper(xcounty),]
            }else{
                xx <- xx[xx$COUNTY != "" & !is.na(xx$COUNTY),]
            }
            if (!input$displaytotal){
                xx <- xx[xx$AREA != "TOTAL",] #DEBUG-CHECK - was COUNTY
            }
            names(xx)[3:10] <- c("DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            xx <- xx[(is.na(xx$TOT1_N) | xx$TOT1_N >= input$minvotes) |
                     (is.na(xx$TOT2_N) | xx$TOT2_N >= input$minvotes),]
            xx <- xx[(is.na(xx$TOT1_N) | xx$TOT1_N >= input$minvotes2) &
                     (is.na(xx$TOT2_N) | xx$TOT2_N >= input$minvotes2),]
            row.names(xx) <- seq(1:NROW(xx)) #DEBUG-TEST NROW(xx) == 0
            xxtmp <- xx[xx$DEM1 > 0 & xx$REP1 > 0 & xx$DEM2 > 0 & xx$REP2 > 0,]
            if (NROW(xxtmp) > 0){ # filter only if rows available
                xx <- xxtmp
            }
            if (input$party == "Democrat"){
                preparty <- "DEM"
                party1 <- "DEM1"
                party2 <- "DEM2"
            }
            else if (input$party == "Republican"){
                preparty <- "REP"
                party1 <- "REP1"
                party2 <- "REP2"
            }
            else if (input$party == "Total"){
                preparty <- "TOT"
                party1 <- "TOTAL1"
                party2 <- "TOTAL2"
            }
            else{
                preparty <- "MAR"
                party1 <- "MARGIN1"
                party2 <- "MARGIN2"
            }
            party_sh <- paste0(preparty,"_SH")
            party1n <- "TOT1_N"
            party2n <- "TOT2_N"
            xx$Party <- ""
            if (input$xlimit2 != ""){
                vlimit <- as.numeric(unlist(strsplit(input$xlimit2, ",")))
                vcolor <- unlist(strsplit(input$lcolor2, ","))
                vcolor <- rep_len(vcolor,(length(vlimit)+1))
                vparty <- unlist(strsplit(input$xparty2, ","))
                xx$Party <- vparty[length(vparty)]
                xx$Party[xx[["MARGIN1"]] < vlimit[1]] <- vparty[1]
                xx$Color <- vcolor[length(vcolor)]
                xx$Color[xx[["MARGIN1"]] < vlimit[1]] <- vcolor[1]
                for (i in 1:(length(vlimit)-1)){
                    xx$Party[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vparty[i+1]
                    xx$Color[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vcolor[i+1]
                }
            }
            isParty <- NULL
            for (i in 1:length(vparty)){
                isParty <- c(isParty, any(xx$Party == vparty[i]))
            }
            if (input$sizefor2){
                xx$Votes <- xx[[party2n]]
            }
            else{
                xx$Votes <- xx[[party1n]]
            }
            if (input$xdxplot2){
                gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            }
            else{
                gg <- ggplot(xx, aes_string(x=party1, y=party2))
            }
            gg <- gg + geom_point(data=xx, alpha=as.numeric(input$xalpha2),
                                  aes_string(color="Party",size="Votes"))
            if (input$party == "Margin" & input$units != "Percent ratio"){
                if (input$xdxplot2){
                    gg <- gg + geom_abline(intercept=0, slope=-1, color=input$ncolor2, linetype="dashed")
                    if (input$minmax){
                        gg <- gg + geom_abline(intercept=100, slope=-1, color=input$ncolor2, linetype="dotted")
                        gg <- gg + geom_abline(intercept=-100, slope=-1, color=input$ncolor2, linetype="dotted")
                    }
                }
                else{
                    gg <- gg + geom_abline(intercept=0, slope=1, color=input$ncolor2, linetype="dashed")
                }
            }
            if (input$party == "Margin" | input$units == "Count"){
                gg <- gg + geom_vline(xintercept=0, color=input$ncolor2)
            }
            if (input$party == "Margin"){
                if (NROW(xx) == 1 | input$forcex |
                    (min(xx$MAR_SH, na.rm = TRUE) <= 0 &
                     max(xx$MAR_SH, na.rm = TRUE) >= 0)){
                    gg <- gg + geom_hline(yintercept=0, color=input$ncolor2)
                }
            }
            vcolor <- unlist(strsplit(input$xcolor2, ","))
            vcolor <- vcolor[isParty]
            if (length(vcolor) > 0){
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            vrange_n <- as.numeric(unlist(strsplit(input$vrange, ",")))
            if (input$vbreaks != ""){
                vbreaks_n <- as.numeric(unlist(strsplit(input$vbreaks, ",")))
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 breaks = vbreaks_n)
            }
            else if (input$vtrans != "" & substr(input$vtrans,1,1) != "#"){
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 trans = input$vtrans)
            }
            else{
                gg <- gg + scale_size_continuous(range = vrange_n)
            }
            labels <- getlabels("plot", xcounty, xtype)
            gg <- gg + ggtitle(labels[1])
            gg <- gg + xlab(labels[2])
            gg <- gg + ylab(labels[3])
            xx$POS   <- 0
            if (input$showall2){
                xx$POS <- 2 # default to right
            }
            if (input$label2 == "Index"){
                xx$LABEL <- row.names(xx)
            }
            else if (input$label2 == "County"){
                xx$LABEL <- str_to_title(xx$COUNTY)
            }
            else if (input$label2 == "CountyID"){
                xx$LABEL <- xx$COUNTY
                # if (input$state2 == "TX"){
                #     xx$LABEL <- sub("^0+", "", substring(xx$AREA,1,3))
                # }
            }
            else if (input$label2 == "Area"){
                xx$LABEL <- str_to_title(xx$AREA)
                # if (input$state2 == "TX"){
                #     xx$LABEL <- sub("^0+", "", substring(xx$AREA,4))
                # }
            }
            else if (input$label2 == "CNTYVTD"){
                xx$LABEL <- xx$AREA
            }
            spos1_2 <- unlist(strsplit(input$pos1_2, ","))
            xx$POS[xx$AREA %in% spos1_2] <- 1
            xx$POS[row.names(xx) %in% spos1_2] <- 1
            spos2_2 <- unlist(strsplit(input$pos2_2, ","))
            xx$POS[xx$AREA %in% spos2_2] <- 2
            xx$POS[row.names(xx) %in% spos2_2] <- 2
            spos3_2 <- unlist(strsplit(input$pos3_2, ","))
            xx$POS[xx$AREA %in% spos3_2] <- 3
            xx$POS[row.names(xx) %in% spos3_2] <- 3
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            xx$LABEL[xx$POS == 0] <- ""
            if (input$xdxplot2){
                if (input$party == "Democrat"){
                    gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM_SH, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Republican"){
                    gg <- gg + annotate("text", x = xx$REP1, y =xx$REP_SH, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Total"){
                    gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT_SH, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else{
                    gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MAR_SH, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
            }
            else{
                if (input$party == "Democrat"){
                    gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM2, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Republican"){
                    gg <- gg + annotate("text", x = xx$REP1, y =xx$REP2, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else if (input$party == "Total"){
                    gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT2, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
                else{
                    gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MARGIN2, label = xx$LABEL,
                                        color=xx$Color, hjust = 0, vjust = xx$VJUST)
                }
            }
            xx <- NULL
            yy <- NULL
            if(input$xscale2 != ""){
                sxx <- unlist(strsplit(input$xscale2, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(input$yscale2 != ""){
                syy <- unlist(strsplit(input$yscale2, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }
        output$areaPlot2 <- renderPlot({
            xx <- getdata12()
            doAreaPlot2(xx, input$xcounty, 1)
        }, height = 600, width = 1000)
        output$areaPlot2s <- renderImage({
            xx <- getdata12()
            counties <- input$aplot2_counties
            if (substr(counties,1,1) %in% c("","#")){
                cc <- getCounties()
            }
            else{
                COUNTY <- unlist(strsplit(counties, ","))
                cc <- data.frame(COUNTY)
            }
            nn <- input$aplot2_cols * input$aplot2_rows
            ist <- input$aplot2_start
            imx <- min(nn, (1+nrow(cc)-ist))
            pp <- NULL
            for (i in 1:nn) pp[[i]] <- ggplot() + theme_void()
            for (i in 1:imx) pp[[i]] <- doAreaPlot2(xx, cc[(ist+i-1),"COUNTY"], 2)
            pp <- plot_grid(plotlist = pp, ncol = input$aplot2_cols)
            labels <- getlabels("plot", "-", 1)
            title <- ggdraw() + draw_label(labels[1], fontface='bold')
            #pp2 <- plot_grid(title, pp, ncol=1, rel_heights=c(0.1, 1)) # DEBUG-TEST - change 2nd rel_height from 1 to 8 for 24 rows
            pp2 <- plot_grid(title, pp, ncol=1, rel_heights=c(0.3, input$aplot2_rows)) # DEBUG-TEST - change 2nd rel_height from 1 to 8 for 24 rows
            filename <- "tmp_areaPlot2s.png"
            # Possibly add option to save files
            # filename <- paste0("png/voting_oe_",input$xoffice[1],"_",input$xoffice[2],"_",
            #                    input$sortcounty,"_",input$sortcountydir,
            #                    input$aplot2_cols,"_",input$aplot2_rows)
            #height <- (input$aplot2_rows * 200) + 60
            height <- (input$aplot2_rows * 600 / input$aplot2_cols) + 60
            if (nn == 1){
                pp2 <- doAreaPlot2(xx, input$xcounty, 3)
            }
            factor <- input$aplot2_factor
            png(filename, width = 10 * factor, height = height * factor / 100)
            print(pp2)
            dev.off()
            list(src = filename,
                 contentType = 'image/png',
                 #width = 400,
                 #height = 300,
                 alt = "This is alternate text")
        }, deleteFile = TRUE)
        output$areaPlot2b <- renderPlot({
            xx <- getdata12()
            # Move filtering to after getdata12()
            if (input$xcounty != "" & input$xcounty != "(all)"){
                xx <- xx[xx$COUNTY == input$xcounty,]
            }
            else{
                xx <- xx[xx$COUNTY != "" & !is.na(xx$COUNTY),]
            }
            if (!input$displaytotal){
                xx <- xx[xx$AREA != "TOTAL",]
            }
            names(xx)[3:10] <- c("DEM1","REP1","MARGIN1","TOTAL1","DEM2","REP2","MARGIN2","TOTAL2")
            xx <- xx[(is.na(xx$TOT1_N) | xx$TOT1_N >= input$minvotes) |
                     (is.na(xx$TOT2_N) | xx$TOT2_N >= input$minvotes),]
            xx <- xx[(is.na(xx$TOT1_N) | xx$TOT1_N >= input$minvotes2) &
                     (is.na(xx$TOT2_N) | xx$TOT2_N >= input$minvotes2),]
            row.names(xx) <- seq(1:NROW(xx))
            xx <- xx[xx$DEM1 > 0 & xx$REP1 > 0 & xx$DEM2 > 0 & xx$REP2 > 0,]
            if (input$party == "Democrat"){
                preparty <- "DEM"
                party1 <- "DEM1"
                party2 <- "DEM2" # areaPlot2b code
            }
            else if (input$party == "Republican"){
                preparty <- "REP"
                party1 <- "REP1"
                party2 <- "REP2" # areaPlot2b code
            }
            else if (input$party == "Total"){
                preparty <- "TOT"
                party1 <- "TOTAL1"
                party2 <- "TOTAL2" # areaPlot2b code
            }
            else{
                preparty <- "MAR"
                party1 <- "MARGIN1"
                party2 <- "MARGIN2" # areaPlot2b code
            }
            party_sh <- paste0(preparty,"_SH")
            party1n <- "TOT1_N"
            party2n <- "TOT2_N"
            xx$Party <- ""
            if (input$xlimit2b != ""){
                vlimit <- as.numeric(unlist(strsplit(input$xlimit2b, ",")))
                vcolor <- unlist(strsplit(input$lcolor2b, ","))
                vcolor <- rep_len(vcolor,(length(vlimit)+1))
                vparty <- unlist(strsplit(input$xparty2b, ","))
                xx$Party <- vparty[length(vparty)]
                xx$Party[xx[["MARGIN1"]] < vlimit[1]] <- vparty[1]
                xx$Color <- vcolor[length(vcolor)]
                xx$Color[xx[["MARGIN1"]] < vlimit[1]] <- vcolor[1]
                #START areaPlot2b code
                xx$Party2 <- vparty[length(vparty)]
                xx$Party2[xx[["MARGIN2"]] < vlimit[1]] <- vparty[1]
                xx$Color2 <- vcolor[length(vcolor)]
                xx$Color2[xx[["MARGIN2"]] < vlimit[1]] <- vcolor[1]
                #STOP areaPlot2b code
                for (i in 1:(length(vlimit)-1)){
                    xx$Party[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vparty[i+1]
                    xx$Color[xx[["MARGIN1"]] >= vlimit[i] & xx[["MARGIN1"]] < vlimit[i+1]] <- vcolor[i+1]
                    #START areaPlot2b code
                    xx$Party2[xx[["MARGIN2"]] >= vlimit[i] & xx[["MARGIN2"]] < vlimit[i+1]] <- vparty[i+1]
                    xx$Color2[xx[["MARGIN2"]] >= vlimit[i] & xx[["MARGIN2"]] < vlimit[i+1]] <- vcolor[i+1]
                    #STOP areaPlot2b code
                }
            }
            isParty <- NULL
            for (i in 1:length(vparty)){
                # isParty <- c(isParty, any(xx$Party == vparty[i])) # areaPlot2 code
                isParty <- c(isParty, any(xx$Party == vparty[i]) | any(xx$Party2 == vparty[i])) # areaPlot2b code
            }
            #START areaPlot2b code
            # gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            # gg <- gg + geom_point(data=xx, size=3, alpha=as.numeric(input$xalpha2b),
            #                       aes_string(color="Party",shape="Votes"))
            xx <- xx[order(xx[[party1]]),]
            if (substr(input$xoffice[1], 9, 12) == "Pres"){
                xx$Race <- substr(input$xoffice[1], 4, 12)
            }
            else{
                xx$Race <- substr(input$xoffice[1], 4, 11)
            }
            xx[[party_sh]] <- seq(1,NROW(xx))
            xx2 <- xx
            if (substr(input$xoffice[2], 9, 12) == "Pres"){
                xx2$Race <- substr(input$xoffice[2], 4, 12)
            }
            else{
                xx2$Race <- substr(input$xoffice[2], 4, 11)
            }
            xx2[[party1]] <- xx2[[party2]]
            xx$Votes <- xx[[party1n]]
            xx2$Votes <- xx2[[party2n]]
            gg <- ggplot(xx, aes_string(x=party1, y=party_sh))
            gg <- gg + geom_point(data=xx, alpha=as.numeric(input$xalpha2b),
                                  aes_string(color="Party",shape="Race",size="Votes"))
            gg <- gg + geom_point(data=xx2, alpha=as.numeric(input$xalpha2b),
                                  aes_string(color="Party2",shape="Race",size="Votes"))
            gg <- gg + scale_y_reverse()
            #STOP areaPlot2b code
            # if (input$party == "Margin"){ # areaPlot2 code only
            #     gg <- gg + geom_abline(intercept=0, slope=-1, color=input$ncolor2b, linetype="dashed")
            # }
            if (input$party == "Margin" | input$units == "Count"){
                gg <- gg + geom_vline(xintercept=0, color=input$ncolor2b)
            }
            if (input$party == "Margin" &
                min(xx$MAR_SH, na.rm = TRUE) <= 0 &
                max(xx$MAR_SH, na.rm = TRUE) >= 0){
                gg <- gg + geom_hline(yintercept=0, color=input$ncolor2b)
            }
            vcolor <- unlist(strsplit(input$xcolor2b, ","))
            vcolor <- vcolor[isParty]
            if (length(vcolor) > 0){
                gg <- gg + scale_fill_manual(values = vcolor) # Bar Plot
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            vshape <- as.numeric(unlist(strsplit(input$vshapeb, ",")))
            if (length(vshape) > 1){
                gg <- gg + scale_shape_manual(values = vshape) # Line Graph
            }
            vrange_n <- as.numeric(unlist(strsplit(input$vrange2b, ",")))
            if (input$vbreaks2b != ""){
                vbreaks_n <- as.numeric(unlist(strsplit(input$vbreaks2b, ",")))
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 breaks = vbreaks_n)
            }
            else if (input$vtrans2b != "" & substr(input$vtrans2b,1,1) != "#"){
                gg <- gg + scale_size_continuous(range = vrange_n,
                                                 trans = input$vtrans2b)
            }
            else{
                gg <- gg + scale_size_continuous(range = vrange_n)
            }
            labels <- getlabels("plot2b", input$xcounty, 1)
            gg <- gg + ggtitle(labels[1])
            gg <- gg + xlab(labels[2])
            gg <- gg + ylab(labels[3])
            xx$POS   <- 0
            if (input$showall2b){
                xx$POS <- 2 # default to right
            }
            if (input$label2b == "Index"){
                xx$LABEL <- row.names(xx)
            }
            else if (input$label2b == "County"){
                xx$LABEL <- str_to_title(xx$COUNTY)
            }
            else if (input$label2b == "CountyID"){
                if (input$state2 == "TX"){
                    xx$LABEL <- sub("^0+", "", substring(xx$AREA,1,3))
                }
                else{
                    xx$LABEL <- xx$COUNTY
                }
            }
            else if (input$label2b == "Area"){
                if (input$state2 == "TX"){
                    xx$LABEL <- sub("^0+", "", substring(xx$AREA,4))
                }
                else{
                    xx$LABEL <- str_to_title(xx$AREA)
                }
            }
            else if (input$label2b == "CNTYVTD"){
                xx$LABEL <- xx$AREA
            }
            spos1_2 <- unlist(strsplit(input$pos1_2b, ","))
            xx$POS[xx$AREA %in% spos1_2] <- 1
            xx$POS[row.names(xx) %in% spos1_2] <- 1
            spos2_2 <- unlist(strsplit(input$pos2_2b, ","))
            xx$POS[xx$AREA %in% spos2_2] <- 2
            xx$POS[row.names(xx) %in% spos2_2] <- 2
            spos3_2 <- unlist(strsplit(input$pos3_2b, ","))
            xx$POS[xx$AREA %in% spos3_2] <- 3
            xx$POS[row.names(xx) %in% spos3_2] <- 3
            xx$VJUST <- 0.5
            xx$VJUST[xx$POS == 1] <- -1
            xx$VJUST[xx$POS == 3] <- 2
            xx$PREPEND <- ""
            xx$PREPEND[xx$POS == 2] <- "  "
            xx$LABEL <- paste0(xx$PREPEND,xx$LABEL)
            xx$LABEL[xx$POS == 0] <- ""
            if (input$party == "Democrat"){
                gg <- gg + annotate("text", x = xx$DEM1, y =xx$DEM_SH, label = xx$LABEL,
                                    color=xx$Color, hjust = 0, vjust = xx$VJUST)
            }
            else if (input$party == "Republican"){
                gg <- gg + annotate("text", x = xx$REP1, y =xx$REP_SH, label = xx$LABEL,
                                    color=xx$Color, hjust = 0, vjust = xx$VJUST)
            }
            else if (input$party == "Total"){
                gg <- gg + annotate("text", x = xx$TOTAL1, y =xx$TOT_SH, label = xx$LABEL,
                                    color=xx$Color, hjust = 0, vjust = xx$VJUST)
            }
            else{
                gg <- gg + annotate("text", x = xx$MARGIN1, y =xx$MAR_SH, label = xx$LABEL,
                                    color=xx$Color, hjust = 0, vjust = xx$VJUST)
                gg <- gg + annotate("text", x = xx$MARGIN2, y =xx$MAR_SH, label = xx$LABEL,
                                    color=xx2$Color2, hjust = 0, vjust = xx$VJUST) # areaPlot2b code
            }
            xx <- NULL
            yy <- NULL
            if(input$xscale2b != ""){
                sxx <- unlist(strsplit(input$xscale2b, ","))
                xx <- as.numeric(sxx)
                if (length(sxx) == 3){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[3]))
                }
                else if (length(sxx) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(xx[1],xx[2],xx[3]),
                                                  minor_breaks = seq(xx[1],xx[2],xx[4]))
                }
            }
            if(input$yscale2b != ""){
                syy <- unlist(strsplit(input$yscale2b, ","))
                yy <- as.numeric(syy)
                if (length(syy) == 3){
                    gg <- gg + scale_y_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[3]))
                }
                else if (length(syy) == 4){
                    gg <- gg + scale_x_continuous(breaks = seq(yy[1],yy[2],yy[3]),
                                                  minor_breaks = seq(yy[1],yy[2],yy[4]))
                }
            }
            if (length(xx) >= 2){
                if (length(yy) >= 2){
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]), ylim = c(yy[1], yy[2]))
                }
                else{
                    gg <- gg + coord_cartesian(xlim = c(xx[1], xx[2]))
                }
            }
            else if (length(yy) >= 2){
                gg <- gg + coord_cartesian(ylim = c(yy[1], yy[2]))
            }
            return(gg)
        }, height = 600, width = 1000)
        getDeltaM <- function(xx, county){
            xx <- xx[xx$COUNTY == county,]
            oo <- orderdf(xx,input$xsortcol,input$xsortdesc)
            
            nrow <- NROW(oo)
            if (nrow %% 2 == 1){
                imed <- ceiling(NROW(oo) / 2)
                dem1 <- sum(oo[1:imed,4])
                rep1 <- sum(oo[1:imed,5])
                tot1 <- sum(oo[1:imed,3])
                vot1 <- oo[imed,3]
            }
            else{
                imed <- NROW(oo) / 2
                imed2 <- imed + 1
                dem1 <- sum(oo[1:imed,4]) + round(oo[imed2,4]/2)
                rep1 <- sum(oo[1:imed,5]) + round(oo[imed2,5]/2)
                tot1 <- sum(oo[1:imed,3]) + round(oo[imed2,3]/2)
                vot1 <- round((oo[imed,3] + oo[imed2,3])/2)
            }
            dem2 <- sum(oo[,4])
            rep2 <- sum(oo[,5])
            tot2 <- sum(oo[,3])
            deltaM <- 100 * ((rep2-dem2)/tot2 - (rep1-dem1)/tot1)
            ret <- c(deltaM, tot1, vot1)
            return(ret)
        }
        output$myTextCounties <- renderPrint({
            dd <- getCounties()
            dp <- 1
            for (i in 2:NCOL(dd)){
                if (i == 4){
                    dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
                }
                else if (i == 5){
                    dd[,i] <- format(round(dd[,i], 0), big.mark=",", scientific=FALSE)
                }
                else{
                    dd[,i] <- format(dd[,i], big.mark=",", scientific=FALSE)
                }
            }
            dd
        })
        getAreas <- function(){
            dd <- getdata()
            if (is.null(dd)){
                print("##### ERROR in getAreas: getdata() returned NULL")
                return(dd)
            }
            if (class(dd) == "list"){
                print("RACE required: Set STATE, YEAR, ELECTION, COUNTY, OFFICE and click 'ADD RACE'")
                return(NULL)
            }
            ee <- dd[dd$AREA != "TOTAL",] #remove TOTAL
            if (NROW(ee) > 0) dd <- ee
            dd <- dd[is.na(dd$TOTAL) | dd$TOTAL >= input$minvotes,]
            row.names(dd) <- seq(1:NROW(dd))
            #TODO - USING CHECKBOX FROM CVT INPUT PANEL, MOVE TO MAIN INPUT PANEL
            if (input$area_x0vote){
                dd <- dd[dd[4] > 0 & dd[5] > 0,] # delete if DEM or REP votes == 0 
            }
            if (input$xcounty != "" & input$xcounty != "(all)"){
                dd <- dd[dd$COUNTY == input$xcounty,]
            }
            ir <- 1
            if (input$xsortcol != 0){
                if (!input$xsortdesc){
                    dd <- dd[order(dd[input$xsortcol]),]
                }
                else{
                    if (class(dd[input$xsortcol]) == "numeric"){
                        dd <- dd[order(-dd[input$xsortcol]),]
                    }
                    else{
                        dd <- dd[order(dd[input$xsortcol]),]
                        dd <- dd %>% arrange(desc(row_number()))
                    }
                }
            }
            ddtot <- data.frame(COUNTY="",AREA="TOTAL",t(colSums(dd[,c(-1,-2)])))
            names(ddtot) <- names(dd)
            dd <- rbind(dd, ddtot)
            return(dd)
        }
        output$myTextAreas <- renderPrint({
            dd <- getAreas()
            if (is.null(dd)) return(dd)
            #START OF CODE THAT DOWNLOADED AS (c(...))
            csum <- cumsum(dd[3])
            dd$DEM_CVT <- 100 * cumsum(dd[4]) / csum
            dd$REP_CVT <- 100 * cumsum(dd[5]) / csum
            #dd <- dd %>% mutate(DDEM=100*(DEM-lag(DEM)))
            #dd <- dd %>% mutate(DREP=100*(REP-lag(REP)))
            #END OF CODE THAT DOWNLOADED AS (c(...))
            dp <- 2
            for (i in 3:NCOL(dd)){
                dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
            }
            row.names(dd) <- seq(1:NROW(dd))
            cat(paste0(input$xcounty,", ",input$state2,": Race ",input$xoffice[1],"\n\n"))
            dd
        })
        getAreas2 <- function(){
            dd <- getdata12()
            # Move filtering to after getdata12()
            if (input$xcounty != "" & input$xcounty != "(all)"){
                dd <- dd[dd$COUNTY == toupper(input$xcounty),] #CHANGE_OE - add toupper
            }
            else{
                dd <- dd[dd$COUNTY != "" & !is.na(dd$COUNTY),]
            }
            dd <- dd[(is.na(dd$TOT1_N) | dd$TOT1_N >= input$minvotes) |
                     (is.na(dd$TOT2_N) | dd$TOT2_N >= input$minvotes),]
            row.names(dd) <- seq(1:NROW(dd))
            #dd <- dd[dd$DEM1 > 0 & dd$REP1 > 0 & dd$DEM2 > 0 & dd$REP2 > 0,] #keep
            if (input$units == "Percent"){
                dd <- dd[,c(1:(NCOL(dd)-5),(NCOL(dd)-1),NCOL(dd))]
            }
            else{
                dd <- dd[,1:(NCOL(dd)-5)]
            }
            if (input$xsortcol2 != 0){
                if (!input$xsortdesc2){
                    dd <- dd[order(dd[input$xsortcol2]),]
                }
                else{
                    if (class(dd[input$xsortcol2]) == "numeric"){
                        dd <- dd[order(-dd[input$xsortcol2]),]
                    }
                    else{
                        dd <- dd[order(dd[input$xsortcol2]),]
                        dd <- dd %>% arrange(desc(row_number()))
                    }
                }
            }
            return(dd)
        }
        output$myTextAreas2 <- renderPrint({
            dd <- getAreas2()
            if (input$xcounty != "" & input$xcounty != "(all)"){
                if (input$units == "Count"){
                    dd <- rbind(dd, data.frame(COUNTY="TOTAL",AREA="TOTAL",
                                               t(colSums(dd[,c(-1,-2)],na.rm = TRUE)))) #DEBUG-TEST
                }
            }
            # Format decimal numbers into character strings
            dp <- 2
            for (i in 3:NCOL(dd)){
                dd[,i] <- format(round(dd[,i], dp), big.mark=",", scientific=FALSE)
            }
            cat(paste0(getlabels("text", input$xcounty, 1)[1],"\n\n"))
            print(dd)
        })
        output$heatmap <- renderImage({
            dd <- getdataN()
            nr <- NROW(dd)
            if (nr < 100){
                for (i in 1:NROW(dd)){
                    dd$COUNTY[i] <- sprintf("%02d-%s", i, dd$COUNTY[i])
                }
            }
            else{
                for (i in 1:NROW(dd)){
                    dd$COUNTY[i] <- sprintf("%03d-%s", i, dd$COUNTY[i])
                }
            }
            if (names(dd)[2] == "AREA"){
                dd <- dd[-1]
                names(dd)[1] <- "COUNTY"
            }
            dd[,2:NCOL(dd)] <- scale(dd[,2:NCOL(dd)])
            ee <- dd %>%
                gather("RACE", "VALUE", -COUNTY)
            #ee$COUNTY <- as.numeric(as.factor(ee$COUNTY))
            ee$COUNTY <- as.factor(ee$COUNTY)
            ee$RACE <- as.factor(ee$RACE)

            outfile <- tempfile(fileext='.png')
            # Generate the PNG
            png(outfile, width=input$hm_width, height=input$hm_height)

            gg <- ggplot(ee, aes(x = RACE, y = COUNTY, fill = VALUE))
            gg <- gg + geom_tile()
            gg <- gg + scale_fill_gradient(low="red", high="green")
            #gg <- ggplot(ee, aes(x = COUNTY, y = VALUE, fill = RACE))
            #gg <- gg + geom_line()
            #gg <- gg + scale_fill_gradient(low="red", high="green")
            #gg <- ggplot(ee, aes(x = COUNTY, y = VALUE))
            ###gg <- ggplot(ee, aes(y = COUNTY, x = VALUE))
            #gg <- gg + geom_point(aes_string(color="Candidate",shape="Candidate"), size=3, alpha=as.numeric(input$xalpha1))
            #gg <- gg + geom_line(aes_string(color="RACE"), size=2, alpha=as.numeric(input$xalpha1))
            ###gg <- gg + geom_point(aes_string(color="RACE", shape="RACE"), size=2)
            #   gg <- gg + geom_line(aes_string(color="RACE"), size=2)
            #gg <- gg + geom_line()
            vlimits <- as.numeric(unlist(strsplit(input$hm_limits,",")))
            minlimit <- vlimits[1]
            maxlimit <- vlimits[2]
            if (minlimit >= maxlimit){
                # abslimit <- max(abs(min(ee$VALUE)),abs(max(ee$VALUE)))
                # minlimit <- -abslimit
                # maxlimit <- abslimit
                minlimit <- min(ee$VALUE)
                maxlimit <- max(ee$VALUE)
            }
            vcolors <- unlist(strsplit(input$hm_colors,","))
            if (length(vcolors) < 3){
                gg <- gg + scale_fill_gradient(low = vcolors[1], high = vcolors[2],
                                               limits = c(minlimit, maxlimit))
            }
            else{
                gg <- gg + scale_fill_gradient2(low = vcolors[1], mid = vcolors[2],
                                                high = vcolors[3], midpoint = 0,
                                                limits = c(minlimit, maxlimit))
            }
            print(gg)
            dev.off()
            # Return a list containing the filename
            list(src = outfile,
                 contentType = 'image/png',
                 #width = 400,
                 #height = 300,
                 alt = "This is alternate text")
        }, deleteFile = TRUE)
        output$plotn <- renderImage({
            dd <- getdataN()
            nr <- NROW(dd)
            xlabel <- "County"
            ylabel <- paste("Margin Vote Share",input$units)
            if (names(dd)[2] == "AREA"){
                dd <- dd[-1]
                names(dd)[1] <- "COUNTY"
                xlabel <- "Precinct"
                if (input$state2 == "WI"){
                    xlabel <- "Voting Area"
                }
            }
            if (input$scalecols){
                dd[,2:NCOL(dd)] <- scale(dd[,2:NCOL(dd)])
                ylabel <- paste("Scaled Margin Vote Share",input$units)
            }
            ylabel <- paste0(ylabel,"\nSources: see http://econdataus.com/voting_oe.htm")
            title <- getlabels("plotn",input$xcounty,1)
            ee <- dd %>%
                gather("RACE", "VALUE", -COUNTY)
            #ee$COUNTY <- as.numeric(as.factor(ee$COUNTY))
            ee$COUNTY <- factor(ee$COUNTY, levels = unique(ee$COUNTY))
            ee$RACE <- factor(ee$RACE, levels = unique(ee$RACE))

            outfile <- tempfile(fileext='.png')
            # Generate the PNG
            png(outfile, width=input$hm_width, height=input$hm_height)
            
            gg <- ggplot(ee, aes(x = COUNTY, y = VALUE, group = RACE))
            gg <- gg + geom_point(aes_string(color="RACE", shape="RACE"), size=3)
            #gg <- gg + geom_point(aes_string(color="RACE", shape="RACE"))
            gg <- gg + geom_line(aes(color=RACE))
            gg <- gg + geom_hline(yintercept=0, color=input$ncolor2)
            gg <- gg + coord_flip()
            gg <- gg + ggtitle(title)
            gg <- gg + xlab(xlabel) + ylab(ylabel)
            vcolor <- unlist(strsplit(input$xcolor_n, ","))
            #vcolor <- vcolor[isParty]
            if (length(vcolor) > 0){
                gg <- gg + scale_color_manual(values = vcolor) # Line Graph
            }
            vshape <- as.numeric(unlist(strsplit(input$xshape_n, ",")))
            if (length(vshape) > 1){
                gg <- gg + scale_shape_manual(values = vshape) # Line Graph
            }
            vlimits <- as.numeric(unlist(strsplit(input$hm_limits,",")))
            minlimit <- vlimits[1]
            maxlimit <- vlimits[2]
            if (minlimit >= maxlimit){
                # abslimit <- max(abs(min(ee$VALUE)),abs(max(ee$VALUE)))
                # minlimit <- -abslimit
                # maxlimit <- abslimit
                minlimit <- min(ee$VALUE)
                maxlimit <- max(ee$VALUE)
            }
            # vcolors <- unlist(strsplit(input$hm_colors,","))
            # if (length(vcolors) < 3){
            #     gg <- gg + scale_fill_gradient(low = vcolors[1], high = vcolors[2],
            #                                    limits = c(minlimit, maxlimit))
            # }
            # else{
            #     gg <- gg + scale_fill_gradient2(low = vcolors[1], mid = vcolors[2],
            #                                     high = vcolors[3], midpoint = 0,
            #                                     limits = c(minlimit, maxlimit))
            # }
            print(gg)
            dev.off()
            # Return a list containing the filename
            list(src = outfile,
                 contentType = 'image/png',
                 #width = 400,
                 #height = 300,
                 alt = "This is alternate text")
        }, deleteFile = TRUE)
        # output$myTextAreasN <- renderPrint({
        #     dd <- getdataN()
        #     #print(dd)
        #     return(dd)
        # })
        output$myTextAreasN <- renderPrint({
            xx <- getAreas2()
            xx$PCHNG <- 100 * (xx$TOTAL2-xx$TOTAL1) / xx$TOTAL1
            #print(xx)
            return(xx)
        })
        getIndicators <- function(pp, ccin, varfrom, varto, ind){
            pvarto <- paste0("p",varto)
            pp$p1 <- 0
            pp$p1[as.numeric(pp[[varfrom]]) >= 1] <- 1
            pp$p2 <- 0
            pp$p2[as.numeric(pp[[varfrom]]) >= input$minprevotes2] <- 1
            pp$n0 <- 0
            pp$n1 <- 0
            pp$n2 <- 0
            pp$n3 <- 0
            pp$n4 <- 0
            pp$n5 <- 0
            pp$n6 <- 0
            pp$n7 <- 0
            pp$n8 <- 0
            pp$n9 <- 0
            if (ind == "2BL"){
                pp$nn <- as.integer(substr(pp[[varfrom]],2,2))
                pp$n0[pp$nn == 0 & pp$p2 > 0] <- 1
                pp$n1[pp$nn == 1 & pp$p2 > 0] <- 1
                pp$n2[pp$nn == 2 & pp$p2 > 0] <- 1
                pp$n3[pp$nn == 3 & pp$p2 > 0] <- 1
                pp$n4[pp$nn == 4 & pp$p2 > 0] <- 1
                pp$n5[pp$nn == 5 & pp$p2 > 0] <- 1
                pp$n6[pp$nn == 6 & pp$p2 > 0] <- 1
                pp$n7[pp$nn == 7 & pp$p2 > 0] <- 1
                pp$n8[pp$nn == 8 & pp$p2 > 0] <- 1
                pp$n9[pp$nn == 9 & pp$p2 > 0] <- 1
                cc <- pp %>%
                    group_by(COUNTY) %>%
                    summarize(p0=length(AREA), p1=sum(p1), p2=sum(p2),
                              n0=sum(n0), n1=sum(n1), n2=sum(n2), n3=sum(n3), n4=sum(n4),
                              n5=sum(n5), n6=sum(n6), n7=sum(n7), n8=sum(n8), n9=sum(n9))
                #rr <- c(0.120, 0.114, 0.109, 0.104, 0.100, 0.097, 0.093, 0.090, 0.088, 0.085)
                #values from https://www.statistics.gov.hk/wsc/CPS021-P2-S.pdf
                rr <- c(0.1197,0.1139,0.1088,0.1043,0.1003,0.0967,0.0934,0.0904,0.0876,0.0850)
                cc[[varto]] <-
                    (cc$n0 - cc$p2*rr[1])^2 / (cc$p2*rr[1]) +
                    (cc$n1 - cc$p2*rr[2])^2 / (cc$p2*rr[2]) +
                    (cc$n2 - cc$p2*rr[3])^2 / (cc$p2*rr[3]) +
                    (cc$n3 - cc$p2*rr[4])^2 / (cc$p2*rr[4]) +
                    (cc$n4 - cc$p2*rr[5])^2 / (cc$p2*rr[5]) +
                    (cc$n5 - cc$p2*rr[6])^2 / (cc$p2*rr[6]) +
                    (cc$n6 - cc$p2*rr[7])^2 / (cc$p2*rr[7]) +
                    (cc$n7 - cc$p2*rr[8])^2 / (cc$p2*rr[8]) +
                    (cc$n8 - cc$p2*rr[9])^2 / (cc$p2*rr[9]) +
                    (cc$n9 - cc$p2*rr[10])^2 / (cc$p2*rr[10])
                cc[[pvarto]] <- cc$p2
            }
            else if (ind == "C05s"){
                pp$nn <- pp[[varfrom]] %% 10
                pp$n0[pp$nn == 0 & pp$p2 > 0] <- 1
                pp$n1[pp$nn == 1 & pp$p2 > 0] <- 1
                pp$n2[pp$nn == 2 & pp$p2 > 0] <- 1
                pp$n3[pp$nn == 3 & pp$p2 > 0] <- 1
                pp$n4[pp$nn == 4 & pp$p2 > 0] <- 1
                pp$n5[pp$nn == 5 & pp$p2 > 0] <- 1
                pp$n6[pp$nn == 6 & pp$p2 > 0] <- 1
                pp$n7[pp$nn == 7 & pp$p2 > 0] <- 1
                pp$n8[pp$nn == 8 & pp$p2 > 0] <- 1
                pp$n9[pp$nn == 9 & pp$p2 > 0] <- 1
                cc <- pp %>%
                    group_by(COUNTY) %>%
                    summarize(p0 = length(AREA), p1 = sum(p1), p2 = sum(p2),
                              nn = (sum(n0) + sum(n5)) / length(AREA),
                              n0=sum(n0), n1=sum(n1), n2=sum(n2), n3=sum(n3), n4=sum(n4),
                              n5=sum(n5), n6=sum(n6), n7=sum(n7), n8=sum(n8), n9=sum(n9))
                    cc[[varto]] <- cc$nn
                    cc[[pvarto]] <- cc$p2
            }
            else if (ind == "LastC"){
                pp$nn <- pp[[varfrom]] %% 10
                pp$n0[pp$nn == 0 & pp$p2 > 0] <- 1
                pp$n1[pp$nn == 1 & pp$p2 > 0] <- 1
                pp$n2[pp$nn == 2 & pp$p2 > 0] <- 1
                pp$n3[pp$nn == 3 & pp$p2 > 0] <- 1
                pp$n4[pp$nn == 4 & pp$p2 > 0] <- 1
                pp$n5[pp$nn == 5 & pp$p2 > 0] <- 1
                pp$n6[pp$nn == 6 & pp$p2 > 0] <- 1
                pp$n7[pp$nn == 7 & pp$p2 > 0] <- 1
                pp$n8[pp$nn == 8 & pp$p2 > 0] <- 1
                pp$n9[pp$nn == 9 & pp$p2 > 0] <- 1
                cc <- pp %>%
                    group_by(COUNTY) %>%
                    summarize(p0 = length(AREA), p1 = sum(p1), p2 = sum(p2), nn = mean(nn),
                              n0=sum(n0), n1=sum(n1), n2=sum(n2), n3=sum(n3), n4=sum(n4),
                              n5=sum(n5), n6=sum(n6), n7=sum(n7), n8=sum(n8), n9=sum(n9))
                cc[[varto]] <- cc$nn
                cc[[pvarto]] <- cc$p2
            }
            if (input$showpcts2){
                cc$ntot <- cc$n0 + cc$n1 + cc$n2 + cc$n3 + cc$n4 + cc$n5 + cc$n6 + cc$n7 + cc$n8 + cc$n9
                cc$n0 <- 100 * cc$n0 / cc$ntot
                cc$n1 <- 100 * cc$n1 / cc$ntot
                cc$n2 <- 100 * cc$n2 / cc$ntot
                cc$n3 <- 100 * cc$n3 / cc$ntot
                cc$n4 <- 100 * cc$n4 / cc$ntot
                cc$n5 <- 100 * cc$n5 / cc$ntot
                cc$n6 <- 100 * cc$n6 / cc$ntot
                cc$n7 <- 100 * cc$n7 / cc$ntot
                cc$n8 <- 100 * cc$n8 / cc$ntot
                cc$n9 <- 100 * cc$n9 / cc$ntot
            }
            if (is.null(ccin)){
                cc$NOUT <- 0
                if (input$inclcounty2){
                    cc$NOUT[cc$COUNTY == input$xcounty] <- 1
                }
                else{
                    cc$NOUT[cc[[varto]] < input$minlimit2 | cc[[varto]] > input$maxlimit2] <- 1
                }
                cc <- cc[c("COUNTY", "p0", "p1", "NOUT", pvarto, varto,
                           "n0", "n1", "n2", "n3", "n4", "n5", "n6", "n7", "n8", "n9")]
                return(cc)
            }
            cc <- cc[c("COUNTY", pvarto, varto)]
            cc$NOUT2 <- 0
            cc$NOUT2[cc[[varto]] < input$minlimit2 | cc[[varto]] > input$maxlimit2] <- 1
            cc <- merge(x = ccin, y = cc, by = "COUNTY", all = TRUE)
            cc$NOUT <- cc$NOUT + cc$NOUT2
            cc <- cc[, names(cc) != "NOUT2"]
            return(cc)
        }
        observeEvent(input$indicator2,{
            ind <- input$indicator2
            if (ind == "2BL"){
                updateNumericInput(session = session, "minlimit2", value = 0)
                updateNumericInput(session = session, "maxlimit2", value = 16.9)
            }
            else if (ind == "C05s"){
                updateNumericInput(session = session, "minlimit2", value = 0.1)
                updateNumericInput(session = session, "maxlimit2", value = 0.3)
            }
            else if (ind == "LastC"){
                updateNumericInput(session = session, "minlimit2", value = 3.5)
                updateNumericInput(session = session, "maxlimit2", value = 5.5)
            }
        })
        output$myIndicator <- renderPrint({
            dp <- input$decimals2
            pp <- getdata()
            if (input$bigsmall2 > 0){
                pp$COUNTY <- "Big"
                pp$COUNTY[pp$TOTAL < input$bigsmall2] <- "Small"
            }
            namedem <- names(pp)[4]
            namerep <- names(pp)[5]
            names(pp)[4] <- "DEM"
            names(pp)[5] <- "REP"
            cc <- NULL
            for (iv in input$indvar2){
                cc <- getIndicators(pp, cc, iv, iv, input$indicator2)
                cc[[iv]] <- format(round(cc[[iv]], dp), big.mark=",", scientific=FALSE)
            }
            if (!input$showcounts2){
                cc <- subset(cc, select=-c(n0,n1,n2,n3,n4,n5,n6,n7,n8,n9))
            }
            else if (input$showpcts2){
                cc$n0 <- format(round(cc$n0, dp), big.mark=",", scientific=FALSE)
                cc$n1 <- format(round(cc$n1, dp), big.mark=",", scientific=FALSE)
                cc$n2 <- format(round(cc$n2, dp), big.mark=",", scientific=FALSE)
                cc$n3 <- format(round(cc$n3, dp), big.mark=",", scientific=FALSE)
                cc$n4 <- format(round(cc$n4, dp), big.mark=",", scientific=FALSE)
                cc$n5 <- format(round(cc$n5, dp), big.mark=",", scientific=FALSE)
                cc$n6 <- format(round(cc$n6, dp), big.mark=",", scientific=FALSE)
                cc$n7 <- format(round(cc$n7, dp), big.mark=",", scientific=FALSE)
                cc$n8 <- format(round(cc$n8, dp), big.mark=",", scientific=FALSE)
                cc$n9 <- format(round(cc$n9, dp), big.mark=",", scientific=FALSE)
            }
            nr0 <- NROW(cc)
            cat(paste0(nr0," Counties\n"))
            cc <- cc[cc$p1 >= input$minprecints2,]
            nr1 <- NROW(cc)
            cat(paste0(nr1," Counties with more than ",input$minprecints2," precincts with 1 or more votes\n"))
            cc <- cc[cc$NOUT > 0,]
            nr2 <- NROW(cc)
            cat(paste0(nr2," Counties with 1 or more values out of limits\n"))
            nout <- sum(cc$NOUT)
            nall <- nr1 * length(input$indvar2)
            pout <- 100 * nout / nall
            pout <- format(round(pout, dp), big.mark=",", scientific=FALSE)
            cat(paste0(nout," of ",nall," = ",pout," percent of values out of limits\n\n"))
            return(as.data.frame(cc))
        })
        output$myIndPlot <- renderPlot({
            pp <- getdata()
            namedem <- names(pp)[4]
            namerep <- names(pp)[5]
            names(pp)[4] <- "DEM"
            names(pp)[5] <- "REP"
            cc <- NULL
            for (iv in input$indvar2){
                cc <- getIndicators(pp, cc, iv, iv, input$indicator2)
            }
            cc <- cc[cc$p1 >= input$minprecints2,]
            cc <- cc[cc$NOUT > 0,]
            ylabel <- "COUNT"
            if (input$showpcts2){
                ylabel <- "PERCENT"
            }
            title <- paste0(str_to_title(ylabel),"s of ",input$indicator2," Digits"," in ",
                            input$xoffice[1]," ",str_to_title(input$indvar2),
                            " Votes (>= ",input$minprecints2," Areas with >= ",
                            input$minprevotes2," Votes, Limits = ",
                            input$minlimit2,",",input$maxlimit2,")")
            ee <- cc %>%
                gather("DIGIT", "COUNT", c("n0","n1","n2","n3","n4","n5","n6","n7","n8","n9"))
            gg <- ggplot(ee, aes(x = DIGIT, y = COUNT, group = COUNTY))
            gg <- gg + geom_point(aes_string(color="COUNTY", shape="COUNTY"), size=3)
            gg <- gg + geom_line(aes(color=COUNTY))
            gg <- gg + ggtitle(title) + ylab(ylabel)
            return(gg)
        })
        output$myLeaflet <- renderLeaflet({
            #dd <- getdata() # COUNTY,DEM1,REP1,MARGIN1,TOTAL1,DEM2,REP2,MARGIN2,TOTAL2,
            # DEM_SH,REP_SH,MAR_SH,TOT_SH,DEM1_N,REP1_N,MAR1_N,TOT1_N,TOT2_N
            dp <- input$decimals2
            pp <- getdata()
            namedem <- names(pp)[4]
            namerep <- names(pp)[5]
            names(pp)[4] <- "DEM"
            names(pp)[5] <- "REP"
            cc <- NULL
            for (iv in input$indvar2){
                cc <- getIndicators(pp, cc, iv, iv, input$indicator2)
            }
            mapvar <- input$indvar2[1]
            dd <- cc
            ee <- dd[!is.na(dd[[mapvar]]),]
            if (input$maplimitset2 == "Auto set to min,max"){
                minlimit <- floor(min(ee[[mapvar]]))
                maxlimit <- ceiling(max(ee[[mapvar]]))
                maplimits <- paste0(minlimit,",",maxlimit)
                updateTextInput(session, "maplimits2", value = maplimits)
            }
            else if (input$maplimitset2 == "Auto set balanced"){
                minlimit <- floor(min(ee[[mapvar]]))
                maxlimit <- ceiling(max(ee[[mapvar]]))
                abslimit <- max(abs(minlimit), abs(maxlimit))
                stepsize <- ceiling(abslimit/5)
                abslimit <- ceiling(abslimit/stepsize) * stepsize
                maplimits <- paste0("-",abslimit,",",abslimit,",",stepsize)
                updateTextInput(session, "maplimits2", value = maplimits)
            }
            limits <- unlist(strsplit(input$maplimits2, ","))
            if (length(limits) <= 2){
                pal <- colorNumeric(input$mapcolors2, dd[[mapvar]])
            }
            else if (length(limits) == 3){
                bins <- seq.int(limits[1], limits[2], limits[3])
                pal <- colorBin(input$mapcolors2, domain = dd[[mapvar]], bins = bins)
            }
            else{
                bins <- limits
                pal <- colorBin(input$mapcolors2, domain = dd[[mapvar]], bins = bins)
            }
            
            istate <- which(stabbr %in% input$state2)
            counties <- rgdal::readOGR("gz_2010_us_050_00_20m.json")
            counties <- counties[counties$STATE == statsd[istate],]
            cc <- st_as_sf(counties)
            cc$NAME   <- toupper(cc$NAME)
            dd$COUNTY <- toupper(dd$COUNTY)
            ee <- cc %>% left_join(dd, by = c("NAME" = "COUNTY"))
            ee$var <- ee[[input$indvar2[1]]]

            mm <- leaflet(ee) %>%
                addTiles() %>%
                addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 0.5,
                            fillColor = ~pal(var),
                            label = ~paste0(NAME,": ",formatC(var, big.mark = ",")," (",p1,")")) %>%
                addLegend(pal = pal, values = ~(var), opacity = 1.0)
            print(mm)
        })
        
        
        
        output$getcsv <- downloadHandler(
            filename = function(){
                paste0(input$xoffice[1],"_",input$xcounty,"_",input$units,".csv")
            },
            content = function(file){
                xx <- getAreas()
                fn <- paste0(input$xoffice[1],"_",input$xcounty,"_",input$units,".csv")
                catmsg(paste0("====> write_csv(",fn,")"))
                write_csv(xx, file)
            }
        )
        output$getexcel <- downloadHandler(
            filename = function(){
                paste0(input$xoffice[1],"_",input$xcounty,"_",input$units,".xlsx")
            },
            content = function(file){
                xx <- getAreas()
                fn <- paste0(input$xoffice[1],"_",input$xcounty,"_",input$units,".xlsx")
                catmsg(paste0("====> write.xlsx(",fn,")"))
                write.xlsx(xx,file,sheetName = input$xcounty)
            }
        )
        output$getcsv2 <- downloadHandler(
            filename = function(){
                paste0(input$xoffice[1],"_",input$xoffice[2],"_",
                       input$xcounty,"_",input$units,".csv")
            },
            content = function(file){
                xx <- getAreas2()
                fn <- paste0(input$xoffice[1],"_",input$xoffice[2],"_",
                             input$xcounty,"_",input$units,".csv")
                catmsg(paste0("====> write_cvs(",fn,")"))
                write_csv(xx, file)
            }
        )
        output$getexcel2 <- downloadHandler(
            filename = function(){
                paste0(input$xoffice[1],"_",input$xoffice[2],"_",
                       input$xcounty,"_",input$units,".xlsx")
            },
            content = function(file){
                xx <- getAreas2()
                fn <- paste0(input$xoffice[1],"_",input$xoffice[2],"_",
                             input$xcounty,"_",input$units,".xlsx")
                catmsg(paste0("====> write.xlsx(",fn,")"))
                write.xlsx(xx,file,sheetName = input$xcounty)
            }
        )
        createfiles <- function(races){
            if (input$createfiles){
                for (i in 1:length(races)){
                    if (races[i] == "AZ_2018_Senate"){
                        createAZ_2018_Senate()
                    }
                    else if (races[i] == "AZ_2020_President"){
                        createAZ_2020_President("")
                    }
                    else if (races[i] == "AZ_2020_President_Early"){
                        createAZ_2020_President("Early Ballots")
                    }
                    else if (races[i] == "AZ_2020_President_Polls"){
                        createAZ_2020_President("Polling Place")
                    }
                    else if (races[i] == "AZ_2020_President_Prov"){
                        createAZ_2020_President("Provisional Ballots")
                    }
                    else if (races[i] == "AZ_2020_Senate"){
                        createAZ_2020_Senate("")
                    }
                    else if (races[i] == "AZ_2020_Senate_Early"){
                        createAZ_2020_Senate("Early Ballots")
                    }
                    else if (races[i] == "AZ_2020_Senate_Polls"){
                        createAZ_2020_Senate("Polling Place")
                    }
                    else if (races[i] == "AZ_2020_Senate_Prov"){
                        createAZ_2020_Senate("Provisional Ballots")
                    }
                    else if (races[i] == "CA_2016_President"){
                        createCA_2016_President()
                    }
                    else if (races[i] == "CA_2018_Governor"){
                        createCA_2018_Governor()
                    }
                    else if (races[i] == "CA_2020_President"){
                        createCA_2020_President()
                    }
                    else if (races[i] == "CA_2020_House"){
                        createCA_2020_House()
                    }
                    else if (races[i] == "CO_2018_Governor"){
                        createCO_2018_Governor()
                    }
                    else if (races[i] == "CA_2018_House"){
                        createCA_2018_House()
                    }
                    else if (races[i] == "CA_2016_President"){
                        createCA_2016_President()
                    }
                    else if (races[i] == "CO_2020_President"){
                        createCO_2020_President()
                    }
                    else if (races[i] == "CO_2020_Registered"){
                        createCO_2020_Registered()
                    }
                    else if (races[i] == "FL_2016_President"){
                        createFL_2016_President()
                    }
                    else if (races[i] == "FL_2018_Governor"){
                        createFL_2018_Governor()
                    }
                    else if (races[i] == "FL_2018_Senate"){
                        createFL_2018_Senate()
                    }
                    else if (races[i] == "FL_2018_House"){
                        createFL_2018_House()
                    }
                    else if (races[i] == "FL_2020_President"){
                        createFL_2020_President()
                        #createFL_2020_Counties()
                        #createFL_2020_County_Codes()
                    }
                    else if (races[i] == "FL_2020_President_OE"){
                        createFL_2020_President_OE()
                    }
                    else if (races[i] == "FL_2020_House"){
                        createFL_2020_House()
                    }
                    else if (races[i] == "FL_2020_House_OE"){
                        createFL_2020_House_OE()
                    }
                    else if (races[i] == "FL_2020_House_CD27"){
                        createFL_2020_House_CD27()
                    }
                    else if (races[i] == "FL_2020_State_House"){
                        createFL_2020_State_House()
                    }
                    else if (races[i] == "FL_2020_State_Senate"){
                        createFL_2020_State_Senate()
                    }
                    else if (races[i] == "FL_2020_Ballots"){
                        createFL_2020_Ballots()
                    }
                    else if (races[i] == "FL_2020_Registered_OE"){
                        createFL_2020_Registered_OE()
                    }
                    else if (races[i] == "FL_2016_Registered"){
                        createFL_2016_Registered()
                    }
                    else if (races[i] == "FL_2018_Registered"){
                        createFL_2018_Registered()
                    }
                    else if (races[i] == "FL_2020_Registered"){
                        createFL_2020_Registered()
                    }
                    else if (races[i] == "IA_2016_President"){
                        #createIA_2020_Counties()
                        createIA_2016_President()
                    }
                    else if (races[i] == "IA_2018_Governor"){
                        createIA_2018_Governor()
                    }
                    else if (races[i] == "IA_2018_House_CD1"){
                        createIA_2018_House_CD1()
                    }
                    else if (races[i] == "IA_2020_House_CD1"){
                        createIA_2020_House_CD1()
                    }
                    else if (races[i] == "IA_2018_House_CD2"){
                        createIA_2018_House_CD2()
                    }
                    else if (races[i] == "IA_2020_House_CD2"){
                        createIA_2020_House_CD2()
                    }
                    else if (races[i] == "IA_2018_House_CD3"){
                        createIA_2018_House_CD3()
                    }
                    else if (races[i] == "IA_2020_House_CD3"){
                        createIA_2020_House_CD3()
                    }
                    else if (races[i] == "IA_2018_House_CD4"){
                        createIA_2018_House_CD4()
                    }
                    else if (races[i] == "IA_2020_House_CD4"){
                        createIA_2020_House_CD4()
                    }
                    else if (races[i] == "IA_2020_President"){
                        #createIA_2020_Counties()
                        createIA_2020_President()
                    }
                    else if (races[i] == "IA_2020_Senate"){
                        createIA_2020_Senate()
                    }
                    else if (races[i] == "ME_2014_Senate"){
                        createME_2014_Senate()
                    }
                    else if (races[i] == "ME_2016_President"){
                        createME_2016_President()
                    }
                    else if (races[i] == "ME_2018_Governor"){
                        createME_2018_Governor()
                    }
                    else if (races[i] == "ME_2018_Senate"){
                        createME_2018_Senate()
                    }
                    else if (races[i] == "ME_2020_President"){
                        createME_2020_President()
                    }
                    else if (races[i] == "ME_2020_Senate"){
                        createME_2020_Senate()
                    }
                    else if (races[i] == "ME_2020_House"){
                        createME_2020_House()
                    }
                    else if (races[i] == "MN_2018_Senate"){
                        createMN_2018_Senate()
                    }
                    else if (races[i] == "MN_2018_Senate2"){
                        createMN_2018_Senate2()
                    }
                    else if (races[i] == "MN_2020_President"){
                        createMN_2020_President()
                    }
                    else if (races[i] == "MN_2020_Senate"){
                        createMN_2020_Senate()
                    }
                    else if (races[i] == "MT_2018_Senate"){
                        createMT_2018_Senate()
                    }
                    else if (races[i] == "MT_2020_President"){
                        createMT_2020_President()
                    }
                    else if (races[i] == "MT_2020_Senate"){
                        createMT_2020_Senate()
                    }
                    else if (races[i] == "NC_2018_House"){
                        createNC_2018_House()
                    }
                    else if (races[i] == "NC_2020_President"){
                        createNC_2020_President()
                    }
                    else if (races[i] == "NC_2020_Senate"){
                        createNC_2020_Senate()
                    }
                    else if (races[i] == "NC_2020_Governor"){
                        createNC_2020_Governor()
                    }
                    else if (races[i] == "NV_2016_President"){
                        createNV_2016_President()
                    }
                    else if (races[i] == "NV_2018_Senate"){
                        createNV_2018_Senate()
                    }
                    else if (races[i] == "NV_2020_President"){
                        createNV_2020_President()
                    }
                    else if (races[i] == "OH_2004_President"){
                        createOH_2004_President()
                    }
                    else if (races[i] == "OH_2016_President"){
                        createOH_2016_President()
                    }
                    else if (races[i] == "OH_2016_Senate"){
                        createOH_2016_Senate()
                    }
                    else if (races[i] == "OH_2018_Governor"){
                        createOH_2018_Governor()
                    }
                    else if (races[i] == "OH_2018_Senate"){
                        createOH_2018_Senate()
                    }
                    else if (races[i] == "OH_2020_President"){
                        createOH_2020_President()
                    }
                    else if (races[i] == "OH_2020_Registered"){
                        createOH_2020_Registered()
                    }
                    else if (races[i] == "SC_2016_President"){
                        createSC_2016_President()
                    }
                    else if (races[i] == "SC_2018_Governor"){
                        createSC_2018_Governor()
                    }
                    else if (races[i] == "SC_2020_President"){
                        createSC_2020_President()
                    }
                    else if (races[i] == "SC_2020_Senate"){
                        createSC_2020_Senate()
                    }
                    else if (races[i] == "SC_2020_Registered"){
                        createSC_2020_Registered()
                    }
                    else if (races[i] == "TX_2016_President"){
                        createTX_2016_President()
                    }
                    else if (races[i] == "TX_2018_AG"){
                        createTX_2018_AG()
                    }
                    else if (races[i] == "TX_2018_AG_OE"){
                        createTX_2018_AG_OE()
                    }
                    else if (races[i] == "TX_2018_Governor"){
                        createTX_2018_Governor()
                    }
                    else if (races[i] == "TX_2018_Governor_OE"){
                        createTX_2018_Governor_OE()
                    }
                    else if (races[i] == "TX_2018_Senate"){
                        createTX_2018_Senate()
                    }
                    else if (races[i] == "TX_2018_Senate_OE"){
                        createTX_2018_Senate_OE()
                    }
                    else if (races[i] == "TX_2018_House"){
                        createTX_2018_House()
                    }
                    else if (races[i] == "TX_2018_House_OE"){
                        createTX_2018_House_OE()
                    }
                    else if (races[i] == "TX_2018_House_210624"){
                        createTX_2018_House_210624()
                    }
                    else if (races[i] == "TX_2018_State_House"){
                        createTX_2018_State_House()
                    }
                    else if (races[i] == "TX_2018_State_Senate"){
                        createTX_2018_State_Senate()
                    }
                    else if (races[i] == "TX_2018_RR_Commission"){
                        createTX_2018_RR_Commission()
                    }
                    else if (races[i] == "TX_2020_President_OE"){
                        createTX_2020_President_OE()
                    }
                    else if (races[i] == "TX_2020_President"){
                        createTX_2020_President()
                    }
                    else if (races[i] == "TX_2020_Senate_OE"){
                        createTX_2020_Senate_OE()
                    }
                    else if (races[i] == "TX_2020_Senate"){
                        createTX_2020_Senate()
                    }
                    else if (races[i] == "TX_2020_House_OE"){
                        createTX_2020_House_OE()
                    }
                    else if (races[i] == "TX_2020_House"){
                        createTX_2020_House()
                    }
                    else if (races[i] == "TX_2020_House_210624"){
                        createTX_2020_House_210624()
                    }
                    else if (races[i] == "TX_2020_State_House"){
                        createTX_2020_State_House()
                    }
                    else if (races[i] == "TX_2020_State_Senate"){
                        createTX_2020_State_Senate()
                    }
                    else if (races[i] == "TX_2020_RR_Commission"){
                        createTX_2020_RR_Commission()
                    }
                    else if (races[i] == "TX_2016_President_210604"){
                        createTX_2016_President_210604()
                    }
                    else if (races[i] == "TX_2018_AG_210605"){
                        createTX_2018_AG_210605()
                    }
                    else if (races[i] == "TX_2018_Governor_210605"){
                        createTX_2018_Governor_210605()
                    }
                    else if (races[i] == "TX_2018_Senate_210605"){
                        createTX_2018_Senate_210605()
                    }
                    else if (races[i] == "TX_2020_President_210602"){
                        createTX_2020_President_210602()
                    }
                    else if (races[i] == "TX_2020_Senate_210603"){
                        createTX_2020_Senate_210603()
                    }
                    else if (races[i] == "TX_2020_President_SOS"){
                        createTX_2020_President_SOS()
                    }
                    else if (races[i] == "TX_2020_Senate_SOS"){
                        createTX_2020_Senate_SOS()
                    }
                    else if (races[i] == "TX_2020_House_SOS"){
                        createTX_2020_House_SOS()
                    }
                    else if (races[i] == "VA_2016_President"){
                        createVA_2016_President()
                    }
                    else if (races[i] == "VA_2017_Governor"){
                        createVA_2017_Governor()
                    }
                    else if (races[i] == "VA_2018_Senate"){
                        createVA_2018_Senate()
                    }
                    else if (races[i] == "VA_2020_President"){
                        createVA_2020_President()
                    }
                    else if (races[i] == "VA_2021_Governor"){
                        createVA_2021_Governor()
                    }
                    else if (races[i] == "WI_2016_President"){
                        createWI_2016_President()
                    }
                    else if (races[i] == "WI_2016_President0"){
                        createWI_2016_President0()
                    }
                    else if (races[i] == "WI_2016_President_Recount"){
                        createWI_2016_President_Recount()
                    }
                    else if (races[i] == "WI_2018_Governor"){
                        createWI_2018_Governor()
                    }
                    else if (races[i] == "WI_2018_Senate"){
                        createWI_2018_Senate()
                    }
                    else if (races[i] == "WI_2018_SupremeCourt"){
                        createWI_2018_SupremeCourt()
                    }
                    else if (races[i] == "WI_2018_House"){
                        createWI_2018_House()
                    }
                    else if (races[i] == "WI_2018_State_Senate"){
                        createWI_2018_State_Senate()
                    }
                    else if (races[i] == "WI_2020_President"){
                        createWI_2020_President()
                    }
                    else if (races[i] == "WI_2020_SupremeCourt"){
                        createWI_2020_SupremeCourt()
                    }
                    else if (races[i] == "WI_2020_House"){
                        createWI_2020_House()
                    }
                    else if (races[i] == "WI_2020_State_Senate"){
                        createWI_2020_State_Senate()
                    }
                    else if (races[i] == "WI_2020_State_Assembly"){
                        createWI_2020_State_Assembly()
                    }
                    else{
                        catmsg(paste0("Unknown race: ",races[i]))
                    }
                }
            }
        }
        getCounties <- reactive({
            xx <- getdata()
            xx <- xx[is.na(xx$TOTAL) | xx$TOTAL >= input$minvotes,]
            row.names(xx) <- seq(1:NROW(xx))
            yy <- xx %>%
                group_by(COUNTY) %>%
                summarize(AREAS = length(AREA), VOTES = sum(TOTAL))
            yy <- yy[yy$COUNTY != "",]
            yy$deltaM <- 0
            yy$deltaMxV <- 0
            yy$totalM <- 0
            yy$votesM <- 0
            for (i in 1:NROW(yy)){
                ret <- getDeltaM(xx, yy$COUNTY[i])
                yy$deltaM[i] <- ret[1]
                yy$deltaMxV[i] <- ret[1] * yy$VOTES[i] / 100
                yy$totalM[i] <- ret[2]
                yy$votesM[i] <- ret[3]
            }
            yy <- orderdf(yy,input$sortcounty,(input$sortcountydir == "Desc"))
            # counties <- c(yy$COUNTY,"(all)")
            # current_county <- input$xcounty
            # if (current_county == ""){
            #     updateSelectInput(session,"xcounty",choices = counties)
            # }
            # else{
            #     updateSelectInput(session,"xcounty",choices = counties,selected = current_county)
            # }
            return(data.frame(yy))
        })
        # Match using party instead of name if multiple races in state
        # office = office set when race added
        # party  = c("DEM","REP","LIB","GRN")
        createfilep <- function(xx,col,office,party){
            xx <- xx[,col]
            names(xx) <- c("COUNTY","AREA","Office","DIST","TOTAL","Party","Name","Votes")
            xx <- xx[xx$Office == office,] #done before call?
            xx$Name[is.na(xx$Name)] <- xx$Office[is.na(xx$Name)] #DEBUG-TEST for Registered Voters
            zxxcreate <<- xx #DEBUG-RM - cleaned input before xparty1 and NA check
            if (input$xparty1 != "(all)"){
                xx <- xx[!is.na(xx$Party) & xx$Party == input$xparty1,]
            }
            xx$Name <- gsub("OVER VOTE", "Overvote", xx$Name,ignore.case = TRUE)
            xx$Name <- gsub("UNDER VOTE","Undervote",xx$Name,ignore.case = TRUE)
            xx$Party[is.na(xx$Party)] <- "UNK"
            xx$Party[xx$Party == "STATS"] <- "VOTE"
            # Check District and TOTAL, if necessary
            # if (input$xcounty == "(all)" OR namefmt == ""){
            #     xx$Name <- "Other" except for DEM and REP
            #     catmsg("SKIP REFORMAT OF Name FOR County == (all)")
            # }
            if (input$namefmt != "" & input$xcounty != "(all)"){
                if (input$xcounty == "(all)"){ #don't use loop for all
                    getfirst <- function(x) {return(unlist(x)[1])}
                    getlast  <- function(x) {return(unlist(x)[length(x)])}
                    ss <- str_split(xx$Name,"/")
                    nn <- lapply(ss,getfirst)
                    nn <- str_trim(nn)
                    ss <- str_split(nn," ")
                    nn <- lapply(ss,getlast)
                    nn <- str_to_title(nn)
                    nameparty <- sprintf(input$namefmt,nn,xx$Party)
                    xx$Name <- paste0(xx$Party,"|",nameparty)
                }
                else{
                    for (i in 1:NROW(xx)){
                        ss <- str_split(xx$Name[i],"/")
                        nn <- str_trim(unlist(ss)[1])
                        ss <- str_split(nn," ")
                        uu <- unlist(ss)
                        xx$Name[i] <- str_to_title(uu[length(uu)])
                        nameparty <- sprintf(input$namefmt,xx$Name[i],xx$Party[i])
                        xx$Name[i] <- paste0(xx$Party[i],"|",nameparty)
                    }
                }
            }
            else{
                xx$Name <- "Other"
                for (i in 1:NROW(xx)){
                    for (j in 1:length(party)){
                        if (party[j] == xx$Party[i]){
                            xx$Name[i] <- str_to_title(party[j])
                            break
                        }
                    }
                }
                xx$Party[xx$Name == "Other"] <- "OTHER"
            }
            xx <- xx[,c("DIST","COUNTY","AREA","Name","Votes")]
            # check for matches first???
            xx$Votes[is.na(xx$Votes)] <- 0 # change NAs to 0 before group_by
            xx <- xx %>%
                group_by(DIST,COUNTY,AREA,Name) %>%
                summarize(Votes=sum(as.integer(Votes)))
            #xx$Votes[is.na(xx$Votes)] <- 0 # unnecessary?
            xx <- xx %>% spread(Name,Votes)
            for (i in 4:NCOL(xx)){
                xx[,i][is.na(xx[,i])] <- 0
            }
            if (input$namefmt != "" & input$xcounty != "(all)"){
                namesxx <- names(xx)
                partyxx <- toupper(namesxx) #???
                for (j in 4:NCOL(xx)){ #was -1
                    ss <- str_split(namesxx[j],"\\|")
                    uu <- unlist(ss)
                    partyxx[j] <- uu[1]
                    namesxx[j] <- uu[2]
                }
                names(xx) <- namesxx
                xx$TOTAL <- 0
                namesxx <- names(xx)
                partyxx <- c(partyxx, "TOTAL")
            }
            else{
                namesxx <- names(xx)
                partyxx <- toupper(namesxx)
                if (!("DEM" %in% partyxx)){
                    xx$Dem <- 0
                }
                if (!("REP" %in% partyxx)){
                    xx$Rep <- 0
                }
                xx$TOTAL <- 0
                namesxx <- names(xx)
                partyxx <- toupper(namesxx)
            }
            ii <- c(1,2,3,NCOL(xx))
            idem <- 0
            irep <- 0
            if ("DEM" %in% partyxx){
                idem <- which(partyxx == "DEM")
                ii <- c(ii, idem)
            }
            if ("REP" %in% partyxx){
                irep <- which(partyxx == "REP")
                ii <- c(ii, irep)
            }
            for (j in 4:(NCOL(xx)-1)){
                if (!(j %in% idem) & !(j %in% irep)){ # check with %in% DEBUG-CHECK!!!
                    ii <- c(ii, j)
                }
            }
            xx <- xx[,ii]
            namesxx <- namesxx[ii]
            partyxx <- partyxx[ii]
            names(xx) <- namesxx
            lst <- list(partyxx,xx)
            return(lst)
        }
        read_oe <- function(race){
            catmsg(paste0("START read_oe(",race,")")) #DEBUG
            # filename <- paste0(election,"__",tolower(county),"__precinct.csv")
            # filepath <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-",input$state2,"/master/",
            #                    input$xyear,"/counties/",filename)
            catmsg("===== rdata =====")
            capture.output(print(rdata), file = "catlog.txt", append = TRUE)
            rdat <- rdata[rdata$race == race,]
            xcounty <- input$xcounty
            #if (length(xcounty) > 1 | (length(xcounty) == 1 & (xcounty[1] == "" | xcounty[1] == "(all)"))){
            if (!input$countyfiles | length(xcounty) > 1 | (length(xcounty) == 1 & (xcounty[1] == "" | xcounty[1] == "(all)"))){
                filename <- paste0(rdat$election,"__precinct.csv")
                filepath <- paste0(data_dir,input$xelection,"__precinct.csv")
                catmsg(paste0("filename=",filename)) #DEBUG
                catmsg(paste0("filepath=",filepath)) #DEBUG
                if (exists(filename, envir = .GlobalEnv)){
                    catmsg(paste0("Election object ",filename," exists"))
                    cc <- get(filename, envir = .GlobalEnv)
                }
                else if (file.exists(filepath)){
                    catmsg(paste0("BEFORE read_csv(",filepath,")"))
                    cc <- read_csv(filepath, guess_max = 1000000) #DEBUG-TEST
                    cc <- cc[c("county","precinct","office","district","party","candidate","votes")] #DEBUG_TEST
                    catmsg(paste0(" AFTER read_csv(",filepath,")"))
                    assign(filename, cc, envir = .GlobalEnv)
                }
                else{
                    catmsg(paste0("BEFORE getElectionFile()"))
                    cc <- getElectionFile()
                    catmsg(paste0(" AFTER getElectionFile()"))
                    assign(filename, cc, envir = .GlobalEnv)
                }
                if (length(xcounty) > 1){
                    if ("(all)" %in% xcounty){
                        cc$county[!(cc$county %in% xcounty)] <- "(other)"
                    }
                    else{
                        cc <- cc[cc$county %in% xcounty,]
                    }
                }
                else{
                    catmsg(paste0("  FIX read_oe: xcounty=|",xcounty,"|"))
                    if (xcounty != "(all)"){ #DEBUG-FIX
                        cc <- cc[cc$county == xcounty,]
                    }
                }
            }
            else{
                filename <- paste0(rdat$election,"__",tolower(xcounty),"__precinct.csv")
                filepath <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-",
                                   tolower(rdat$state),"/master/",rdat$year,"/counties/",filename)
                catmsg(paste0("filename=",filename)) #DEBUG
                catmsg(paste0("filepath=",filepath)) #DEBUG
                if (exists(filename, envir = .GlobalEnv)){
                    cc <- get(filename, envir = .GlobalEnv)
                }
                else{
                    cc <- NULL
                    result = tryCatch({
                        cc <- read_csv(filepath)
                        assign(filename, cc, envir = .GlobalEnv)
                    }, warning = function(w) {
                        print(paste0("WARNING in read_oe: ",w))
                    }, error = function(e) {
                        print(paste0("ERROR in read_oe: ",e))
                    }, finally = {
                        #cleanup-code
                    })
                }
            }
            catmsg(paste0("NROW=",NROW(cc)))
            xx <- cc[cc$office == rdat$office,] #DEBUG-CHECK
            catmsg(paste0("NROW=",NROW(xx)," with office == ",rdat$office))
            columns <- c("county","precinct","office","district","total","party","candidate","votes")
            #xx <- cleanTX_2020(xx)
            xx$total <- 0
            #office <- "Railroad Commissioner"
            party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
            #filename <- "TX_2018_RR_Commission.csv"
            lst <- createfilep(xx,columns,rdat$office,party) #DEBUG-CHECK
            catmsg(paste0("NROW=",NROW(lst[[2]])," returned from createfilep"))
            return(lst)
        }
        getrace <- function(race){
            #filenamex <- paste0(data_dir,race,".csv")
            #createfiles(race)
            xx0 <- NULL
            xxparty <- NULL
            tryCatch(
                expr = {
                    catmsg(paste0("BEFORE read_oe(",race,")"))
                    lst <- read_oe(race)
                    catmsg(paste0(" AFTER read_oe(",race,")"))
                    xxparty <- lst[[1]]
                    xxparty <- t(tibble(xxparty)) # emulate return from read_delim in voting_areas
                    xxparty <- toupper(xxparty) #DEBUG-TEST
                    xx0 <- lst[[2]]
                },
                error = function(e){
                    message('ERROR in getrace:')
                    errmsg(e)
                },
                warning = function(w){
                    message('WARNING in getrace:')
                    warnmsg(w)
                },
                finally = {
                    message('All done in getrace, quitting.')
                }
            )
            if (is.null(xx0)){
                print(paste0("ERROR: getrace(",race,") returns NULL"))
                return(NULL)
            }
            if (names(xx0)[1] == "DIST"){
                if (sum(!is.na(xx0$DIST)) > 0){
                    if (input$dist != ""){
                        xx0 <- xx0[xx0$DIST == input$dist,]
                    }
                    else{
                        sudist <- sort(unique(xx0$DIST))
                        updateSelectInput(session, "dist", NULL,
                                          choices = c("", sudist), selected = "")
                    }
                }
                if (input$displaydists){
                    xx0$AREA <- xx0$DIST
                }
                xx0 <- xx0[,-1]
                #xxparty <- xxparty[,-1]
                xxparty <- t(tibble(xxparty[,-1])) #DEBUG-FIX
            }
            # Remove columns where __party starts with X_, rows where AREA starts with X_
            # (Code is unnecessary and causes error on NV - comment out)
            # xx0 <- xx0[,!grepl("^X_",xxparty)]
            # xxparty <- xxparty[,!grepl("^X_",xxparty)]
            # xx0 <- xx0[ !grepl("^X_",xx0$AREA),]
            
            idem <- which(xxparty == "DEM")
            irep <- which(xxparty == "REP")
            if (length(idem) == 0){
                catmsg(paste0("##### WARNING: race has no DEM"))
                xx0$DEM <- 0
                idem <- NCOL(xx0)
                xxparty <- c(xxparty,"DEM")
            }
            else if (length(idem) > 1){
                catmsg(paste0("##### WARNING: race has multiple DEMs"))
                zmd0 <<- xx0 #DEBUG-RM
                zmdp <<- xxparty #DEBUG-RM
                if (input$sumparty){
                    xxparty[idem[1]] <- "DEM"
                    names(xx0)[idem[1]] <- "DEM"
                    for (i in 2:length(idem)){
                        xx0[idem[1]] <- xx0[idem[1]] + xx0[idem[i]]
                        xx0[idem[i]] <- 0
                        xxparty[idem[i]] <- ""
                    }
                    idem <- idem[1]
                }
                else if (input$topparty){
                    dsums <- colSums(xx0[,idem])
                    imax <- which.max(dsums)
                    idem <- idem[imax]
                }
                else{
                    idem <- idem[1]
                }
            }
            if (length(irep) == 0){
                catmsg(paste0("##### WARNING: race has no REP"))
                xx0$REP <- 0
                irep <- NCOL(xx0)
                xxparty <- c(xxparty,"REP")
            }
            else if (length(irep) > 1){
                zmr0 <<- xx0 #DEBUG-RM
                zmrp <<- xxparty #DEBUG-RM
                catmsg(paste0("##### WARNING: race has multiple REPs"))
                if (input$sumparty){
                    xxparty[irep[1]] <- "REP"
                    names(xx0)[irep[1]] <- "REP"
                    for (i in 2:length(irep)){
                        xx0[irep[1]] <- xx0[irep[1]] + xx0[irep[i]]
                        xx0[irep[i]] <- 0
                        xxparty[irep[i]] <- ""
                    }
                    irep <- irep[1]
                }
                else if (input$topparty){
                    rsums <- colSums(xx0[,irep])
                    imax <- which.max(rsums)
                    irep <- irep[imax]
                }
                else{
                    irep <- irep[1]
                }
            }
            
            # if (NCOL(xx0) > 5){
            #     ii <- c(1,2,3,idem,irep)
            #     for (i in 4:NCOL(xx0)){
            #         if (i != idem & i != irep) ii <- c(ii,i)
            #     }
            #     xx <- xx0[,ii] # COUNTY,AREA,TOTAL,DEM1,REP1,...
            # }
            # else{
            #     xx <- xx0[,c(1,2,3,idem,irep)] # COUNTY,AREA,TOTAL,DEM1,REP1
            # }
            ii <- c(1,2,3,idem,irep)
            for (i in 4:NCOL(xx0)){
                if (i != idem & i != irep & xxparty[i] != "") ii <- c(ii,i)
            }
            xx <- xx0[,ii] # COUNTY,AREA,TOTAL,DEM1,REP1,...
            xx <- as.data.frame(xx)
            
            if (input$skip_rows != ""){
                vskip_rows <- as.numeric(unlist(strsplit(input$skip_rows, ",")))
                for (i in length(vskip_rows):1){
                    xx <- xx[-vskip_rows[i],]
                }
            }
            #Set all candidate NAs to zero
            for (j in 3:NCOL(xx)){
                xx[is.na(xx[,j]),j] <- 0
            }
            if (input$totall_cand){
                xx$TOTAL <- 0
                for (j in 4:NCOL(xx)){
                    xx$TOTAL <- xx$TOTAL + xx[,j]
                }
            }
            if (input$incl_cand != ""){
                #vincl_cand <- as.numeric(unlist(strsplit(input$incl_cand, ","))) + 3
                scand <- unlist(strsplit(input$incl_cand, ","))
                vincl_cand <- NULL
                for (i in 1:length(scand)){
                    if (grepl("-", scand[i])){
                        nn <- unlist(strsplit(scand[i], "-"))
                        n1 <- as.numeric(nn[1])
                        n2 <- as.numeric(nn[2])
                        vincl_cand <- c(vincl_cand,seq(n1,n2)+3)    
                    }
                    else{
                        vincl_cand <- c(vincl_cand,as.numeric(scand[i])+3)
                    }
                }
                if (!input$totall_cand){
                    xx$TOTAL <- 0
                }
                for (i in 1:length(vincl_cand)){
                    if (vincl_cand[i] <= NCOL(xx)){
                        if (!input$totall_cand){
                            xx$TOTAL <- xx$TOTAL + xx[,vincl_cand[i]]
                        }
                    }
                    else{
                        vincl_cand <- vincl_cand[1:(i-1)]
                        break
                    }
                }
                xx <- xx[,c(1,2,3,vincl_cand)]
            }
            dd <- xx
            ddtot <- data.frame(COUNTY="",AREA="TOTAL",t(colSums(dd[,c(-1,-2)])))
            names(ddtot) <- names(dd)
            dd <- rbind(dd,ddtot)
            return(dd)
        }
        fixrace <- function(dd){
            dd$AREA <- gsub("^Precinct [0]*","",dd$AREA, ignore.case = TRUE)
            return(dd)
        }
        modarea <- function(dd, yr){
            # Move area modification to function called by getdata() & getdata2()
            # if (input$xcounty != "" & input$xcounty != "(all)"){
            #     dd <- dd[dd$COUNTY == input$xcounty,]
            # }
            # else{
            #     dd <- dd[dd$COUNTY != "" & !is.na(dd$COUNTY),]
            # }
            if (input$areamod != ""){
                dogroup <- FALSE
                for (i in 1:nchar(input$areamod)){
                    ch1 <- substr(input$areamod,i,i)
                    if (ch1 == "*"){
                        if (input$state2 == "CA"){
                            dd$AREA <- gsub("^[0]+","",dd$AREA)
                        }
                    }
                    else if (ch1 == ">"){
                        if (input$state2 == "CA"){
                            dogroup <- TRUE
                            dd$AREA <- gsub("[A-Z]+$","",dd$AREA)
                        }
                        else if (input$state2 == "FL" & yr >= 2020){
                            dogroup <- TRUE
                            # dd$AREA[dd$COUNTY == "Miami-Dade"] <-
                            #     gsub(".$","",dd$AREA[dd$COUNTY == "Miami-Dade"])
                            dd$AREA <- gsub("^[0]+","",dd$AREA)
                            dd$AREA <- gsub("\\.\\d","",dd$AREA)
                        }
                    }
                    else if (ch1 == "="){
                        dogroup <- TRUE
                        dd$AREA <- "COUNTY" #DEBUG-TEST - was TOTAL
                    }
                    else if (ch1 == "-"){
                        dogroup <- TRUE
                        pat <- substring(input$areamod,i+1)
                        dd$AREA <- gsub(pat,"",dd$AREA)
                        break
                    }
                    else if (ch1 == "#"){
                        break
                    }
                }
                if (dogroup){
                    dd <- dd %>%
                        group_by(COUNTY,AREA) %>%
                        summarize(across(where(is.numeric), sum))
                    dd <- as.data.frame(dd)
                }
            }
            dd
        }
        getdata <- reactive({
            races <- input$xraces
            nraces <- length(races)
            dd <- NULL
            if (nraces >= 1){
                dd <- getrace(races[1])
                #yr <- as.numeric(substr(races[1],4,7))
                yr <- as.numeric(substr(races[1],4,5)) + 2000 #DEBUG-CHECK
            }
            dd <- fixrace(dd)     # clean AREA, like removing leading PRECINCT
            dd <- modarea(dd, yr) # modify AREA
            zdd1 <<- dd #DEBUG-RM
            dd
        })
        getdata2 <- reactive({
            races <- input$xraces
            nraces <- length(races)
            dd <- NULL
            if (nraces >= 2){
                dd <- getrace(races[2])
                yr <- as.numeric(substr(races[2],4,5)) + 2000 #DEBUG-CHECK
            }
            dd <- fixrace(dd)
            dd <- modarea(dd, yr)
            zdd2 <<- dd #DEBUG-RM
            dd
        })
        getdata12 <- reactive({
            xx <- getdata()
            if (is.null(xx)){
                cat("ERROR: Select two races\n")
                return(NULL)
            }
            if (input$displaydists){
                xx <- xx %>%
                    group_by(COUNTY,AREA) %>%
                    summarize(across(where(is.numeric), sum))
                yy <- xx
            }
            else{
                yy <- getdata2()
                if (is.null(yy)){
                    cat("ERROR: Select second race\n")
                    return(NULL)
                }
            }
            namesxx <- names(xx)
            namesyy <- names(yy)
            names(xx)[3:5] <- c("TOTAL1","DEM1","REP1")
            names(yy)[3:5] <- c("TOTAL2","DEM2","REP2")
            xx$MARGIN1 <- xx$DEM1 - xx$REP1
            yy$MARGIN2 <- yy$DEM2 - yy$REP2
            xx <- xx[,c(1,2,4,5,NCOL(xx),3),]
            yy <- yy[,c(1,2,4,5,NCOL(yy),3),]
            if (input$toupper){
                xx$COUNTY <- toupper(xx$COUNTY)
                yy$COUNTY <- toupper(yy$COUNTY)
                xx$AREA   <- toupper(xx$AREA)
                yy$AREA   <- toupper(yy$AREA)
            }
            if (input$cleanlevel > 0){
                #xx$COUNTY <- toupper(xx$COUNTY)
                #yy$COUNTY <- toupper(yy$COUNTY)
                xx$AREA <- toupper(xx$AREA)
                yy$AREA <- toupper(yy$AREA)
            }
            if (input$cleanlevel > 1){
                xx$AREA <- gsub(" WARDS "," WARD ",xx$AREA)
                yy$AREA <- gsub(" WARDS "," WARD ",yy$AREA)
            }
            dd <- as.data.frame(merge(xx, yy, by = c("COUNTY","AREA"), all = TRUE))
            if (input$showother){
                ee <- dd[is.na(dd$MARGIN1) | is.na(dd$MARGIN2),]
                if (NROW(ee) > 0){
                    dd <- dd[!is.na(dd$MARGIN1) & !is.na(dd$MARGIN2),]
                    ee$AREA <- "Other"
                    ee <- ee %>%
                        group_by(COUNTY,AREA) %>%
                        summarize(DEM1=sum(DEM1, na.rm = TRUE),
                                  REP1=sum(REP1, na.rm = TRUE),
                                  MARGIN1=sum(MARGIN1, na.rm = TRUE),
                                  TOTAL1=sum(TOTAL1, na.rm = TRUE),
                                  DEM2=sum(DEM2, na.rm = TRUE),
                                  REP2=sum(REP2, na.rm = TRUE),
                                  MARGIN2=sum(MARGIN2, na.rm = TRUE),
                                  TOTAL2=sum(TOTAL2, na.rm = TRUE))
                    ee <- as.data.frame(ee)
                    dd <- rbind(dd, ee)
                }
            }
            if (input$areafilter != ""){
                dd <- dd[grepl(input$areafilter, dd$AREA),]
            }
            dd$DEM_SH <- dd$DEM2 - dd$DEM1
            dd$REP_SH <- dd$REP2 - dd$REP1
            dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
            dd$TOT_SH <- dd$TOTAL2 - dd$TOTAL1
            dd$DEM1_N <- dd$DEM1
            dd$REP1_N <- dd$REP1
            dd$MAR1_N <- dd$MARGIN1
            dd$TOT1_N <- dd$TOTAL1
            dd$TOT2_N <- dd$TOTAL2
            ddnt <- dd[dd$AREA != "TOTAL",] #fix Area2 TOTALs
            dd <- rbind(dd, data.frame(COUNTY="TOTAL",AREA="TOTAL",
                                       t(colSums(ddnt[,c(-1,-2)],na.rm = TRUE))))
            
            if (input$units == "Percent"){
                dd$DEM1 <- 100 * dd$DEM1 / dd$TOTAL1
                dd$REP1 <- 100 * dd$REP1 / dd$TOTAL1
                dd$MARGIN1 <- 100 * dd$MARGIN1 / dd$TOTAL1
                dd$DEM2 <- 100 * dd$DEM2 / dd$TOTAL2
                dd$REP2 <- 100 * dd$REP2 / dd$TOTAL2
                dd$MARGIN2 <- 100 * dd$MARGIN2 / dd$TOTAL2
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                dd$TOTAL2 <- dd$DEM2 + dd$REP2
                dd$DEM_SH <- dd$DEM2 - dd$DEM1
                dd$REP_SH <- dd$REP2 - dd$REP1
                dd$MAR_SH <- dd$MARGIN2 - dd$MARGIN1
                dd$TOT_SH <- dd$TOTAL2 - dd$TOTAL1
            }
            else if (input$units == "Percent ratio"){
                dd$DEM_SH <- 100 * dd$DEM1 / dd$DEM2
                dd$REP_SH <- 100 * dd$REP1 / dd$REP2
                dd$MAR_SH <- 100 * dd$TOTAL1 / dd$TOTAL2
                dd$TOT_SH <- 100 * dd$TOTAL1 / dd$TOTAL2
                #assume percent for x-axis
                dd$DEM1 <- 100 * dd$DEM1 / dd$TOTAL1
                dd$REP1 <- 100 * dd$REP1 / dd$TOTAL1
                dd$MARGIN1 <- 100 * dd$MARGIN1 / dd$TOTAL1
                # dd$DEM2 <- 100 * dd$DEM2 / dd$TOTAL2
                # dd$REP2 <- 100 * dd$REP2 / dd$TOTAL2
                # dd$MARGIN2 <- 100 * dd$MARGIN2 / dd$TOTAL2
                dd$TOTAL1 <- dd$DEM1 + dd$REP1
                # dd$TOTAL2 <- dd$DEM2 + dd$REP2
            }
            names(dd)[3:4] <- namesxx[4:5] # reset names
            names(dd)[7:8] <- namesyy[4:5]
            row.names(dd) <- seq(1:NROW(dd))
            dd
        })
        getdataN <- reactive({
            races <- input$xraces
            nraces <- length(races)
            if (nraces < 1){
                cat("ERROR: Select one or more races\n")
                return(NULL)
            }
            if (input$xparty1 == "(all)"){
                if (nraces < 2){
                    cat("ERROR: Select two or more races\n")
                    return(NULL)
                }
                for (i in 1:nraces){
                    xx <- getrace(races[i])
                    if (is.null(xx) | NCOL(xx) < 5){ #if no race with DEM and REP
                        next
                    }
                    if (input$xcounty != "" & input$xcounty != "(all)"){
                        xx <- xx[xx$COUNTY == input$xcounty,]
                    }
                    else{
                        xx <- xx[xx$COUNTY != "" & !is.na(xx$COUNTY),]
                    }
                    names(xx)[3:5] <- c("TOTAL","DEM","REP")
                    if (input$bycounty){
                        xx <- xx %>%
                            group_by(COUNTY) %>%
                            summarize(TOTAL=sum(TOTAL), DEM=sum(DEM), REP=sum(REP))
                    }
                    rf <- input$racefmt
                    racei <- races[i]
                    rsplit <- strsplit(racei, "_")
                    if (length(unlist(rsplit)) > 1){
                        from <- as.numeric(substr(rf,1,1))
                        to   <- as.numeric(substr(rf,2,2))
                        racen <- substr(unlist(rsplit)[2],from,to)
                    }
                    if (length(unlist(rsplit)) > 2){
                        sep  <- substr(rf,3,3)
                        from <- as.numeric(substr(rf,4,4))
                        to   <- as.numeric(substr(rf,5,5))
                        if (to < from){
                            racen <- paste0(racen,sep,substring(unlist(rsplit)[3],from))
                        }
                        else{
                            racen <- paste0(racen,sep,substr(unlist(rsplit)[3],from,to))
                        }
                    }
                    if (length(unlist(rsplit)) > 3){
                        sep  <- substr(rf,6,6)
                        from <- as.numeric(substr(rf,7,7))
                        to   <- as.numeric(substr(rf,8,8))
                        racen <- paste0(racen,sep,substr(unlist(rsplit)[4],from,to))
                    }
                    if (input$partyn == "Margin"){
                        xx[[racen]] <- xx$DEM - xx$REP
                    }
                    else if (input$partyn == "Total"){
                        xx[[racen]] <- xx$TOTAL
                    }
                    else if (input$partyn == "Dem"){
                        xx[[racen]] <- xx$DEM
                    }
                    else if (input$partyn == "Dem+"){
                        xx[[racen]] <- xx$TOTAL - xx$REP
                    }
                    else if (input$partyn == "Rep"){
                        xx[[racen]] <- xx$REP
                    }
                    else if (input$partyn == "Rep+"){
                        xx[[racen]] <- xx$TOTAL - xx$DEM
                    }
                    else{
                        xx[[racen]] <- xx$TOTAL - xx$DEM - xx$REP
                    }
                    if (input$units != "Count"){
                        xx[[racen]] <- 100 * xx[[racen]] / xx$TOTAL
                    }
                    if (input$bycounty){
                        xx <- xx[,c(1,NCOL(xx)),]
                    }
                    else{
                        xx <- xx[,c(1,2,NCOL(xx)),]
                    }
                    if (i == 1){
                        dd <- xx
                    }
                    else{
                        if (input$bycounty){
                            dd <- as.data.frame(merge(dd, xx, by = c("COUNTY"), all = TRUE))
                        }
                        else{
                            dd <- as.data.frame(merge(dd, xx, by = c("COUNTY","AREA"), all = TRUE))
                        }
                    }
                }
            }
            else{
                xx <- getrace(races[1])
                for (i in 4:NCOL(xx)){
                    if (input$units != "Count"){
                        xx[,i] <- 100 * xx[,i] / xx$TOTAL # xx[,3]
                    }
                }
                xx <- xx[xx$AREA != "TOTAL",]
                xx <- xx[-3]
                if (NCOL(xx) > 8){
                    xx <- xx[,1:8]
                }
                if (input$bycounty){
                    xx <- xx[-2]
                }
                dd <- xx
            }
            xsortcolN <- input$xsortcolN
            if (xsortcolN < 0){
                xsortcolN <- xsortcolN + NCOL(dd) + 1
            }
            else{
                xsortcolN <- xsortcolN + 2
            }
            normalizeN <- input$normalizeN
            if (normalizeN < 0){
                normalizeN <- normalizeN + NCOL(dd) + 1
            }
            if (normalizeN > 1){
                ddnorm <- as.numeric(dd[,normalizeN])
                for (i in 3:NCOL(dd)){
                    dd[,i] <- as.numeric(dd[,i]) - ddnorm
                }
            }
            if (xsortcolN != 0){
                if (input$xsortdescN){
                    dd <- dd[order(dd[xsortcolN]),]
                }
                else{
                    if (class(dd[xsortcolN]) == "numeric"){
                        dd <- dd[order(-dd[xsortcolN]),]
                    }
                    else{
                        dd <- dd[order(dd[xsortcolN]),]
                        dd <- dd %>% arrange(desc(row_number()))
                    }
                }
            }
            if (normalizeN > 1) dd <- dd[-normalizeN]
            dd <- fixrace(dd)
            dd
        })
        observeEvent(input$mapsave,{
            eventid <- "Map"
            parmid <- c("maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observe({
            eventid <- "Plot"
            loadid <- "plotload"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            parmup <- c("checkbox", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "select",
                        "vlimit", "vshape", "vdesc")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = as.logical(pp$value[pp$label == parmid[i]]))
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        observeEvent(input$plotsave,{
            eventid <- "Plot"
            parmid <- c("showrow", "pos1", "pos3", "xscale", "yscale",
                        "xlimit", "xcolor", "xparty", "noparty",
                        "vlimit", "vshape", "vdesc")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                newversion <- max(parms$version) + 1
            }
            else{
                parms <- data.frame(version=integer(),
                                    label=character(),
                                    value=character(),
                                    stringsAsFactors = FALSE)
                newversion <- 1
            }
            nr <- NROW(parms)
            version <- rep(newversion, length(parmid))
            label <- parmid
            value <- NULL
            for (i in 1:length(parmid)){
                value <- c(value, input[[parmid[i]]])
            }
            aparms <- data.frame(version, label, value)
            parms <- rbind(parms, aparms)
            write_csv(parms, filename)
        })
        observe({
            eventid <- "Map"
            loadid <- "mapload"
            parmid <- c("minpop", "longoff", "skipcity",
                        "showcity", "maplimitset", "maplimits",
                        "mapyear","mapvar","mapcolors")
            parmup <- c("numeric", "numeric", "skipcity",
                        "showcity", "select", "maplimits",
                        "numeric","select","mapcolors")
            filename <- paste0(data_dir,input$state2,"_",eventid,"_Parms.csv")
            if (file.exists(filename)){
                parms <- read_csv(filename)
                loadversion <- input[[loadid]]
                pp <- parms[parms$version == loadversion,]
                for (i in 1:length(parmid)){
                    if (parmup[i] == "numeric"){
                        updateNumericInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "select"){
                        updateSelectInput(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "checkbox"){
                        updateCheckboxInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                    else if (parmup[i] == "radio"){
                        updateRadioButtons(session, parmid[i], selected = pp$value[pp$label == parmid[i]])
                    }
                    else{
                        updateTextInput(session, parmid[i], value = pp$value[pp$label == parmid[i]])
                    }
                }
            }
        })
        # observeEvent(input$state2,{
        #     url <- paste0("https://github.com/openelections/openelections-data-",
        #                   tolower(input$state2))
        #     #url = url(url, "rb")
        #     html <- url %>% read_html()
        #     opens <- html %>% html_elements(".js-navigation-open")
        #     choices <- NULL
        #     for (i in 1:length(opens)){
        #         txt <- opens[i] %>% html_text()
        #         if (grepl("^\\d+$",txt)){
        #             choices <- c(choices, txt)
        #         }
        #     }
        #     # Sys.sleep(10)
        #     # closeAllConnections()
        #     # gc()
        #     updateSelectInput(session,"xyear",choices = choices,selected = choices[1])
        #     #close(url)
        # })
        getElectionfile <- function(){
            efiles <- cfiles[as.character(cfiles$elections) == input$xelection,]
            if (input$createfiles){
                reqcols <- c("county","precinct","office","district","party","candidate","votes")
                curcols <- reqcols
                optcols <- NULL
                enames <- efiles$filenames
                yy <- NULL
                for (ename in enames){
                    print(paste0("BEFORE read_csv(",ename,")")) #DEBUG-RM
                    filepath <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-",
                                       tolower(input$state2),"/master/",input$xyear,"/counties/",ename)
                    xx0 <- read_csv(filepath)
                    cols0 <- names(xx0)
                    xx <- xx0[,reqcols]
                    for (col in optcols){
                        xx[[col]] <- ""
                    }
                    for (col in cols0){
                        if (col %in% curcols){
                            if (!(col %in% reqcols)){ # in optcols
                                xx[[col]] <- xx0[[col]]
                            }
                        }
                        else{
                            curcols <- c(curcols,col)
                            optcols <- c(optcols,col)
                            xx[[col]] <- xx0[[col]]
                            if (!is.null(yy)){
                                yy[[col]] <- ""
                            }
                        }
                    }
                    if (is.null(yy)){
                        yy <- xx
                    }
                    else{
                        yy <- rbind(yy, xx)
                    }
                }
                filename <- paste0(data_dir,input$xelection,"__precinct.csv")
                write_csv(yy, filename)
            }
        }
        #function from https://stackoverflow.com/questions/52911812/check-if-url-exists-in-r
        valid_url <- function(url_in,t=2){
            con <- url(url_in)
            check <- suppressWarnings(try(open.connection(con,open="rt",timeout=t),silent=T)[1])
            suppressWarnings(try(close.connection(con),silent=T))
            ifelse(is.null(check),TRUE,FALSE)
        }
        getElections <- function(){
            url <- paste0("https://github.com/openelections/openelections-data-",
                          tolower(input$state2),"/tree/master/",input$xyear)
            if (input$countyfiles){
                url2 <- paste0(url,"/counties")
                if (valid_url(url2)){
                    url <- url2
                }
            }
            #closeAllConnections()
            htm <- NULL
            result = tryCatch({
                htm <- url %>% read_html()
            }, warning = function(w) {
                print(paste0("WARNING in getElections: ",w))
            }, error = function(e) {
                print(paste0("ERROR in getElections: ",e))
            }, finally = {
                #cleanup-code
            })
            if (!is.null(htm)){
                opens <- htm %>% html_elements(".js-navigation-open")
                filenames <- NULL
                elections <- NULL
                counties  <- NULL
                for (i in 1:length(opens)){
                    txt <- opens[i] %>% html_text()
                    if (input$countyfiles){
                        pattern <- paste0("^(\\d+)__",tolower(input$state2),
                                          "__(\\w+)__(\\w+)__precinct.csv$")
                    }
                    else{
                        pattern <- paste0("^(\\d+)__",tolower(input$state2),
                                          "__(\\w+)__precinct.csv$")
                    }
                    mm <- str_match(txt, pattern)
                    if(!is.na(mm[1,1])){
                        filenames <- c(filenames, txt)
                        election <- paste0(mm[1,2],"__",tolower(input$state2),"__",mm[1,3])
                        elections <- c(elections, election)
                        if (input$countyfiles){
                            counties <- c(counties, str_to_title(mm[1,4]))
                        }
                        else{
                            counties <- c(counties, "")
                        }
                    }
                }
                cfiles <<- data.frame(filenames, elections, counties)
                uelections <- unique(cfiles$elections)
                print(uelections) #DEBUG
                updateSelectInput(session,"xelection",choices = uelections,selected = uelections[1])
            }
        }
        observeEvent(input$state2,{
            print(paste0("START observeEvent(",input$state2,")"))
            getElections()
        })
        observeEvent(input$xyear,{
            print(paste0("START observeEvent(",input$xyear,")"))
            getElections()
        })
        observeEvent(input$xelection,{
            print(paste0("START observeEvent(",input$xelection,")"))
            if (input$xelection != ""){
                #getElectionfile()
                efiles <- cfiles[as.character(cfiles$elections) == input$xelection,]
                ucounties <- unique(as.character(efiles$counties))
                if (!input$countyfiles){
                    filename <- efiles$filenames
                    data_path <- "data"
                    localpath <- paste0(data_path,"/",filename)
                    if (file.exists(localpath)){
                        xx <- read_csv(localpath)
                    }
                    else{
                        # filepath <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-",
                        #                    tolower(input$state2),"/master/",input$xyear,"/",filename)
                        #                   https://github.com/openelections/openelections-data-fl/blob/master/2022/20221108__fl__general__precinct.csv
                        #                   https://github.com/openelections/openelections-data-fl/raw/master/2022/20221108__fl__general__precinct.csv
                        filepath <- paste0("https://github.com/openelections/openelections-data-",
                                           tolower(input$state2),"/raw/master/",input$xyear,"/",filename)
                        #xx <- read_csv(filepath)
                        xx <- read_csv(filepath, guess_max = 1000000)
                        validate_counties(xx, input$xelection)
                        if (file.exists(data_path)){
                            catmsg(paste0("====> write_csv(",localpath,")"))
                            write_csv(xx, localpath)
                        }
                    }
                    ucounties <- unique(xx$county)
                }
                xcounty <- input$xcounty
                if (!(xcounty %in% ucounties)){
                    xcounty <- ucounties[1]
                }
                ucounties <- sort(ucounties)
                ucounties <- c(ucounties,"(all)")
                updateSelectInput(session,"xcounty",choices = ucounties,selected = xcounty)
                # add from end of observeEvent(input$xcounty
                uoffices <- unique(xx$office)
                xoffice <- input$xoffice
                if (!(xoffice %in% uoffices)){
                    xoffice <- uoffices[1]
                }
                updateSelectInput(session,"xoffice",choices = uoffices,selected = xoffice)
            }
        })
        observeEvent(input$xcounty,{
            print(paste0("START observeEvent(",input$xcounty,")"))
            xcounty <- input$xcounty
            if (length(xcounty) > 0 & xcounty[1] != "" & (xcounty[1] != "(all)" | !input$countyfiles)){
                if(input$countyfiles){
                    cfile <- cfiles[as.character(cfiles$elections) == input$xelection & cfiles$counties == xcounty[1],]
                    filename <- as.character(cfile$filenames)
                    filepath <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-",
                                       tolower(input$state2),"/master/",input$xyear,"/counties/",filename)
                }
                else{
                    cfile <- cfiles[as.character(cfiles$elections) == input$xelection,]
                    filename <- as.character(cfile$filenames)
                    filepath <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-",
                                       tolower(input$state2),"/master/",input$xyear,"/",filename)
                }
                if (!exists(filename, envir = .GlobalEnv)){
                    xx <- NULL
                    local_filepath <- paste0("./data/",filename)
                    if (file.exists(local_filepath)){
                        print(paste0("local_filepath=",local_filepath,"|"))
                        filepath <- local_filepath
                    }
                    result = tryCatch({
                        #xx <- read_csv(filepath, col_types = cols(.default = "c"))
                        xx <- read_csv(filepath, guess_max = 1000000)
                        assign(filename, xx, envir = .GlobalEnv)
                    }, warning = function(w) {
                        print(paste0("WARNING in observeEvent(xcounty): ",w))
                        print(paste0("read_csv(",filepath,")"))
                    }, error = function(e) {
                        print(paste0("ERROR in observeEvent(xcounty): ",e))
                    }, finally = {
                        #cleanup-code
                    })
                }
                else{
                    xx <- get(filename, envir = .GlobalEnv)
                }
                if (xcounty[1] != "(all)"){
                    xx <- xx[xx$county == xcounty[1],]
                }
                print(head(xx)) #DEBUG-RM
                uoffices <- unique(xx$office)
                xoffice <- input$xoffice
                if (!(xoffice %in% uoffices)){
                    xoffice <- uoffices[1]
                }
                updateSelectInput(session,"xoffice",choices = uoffices,selected = xoffice)
            }
        })
        #observeEvent(input$xoffice,{
        observeEvent(input$addrace,{
            #getCounties()
            state <- input$state2
            year <- input$xyear
            election <- input$xelection
            county <- input$xcounty
            office <- input$xoffice # change to input$xoffice #DEBUG-CHANGE
            if (is.na(office)){
                office <- "NA"
            }
            mm <- str_match(election,"20(\\d+)")
            race <- paste0(toupper(state),"_",mm[1,2], "_",office)
            new_xraces <- c(input$xraces, race)
            new_rdata <- data.frame(state,year,election,county,office,race) #keep info for each race
            if (is.null(rdata)){
                rdata <<- new_rdata
            }
            else{
                if (!(new_rdata$race %in% rdata$race)){
                    rdata <<- rbind(rdata, new_rdata)
                }
                #tfiles <- rbind(rdata, new_rdata)
                #rdata <<- tfiles
            }
            catmsg("***** rdata *****")
            capture.output(print(rdata), file="catlog.txt", append = TRUE)
            updateSelectInput(session,"xraces",choices = new_xraces,selected = new_xraces)
        })
        observeEvent(input$clrrace,{
            updateSelectInput(session,"xraces",choices = "",selected = "")
        })
        observeEvent(input$sortcounty,{
            if (input$sortcounty == "COUNTY"){
                updateRadioButtons(session,"sortcountydir",selected = "Ascending")
            }
            else{
                updateRadioButtons(session,"sortcountydir",selected = "Desc")
            }
            #getCounties() #DEBUG-CHECK - COMMENT OUT
        })
        observeEvent(input$checkstate,{
            check_state(input$state2)
        })
        observeEvent(input$checkall,{
            for (ss in state2s){
                check_state(ss)
            }
        })
        observe({
            catmsg(paste0("v3: ",input$state2," ",input$tabs))
        })
        check_state <- function(state2){
            ff <- "check_state.txt"
            if (toupper(state2) == "WI"){
                areaname <- "ward"
            }
            else{
                areaname <- "precinct"
            }
            xelection <- input$xelection
            mm <- str_match(xelection,"(\\d+)\\_\\_([A-Za-z]{2})\\_\\_([A-Za-z\\_]+)")
            filename <- paste0(mm[1,2],"__",tolower(state2),"__",mm[1,4],"__",areaname,".csv")
            xelection2 <- paste0(mm[1,2],"__",tolower(state2),"__",mm[1,4])
            filepath <- paste0("https://raw.githubusercontent.com/openelections/openelections-data-",
                               tolower(state2),"/master/",input$xyear,"/",filename)
            catfile(ff,paste0("########## START check_state(",state2,"), filename=",filename))
            result = tryCatch({
                xx <- read_csv(filepath)
                if (toupper(state2) == "WI"){
                    names(xx)[names(xx) == "ward"] <- "precint"
                }
                reqcols <- c("county","precinct","office","district","party","candidate","votes")
                for (col in reqcols){
                    if (!(col %in% names(xx))){
                        catfile(ff,paste0("=========> ",filename," missing column ",col))
                    }
                }
                ucounties <- unique(xx$county)
                for (cc in ucounties){
                    yy <- xx[xx$county == cc,]
                    nna <- sum(is.na(yy$precinct))
                    if (nna > 0){
                        catfile(ff,paste0("  ",nna," of ",NROW(yy)," precints are NA in ",xelection2,", county ",cc))
                    }
                }
            # }, warning = function(w) {
            #     catfile(ff,paste0("WARNING in check_state(",state2,"): ",w))
            }, error = function(e) {
                catfile(ff,paste0("ERROR in check_state(",state2,"): ",e))
            }, finally = {
                #cleanup-code
            })
        }
        validate_counties <- function(xx, xelection){
            reqcols <- c("county","precinct","office","district","party","candidate","votes")
            for (col in reqcols){
                if (!(col %in% names(xx))){
                    catmsg(paste0("########## ",filename," missing column ",col))
                }
            }
            ucounties <- unique(xx$county)
            for (cc in ucounties){
                yy <- xx[xx$county == cc,]
                nna <- sum(is.na(yy$precinct))
                if (nna > 0){
                    catmsg(paste0("########## ",nna," of ",NROW(yy)," precints are NA in ",xelection,", county ",cc))
                }
            }
        }
    }
)
