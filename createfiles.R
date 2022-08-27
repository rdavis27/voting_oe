## All ElectionWare (EW) counties and filenames
# az_ew_counties <- c(
#     "Apache","Cochise",
#     "Gila","Graham",
#     "Greenlee","La Paz",
#     "Mohave","Navajo",
#     "Pima","Pinal",
#     "Santa Cruz","Yuma")
# az_ew_files <- c(
#     "6967.Apache.Detail.txt","6970.Cochise.Detail.txt",
#     "6972.Gila_.Detail.txt","6887.Graham.Detail.txt",
#     "6841.Greenlee.Detail.txt","6957.La Paz.Detail.txt",
#     "6969.Mohave.Detail.txt","6955.Navajo.Detail.txt",
#     "6981.Pima_.Detail.txt","6956.Pinal_.Detail.txt",
#     "6971.Santa Cruz.Detail.txt","6979.Yuma_.Detail.txt")
## Only the following ElectionWare (EW) counties work currently
## due to changes in precinct names from 2018 to 2020.
az_ew_counties <- c(
    "Apache","Cochise","Gila","Graham","Greenlee","La Paz",
    "Mohave","Navajo","Pima","Pinal","Santa Cruz","Yuma")
az_ew_files <- c(
    "6967.Apache.Detail.txt","6970.Cochise.Detail.txt",
    "6972.Gila_.Detail.txt","6887.Graham.Detail.txt",
    "6841.Greenlee.Detail.txt","6957.La Paz.Detail.txt",
    "6969.Mohave.Detail.txt","6955.Navajo.Detail.txt","6981.Pima_.Detail.txt",
    "6956.Pinal_.Detail.txt","6971.Santa Cruz.Detail.txt","6979.Yuma_.Detail.txt")
createAZ_2018_Senate <- function(){ # currently just loads Maricopa
    xx <- read_delim(paste0(input_dir,"AZ/2018/6989.Maricopa.Detail.txt"),'\t',
                     col_names = TRUE, col_types = "cccdddddcdddddddddd")
    office <- "US Senate" #UPDATE
    xx <- xx[xx$CONTEST_FULL_NAME == office,]
    xx <- xx[,c("PRECINCT_NAME","CANDIDATE_FULL_NAME","TOTAL")] #leave off "IS_WRITEIN","undervote","overvote"
    xx$COUNTY <- "Maricopa"
    xx$TOT <- 0
    xx <- xx[,c("COUNTY","PRECINCT_NAME","CANDIDATE_FULL_NAME","TOTAL")]
    names(xx) <- c("COUNTY","AREA","Candidate","Votes")
    xx$Candidate <- gsub(" - ","_",xx$Candidate)
    xx$Candidate <- gsub(" ","",xx$Candidate)
    xx$Candidate[xx$Candidate == "Write-InCandidate"] <- "NON_WRITEIN"
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    #COUNTY   AREA           `DEM_SINEMA,KYRSTEN` `GRN_GREEN,ANGELA` NON_WRITEIN `REP_MCSALLY,MARTHA` TOTAL
    #Maricopa 0001 ACACIA                    1209                 84           8                  891     0
    
    start <- c( 8,12,102,112,168,206)
    end   <- c(11,17,104,167,205,234)
    nms   <- c("AREA","Votes","Party","Contest","Candidate","AreaName")
    cc <- az_ew_files
    for (i in 1:length(cc)){
        filename <- paste0(input_dir,"AZ/2018/",cc[i])
        dd <- read_fwf(filename, fwf_positions(start, end, nms), col_types = "cicccc")
        if (az_ew_counties[i] == "Pima"){
            office <- "U.S. SENATOR" #UPDATE
        }
        else if (az_ew_counties[i] %in% c("Gila","Greenlee","Mohave")){
            office <- "United States Senator" #UPDATE
        }
        else{
            office <- "U.S. Senator" #UPDATE
        }
        dd <- dd[substr(dd$Contest,1,nchar(office)) == office,]
        if (NROW(dd) == 0){
            catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
            next
        }
        dd$COUNTY <- az_ew_counties[i]
        #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
        #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
        dd$AREA[dd$COUNTY == "Apache"] <- dd$AreaName[dd$COUNTY == "Apache"]
        dd$AREA[dd$COUNTY == "Cochise"] <- dd$AreaName[dd$COUNTY == "Cochise"]
        dd$AREA[dd$COUNTY == "Cochise"] <- dd$AreaName[dd$COUNTY == "Cochise"]
        dd$AREA[dd$COUNTY == "Gila"] <- dd$AreaName[dd$COUNTY == "Gila"]
        dd$AREA[dd$COUNTY == "Graham"] <- dd$AreaName[dd$COUNTY == "Graham"]
        dd$AREA[dd$COUNTY == "Greenlee"] <- dd$AreaName[dd$COUNTY == "Greenlee"]
        dd$AREA[dd$COUNTY == "La Paz"] <- dd$AreaName[dd$COUNTY == "La Paz"]
        dd$AREA[dd$COUNTY == "Mohave"] <- dd$AreaName[dd$COUNTY == "Mohave"]
        dd$AREA[dd$COUNTY == "Navajo"] <- dd$AreaName[dd$COUNTY == "Navajo"]
        dd$AREA[dd$COUNTY == "Pinal"] <- dd$AreaName[dd$COUNTY == "Pinal"]
        dd$AREA[dd$COUNTY == "Santa Cruz"] <- dd$AreaName[dd$COUNTY == "Santa Cruz"]
        dd <- dd[,c("COUNTY","AREA","Candidate","Party","Votes")]
        dd$AREA[dd$COUNTY == "Pima"] <- gsub("^0","",dd$AREA[dd$COUNTY == "Pima"])
        dd$AREA[dd$COUNTY == "Yuma"] <- gsub("^0","",dd$AREA[dd$COUNTY == "Yuma"])
        dd$Party[dd$Party == "."] <- "NON"
        dd$Candidate[dd$Candidate == "WRITE-IN"] <- "WRITEIN"
        for (j in 1:NROW(dd)){
            dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
            dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
            if (!is.na(dd$Party[j])){
                dd$Candidate[j] <- paste0(dd$Party[j],"_",dd$Candidate[j])
            }
        }
        dd <- dd[,-4] # delete Party
        # check for matches first???
        dd <- dd %>%
            group_by(COUNTY,AREA,Candidate) %>%
            summarize(Votes=sum(Votes))
        dd <- dd %>% spread(Candidate,Votes)
        dd$TOTAL <- 0
        dd <- subset(dd,select = -c(OVERVOTES,UNDERVOTES))
        xx <- rbind(xx,dd)
    }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        if (namesxx[j] == "WRITE-IN"){
            partyxx[j] <- "Writein"
            namesxx[j] <- "Writein"
        }
        else{
            strs <- unlist(strsplit(namesxx[j],split="_"))
            partyxx[j] <- strs[1]
            if (length(strs) >= 2){
                fullname <- strs[2]
                namesxx[j] <- unlist(strsplit(fullname,split=","))[1] #last name
            }
        }
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"AZ_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"AZ_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createAZ_2020_President <- function(votetypes){
    txt <- "text"
    num <- "numeric"
    key <- "text"
    boo <- "text" # boo as numeric causes error
    xx <- read_excel(paste0(input_dir,"AZ/2020/Results.Detail_2020General.xml/",
                            "Results.Detail_2020General .xls.xlsx"),
                     sheet = "Sheet1", skip = 0, col_names = TRUE,
                     col_types = c(txt,txt,txt,num,key, #key
                                   txt,num,num,num,num, #precinctsParticipating
                                   num,num,key,txt,key, #districtKey
                                   txt,num,num,boo,num, #countiesParticipating
                                   num,num,num,num,key, #key6
                                   txt,num,num,num,key, #key11
                                   txt,num,txt,num,boo, #isWriteIn
                                   key,txt,num,txt,num, #votes14
                                   key,txt,num,txt,num)) #votes19
    office <- "President of the United States" #UPDATE
    xx <- xx[xx$contestLongName == office,]
    if (votetypes != "" & votetypes != "all"){
        xx <- xx[xx$voteTypeName18 %in% votetypes,]
    }
    xx <- xx[,c("name13","name16","choiceName","party","votes19")]
    names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
    xx <- xx[!is.na(xx$AREA),]
    xx$AREA[xx$COUNTY == "Yuma"] <- gsub("^PRECINCT ","",xx$AREA[xx$COUNTY == "Yuma"])
    xx$Party[xx$Party == "Party for Socialism and Liberation"] <- "PSL"
    for (j in 1:NROW(xx)){
        xx$Candidate[j] <- head(strsplit(xx$Candidate[j],split=",")[[1]],1) #use last name
        xx$Candidate[j] <- gsub(" ","",xx$Candidate[j]) # remove blanks
        if (!is.na(xx$Party[j])){
            xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
        }
    }
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    #gxx2 <<- xx #DEBUG-RM
    # for (j in 4:(NCOL(xx)-1)){
    #     xx$TOTAL <- xx$TOTAL + xx[,j]
    # }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    #gxx3 <<- xx #DEBUG-RM
    #START OF CODE FOR PRESIDENT ONLY
    # xx <- xx[,c(1:5,8:10,7,6)]
    # names(xx)[9:10] <- c("Writein","Misc")
    # xx$Writein[is.na(xx$Writein)] <- 0
    # namesxx <- namesxx[c(1:5,8:10,7,6)]
    # partyxx <- partyxx[c(1:5,8:10,7,6)]
    #END OF CODE FOR PRESIDENT ONLY
    names(xx) <- namesxx
    if (votetypes[1] == "Early Ballots"){
        fileout <- paste0("AZ_2020_President_Early.csv")
    }
    else if (votetypes[1] == "Polling Place"){
        fileout <- paste0("AZ_2020_President_Polls.csv")
    }
    else if (votetypes[1] == "Provisional Ballots"){
        fileout <- paste0("AZ_2020_President_Prov.csv")
    }
    else{
        fileout <- paste0("AZ_2020_President.csv")
    }
    write(paste(partyxx, collapse = " "), paste0(data_dir,fileout))
    write_delim(xx, paste0(data_dir,fileout), append = TRUE, col_names = TRUE)
}
createAZ_2020_Senate <- function(votetypes){
    txt <- "text"
    num <- "numeric"
    key <- "text"
    boo <- "text" # boo as numeric causes error
    xx <- read_excel(paste0(input_dir,"AZ/2020/Results.Detail_2020General.xml/",
                            "Results.Detail_2020General .xls.xlsx"),
                     sheet = "Sheet1", skip = 0, col_names = TRUE,
                     col_types = c(txt,txt,txt,num,key, #key
                                   txt,num,num,num,num, #precinctsParticipating
                                   num,num,key,txt,key, #districtKey
                                   txt,num,num,boo,num, #countiesParticipating
                                   num,num,num,num,key, #key6
                                   txt,num,num,num,key, #key11
                                   txt,num,txt,num,boo, #isWriteIn
                                   key,txt,num,txt,num, #votes14
                                   key,txt,num,txt,num)) #votes19
    office <- "U.S. Senator (Term Expires Jan. 2023)" #UPDATE
    xx <- xx[xx$contestLongName == office,]
    if (votetypes != "" & votetypes != "all"){
        xx <- xx[xx$voteTypeName18 %in% votetypes,]
    }
    xx <- xx[,c("name13","name16","choiceName","party","votes19")]
    names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
    xx <- xx[!is.na(xx$AREA),]
    xx$AREA[xx$COUNTY == "Yuma"] <- gsub("^PRECINCT ","",xx$AREA[xx$COUNTY == "Yuma"])
    xx$Party[xx$Party == "Party for Socialism and Liberation"] <- "PSL"
    for (j in 1:NROW(xx)){
        xx$Candidate[j] <- head(strsplit(xx$Candidate[j],split=",")[[1]],1) #use last name
        xx$Candidate[j] <- gsub(" ","",xx$Candidate[j]) # remove blanks
        if (!is.na(xx$Party[j])){
            xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
        }
    }
    xx$Candidate[xx$Candidate == "Rodriguez_IND"] <- "Rodriguez1_IND"
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    #gxx2 <<- xx #DEBUG-RM
    # for (j in 4:(NCOL(xx)-1)){
    #     xx$TOTAL <- xx$TOTAL + xx[,j]
    # }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
    idem <- 0
    irep <- 0
    if ("DEM" %in% partyxx){
        idem <- which(namesxx == "Kelly") #specify name if multiple DEMs
        ii <- c(ii, idem)
    }
    if ("REP" %in% partyxx){
        irep <- which(namesxx == "McSally") #specify name if multiple REPs
        ii <- c(ii, irep)
    }
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    #gxx3 <<- xx #DEBUG-RM
    #START OF CODE FOR PRESIDENT ONLY
    # xx <- xx[,c(1:5,8:10,7,6)]
    # names(xx)[9:10] <- c("Writein","Misc")
    # xx$Writein[is.na(xx$Writein)] <- 0
    # namesxx <- namesxx[c(1:5,8:10,7,6)]
    # partyxx <- partyxx[c(1:5,8:10,7,6)]
    #END OF CODE FOR PRESIDENT ONLY
    names(xx) <- namesxx
    if (votetypes[1] == "Early Ballots"){
        fileout <- paste0("AZ_2020_Senate_Early.csv")
    }
    else if (votetypes[1] == "Polling Place"){
        fileout <- paste0("AZ_2020_Senate_Polls.csv")
    }
    else if (votetypes[1] == "Provisional Ballots"){
        fileout <- paste0("AZ_2020_Senate_Prov.csv")
    }
    else{
        fileout <- paste0("AZ_2020_Senate.csv")
    }
    write(paste(partyxx, collapse = " "), paste0(data_dir,fileout))
    write_delim(xx, paste0(data_dir,fileout), append = TRUE, col_names = TRUE)
}
createCA_2016_President <- function(){
    catmsg("##### START createCA_2016_President #####")
    cc <- read_delim(paste0(input_dir,"CA/CA_county-list.csv"),',')
    xx <- read_delim(paste0(input_dir,"CA/2016/",
                            "state_g16_sov_data_by_g16_svprec.csv"),',')
    xx <- xx[,c("COUNTY","SVPREC","TOTVOTE","PRSDEM01","PRSREP01",
                "PRSLIB01","PRSGRN01","PRSPAF01")]
    names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Johnson","Stein","LaRiva")
    xx <- xx[!grepl("_TOT$", xx$AREA),]
    xx <- xx[!grepl("^CNTYTOT$", xx$AREA),]
    xx <- xx[!grepl("^SOVTOT$", xx$AREA),]
    # xx$AREA <- gsub("A[ ]*$","",xx$AREA)
    xx$AREA <- gsub("^[ ]*[0]+","",xx$AREA)
    # xx <- xx %>%
    #     group_by(COUNTY,AREA) %>%
    #     summarize(TOTAL=sum(TOTAL),
    #               Clinton=sum(Clinton),
    #               Trump=sum(Trump),
    #               Johnson=sum(Johnson),
    #               Stein=sum(Stein),
    #               LaRiva=sum(LaRiva))            
    xx$COUNTY <- cc$county_name[xx$COUNTY]
    #xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","PAF")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"CA_2016_President.csv"))
    write_delim(xx, paste0(data_dir,"CA_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createCA_2018_Governor <- function(){
    catmsg("##### START createCA_2018_Governor #####")
    cc <- read_delim(paste0(input_dir,"CA/CA_county-list.csv"),',')
    xx <- read_delim(paste0(input_dir,"CA/2018/",
                            "state_g18_sov_data_by_g18_svprec.csv"),',')
    xx <- xx[,c(1,4,19,48,49)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Newsom","Cox")
    xx <- xx[!grepl("_TOT$", xx$AREA),]
    xx <- xx[!grepl("^CNTYTOT$", xx$AREA),]
    xx <- xx[!grepl("^SOVTOT$", xx$AREA),]
    # xx$AREA <- gsub("A[ ]*$","",xx$AREA)
    xx$AREA <- gsub("^[ ]*[0]+","",xx$AREA)
    # xx <- xx %>%
    #     group_by(COUNTY,AREA) %>%
    #     summarize(TOTAL=sum(TOTAL),
    #               Newsom=sum(Newsom),
    #               Cox=sum(Cox))            
    xx$COUNTY <- cc$county_name[xx$COUNTY]
    #xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"CA_2018_Governor.csv"))
    write_delim(xx, paste0(data_dir,"CA_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}
createCA_2018_House <- function(){
    catmsg("##### START createCA_2018_House #####")
    cc <- read_delim(paste0(input_dir,"CA/CA_county-list.csv"),',')
    xx <- read_delim(paste0(input_dir,"CA/2018/",
                            "state_g18_sov_data_by_g18_svprec.csv"),',')
    xx <- xx[,c("COUNTY","SVPREC","TOTVOTE","CNGDEM01","CNGREP01",
                "CNGDEM02","CNGREP02","CNGIND01","CNGGRN01")]
    names(xx) <- c("COUNTY","AREA","TOTAL","Dem","Rep",
                   "Dem2","Rep2","Ind","Grn")
    xx <- xx[!grepl("_TOT$", xx$AREA),]
    xx <- xx[!grepl("^CNTYTOT$", xx$AREA),]
    xx <- xx[!grepl("^SOVTOT$", xx$AREA),]
    # xx$AREA <- gsub("A[ ]*$","",xx$AREA)
    xx$AREA <- gsub("^[ ]*[0]+","",xx$AREA)
    # xx <- xx %>%
    #     group_by(COUNTY,AREA) %>%
    #     summarize(TOTAL=sum(TOTAL),
    #               Dem=sum(Dem),
    #               Rep=sum(Rep),
    #               Dem2=sum(Dem2),
    #               Rep2=sum(Rep2),
    #               Ind=sum(Ind),
    #               Grn=sum(Grn))            
    xx$COUNTY <- cc$county_name[xx$COUNTY]
    #xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","DEM2","REP2","IND","GRN")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"CA_2018_House.csv"))
    write_delim(xx, paste0(data_dir,"CA_2018_House.csv"), append = TRUE, col_names = TRUE)
}
createCA_2020_President <- function(){
    catmsg("##### START createCA_2020_President #####")
    cc <- read_delim(paste0(input_dir,"CA/CA_county-list.csv"),',')
    xx <- read_delim(paste0(input_dir,"CA/2020/",
                            "state_g20_sov_data_by_g20_svprec.csv"),',')
    xx <- xx[,c(1,4,19,42,46,44,43,41,45)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Jorgensen","Hawkins","DeLaFuente","LaRiva")
    xx <- xx[!grepl("_TOT$", xx$AREA),]
    xx <- xx[!grepl("^CNTYTOT$", xx$AREA),]
    xx <- xx[!grepl("^SOVTOT$", xx$AREA),]
    # xx$AREA <- gsub("A[ ]*$","",xx$AREA)
    xx$AREA <- gsub("^[ ]*[0]+","",xx$AREA)
    # xx <- xx %>%
    #     group_by(COUNTY,AREA) %>%
    #     summarize(TOTAL=sum(TOTAL),
    #               Biden=sum(Biden),
    #               Trump=sum(Trump),
    #               Jorgensen=sum(Jorgensen),
    #               Hawkins=sum(Hawkins),
    #               DeLaFuente=sum(DeLaFuente),
    #               LaRiva=sum(LaRiva))            
    xx$COUNTY <- cc$county_name[xx$COUNTY]
    #xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","AIP","PAF")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"CA_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"CA_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createCA_2020_House <- function(){
    catmsg("##### START createCA_2020_House #####")
    cc <- read_delim(paste0(input_dir,"CA/CA_county-list.csv"),',')
    xx <- read_delim(paste0(input_dir,"CA/2020/",
                            "state_g20_sov_data_by_g20_svprec.csv"),',')
    xx <- xx[,c("COUNTY","SVPREC","TOTVOTE",
                "CNGDEM01","CNGREP01","CNGDEM02")]
    names(xx) <- c("COUNTY","AREA","TOTAL","Dem","Rep","Dem2")
    xx <- xx[!grepl("_TOT$", xx$AREA),]
    xx <- xx[!grepl("^CNTYTOT$", xx$AREA),]
    xx <- xx[!grepl("^SOVTOT$", xx$AREA),]
    # xx$AREA <- gsub("A[ ]*$","",xx$AREA)
    xx$AREA <- gsub("^[ ]*[0]+","",xx$AREA)
    # xx <- xx %>%
    #     group_by(COUNTY,AREA) %>%
    #     summarize(TOTAL=sum(TOTAL),
    #               Dem=sum(Dem),
    #               Rep=sum(Rep),
    #               Dem2=sum(Dem2))            
    xx$COUNTY <- cc$county_name[xx$COUNTY]
    #xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","DEM2")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"CA_2020_House.csv"))
    write_delim(xx, paste0(data_dir,"CA_2020_House.csv"), append = TRUE, col_names = TRUE)
}
createCO_2020_President <- function(){
    xx <- read_excel(paste0(input_dir,"CO/2020/2020GEPrecinctLevelResultsPosted.xlsx"),
                     sheet = "Sheet1", skip = 0)
    names(xx) <- c("State","Year","ElectionType","COUNTY","AREA",
                   "Office","Name","Party","Votes","YesVotes","NoVotes")
    office <- "President/Vice President" #UPDATE
    xx <- xx[xx$Office == office,]
    xx <- xx[,c("COUNTY","AREA","Name","Party","Votes")]
    xx$Name <- gsub("De La Fuente","DeLaFuente",xx$Name)
    xx$Name <- gsub("Jacob-Fambro","JacobFambro",xx$Name)
    xx$Name <- gsub("La Riva","LaRiva",xx$Name)
    for (j in 1:NROW(xx)){
        #name1 <- trimws(head(strsplit(xx$Name[j],split="/")[[1]],1)) #Joseph R. Biden / Kamala D. Harris
        #xx$Name[j] <- tail(strsplit(name1,split=" ")[[length(name1)]],1) #use last name
        if (grepl("^Democratic", xx$Party[j])){
            xx$Name[j] <- "DEM_Biden"
        }
        else if (grepl("^Republican", xx$Party[j])){
            xx$Name[j] <- "REP_Trump"
        }
        else if (grepl("^Libertarian", xx$Party[j])){
            xx$Name[j] <- "LIB_Jorgensen"
        }
        else if (grepl("^Green", xx$Party[j])){
            xx$Name[j] <- "GRN_Hawkins"
        }
        else{
            xx$Name[j] <- paste0("IND_Other")
        }
    }
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Name) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Name,Votes)
    xx$TOTAL <- 0
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    if ("LIB" %in% partyxx){
        ilib <- which(partyxx == "LIB")
        ii <- c(ii, ilib)
    }
    if ("GRN" %in% partyxx){
        igrn <- which(partyxx == "GRN")
        ii <- c(ii, igrn)
    }
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep & j != ilib & j != igrn){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"CO_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"CO_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createCO_2020_Registered <- function(){
    xx <- read_excel(paste0(input_dir,"CO/2020/2020GEPrecinctLevelTurnoutPosted.xlsx"),
                     sheet = "Sheet1", skip = 0)
    names(xx) <- c("State","Year","ElectionType","COUNTY","AREA",
                   "Active","Inactive","Voters","Ballots","Turnout")
    xx <- xx[,c("COUNTY","AREA","Voters","Active","Inactive")]
    names(xx) <- c("COUNTY","AREA","TOTAL","DemReg","RepReg")
    xx$COUNTY <- str_to_title(xx$COUNTY)
    xx$DemReg <- xx$TOTAL - 1
    xx$RepReg <- 1
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"CO_2020_Registered.csv"))
    write_delim(xx, paste0(data_dir,"CO_2020_Registered.csv"), append = TRUE, col_names = TRUE)
}
createCO_2018_Governor <- function(){
    xx <- read_excel(paste0(input_dir,"CO/2018/2018GEPrecinctLevelResults.xlsx"),
                     sheet = "Sheet1", skip = 0)
    names(xx) <- c("State","Year","ElectionType","COUNTY","AREA",
                   "Office","Name","Party","Votes","YesVotes","NoVotes")
    office <- "Governor" #UPDATE
    xx <- xx[xx$Office == office,]
    xx <- xx[,c("COUNTY","AREA","Name","Party","Votes")]
    xx$Name <- gsub("De La Fuente","DeLaFuente",xx$Name)
    xx$Name <- gsub("Jacob-Fambro","JacobFambro",xx$Name)
    xx$Name <- gsub("La Riva","LaRiva",xx$Name)
    for (j in 1:NROW(xx)){
        #name1 <- trimws(head(strsplit(xx$Name[j],split="/")[[1]],1)) #Joseph R. Biden / Kamala D. Harris
        #xx$Name[j] <- tail(strsplit(name1,split=" ")[[length(name1)]],1) #use last name
        if (grepl("^Democratic", xx$Party[j])){
            xx$Name[j] <- "DEM_Polis"
        }
        else if (grepl("^Republican", xx$Party[j])){
            xx$Name[j] <- "REP_Stapleton"
        }
        else if (grepl("^Libertarian", xx$Party[j])){
            xx$Name[j] <- "LIB_Helker"
        }
        else if (grepl("^Unity Party", xx$Party[j])){ #Unity Party of Colorado
            xx$Name[j] <- "UPC_Hammons"
        }
        else{
            xx$Name[j] <- paste0("IND_Other")
        }
    }
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Name) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Name,Votes)
    xx$TOTAL <- 0
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    if ("LIB" %in% partyxx){
        ilib <- which(partyxx == "LIB")
        ii <- c(ii, ilib)
    }
    if ("UPC" %in% partyxx){
        iupc <- which(partyxx == "UPC")
        ii <- c(ii, iupc)
    }
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep & j != ilib & j != iupc){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"CO_2018_Governor.csv"))
    write_delim(xx, paste0(data_dir,"CO_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}

createFL_2020_County_Codes <- function(){
    files <- list.files(paste0(input_dir,"FL/2020-general-election-rev/"),
                        "*_PctResults20201103.txt")
    cc <- substr(files,1,3)
    print(cc)
    dd <- data.frame(cc)
    write_delim(dd, paste0(data_dir,"FL_County_Codes.csv"), append = FALSE, col_names = TRUE)
}
createFL_2020_Counties <- function(){
    filenamex <- paste0(data_dir,"FL_2020_President.csv")
    xxparty <- read_delim(filenamex, ' ', col_names = FALSE, n_max = 1)
    xx0 <<- read_delim(filenamex, ' ', skip = 1) #make xx0 available after tryCatch
    xx <- unique(xx0$COUNTY)
    dd <- data.frame(xx)
    names(dd) <- "COUNTY"
    write_delim(dd, paste0(data_dir,"FL_Counties.csv"), append = FALSE, col_names = TRUE)
}
fl_county_codes <- c(
    "ALA","BAK","BAY","BRA","BRE",
    "BRO","CAL","CHA","CIT","CLA",
    "CLL","CLM","DAD","DES","DIX",
    "DUV","ESC","FLA","FRA","GAD",
    "GIL","GLA","GUL","HAM","HAR",
    "HEN","HER","HIG","HIL","HOL",
    "IND","JAC","JEF","LAF","LAK",
    "LEE","LEO","LEV","LIB","MAD",
    "MAN","MON","MRN","MRT","NAS",
    "OKA","OKE","ORA","OSC","PAL",
    "PAS","PIN","POL","PUT","SAN",
    "SAR","SEM","STJ","STL","SUM",
    "SUW","TAY","UNI","VOL","WAK",
    "WAL","WAS")
fl_counties <- c(
    "Alachua","Baker","Bay","Bradford","Brevard",
    "Broward","Calhoun","Charlotte","Citrus","Clay",
    "Collier","Columbia","Miami-Dade","Desoto","Dixie",
    "Duval","Escambia","Flagler","Franklin","Gadsden",
    "Gilchrist","Glades","Gulf","Hamilton","Hardee",
    "Hendry","Hernando","Highlands","Hillsborough","Holmes",
    "Indian River","Jackson","Jefferson","Lafayette","Lake",
    "Lee","Leon","Levy","Liberty","Madison",
    "Manatee","Monroe","Marion","Martin","Nassau",
    "Okaloosa","Okeechobee","Orange","Osceola","Palm Beach",
    "Pasco","Pinellas","Polk","Putnam","Santa Rosa",
    "Sarasota","Seminole","St. Johns","St. Lucie","Sumter",
    "Suwannee","Taylor","Union","Volusia","Wakulla",
    "Walton","Washington")
createFL_2016_President <- function(){
    # input_dir <- "input/"
    # data_dir  <- "data/"
    cc <- fl_county_codes
    xx <- NULL
    for (i in 1:length(cc)){
        dd <- read_delim(paste0(input_dir,"FL/precinctlevelelectionresults2016gen/",
                                cc[i],"_PctResults20161108.txt"), '\t', quote = "",
                         col_names = FALSE, col_types = "ccdccccddddccdccddd")
        #Define names if col_names == FALSE
        names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                       "AreaId","AREA","RegAll","RegRep","RegDem",
                       "RegOth","Contest","DIST","ConCode","Candidate",
                       "Party","RegId","CandNo","Votes")
        #Filter by office (may vary by county)
        if (cc[i] == "SEM"){
            office <- "PRESIDENT OF THE UNITED STATES" #UPDATE
        }
        else{
            office <- "President of the United States" #UPDATE
        }
        dd <- dd[dd$Contest == office,]
        if (NROW(dd) == 0){
            catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
            next
        }
        #Filter out blank lines, if necessary
        #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
        #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
        #Set AREA to match between races
        if (cc[i] %in% c("HEN","HER","JEF","LEV","MON","OKE","PUT","STL")){
            dd$AREA <- sub("^0+", "", dd$AreaId)
        }
        else if (cc[i] %in% c("ALA","DES","DIX","FLA","HAR")){
            dd$AREA <- str_pad(dd$AreaId,2,side = "left",pad = "0")
        }
        else if (cc[i] %in% c("PAS","SEM")){
            dd$AREA <- str_pad(dd$AreaId,3,side = "left",pad = "0")
        }
        else if (cc[i] == "DAD"){
            dd$AREA <- str_pad(paste0(dd$AreaId,"0"),4,side = "left",pad = "0")
        }
        else{
            dd$AREA <- dd$AreaId
        }
        #Set dd to minimally required fields 
        dd <- dd[,c("DIST","COUNTY","AREA","Candidate","Party","Votes")]
        #Standardize candidates if necessary
        dd$Candidate[dd$Candidate == "WriteInVotes"] <- "WriteinVotes"
        #Following are standarizations for Seminole County
        dd$Candidate[dd$Candidate == "Write-in"] <- "WriteinVotes"
        dd$Candidate[dd$Candidate == "Times Blank Voted"] <- "UnderVotes"
        dd$Candidate[dd$Candidate == "Times Over Voted"] <- "OverVotes"
        dd$Candidate[dd$Candidate == "Roque De La Fuente"] <- "Roque DeLaFuente"
        #Combine candidate and party for matching
        for (j in 1:NROW(dd)){
            if (cc[i] == "SEM"){
                dd$Candidate[j] <- tail(strsplit(dd$Candidate[j],split=" ")[[1]],1) #last name
                dd$COUNTY <- "Seminole"
            }
            else{
                dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
                dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
            }
            if (!is.na(dd$Party[j])){
                dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
            }
        }
        dd <- dd[,-5] # delete Party
        # Check for matches first???
        dd <- dd %>%
            group_by(DIST,COUNTY,AREA,Candidate) %>%
            summarize(Votes=sum(Votes))
        dd <- dd %>% spread(Candidate,Votes)
        dd$TOTAL <- 0
        # Compute total???
        # for (j in 4:(NCOL(dd)-1)){
        #     if (!is.na(dd[,j])){
        #         dd$TOTAL <- dd$TOTAL + dd[,j]
        #     }
        # }
        xx <- rbind(xx,dd)
    }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 4:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
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
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2016_President.csv"))
    write_delim(xx, paste0(data_dir,"FL_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createFL_2018_Governor <- function(){
    cc <- fl_county_codes
    xx <- NULL
    for (i in 1:length(cc)){
        dd <- read_delim(paste0(input_dir,"FL/precinctlevelelectionresults2018gen/",
                                cc[i],"_PctResults20181106.txt"), '\t', quote = "",
                         col_names = FALSE, col_types = "ccdccccddddccdccddd")
        names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                       "AreaId","AREA","RegAll","RegRep","RegDem",
                       "RegOth","Contest","DIST","ConCode","Candidate",
                       "Party","RegId","CandNo","Votes")
        office <- "Governor" #UPDATE
        dd <- dd[dd$Contest == office,]
        if (NROW(dd) == 0){
            catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
            next
        }
        #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
        #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
        if (cc[i] %in% c("HEN","HER","OKE","PUT")){
            dd$AREA <- sub("^0+", "", dd$AreaId)
        }
        else if (cc[i] %in% c("ALA","DIX","HAR")){
            dd$AREA <- str_pad(dd$AreaId,2,side = "left",pad = "0")
        }
        else if (cc[i] == "PAS"){
            dd$AREA <- str_pad(dd$AreaId,3,side = "left",pad = "0")
        }
        else if (cc[i] == "DAD"){
            dd$AREA <- str_pad(paste0(dd$AreaId,"0"),4,side = "left",pad = "0")
        }
        else{
            dd$AREA <- dd$AreaId
        }
        dd <- dd[,c("DIST","COUNTY","AREA","Candidate","Party","Votes")]
        for (j in 1:NROW(dd)){
            dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
            dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
            if (!is.na(dd$Party[j])){
                dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
            }
        }
        dd <- dd[,-5] # delete Party
        # check for matches first???
        dd <- dd %>%
            group_by(DIST,COUNTY,AREA,Candidate) %>%
            summarize(Votes=sum(Votes))
        dd <- dd %>% spread(Candidate,Votes)
        dd$TOTAL <- 0
        # for (j in 4:(NCOL(dd)-1)){
        #     dd$TOTAL <- dd$TOTAL + dd[,j]
        # }
        xx <- rbind(xx,dd)
    }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 4:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
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
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2018_Governor.csv"))
    write_delim(xx, paste0(data_dir,"FL_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}
createFL_2018_Senate <- function(){
    cc <- fl_county_codes
    xx <- NULL
    for (i in 1:length(cc)){
        dd <- read_delim(paste0(input_dir,"FL/precinctlevelelectionresults2018gen/",
                                cc[i],"_PctResults20181106.txt"), '\t', quote = "",
                         col_names = FALSE, col_types = "ccdccccddddccdccddd")
        names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                       "AreaId","AREA","RegAll","RegRep","RegDem",
                       "RegOth","Contest","DIST","ConCode","Candidate",
                       "Party","RegId","CandNo","Votes")
        office <- "United States Senator" #UPDATE
        dd <- dd[dd$Contest == office,]
        if (NROW(dd) == 0){
            catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
            next
        }
        #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
        #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
        if (cc[i] %in% c("HEN","HER","OKE","PUT")){
            dd$AREA <- sub("^0+", "", dd$AreaId)
        }
        else if (cc[i] %in% c("ALA","DIX","HAR")){
            dd$AREA <- str_pad(dd$AreaId,2,side = "left",pad = "0")
        }
        else if (cc[i] == "PAS"){
            dd$AREA <- str_pad(dd$AreaId,3,side = "left",pad = "0")
        }
        else if (cc[i] == "DAD"){
            dd$AREA <- str_pad(paste0(dd$AreaId,"0"),4,side = "left",pad = "0")
        }
        else{
            dd$AREA <- dd$AreaId
        }
        dd <- dd[,c("DIST","COUNTY","AREA","Candidate","Party","Votes")]
        for (j in 1:NROW(dd)){
            dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
            dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
            if (!is.na(dd$Party[j])){
                dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
            }
        }
        dd <- dd[,-5] # delete Party
        # check for matches first???
        dd <- dd %>%
            group_by(DIST,COUNTY,AREA,Candidate) %>%
            summarize(Votes=sum(Votes))
        dd <- dd %>% spread(Candidate,Votes)
        dd$TOTAL <- 0
        # for (j in 4:(NCOL(dd)-1)){
        #     dd$TOTAL <- dd$TOTAL + dd[,j]
        # }
        xx <- rbind(xx,dd)
    }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 4:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
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
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"FL_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createFL_2018_House <- function(){
    cc <- c(
        "ALA","BAK","BAY","BRA","BRE","BRO","CAL","CHA","CIT","CLA",
        "CLL","CLM","DAD","DES","DIX","DUV","ESC","FLA","FRA","GAD",
        "GIL","GLA","GUL","HAM","HAR","HEN","HER","HIG","HIL","HOL",
        "IND","JAC","JEF","LAF","LAK","LEE","LEO","LEV","LIB","MAD",
        "MAN","MON","MRN","MRT","NAS","OKA","OKE","ORA","OSC","PAL",
        "PAS","PIN","POL","PUT","SAN","SAR","SEM","STJ","STL","SUM",
        "SUW","TAY","UNI","VOL","WAK","WAL","WAS")
    zz <- NULL
    for (i in 1:length(cc)){
        xx <- read_delim(paste0(input_dir,"FL/precinctlevelelectionresults2018gen/",
                                cc[i],"_PctResults20181106.txt"), '\t', quote = "",
                         col_names = FALSE, col_types = "ccdccccddddccdccddd")
        names(xx) <- c("Code","County","ElectNo","ElectDate","ElectName",
                       "AreaId","AreaLoc","RegAll","RegRep","RegDem",
                       "RegOth","Contest","Dist","ConCode","Candidate",
                       "Party","RegId","CandNo","Votes")
        xx <- xx[xx$Contest == "Representative in Congress",]
        if (NROW(xx) == 0){
            catmsg(paste0("====> WARNING: ",cc[i]," County had no Representative in Congress"))
            next
        }
        xx <- xx[,c("Dist","County","AreaId","Candidate","Party","Votes")]
        for (j in 1:NROW(xx)){
            if (!is.na(xx$Party[j])){
                xx$Candidate[j] <- xx$Party[j]
            }
            else{
                xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
            }
        }
        xx <- xx[,-5] # delete Party
        xx <- xx %>%
            group_by(Dist,County,AreaId,Candidate) %>%
            summarize(Votes=sum(Votes))
        xx <- xx %>% spread(Candidate,Votes)
        xx$TOTAL <- 0
        yy <- xx[,c("Dist","County","AreaId","TOTAL")]
        yy$DEM <- 0
        yy$REP <- 0
        yy$NPA <- 0
        yy$WriteinVotes <- 0
        yy$OverVotes    <- 0
        yy$UnderVotes   <- 0
        if ("DEM" %in% names(xx)){
            yy$DEM <- xx$DEM
            yy$DEM[is.na(yy$DEM)] <- 0
        }
        if ("REP" %in% names(xx)){
            yy$REP <- xx$REP
            yy$REP[is.na(yy$REP)] <- 0
        }
        if ("NPA" %in% names(xx)){
            yy$NPA <- xx$NPA
            yy$NPA[is.na(yy$NPA)] <- 0
        }
        if ("WriteinVotes" %in% names(xx)){
            yy$WriteinVotes <- xx$WriteinVotes
            yy$WriteinVotes[is.na(yy$WriteinVotes)] <- 0
        }
        if ("OverVotes" %in% names(xx)){
            yy$OverVotes <- xx$OverVotes
            yy$OverVotes[is.na(yy$OverVotes)] <- 0
        }
        if ("UnderVotes" %in% names(xx)){
            yy$UnderVotes <- xx$UnderVotes
            yy$UnderVotes[is.na(yy$UnderVotes)] <- 0
        }
        yy$TOTAL <- yy$DEM + yy$REP + yy$NPA + yy$WriteinVotes
        zz <- rbind(zz,yy)
    }
    names(zz) <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","NPA","WRITEIN","OVERVOTES","UNDERVOTES")
    zz$DIST <- gsub("District ","",zz$DIST)
    zz$DIST <- gsub("^ ","",zz$DIST)
    write(paste(names(zz), collapse = " "), paste0(data_dir,"FL_2018_House.csv"))
    write_delim(zz, paste0(data_dir,"FL_2018_House.csv"), append = TRUE, col_names = TRUE)
}
createFL_2020_President <- function(){
    cc <- fl_county_codes
    xx <- NULL
    for (i in 1:length(cc)){
        dd <- read_delim(paste0(input_dir,"FL/2020-general-election-rev/",
                                cc[i],"_PctResults20201103.txt"), '\t', quote = "",
                         col_names = FALSE, col_types = "ccdccccddddccdccddd")
        names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                       "AreaId","AREA","RegAll","RegRep","RegDem",
                       "RegOth","Contest","DIST","ConCode","Candidate",
                       "Party","RegId","CandNo","Votes")
        office <- "President of the United States" #UPDATE
        dd <- dd[dd$Contest == office,]
        if (NROW(dd) == 0){
            catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
            next
        }
        #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
        #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
        dd$AREA <- dd$AreaId
        dd <- dd[,c("DIST","COUNTY","AREA","Candidate","Party","Votes")]
        for (j in 1:NROW(dd)){
            dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
            dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
            if (!is.na(dd$Party[j])){
                dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
            }
        }
        dd <- dd[,-5] # delete Party
        # check for matches first???
        dd <- dd %>%
            group_by(DIST,COUNTY,AREA,Candidate) %>%
            summarize(Votes=sum(Votes))
        dd <- dd %>% spread(Candidate,Votes)
        dd$TOTAL <- 0
        # for (j in 4:(NCOL(dd)-1)){
        #     dd$TOTAL <- dd$TOTAL + dd[,j]
        # }
        xx <- rbind(xx,dd)
    }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 4:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
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
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"FL_2020_President.csv"), append = TRUE, col_names = TRUE)
}
# Add turnout to file for Election Forensics
createFL_2020_President0EF <- function(){
    cc <- fl_county_codes
    xx <- NULL
    for (i in 1:length(cc)){
        dd <- read_delim(paste0(input_dir,"FL/2020-general-election-rev/",
                                cc[i],"_PctResults20201103.txt"), '\t', quote = "",
                         col_names = FALSE, col_types = "ccdccccddddccdccddd")
        names(dd) <- c("Code","COUNTY","ElectNo","ElectDate","ElectName",
                       "AreaId","AREA","RegAll","RegRep","RegDem",
                       "RegOth","Contest","DIST","ConCode","Candidate",
                       "Party","RegId","CandNo","Votes")
        office <- "President of the United States" #UPDATE
        dd <- dd[dd$Contest == office,]
        if (NROW(dd) == 0){
            catmsg(paste0("====> WARNING: ",cc[i]," COUNTY had no ",office))
            next
        }
        #dd$AREA[is.na(dd$AREA)] <- dd$AreaId[is.na(dd$AREA)]
        #dd$AREA[dd$AREA == ""] <- dd$AreaId[dd$AREA == ""]
        dd$AREA <- dd$AreaId
        dd <- dd[,c("DIST","COUNTY","AREA","RegAll","Candidate","Party","Votes")] #EF
        for (j in 1:NROW(dd)){
            dd$Candidate[j] <- head(strsplit(dd$Candidate[j],split="/")[[1]],1) #Biden / Harris
            dd$Candidate[j] <- gsub(" ","",trimws(dd$Candidate[j]))
            if (!is.na(dd$Party[j])){
                dd$Candidate[j] <- paste0(dd$Candidate[j],"_",dd$Party[j])
            }
        }
        dd <- dd[,-6] # delete Party #EF
        # check for matches first???
        dd <- dd %>%
            group_by(DIST,COUNTY,AREA,RegAll,Candidate) %>% #EF
            summarize(Votes=sum(Votes))
        dd <- dd %>% spread(Candidate,Votes)
        dd$TOTAL <- 0
        # for (j in 4:(NCOL(dd)-1)){
        #     dd$TOTAL <- dd$TOTAL + dd[,j]
        # }
        xx <- rbind(xx,dd)
    }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 5:(NCOL(xx)-1)){ #EF
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,3,4,NCOL(xx)) #EF
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
    nc <- NCOL(xx)
    for (j in 5:(NCOL(xx)-1)){ #EF
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        #EF - uncomment next 3 lines
        if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
            xx[,nc] <- xx[,nc] + xx[,j]
            # for (k in 1:NROW(xx)){
            #     xx[k,nc] <- xx[k,nc] + xx[k,j]
            # }
        }
    }
    zxx <<- xx
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2020_President0EF.csv"))
    write_delim(xx, paste0(data_dir,"FL_2020_President0EF.csv"), append = TRUE, col_names = TRUE)
}
createFL_2020_House <- function(){
    cc <- c(
        "ALA","BAK","BAY","BRA","BRE","BRO","CAL","CHA","CIT","CLA",
        "CLL","CLM","DAD","DES","DIX","DUV","ESC","FLA","FRA","GAD",
        "GIL","GLA","GUL","HAM","HAR","HEN","HER","HIG","HIL","HOL",
        "IND","JAC","JEF","LAF","LAK","LEE","LEO","LEV","LIB","MAD",
        "MAN","MON","MRN","MRT","NAS","OKA","OKE","ORA","OSC","PAL",
        "PAS","PIN","POL","PUT","SAN","SAR","SEM","STJ","STL","SUM",
        "SUW","TAY","UNI","VOL","WAK","WAL","WAS")
    zz <- NULL
    for (i in 1:length(cc)){
        xx <- read_delim(paste0(input_dir,"FL/2020-general-election-rev/",
                                cc[i],"_PctResults20201103.txt"), '\t', quote = "",
                         col_names = FALSE, col_types = "ccdccccddddccdccddd")
        names(xx) <- c("Code","County","ElectNo","ElectDate","ElectName",
                       "AreaId","AreaLoc","RegAll","RegRep","RegDem",
                       "RegOth","Contest","Dist","ConCode","Candidate",
                       "Party","RegId","CandNo","Votes")
        xx <- xx[xx$Contest == "Representative in Congress",]
        if (NROW(xx) == 0){
            catmsg(paste0("====> WARNING: ",cc[i]," County had no Representative in Congress"))
            next
        }
        xx <- xx[,c("Dist","County","AreaId","Candidate","Party","Votes")]
        for (j in 1:NROW(xx)){
            if (!is.na(xx$Party[j])){
                xx$Candidate[j] <- xx$Party[j]
            }
            else{
                xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
            }
        }
        xx <- xx[,-5] # delete Party
        xx <- xx %>%
            group_by(Dist,County,AreaId,Candidate) %>%
            summarize(Votes=sum(Votes))
        xx <- xx %>% spread(Candidate,Votes)
        xx$TOTAL <- 0
        yy <- xx[,c("Dist","County","AreaId","TOTAL")]
        yy$DEM <- 0
        yy$REP <- 0
        yy$NPA <- 0
        yy$WriteinVotes <- 0
        yy$OverVotes    <- 0
        yy$UnderVotes   <- 0
        if ("DEM" %in% names(xx)){
            yy$DEM <- xx$DEM
            yy$DEM[is.na(yy$DEM)] <- 0
        }
        if ("REP" %in% names(xx)){
            yy$REP <- xx$REP
            yy$REP[is.na(yy$REP)] <- 0
        }
        if ("NPA" %in% names(xx)){
            yy$NPA <- xx$NPA
            yy$NPA[is.na(yy$NPA)] <- 0
        }
        if ("WriteinVotes" %in% names(xx)){
            yy$WriteinVotes <- xx$WriteinVotes
            yy$WriteinVotes[is.na(yy$WriteinVotes)] <- 0
        }
        if ("OverVotes" %in% names(xx)){
            yy$OverVotes <- xx$OverVotes
            yy$OverVotes[is.na(yy$OverVotes)] <- 0
        }
        if ("UnderVotes" %in% names(xx)){
            yy$UnderVotes <- xx$UnderVotes
            yy$UnderVotes[is.na(yy$UnderVotes)] <- 0
        }
        yy$TOTAL <- yy$DEM + yy$REP + yy$NPA + yy$WriteinVotes
        zz <- rbind(zz,yy)
    }
    names(zz) <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","NPA","WRITEIN","OVERVOTES","UNDERVOTES")
    zz$DIST <- gsub("District ","",zz$DIST)
    zz$DIST <- gsub("^ ","",zz$DIST)
    write(paste(names(zz), collapse = " "), paste0(data_dir,"FL_2020_House.csv"))
    write_delim(zz, paste0(data_dir,"FL_2020_House.csv"), append = TRUE, col_names = TRUE)
}
createFL_2020_House_CD27 <- function(){
    catmsg("##### START createFL_2020_House_CD27 #####")
    xx <- read_delim(paste0(input_dir,"FL/2020-general-election-rev/",
                            "DAD_PctResults20201103.txt"), '\t', quote = "",
                     col_names = FALSE, col_types = "ccdccccddddccdccddd")
    names(xx) <- c("Code","County","ElectNo","ElectDate","ElectName",
                   "AreaId","AreaLoc","RegAll","RegRep","RegDem",
                   "RegOth","Contest","Dist","ConCode","Candidate",
                   "Party","RegId","CandNo","Votes")
    xx027 <<- xx
    xx <- xx[xx$Contest == "Representative in Congress",]
    xx <- xx[xx$Dist == " District 27",]
    xx <- xx[,c("County","AreaId","Candidate","Votes")]
    for (i in 1:NROW(xx)){
        xx$Candidate[i] <- tail(strsplit(xx$Candidate[i],split=" ")[[1]],1) #use last name
    }
    xx27 <<- xx
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    xx <- xx[,c(1,2,8,5,4,7)]
    names(xx)[1:3] <- c("COUNTY","AREA","TOTAL")
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2020_House_CD27.csv"))
    write_delim(xx, paste0(data_dir,"FL_2020_House_CD27.csv"), append = TRUE, col_names = TRUE)
}
createFL_2016_Registered <- function(){
    txt <- "text"
    num <- "numeric"
    xx <- read_excel(paste0(input_dir,"FL/bookclosing/2016general_precinct.xlsx"),
                     sheet = "RegistrationByPrecinct", skip = 8, col_names = TRUE,
                     col_types = c(txt,txt,txt,num,num,num,num))
    xxr <<- xx #DEBUG-RM
    #names(xx) <- c("BookClosingDate","CountyCode","PrecinctNum","REP","DEM","OTH","TOTAL")
    names(xx) <- c("BookClosingDate","CODE","AreaId","Republican","Democrat","Other","TOTAL")
    xx$COUNTY <- xx$CODE
    for (i in 1:(NROW(xx)-1)){
        if (xx$COUNTY[i] != "Total"){
            xx$COUNTY[i] <- fl_counties[which(fl_county_codes == xx$CODE[i])]
        }
        if (xx$CODE[i] %in% c("HEN","HER","OKE","PUT")){
            xx$AREA[i] <- sub("^0+", "", xx$AreaId[i])
        }
        else if (xx$CODE[i] %in% c("ALA","DIX","HAR")){
            xx$AREA[i] <- str_pad(xx$AreaId[i],2,side = "left",pad = "0")
        }
        else if (xx$CODE[i] == "PAS"){
            xx$AREA[i] <- str_pad(xx$AreaId[i],3,side = "left",pad = "0")
        }
        else if (xx$CODE[i] == "DAD"){
            xx$AREA[i] <- str_pad(paste0(xx$AreaId[i],"0"),4,side = "left",pad = "0")
        }
        else{
            xx$AREA[i] <- xx$AreaId[i]
        }
    }
    #xx$COUNTY <- fl_counties[which(fl_county_codes == xx$CODE)]
    xx <- xx[,c("COUNTY","AREA","TOTAL","Democrat","Republican","Other")]
    xxr2 <<- xx #DEBUG-RM
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2016_Registered.csv"))
    write_delim(xx, paste0(data_dir,"FL_2016_Registered.csv"), append = TRUE, col_names = TRUE)
}
createFL_2018_Registered <- function(){
    txt <- "text"
    num <- "numeric"
    xx <- read_excel(paste0(input_dir,"FL/precinctlevelelectionresults2018gen/2018gen_precinct.xlsx"),
                     sheet = "2018gen_precinct", skip = 8, col_names = TRUE,
                     col_types = c(txt,txt,num,num,num,num))
    xxr <<- xx #DEBUG-RM
    names(xx) <- c("CODE","AreaId","Republican","Democrat","Other","TOTAL")
    xx$COUNTY <- xx$CODE
    for (i in 1:(NROW(xx)-1)){
        if (xx$COUNTY[i] != "Total"){
            xx$COUNTY[i] <- fl_counties[which(fl_county_codes == xx$CODE[i])]
        }
        if (xx$CODE[i] %in% c("HEN","HER","OKE","PUT")){
            xx$AREA[i] <- sub("^0+", "", xx$AreaId[i])
        }
        else if (xx$CODE[i] %in% c("ALA","DIX","HAR")){
            xx$AREA[i] <- str_pad(xx$AreaId[i],2,side = "left",pad = "0")
        }
        else if (xx$CODE[i] == "PAS"){
            xx$AREA[i] <- str_pad(xx$AreaId[i],3,side = "left",pad = "0")
        }
        else if (xx$CODE[i] == "DAD"){
            xx$AREA[i] <- str_pad(paste0(xx$AreaId[i],"0"),4,side = "left",pad = "0")
        }
        else{
            xx$AREA[i] <- xx$AreaId[i]
        }
    }
    #xx$COUNTY <- fl_counties[which(fl_county_codes == xx$CODE)]
    xx <- xx[,c("COUNTY","AREA","TOTAL","Democrat","Republican","Other")]
    xxr2 <<- xx #DEBUG-RM
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2018_Registered.csv"))
    write_delim(xx, paste0(data_dir,"FL_2018_Registered.csv"), append = TRUE, col_names = TRUE)
}
createFL_2020_Registered <- function(){
    txt <- "text"
    num <- "numeric"
    xx <- read_excel(paste0(input_dir,"FL/2020-general-election-rev/4-2020-gen-by-precinct.xlsx"),
                     sheet = "RegistrationByPrecinct", skip = 8, col_names = TRUE,
                     col_types = c(txt,txt,num,num,num,num))
    xxr <<- xx #DEBUG-RM
    names(xx) <- c("CODE","AreaId","Republican","Democrat","Other","TOTAL")
    xx$COUNTY <- xx$CODE
    for (i in 1:(NROW(xx)-1)){
        if (xx$COUNTY[i] != "Total"){
            xx$COUNTY[i] <- fl_counties[which(fl_county_codes == xx$CODE[i])]
        }
        if (xx$CODE[i] %in% c("HEN","HER","OKE","PUT")){
            xx$AREA[i] <- sub("^0+", "", xx$AreaId[i])
        }
        else if (xx$CODE[i] %in% c("ALA","DIX","HAR")){
            xx$AREA[i] <- str_pad(xx$AreaId[i],2,side = "left",pad = "0")
        }
        else if (xx$CODE[i] == "PAS"){
            xx$AREA[i] <- str_pad(xx$AreaId[i],3,side = "left",pad = "0")
        }
        else if (xx$CODE[i] == "DAD"){
            xx$AREA[i] <- str_pad(paste0(xx$AreaId[i],"0"),4,side = "left",pad = "0")
        }
        else{
            xx$AREA[i] <- xx$AreaId[i]
        }
    }
    #xx$COUNTY <- fl_counties[which(fl_county_codes == xx$CODE)]
    xx <- xx[,c("COUNTY","AREA","TOTAL","Democrat","Republican","Other")]
    xxr2 <<- xx #DEBUG-RM
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"FL_2020_Registered.csv"))
    write_delim(xx, paste0(data_dir,"FL_2020_Registered.csv"), append = TRUE, col_names = TRUE)
}
createIA_2020_Counties <- function(){
    #input_dir <- "input/"
    files <- list.files(paste0(input_dir,"IA/2020/"),"*.xlsx")
    cc <- gsub(".xlsx","",files)
    print(cc)
    dd <- data.frame(cc)
    write_delim(dd, paste0(data_dir,"IA_Counties.csv"), append = FALSE, col_names = TRUE)
}
createIA_2016_President <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zzcc <<- cc #DEBUG-RM
    zz <- NULL
    for (i in 1:length(cc)){
        #for (i in 1:1){
        xx <- read_excel(paste0(input_dir,"IA/2016/",cc[i],".xlsx"), sheet = "Sheet1", skip = 0)
        xx <- xx[xx$RaceTitle == "President and Vice President",]
        candidates <- xx$CandidateName
        xx <- xx[,which(grepl(" Total$",names(xx)))]
        xx <- xx[,-NCOL(xx)] #delete Grand Total
        zzxx <<- xx #DEBUG-RM
        areas <- names(xx)
        yy <- data.frame(t(xx))
        for (j in 1:NCOL(yy)){
            namelist <- unlist(strsplit(candidates[j]," "))
            names(yy)[j] <- trimws(namelist[length(namelist)])
        }
        yy$COUNTY <- cc[i]
        yy$AREA <- areas
        zzyy <<- yy #DEBUG-RM
        for (j in (1:(NROW(yy)-1))){
            yy$AREA[j] <- gsub(paste0("^",cc[i],"-"),"",yy$AREA[j])
            yy$AREA[j] <- gsub(" Total$","",yy$AREA[j])
        }
        #yy$TOTAL <- unlist(xx[,NCOL(xx)])
        yy$TOTAL <- 0
        idem <- which(names(yy) == "Kaine") #Clinton DEM
        irep <- which(names(yy) == "Pence") #Trump REP
        ind1 <- which(names(yy) == "Bradley") #Castle CON
        ind2 <- which(names(yy) == "Baraka") #Stein GRN
        ind3 <- which(names(yy) == "Elworth") #Vacek LMN
        ind4 <- which(names(yy) == "Weld") #Johnson LIB
        ind5 <- which(names(yy) == "Stolba") #Kahn NIP
        ind6 <- which(names(yy) == "Steinberg") #DeLaFuente NBP1
        ind7 <- which(names(yy) == "Johnson") #McMullin NBP2
        ind8 <- which(names(yy) == "Banks") #LaRiva PSL
        iwri <- which(names(yy) == "Write-in") #Writein WRI
        yy <- yy[,c(NCOL(yy)-2,NCOL(yy)-1,NCOL(yy),idem,irep,ind1,ind2,ind3,ind4,ind5,ind6,ind7,ind8,iwri)]
        zzyy1 <<- yy #DEBUG-RM
        names(yy) <- c("COUNTY","AREA","TOTAL","Clinton","Trump",
                       "Castle","Stein","Vacek","Johnson","Kahn",
                       "DeLaFuente","McMullin","LaRiva","Writein")
        # yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
        # zz <- yy
        # print(zz)
        # zzzz <<- zz #DEBUG-RM
    }
    partyzz <- names(zz)
    partyzz[4:5] <- c("DEM","REP")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2016_President.csv"))
    write_delim(zz, paste0(data_dir,"IA_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createIA_2018_Governor <- function(){
    #input_dir <- "input/"
    #data_dir  <- "data/"
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        #for (i in 1:1){
        catmsg(paste0("START read_excel(",cc[i],")"))
        dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "3", skip = 2)
        yy <- data.frame(xx[,1])
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        # idem <- which(names(yy) == "Hart") #Hubbell
        # irep <- which(names(yy) == "Gregg") #Reynolds
        # ilib <- which(names(yy) == "Gentry") #Porter
        # ind1 <- which(names(yy) == "Blaskovich") #Siegwarth
        # iwri <- which(names(yy) == "Write-in")
        idem <- which(names(yy) %in% c("Hart", "Hubbell"))
        irep <- which(names(yy) %in% c("Gregg", "Reynolds"))
        ilib <- which(names(yy) %in% c("Gentry", "Porter"))
        ind1 <- which(names(yy) %in% c("Blaskovich", "Siegwarth"))
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,ind1,iwri)]
        names(yy) <- c("COUNTY","AREA","TOTAL","Hubbell","Reynolds",
                       "Porter","Siegwarth","Writein")
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:7] <- c("DEM","REP","LIB","IND")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_Governor.csv"))
    write_delim(zz, paste0(data_dir,"IA_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}
createIA_2018_House_CD1 <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        catmsg(paste0("START read_excel(",cc[i],")"))
        aa <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 0, n_max = 0) # read names
        if (!grepl("^United States Representative District 1", names(aa))){
            catmsg(paste0("SKIP ",names(aa)))
            next
        }
        dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 2)
        yy <- data.frame(xx[,1])
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Finkenauer")
        irep <- which(names(yy) == "Blum")
        ilib <- which(names(yy) == "Hageman")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,iwri)]
        names(yy) <- c("COUNTY","AREA","TOTAL","Finkenauer","Blum",
                       "Hageman","Writein")
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:6] <- c("DEM","REP","LIB")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_House_CD1.csv"))
    write_delim(zz, paste0(data_dir,"IA_2018_House_CD1.csv"), append = TRUE, col_names = TRUE)
}
createIA_2020_House_CD1 <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        catmsg(paste0("START read_excel(",cc[i],")"))
        aa <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 0, n_max = 0) # read names
        if (!grepl("^United States Representative District 1", names(aa))){
            catmsg(paste0("SKIP ",names(aa)))
            next
        }
        dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 2)
        yy <- data.frame(xx[,1])
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Finkenauer")
        irep <- which(names(yy) == "Hinson")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,iwri)]
        names(yy) <- c("COUNTY","AREA","TOTAL","Finkenauer","Hinson","Writein")
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:5] <- c("DEM","REP")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_House_CD1.csv"))
    write_delim(zz, paste0(data_dir,"IA_2020_House_CD1.csv"), append = TRUE, col_names = TRUE)
}
createIA_2018_House_CD2 <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        catmsg(paste0("START read_excel(",cc[i],")"))
        #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
        aa <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 0, n_max = 0) #UPDATE YEAR & SHEET
        if (!grepl("^United States Representative District 2", names(aa))){ #UPDATE CD
            catmsg(paste0("SKIP ",names(aa)))
            next
        }
        dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 1, n_max = 0) #UPDATE YEAR & SHEET
        xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 2) #UPDATE YEAR & SHEET
        yy <- data.frame(xx[,1])
        gxx <<- xx #DEBUG-RM
        gyy <<- yy #DEBUG-RM
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Loebsack") #UPDATE NAMES
        irep <- which(names(yy) == "Peters")
        ilib <- which(names(yy) == "Strauss")
        ind1 <- which(names(yy) == "Clark")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,ind1,iwri)] #UPDATE PARTIES
        names(yy) <- c("COUNTY","AREA","TOTAL","Loebsack","Peters",
                       "Strauss","Clark","Writein") #UPDATE NAMES (change - to _)
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:6] <- c("DEM","REP","LIB") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_House_CD2.csv")) #UPDATE FILENAME
    write_delim(zz, paste0(data_dir,"IA_2018_House_CD2.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
}
createIA_2020_House_CD2 <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        catmsg(paste0("START read_excel(",cc[i],")"))
        #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
        aa <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 0, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
        if (!grepl("^United States Representative District 2", names(aa))){ #UPDATE CD
            catmsg(paste0("SKIP ",names(aa)))
            next
        }
        dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 1, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
        xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 2) #UPDATE YEAR, SHEET & EXTENSION
        yy <- data.frame(xx[,1])
        gxx <<- xx #DEBUG-RM
        gyy <<- yy #DEBUG-RM
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Hart") #UPDATE NAMES
        irep <- which(names(yy) == "Miller-Meeks")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,iwri)] #UPDATE PARTIES
        names(yy) <- c("COUNTY","AREA","TOTAL","Hart","Miller_Meeks",
                       "Writein") #UPDATE NAMES (change - to _)
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:5] <- c("DEM","REP") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_House_CD2.csv")) #UPDATE FILENAME
    write_delim(zz, paste0(data_dir,"IA_2020_House_CD2.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
}
createIA_2018_House_CD3 <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        catmsg(paste0("START read_excel(",cc[i],")"))
        #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
        aa <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 0, n_max = 0) #UPDATE YEAR & SHEET
        if (!grepl("^United States Representative District 3", names(aa))){ #UPDATE CD
            catmsg(paste0("SKIP ",names(aa)))
            next
        }
        dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 1, n_max = 0) #UPDATE YEAR & SHEET
        xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 2) #UPDATE YEAR & SHEET
        yy <- data.frame(xx[,1])
        gxx <<- xx #DEBUG-RM
        gyy <<- yy #DEBUG-RM
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Axne") #UPDATE NAMES
        irep <- which(names(yy) == "Young")
        ilib <- which(names(yy) == "Holder")
        ind1 <- which(names(yy) == "Knupp")
        ind2 <- which(names(yy) == "Jr.")
        ind3 <- which(names(yy) == "Grandanette")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,ind1,ind2,ind3,iwri)] #UPDATE PARTIES
        names(yy) <- c("COUNTY","AREA","TOTAL","Axne","Young",
                       "Holder","Knupp","Elworth_Jr","Grandanette","Writein") #UPDATE NAMES (change - to _)
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:6] <- c("DEM","REP","LIB") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_House_CD3.csv")) #UPDATE FILENAME
    write_delim(zz, paste0(data_dir,"IA_2018_House_CD3.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
}
createIA_2020_House_CD3 <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        catmsg(paste0("START read_excel(",cc[i],")"))
        #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
        aa <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 0, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
        if (!grepl("^United States Representative District 3", names(aa))){ #UPDATE CD
            catmsg(paste0("SKIP ",names(aa)))
            next
        }
        dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 1, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
        xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 2) #UPDATE YEAR, SHEET & EXTENSION
        yy <- data.frame(xx[,1])
        gxx <<- xx #DEBUG-RM
        gyy <<- yy #DEBUG-RM
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Axne") #UPDATE NAMES
        irep <- which(names(yy) == "Young")
        ilib <- which(names(yy) == "Holder")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,iwri)] #UPDATE PARTIES
        names(yy) <- c("COUNTY","AREA","TOTAL","Axne","Young",
                       "Holder","Writein") #UPDATE NAMES (change - to _)
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:6] <- c("DEM","REP","LIB") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_House_CD3.csv")) #UPDATE FILENAME
    write_delim(zz, paste0(data_dir,"IA_2020_House_CD3.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
}
createIA_2018_House_CD4 <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        catmsg(paste0("START read_excel(",cc[i],")"))
        #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
        aa <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 0, n_max = 0) #UPDATE YEAR & SHEET
        if (!grepl("^United States Representative District 4", names(aa))){ #UPDATE CD
            catmsg(paste0("SKIP ",names(aa)))
            next
        }
        dd <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 1, n_max = 0) #UPDATE YEAR & SHEET
        xx <- read_excel(paste0(input_dir,"IA/2018/",cc[i],".xls.xlsx"), sheet = "2", skip = 2) #UPDATE YEAR & SHEET
        yy <- data.frame(xx[,1])
        gxx <<- xx #DEBUG-RM
        gyy <<- yy #DEBUG-RM
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Scholten") #UPDATE NAMES
        irep <- which(names(yy) == "King")
        ilib <- which(names(yy) == "Aldrich")
        ind1 <- which(names(yy) == "Peterson")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,ind1,iwri)] #UPDATE PARTIES
        names(yy) <- c("COUNTY","AREA","TOTAL","Scholten","King",
                       "Aldrich","Peterson","Writein") #UPDATE NAMES (change - to _)
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:6] <- c("DEM","REP","LIB") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2018_House_CD4.csv")) #UPDATE FILENAME
    write_delim(zz, paste0(data_dir,"IA_2018_House_CD4.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
}
createIA_2020_House_CD4 <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        catmsg(paste0("START read_excel(",cc[i],")"))
        #NEED TO VERIFY SHEET FOR ALL COUNTIES!!!
        aa <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 0, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
        if (!grepl("^United States Representative District 4", names(aa))){ #UPDATE CD
            catmsg(paste0("SKIP ",names(aa)))
            next
        }
        dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 1, n_max = 0) #UPDATE YEAR, SHEET & EXTENSION
        xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "4", skip = 2) #UPDATE YEAR, SHEET & EXTENSION
        yy <- data.frame(xx[,1])
        gxx <<- xx #DEBUG-RM
        gyy <<- yy #DEBUG-RM
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Scholten") #UPDATE NAMES
        irep <- which(names(yy) == "Feenstra")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,iwri)] #UPDATE PARTIES
        names(yy) <- c("COUNTY","AREA","TOTAL","Scholten","Feenstra",
                       "Writein") #UPDATE NAMES (change - to _)
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:5] <- c("DEM","REP") #UPDATE PARTIES IF NEEDED (only DEM, REP critical)
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_House_CD4.csv")) #UPDATE FILENAME
    write_delim(zz, paste0(data_dir,"IA_2020_House_CD4.csv"), append = TRUE, col_names = TRUE) #UPDATE FILENAME
}
createIA_2020_President <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        #for (i in 1:1){
        dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "2", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "2", skip = 2)
        yy <- data.frame(xx[,1])
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Harris") #Biden
        irep <- which(names(yy) == "Pence") #Trump
        ind1 <- which(names(yy) == "Richardson") #DeLaFuente
        ind2 <- which(names(yy) == "Mohr") #Blankenship
        ind3 <- which(names(yy) == "Chandler") #King
        ind4 <- which(names(yy) == "Walker") #Hawkins
        ind5 <- which(names(yy) == "Cohen") #Jorgensen
        ind6 <- which(names(yy) == "Ballard") #Pierce
        ind7 <- which(names(yy) == "Tidball") #West
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ind1,ind2,ind3,ind4,ind5,ind6,ind7,iwri)]
        names(yy) <- c("COUNTY","AREA","TOTAL","Biden","Trump",
                       "DeLaFuente","Blankenship","King","Hawkins","Jorgensen",
                       "Pierce","West","Writein")
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:5] <- c("DEM","REP")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_President.csv"))
    write_delim(zz, paste0(data_dir,"IA_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createIA_2020_Senate <- function(){
    cc <- unlist(read_delim(paste0(data_dir,"IA_Counties.csv")," "))
    zz <- NULL
    #for (i in 1:1){
    for (i in 1:length(cc)){
        dd <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"IA/2020/",cc[i],".xlsx"), sheet = "3", skip = 2)
        yy <- data.frame(xx[,1])
        yy$COUNTY <- cc[i]
        k <- 3
        nn <- names(dd)[seq(3,length(dd),3)]
        for (j in seq(5,NCOL(xx),3)){
            yy[,k] <- xx[,j]
            nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- nn[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "Greenfield")
        irep <- which(names(yy) == "Ernst")
        ind1 <- which(names(yy) == "Herzog")
        iwri <- which(names(yy) == "Write-in")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ind1,iwri)]
        names(yy)[2] <- "AREA"
        names(yy)[7] <- "Writein"
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:5] <- c("DEM","REP")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"IA_2020_Senate.csv"))
    write_delim(zz, paste0(data_dir,"IA_2020_Senate.csv"), append = TRUE, col_names = TRUE)
}
me_counties <- c(
    "Androscoggin","Aroostook","Cumberland","Franklin","Hancock",
    "Kennebec","Knox","Lincoln","Oxford","Penobscot",
    "Piscataquis","Sagadahoc","Somerset","Waldo","Washington",
    "York","STATE UOCAVA")
me_cnts <- c(
    "AND","ARO","CUM","FRA","HAN",
    "KEN","KNO","LIN","OXF","PEN",
    "PIS","SAG","SOM","WAL","WAS",
    "YOR","STATE")
createME_2014_Senate <- function(){
    xx <- read_excel(paste0(input_dir,"ME/2014/ussenategen.xlsx"), sheet = "Sheet1", skip = 2)
    names(xx) <- c("AREA","Bellows","Collins","Others","Blank","TOTAL")
    xx$COUNTY <- ""
    xx <- xx[,c("COUNTY","AREA","TOTAL","Bellows","Collins","Others")] # delete Blank
    j <- 1
    xx1 <<- xx #DEBUG-RM
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- me_cnts[j]
        if (grepl(" County Totals",xx$AREA[i])){
            j <- j+1
        }
    }
    xx2 <<- xx #DEBUG-RM
    xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
    xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
    xx <- xx[!grepl(" Totals", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
    #xx <- xx[!is.na(xx$COUNTY),]
    namesxx <- names(xx)
    namesxx[4:5] <- c("DEM","REP")
    xx3 <<- xx #DEBUG-RM
    write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2014_Senate.csv"))
    write_delim(xx, paste0(data_dir,"ME_2014_Senate.csv"), append = TRUE, col_names = TRUE)
}
createME_2016_President <- function(){
    xx <- read_excel(paste0(input_dir,"ME/2016/president.xlsx"), sheet = "Sheet1", skip = 1)
    names(xx) <- c("COUNTY","AREA","Clinton","Johnson","Stein","Trump","Castle","Fox","Kotlikoff","McMullin","BLANK","TBC")
    xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
    xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
    xx <- xx[!grepl(" Total", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
    xx <- xx[,c(1,2,12,3,6,4,5,7,8,9,10)] # delete Blank
    xx <- xx[!is.na(xx$COUNTY),]
    names(xx)[3] <- "TOTAL"
    namesxx <- names(xx)
    namesxx[4:5] <- c("DEM","REP")
    write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2016_President.csv"))
    write_delim(xx, paste0(data_dir,"ME_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createME_2018_Governor <- function(){
    xx <- read_excel(paste0(input_dir,"ME/2018/governor11-6-18.xlsx"), sheet = "Gov", skip = 3)
    names(xx) <- c("COUNTY","AREA","Hayes","Mills","Moody","Others","Blank","TBC")
    xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
    xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
    xx <- xx[!grepl(" Totals", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
    xx <- xx[,c(1,2,NCOL(xx),4,5,3,6)] # delete Blank
    xx <- xx[!is.na(xx$COUNTY),]
    names(xx)[3] <- "TOTAL"
    namesxx <- names(xx)
    namesxx[4:5] <- c("DEM","REP")
    write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2018_Governor.csv"))
    write_delim(xx, paste0(data_dir,"ME_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}
createME_2018_Senate <- function(){
    xx <- read_excel(paste0(input_dir,"ME/2018/us-senate11-6-18.xlsx"), sheet = "US", skip = 3)
    names(xx) <- c("COUNTY","AREA","Brakey","King","Ringelstein","Others","Blank","TBC")
    xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
    xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
    xx <- xx[!grepl(" Totals", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
    xx <- xx[,c(1,2,NCOL(xx),4,3,seq(5,(NCOL(xx)-2)))] # delete Blank
    xx <- xx[!is.na(xx$COUNTY),]
    names(xx)[3] <- "TOTAL"
    namesxx <- names(xx)
    namesxx[4:5] <- c("DEM","REP")
    write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"ME_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createME_2020_President <- function(){
    xx <- read_excel(paste0(input_dir,"ME/2020/ME20_presandvisecnty1120.xlsx"), sheet = "Statewide", skip = 2)
    names(xx) <- c("COUNTY","AREA","Biden","De_La_Fuente","Hawkins","Jorgensen","Trump","Others","Blank","TBC")
    xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
    xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
    xx <- xx[!grepl(" Total", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
    xx <- xx[,c(1,2,10,3,7,4,5,6,8)] # delete Blank
    xx <- xx[!is.na(xx$COUNTY),]
    names(xx)[3] <- "TOTAL"
    namesxx <- names(xx)
    namesxx[4:5] <- c("DEM","REP")
    write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"ME_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createME_2020_Senate <- function(){
    xx <- read_excel(paste0(input_dir,"ME/2020/ME20_ussenator1120.xlsx"), sheet = "US Senator", skip = 2)
    names(xx) <- c("COUNTY","AREA","Collins","Gideon","Linn","Savage","Others","Blank","TBC")
    xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
    xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
    xx <- xx[!grepl(" Total", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
    xx <- xx[,c(1,2,NCOL(xx),4,3,seq(5,(NCOL(xx)-2)))] # delete Blank
    xx <- xx[!is.na(xx$COUNTY),]
    names(xx)[3] <- "TOTAL"
    namesxx <- names(xx)
    namesxx[4:5] <- c("DEM","REP")
    write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2020_Senate.csv"))
    write_delim(xx, paste0(data_dir,"ME_2020_Senate.csv"), append = TRUE, col_names = TRUE)
}
createME_2020_House <- function(){
    xx <- read_excel(paste0(input_dir,"ME/2020/ME20_repcongress1120.xlsx"), sheet = "Dist 1", skip = 2)
    #names(xx) <- c("DIST","COUNTY","AREA","Allen","Pingree","Others","Blank","TBC")
    names(xx) <- c("DIST","COUNTY","AREA","REP","DEM","Others","Blank","TBC")
    xx$COUNTY[grepl(" UOCAVA", xx$AREA)] <- "STATE"
    xx$AREA[grepl(" UOCAVA", xx$AREA)] <- "UOCAVA"
    yy <- read_excel(paste0(input_dir,"ME/2020/ME20_repcongress1120.xlsx"), sheet = "Dist 2", skip = 2)
    #names(yy) <- c("DIST","COUNTY","AREA","Crafts","Golden","Others","Blank","TBC")
    names(yy) <- c("DIST","COUNTY","AREA","REP","DEM","Others","Blank","TBC")
    yy$COUNTY[grepl(" UOCAVA", yy$AREA)] <- "STATE"
    yy$AREA[grepl(" UOCAVA", yy$AREA)] <- "UOCAVA" # set to UOCAVA2 to combine
    xx <- rbind(xx,yy)
    # xx[!is.na(xx$AREA) & xx$AREA == "UOCAVA",4:8] <-
    #     xx[!is.na(xx$AREA) & xx$AREA == "UOCAVA",4:8] +
    #     xx[!is.na(xx$AREA) & xx$AREA == "UOCAVA2",4:8]
    # xx <- xx[xx$AREA != "UOCAVA2",]
    xx <- xx[!grepl(" Total", xx$AREA) & (xx$AREA != "") & !is.na(xx$AREA),]
    xx <- xx[,c(1,2,3,NCOL(xx),5,4,6)] # delete Blank
    xx <- xx[!is.na(xx$COUNTY),]
    names(xx)[4] <- "TOTAL"
    namesxx <- names(xx)
    #namesxx[5:6] <- c("DEM","REP")
    write(paste(namesxx, collapse = " "), paste0(data_dir,"ME_2020_House.csv"))
    write_delim(xx, paste0(data_dir,"ME_2020_House.csv"), append = TRUE, col_names = TRUE)
}
createMN_2018_Senate <- function(){
    catmsg("##### START createMN_2018_Senate #####")
    xx <- read_excel(paste0(input_dir,"MN/2018/","2018-general-federal-state-results-by-precinct-official.xlsx"),
                     sheet = "Precinct-Results", skip = 0)
    xx <- xx[,c(5,2,30,26,25,27:29)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Klobuchar","Newberger","Schuller","Overby","Writein")
    xx <- xx[xx$COUNTY != "No Data",]
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LMN","GRN","Writein")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"MN_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"MN_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createMN_2018_Senate2 <- function(){
    catmsg("##### START createMN_2018_Senate #####")
    xx <- read_excel(paste0(input_dir,"MN/2018/","2018-general-federal-state-results-by-precinct-official.xlsx"),
                     sheet = "Precinct-Results", skip = 0)
    xx <- xx[,c(5,2,36,32,31,33:35)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Smith","Housley","Wellington","Trooien","Writein")
    xx <- xx[xx$COUNTY != "No Data",]
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LMN","IND","Writein")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"MN_2018_Senate2.csv"))
    write_delim(xx, paste0(data_dir,"MN_2018_Senate2.csv"), append = TRUE, col_names = TRUE)
}
createMN_2020_President <- function(){
    catmsg("##### START createMN_2020_President #####")
    xx <- read_excel(paste0(input_dir,"MN/2020/","2020-general-federal-state-results-by-precinct-official.xlsx"),
                     sheet = "Precinct-Results", skip = 0)
    xx <- xx[,c(5,2,37,28,27,35,30,29,31:34,36)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Jorgensen","Hawkins","DeLaFuente","West","Pierce","Riva","Kennedy","Writein")
    #xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","IA","IND1","IND2","SAL","SWP","Writein")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"MN_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"MN_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createMN_2020_Senate <- function(){
    catmsg("##### START createMN_2020_Senate #####")
    xx <- read_excel(paste0(input_dir,"MN/2020/","2020-general-federal-state-results-by-precinct-official.xlsx"),
                     sheet = "Precinct-Results", skip = 0)
    xx <- xx[,c(5,2,43,41,40,38,39,42)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Smith","Lewis","OConnor","Steinberg","Writein")
    #xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LMN","GLC","Writein")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"MN_2020_Senate.csv"))
    write_delim(xx, paste0(data_dir,"MN_2020_Senate.csv"), append = TRUE, col_names = TRUE)
}
createMT_2018_Senate <- function(){
    xx <- read_excel(paste0(input_dir,"MT/2018/2018-GeneralPrecinct-by-Precinct_Votes.xlsx"), sheet = "Sheet1", skip = 6)
    office <- "UNITED STATES SENATOR" #UPDATE
    xx <- xx[xx$RaceName == office,]
    xx <- xx[,c("CountyName","PrecinctName","NameOnBallot","PartyCode","Votes")]
    names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
    xx1 <<- xx #DEBUG-RM
    for (j in 1:NROW(xx)){
        if (grepl(" TESTER", xx$Candidate[j])){
            xx$Candidate[j] <- "DEM_Tester"
        }
        else if (grepl(" ROSENDALE", xx$Candidate[j])){
            xx$Candidate[j] <- "REP_Rosendale"
        }
        else if (grepl(" BRECKENRIDGE", xx$Candidate[j])){
            xx$Candidate[j] <- "LIB_Breckenridge"
        }
        else{
            xx$Candidate[j] <- paste0("IND_",xx$Candidate[j])
        }
    }
    xx2 <<- xx #DEBUG-RM
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    xx3 <<- xx #DEBUG-RM
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"MT_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"MT_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createMT_2020_President <- function(){
    xx <- read_excel(paste0(input_dir,"MT/2020/2020_General_Precinct-by-Precinct.xlsx"), sheet = "Sheet1", skip = 6)
    office <- "PRESIDENT" #UPDATE
    xx <- xx[xx$RaceName == office,]
    xx <- xx[,c("CountyName","PrecinctName","NameOnBallot","PartyCode","Votes")]
    names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
    for (j in 1:NROW(xx)){
        if (grepl(" BIDEN", xx$Candidate[j])){
            xx$Candidate[j] <- "DEM_Biden"
        }
        else if (grepl(" TRUMP", xx$Candidate[j])){
            xx$Candidate[j] <- "REP_Trump"
        }
        else if (grepl(" JORGENSEN", xx$Candidate[j])){
            xx$Candidate[j] <- "LIB_Jorgensen"
        }
        else{
            xx$Candidate[j] <- paste0("IND_",xx$Candidate[j])
        }
    }
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"MT_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"MT_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createMT_2020_Senate <- function(){
    xx <- read_excel(paste0(input_dir,"MT/2020/2020_General_Precinct-by-Precinct.xlsx"), sheet = "Sheet1", skip = 6)
    office <- "UNITED STATES SENATOR" #UPDATE
    xx <- xx[xx$RaceName == office,]
    xx <- xx[,c("CountyName","PrecinctName","NameOnBallot","PartyCode","Votes")]
    names(xx) <- c("COUNTY","AREA","Candidate","Party","Votes")
    xx1 <<- xx #DEBUG-RM
    for (j in 1:NROW(xx)){
        if (grepl(" BULLOCK", xx$Candidate[j])){
            xx$Candidate[j] <- "DEM_Bullock"
        }
        else if (grepl(" DAINES", xx$Candidate[j])){
            xx$Candidate[j] <- "REP_Daines"
        }
        else{
            xx$Candidate[j] <- paste0("IND_",xx$Candidate[j])
        }
    }
    xx2 <<- xx #DEBUG-RM
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    xx3 <<- xx #DEBUG-RM
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    # for (j in 3:(NCOL(xx)-1)){
    #     if (j != idem & j != irep){
    #         ii <- c(ii, j)
    #     }
    # }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"MT_2020_Senate.csv"))
    write_delim(xx, paste0(data_dir,"MT_2020_Senate.csv"), append = TRUE, col_names = TRUE)
}
createNC_2018_House <- function(){
    xx <- read_delim(paste0(input_dir,"NC/results_pct_20181106/",
                            "results_pct_20181106.txt"), '\t')
    #                 col_names = TRUE, col_types = "cccdccccddddddc")
    names(xx) <- c("COUNTY","ElectionDate","AREA","ContestID","ContestType",
                   "ContestName","Candidate","Party","VoteFor","ElectionDay",
                   "OneStop","Absentee","Provisional","Votes","RealPrecinct",
                   "NA16")
    office <- "US HOUSE OF REPRESENTATIVES DISTRICT " #UPDATE
    office_start <- "^US HOUSE OF REPRESENTATIVES" #UPDATE
    xx <- xx[grepl(office_start, xx$ContestName),]
    xx <- xx[,c("COUNTY","AREA","Candidate","Party","Votes")]
    gxx1 <<- xx #DEBUG-RM
    for (j in 1:NROW(xx)){
        #xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
        if (!is.na(xx$Party[j])){
            #xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
            xx$Candidate[j] <- xx$Party[j]
        }
    }
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    gxx2 <<- xx #DEBUG-RM
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    gxx2 <<- xx #DEBUG-RM
    # for (j in 4:(NCOL(xx)-1)){
    #     xx$TOTAL <- xx$TOTAL + xx[,j]
    # }
    namesxx <- names(xx)
    partyxx <- namesxx
    # for (j in 3:(NCOL(xx)-1)){
    #     partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
    #     namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
    # }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    xx <- xx[!is.na(xx$DEM),] #drop if no DEM candidate
    xx <- xx[!is.na(xx$REP),] #drop if no REP candidate
    for (j in 6:NCOL(xx)){
        xx[,j][is.na(xx[,j])] <- 0
    }
    gxx3 <<- xx #DEBUG-RM
    # xx <- xx[,c(1:5,8:10,7,6)]
    # names(xx)[9:10] <- c("Writein","Misc")
    # xx$Writein[is.na(xx$Writein)] <- 0
    # namesxx <- namesxx[c(1:5,8:10,7,6)]
    # partyxx <- partyxx[c(1:5,8:10,7,6)]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"NC_2018_House.csv"))
    write_delim(xx, paste0(data_dir,"NC_2018_House.csv"), append = TRUE, col_names = TRUE)
}
createNC_2020_President <- function(){
    xx <- read_delim(paste0(input_dir,"NC/results_pct_20201103/",
                            "results_pct_20201103.txt"), '\t')
    #                 col_names = TRUE, col_types = "cccdccccddddddc")
    names(xx) <- c("COUNTY","ElectionDate","AREA","ContestID","ContestType",
                   "ContestName","Candidate","Party","VoteFor","ElectionDay",
                   "OneStop","Absentee","Provisional","Votes","RealPrecinct",
                   "NA16")
    office <- "US PRESIDENT" #UPDATE
    xx <- xx[xx$ContestName == office,]
    xx <- xx[,c("COUNTY","AREA","Candidate","Party","Votes")]
    for (j in 1:NROW(xx)){
        xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
        if (!is.na(xx$Party[j])){
            xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
        }
    }
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    #gxx2 <<- xx #DEBUG-RM
    # for (j in 4:(NCOL(xx)-1)){
    #     xx$TOTAL <- xx$TOTAL + xx[,j]
    # }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    #gxx3 <<- xx #DEBUG-RM
    #START OF CODE FOR PRESIDENT ONLY
    xx <- xx[,c(1:5,8:10,7,6)]
    names(xx)[9:10] <- c("Writein","Misc")
    xx$Writein[is.na(xx$Writein)] <- 0
    namesxx <- namesxx[c(1:5,8:10,7,6)]
    partyxx <- partyxx[c(1:5,8:10,7,6)]
    #END OF CODE FOR PRESIDENT ONLY
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"NC_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"NC_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createNC_2020_Senate <- function(){
    xx <- read_delim(paste0(input_dir,"NC/results_pct_20201103/",
                            "results_pct_20201103.txt"), '\t')
    #                 col_names = TRUE, col_types = "cccdccccddddddc")
    names(xx) <- c("COUNTY","ElectionDate","AREA","ContestID","ContestType",
                   "ContestName","Candidate","Party","VoteFor","ElectionDay",
                   "OneStop","Absentee","Provisional","Votes","RealPrecinct",
                   "NA16")
    office <- "US SENATE" #UPDATE
    xx <- xx[xx$ContestName == office,]
    xx <- xx[,c("COUNTY","AREA","Candidate","Party","Votes")]
    for (j in 1:NROW(xx)){
        xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
        if (!is.na(xx$Party[j])){
            xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
        }
    }
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    gxx2 <<- xx #DEBUG-RM
    # for (j in 4:(NCOL(xx)-1)){
    #     xx$TOTAL <- xx$TOTAL + xx[,j]
    # }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    gxx3 <<- xx #DEBUG-RM
    # xx <- xx[,c(1:5,8:10,7,6)]
    # names(xx)[9:10] <- c("Writein","Misc")
    # xx$Writein[is.na(xx$Writein)] <- 0
    # namesxx <- namesxx[c(1:5,8:10,7,6)]
    # partyxx <- partyxx[c(1:5,8:10,7,6)]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"NC_2020_Senate.csv"))
    write_delim(xx, paste0(data_dir,"NC_2020_Senate.csv"), append = TRUE, col_names = TRUE)
}
createNC_2020_Governor <- function(){
    xx <- read_delim(paste0(input_dir,"NC/results_pct_20201103/",
                            "results_pct_20201103.txt"), '\t')
    #                 col_names = TRUE, col_types = "cccdccccddddddc")
    names(xx) <- c("COUNTY","ElectionDate","AREA","ContestID","ContestType",
                   "ContestName","Candidate","Party","VoteFor","ElectionDay",
                   "OneStop","Absentee","Provisional","Votes","RealPrecinct",
                   "NA16")
    office <- "NC GOVERNOR" #UPDATE
    xx <- xx[xx$ContestName == office,]
    xx <- xx[,c("COUNTY","AREA","Candidate","Party","Votes")]
    for (j in 1:NROW(xx)){
        xx$Candidate[j] <- tail(strsplit(xx$Candidate[j],split=" ")[[1]],1) #use last name
        if (!is.na(xx$Party[j])){
            xx$Candidate[j] <- paste0(xx$Candidate[j],"_",xx$Party[j])
        }
    }
    xx <- xx[,-4] # delete Party
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    gxx2 <<- xx #DEBUG-RM
    # for (j in 4:(NCOL(xx)-1)){
    #     xx$TOTAL <- xx$TOTAL + xx[,j]
    # }
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
        # if (names(xx)[j] != "OverVotes" & names(xx)[j] != "UnderVotes"){
        #     xx$TOTAL <- xx$TOTAL + xx[,j]
        # }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    gxx3 <<- xx #DEBUG-RM
    # xx <- xx[,c(1:5,8:10,7,6)]
    # names(xx)[9:10] <- c("Writein","Misc")
    # xx$Writein[is.na(xx$Writein)] <- 0
    # namesxx <- namesxx[c(1:5,8:10,7,6)]
    # partyxx <- partyxx[c(1:5,8:10,7,6)]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"NC_2020_Governor.csv"))
    write_delim(xx, paste0(data_dir,"NC_2020_Governor.csv"), append = TRUE, col_names = TRUE)
}
createNV_2016_President <- function(){
    xx <- read_excel(paste0(input_dir,"NV/2016/2016 General Election Results (Excel Format).xlsx"),
                     sheet = "2016 General Election Results", skip = 2,
                     guess_max = 10000)
    office <- "President and Vice President of the United States" #UPDATE
    xx <- xx[xx$Contest == office,]
    xx <- xx[,c("Jurisdiction","Precinct","Selection","Votes")]
    names(xx) <- c("COUNTY","AREA","Candidate","Votes")
    xx$AREA <- gsub("^Precinct ","",xx$AREA,ignore.case = TRUE) #fix Carson City County
    #xx$AREA <- gsub(" - COUNTY","",xx$AREA,ignore.case = TRUE) #fix Clark County
    xx$AREA[xx$COUNTY == "Clark"] <-
        gsub(" - [A-Za-z]+","",xx$AREA[xx$COUNTY == "Clark"]) #fix Clark County
    xx$Votes[xx$Votes == "*"] <- "1"
    xx$Votes <- as.numeric(xx$Votes)
    for (j in 1:NROW(xx)){
        if (grepl("^CLINTON", xx$Candidate[j])){
            xx$Candidate[j] <- "DEM_Clinton"
        }
        else if (grepl("^TRUMP", xx$Candidate[j])){
            xx$Candidate[j] <- "REP_Trump"
        }
        else if (grepl("^JOHNSON", xx$Candidate[j])){
            xx$Candidate[j] <- "LIB_Johnson"
        }
        else if (grepl("^CASTLE", xx$Candidate[j])){
            xx$Candidate[j] <- "CON_Castle"
        }
        else if (grepl("^DE LA FUENTE", xx$Candidate[j])){
            xx$Candidate[j] <- "REF_DeLaFuente"
        }
        else{
            xx$Candidate[j] <- "IND_Other"
        }
    }
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"NV_2016_President.csv"))
    write_delim(xx, paste0(data_dir,"NV_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createNV_2018_Senate <- function(){
    xx <- read_excel(paste0(input_dir,"NV/2018/Precinct Results 2018 General Election.xlsx"),
                     sheet = "2018 Statewide General Election", skip = 3,
                     guess_max = 10000)
    office <- "United States Senator" #UPDATE
    xx <- xx[xx$Contest == office,]
    xx <- xx[,c("Jurisdiction","Precinct","Selection","Votes")]
    names(xx) <- c("COUNTY","AREA","Candidate","Votes")
    xx$AREA <- gsub("^Precinct ","",xx$AREA,ignore.case = TRUE) #fix multiple counties
    xx$AREA[xx$COUNTY == "Douglas"] <-
        gsub("[0-9] - ","",xx$AREA[xx$COUNTY == "Douglas"]) #fix Douglas County
    xx$AREA[xx$COUNTY == "Humboldt"] <-
        gsub(" - [A-Za-z]+$","",xx$AREA[xx$COUNTY == "Humboldt"]) #fix Humboldt County
    xx$AREA[xx$COUNTY == "Washoe"] <-
        gsub("00$","",xx$AREA[xx$COUNTY == "Washoe"]) #fix Washoe County
    xx$AREA[xx$COUNTY == "Lyon"] <-
        gsub("^[0-9]+_","",xx$AREA[xx$COUNTY == "Lyon"]) #fix Lyon County
    xx$AREA[xx$COUNTY == "Lyon"] <-
        gsub(" - IN CITY$","",xx$AREA[xx$COUNTY == "Lyon"]
             ,ignore.case = TRUE) #fix Lyon County
    xx$AREA[xx$COUNTY == "Lyon"] <-
        gsub(" - OUT OF CITY$","",xx$AREA[xx$COUNTY == "Lyon"],
             ignore.case = TRUE) #fix Lyon County
    # xx$Votes[xx$Votes == "*"] <- "1"
    # xx$Votes <- as.numeric(xx$Votes)
    for (j in 1:NROW(xx)){
        if (grepl("^ROSEN", xx$Candidate[j])){
            xx$Candidate[j] <- "DEM_Rosen"
        }
        else if (grepl("^Heller", xx$Candidate[j])){
            xx$Candidate[j] <- "REP_Heller"
        }
        else if (grepl("^BAKARI", xx$Candidate[j])){
            xx$Candidate[j] <- "IAP_Bakari"
        }
        else if (grepl("^HAGAN", xx$Candidate[j])){
            xx$Candidate[j] <- "LPN_Hagan"
        }
        else if (grepl("^MICHAELS", xx$Candidate[j])){
            xx$Candidate[j] <- "NPP_Michaels"
        }
        else{
            xx$Candidate[j] <- "NP_Other"
        }
    }
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"NV_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"NV_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createNV_2020_President <- function(){
    xx <- read_excel(paste0(input_dir,"NV/2020/2020 General Election Precinct-Level Results.xlsx"),
                     sheet = "Abstract of Vote By Election Ne", skip = 2,
                     guess_max = 10000)
    office <- "President and Vice President of the United States" #UPDATE
    xx <- xx[xx$Contest == office,]
    xx <- xx[,c("Jurisdiction","Precinct","Selection","Votes")]
    names(xx) <- c("COUNTY","AREA","Candidate","Votes")
    xx$AREA <- gsub("^Precinct ","",xx$AREA,ignore.case = TRUE) #fix multiple counties
    xx$AREA[xx$COUNTY == "Washoe"] <-
        gsub("^[A-Za-z -]+ ","",xx$AREA[xx$COUNTY == "Washoe"]) #fix Washoe County
    xx$AREA[xx$COUNTY == "Washoe"] <-
        gsub("[ ]*\\(MP\\)","",xx$AREA[xx$COUNTY == "Washoe"]) #fix Washoe County
    xx$Votes[xx$Votes == "*"] <- "1"
    xx$Votes <- as.numeric(xx$Votes)
    for (j in 1:NROW(xx)){
        if (grepl("^BIDEN", xx$Candidate[j])){
            xx$Candidate[j] <- "DEM_Biden"
        }
        else if (grepl("^TRUMP", xx$Candidate[j])){
            xx$Candidate[j] <- "REP_Trump"
        }
        else if (grepl("^JORGENSEN", xx$Candidate[j])){
            xx$Candidate[j] <- "LIB_Jorgensen"
        }
        else if (grepl("^BLANKENSHIP", xx$Candidate[j])){
            xx$Candidate[j] <- "CON_Blankenship"
        }
        else{
            xx$Candidate[j] <- "IND_Other"
        }
    }
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Candidate) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Candidate,Votes)
    xx$TOTAL <- 0
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,"NV_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"NV_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createOH_2004_President <- function(){
    catmsg("##### START createOH_2004_President #####")
    xx <- read_excel(paste0(input_dir,"OH/2004/","precincts.xls"),
                     sheet = "Election Results", skip = 1)
    xx <- xx[,c(2,4,12,13,14,15,16)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Kerry","Bush","Badnarik","Peroutka")
    xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    xx$AREA <- gsub("^[0-9]+[ ]+","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","CON")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"OH_2004_President.csv"))
    write_delim(xx, paste0(data_dir,"OH_2004_President.csv"), append = TRUE, col_names = TRUE)
}
createOH_2016_President <- function(){
    catmsg("##### START createOH_2016_President #####")
    xx <- read_excel(paste0(input_dir,"OH/2016/","precinct.xlsx"),
                     sheet = "President", skip = 3)
    xx <- xx[,c(1,2,7,12,31,18,28,13,9:11,14:17,19:27,29,30)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Johnson","Stein","Duncan","Bell","Bickelmeyer","Castle","Fox","Hartnell","Hoefling","Jaynes","Keniston","Kirschner","Kotlikoff","Maldonado","Maturen","McMullin","Moorehead","Schriner","Smith","Stroh","Thomson")
    xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    xx$AREA <- gsub("^[0-9]+[ ]+","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","IND","WI1","WI2","WI3","WI4","WI5","WI6","WI7","WI8","WI9","WI10","WI11","WI12","WI13","WI14","WI15","WI16","WI17","WI18")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"OH_2016_President.csv"))
    write_delim(xx, paste0(data_dir,"OH_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createOH_2016_Senate <- function(){
    catmsg("##### START createOH_2016_Senate #####")
    xx <- read_excel(paste0(input_dir,"OH/2016/","precinct.xlsx"),
                     sheet = "U.S. Congress", skip = 3)
    xx <- xx[,c(1,2,7,14,11,10,9,12,13)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Strickland","Portman","DeMare","Connors","Rupert","Stahl")
    xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    xx$AREA <- gsub("^[0-9]+[ ]+","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","GRN","IND1","IND2","WI1")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"OH_2016_Senate.csv"))
    write_delim(xx, paste0(data_dir,"OH_2016_Senate.csv"), append = TRUE, col_names = TRUE)
}
createOH_2018_Governor <- function(){
    catmsg("##### START createOH_2018_Governor #####")
    xx <- read_excel(paste0(input_dir,"OH/2018/","2018-11-06_statewideprecinct_miami.xlsx"),
                     sheet = "Statewide Offices", skip = 3)
    xx <- xx[,c(1,2,7,10,11,14,13,9,12,15)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Cordray","DeWine","Irvine","GadellNewton","Ayres","Duncan","Turner")
    xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    xx$AREA <- gsub("^[0-9]+[ ]+","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","WI1","WI2","WI3")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"OH_2018_Governor.csv"))
    write_delim(xx, paste0(data_dir,"OH_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}
createOH_2018_Senate <- function(){
    catmsg("##### START createOH_2018_Senate #####")
    xx <- read_excel(paste0(input_dir,"OH/2018/","2018-11-06_statewideprecinct_miami.xlsx"),
                     sheet = "U.S. Congress", skip = 3)
    xx <- xx[,c(1,2,7,9,11,10)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Brown","Renacci","Faris")
    xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    xx$AREA <- gsub("^[0-9]+[ ]+","",xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","WI1")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"OH_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"OH_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createOH_2020_President <- function(){
    catmsg("##### START createOH_2020_President #####")
    xx <- read_excel(paste0(input_dir,"OH/2020/","statewideresultsbyprecinct.xlsx"),
                     sheet = "President and Vice President", skip = 3)
    xx <- xx[,c(1,2,7,9,17,15,12,10,11,13,14,16,18)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Jorgensen","Hawkins","Boddie","Carroll","Hoefling","Hunter","Simmons","Wells")
    xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    xx$AREA <- gsub("^[0-9]+[ ]+","",xx$AREA)
    #xx$AREA <- gsub("^[0-9]+","",xx$AREA) #limit to Perry 2020
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","WI1","WI2","WI3","WI4","WI5","WI6")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"OH_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"OH_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createOH_2020_Registered <- function(){
    catmsg("##### START createOH_2020_Registered #####")
    xx <- read_excel(paste0(input_dir,"OH/2020/","statewideresultsbyprecinct.xlsx"),
                     sheet = "President and Vice President", skip = 3)
    xx <- xx[,c(1,2,6,6,6)]
    names(xx) <- c("COUNTY","AREA","TOTAL","DemReg","RepReg")
    xx$DemReg <- xx$DemReg - 1
    xx$RepReg <- 1
    xx$AREA <- gsub("^PRECINCT ","",xx$AREA)
    xx$AREA <- gsub("^[0-9]+[ ]+","",xx$AREA)
    #xx$AREA <- gsub("^[0-9]+","",xx$AREA) #limit to Perry 2020
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"OH_2020_Registered.csv"))
    write_delim(xx, paste0(data_dir,"OH_2020_Registered.csv"), append = TRUE, col_names = TRUE)
}
createSC_2016_President <- function(){
    #input_dir <- "input/"
    #data_dir  <- "data/"
    file <- list.files(paste0(input_dir,"SC/2016/"),"*.xls.xlsx")
    cc <- gsub(".xls.xlsx","",file)
    #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        #for (i in 1:1){
        #catmsg(paste0("START read_excel(",cc[i],") Pres"))
        dd <- read_excel(paste0(input_dir,"SC/2016/",cc[i],".xls.xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"SC/2016/",cc[i],".xls.xlsx"), sheet = "3", skip = 2)
        gdd <<- dd #DEBUG-RM
        gxx <<- xx #DEBUG-RM
        yy <- data.frame(xx[,1])
        yy$COUNTY <- str_to_title(cc[i])
        k <- 3
        nn <- names(dd)[seq(3,length(dd),2)]
        gnn <<- nn #DEBUG-RM
        party <- rep("",length(nn)-1)
        # for (j in 1:(length(nn)-1)){
        #     party[j] <- head(strsplit(nn[j],split=" ")[[1]],1)
        # }
        #pp <- paste0(party,collapse = ",") #DEBUG
        #print(paste0(pp,"  ",cc[i])) #DEBUG
        for (j in seq(4,NCOL(xx),2)){
            yy[,k] <- xx[,j]
            lname <- head(strsplit(nn[k-2],split="/")[[1]],1) #Biden / Harris
            #lname <- gsub(" ","",trimws(lname))
            lname <- trimws(lname)
            lname <- tail(strsplit(lname,split=" ")[[1]],1) #use last name
            names(yy)[k] <- lname
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        gyy2 <<- yy #DEBUG-RM
        idem <- which(names(yy) == "Clinton")
        irep <- which(names(yy) == "Trump")
        ilib <- which(names(yy) == "Johnson")
        igrn <- which(names(yy) == "Stein")
        icon <- which(names(yy) == "Castle")
        ind1 <- which(names(yy) == "McMullin")
        iasc <- which(names(yy) == "Skewes")
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,ilib,igrn,icon,ind1,iasc)]
        names(yy) <- c("COUNTY","AREA","TOTAL","Clinton","Trump",
                       "Johnson","Stein","Castle","McMullin","Skewes")
        yy <- yy[yy$AREA != "Total:",] #delete Total
        gyy3 <<- yy #DEBUG-RM
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:10] <- c("DEM","REP","LIB","GRN","CON","IND","ASC")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2016_President.csv"))
    write_delim(zz, paste0(data_dir,"SC_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createSC_2018_Governor <- function(){
    #input_dir <- "input/"
    #data_dir  <- "data/"
    file <- list.files(paste0(input_dir,"SC/2018/"),"*.xls.xlsx")
    cc <- gsub(".xls.xlsx","",file)
    #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        #for (i in 1:1){
        #catmsg(paste0("START read_excel(",cc[i],") 2018_Gov"))
        dd <- read_excel(paste0(input_dir,"SC/2018/",cc[i],".xls.xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"SC/2018/",cc[i],".xls.xlsx"), sheet = "3", skip = 2)
        gdd <<- dd #DEBUG-RM
        gxx <<- xx #DEBUG-RM
        yy <- data.frame(xx[,1])
        yy$COUNTY <- str_to_title(cc[i])
        k <- 3
        nn <- names(dd)[seq(3,(length(dd)-1),2)]
        gnn <<- nn #DEBUG-RM
        
        for (j in 1:(length(nn))){
            m <- 2 * (j+1)
            if (grepl(" Smith", nn[j])){
                yy[,k] <- xx[,m]
                idem <- k
            }
            else if (grepl(" McMaster", nn[j])){
                yy[,k] <- xx[,m]
                irep <- k
            }
            else if (grepl("Write-In", nn[j])){
                yy[,k] <- xx[,m]
                iwri <- k
            }
            else if (grepl("WRITE-IN", nn[j])){
                yy[,k] <- xx[,m]
                iwri <- k
            }
            else{
                catmsg(paste0("UNEXPECTED name=",nn[j]))
            }
            k <- k+1
        }
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,iwri)]
        names(yy) <- c("COUNTY","AREA","TOTAL","Smith","McMaster","Writein")
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:6] <- c("DEM","REP","WRI")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2018_Governor.csv"))
    write_delim(zz, paste0(data_dir,"SC_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}
createSC_2020_President <- function(){
    #input_dir <- "input/"
    #data_dir  <- "data/"
    file <- list.files(paste0(input_dir,"SC/2020/"),"*.xls.xlsx")
    cc <- gsub(".xls.xlsx","",file)
    #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        #for (i in 1:1){
        #catmsg(paste0("START read_excel(",cc[i],") Pres"))
        dd <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "3", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "3", skip = 2)
        gdd <<- dd #DEBUG-RM
        gxx <<- xx #DEBUG-RM
        yy <- data.frame(xx[,1])
        yy$COUNTY <- str_to_title(cc[i])
        k <- 3
        nn <- names(dd)[seq(3,length(dd),7)]
        gnn <<- nn #DEBUG-RM
        party <- rep("",length(nn)-1)
        for (j in 1:(length(nn)-1)){
            party[j] <- head(strsplit(nn[j],split=" ")[[1]],1)
        }
        #pp <- paste0(party,collapse = ",") #DEBUG
        #print(paste0(pp,"  ",cc[i])) #DEBUG
        for (j in seq(9,NCOL(xx),7)){
            yy[,k] <- xx[,j]
            #nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- party[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "DEM") #Biden
        irep <- which(names(yy) == "REP") #Trump
        igrn <- which(names(yy) == "GRN") #Hawkins
        ialn <- which(names(yy) == "ALN") #DeLaFuente
        ilib <- which(names(yy) == "LIB") #Jorgensen
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,igrn,ialn,ilib)]
        names(yy) <- c("COUNTY","AREA","TOTAL","Biden","Trump",
                       "Hawkins","DeLaFuente","Jorgensen")
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:8] <- c("DEM","REP","GRN","ALN","LIB")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2020_President.csv"))
    write_delim(zz, paste0(data_dir,"SC_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createSC_2020_Senate <- function(){
    #input_dir <- "input/"
    #data_dir  <- "data/"
    file <- list.files(paste0(input_dir,"SC/2020/"),"*.xls.xlsx")
    cc <- gsub(".xls.xlsx","",file)
    #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        #for (i in 1:1){
        #catmsg(paste0("START read_excel(",cc[i],") Senate"))
        dd <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "4", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "4", skip = 2)
        gdd <<- dd #DEBUG-RM
        gxx <<- xx #DEBUG-RM
        yy <- data.frame(xx[,1])
        yy$COUNTY <- str_to_title(cc[i])
        k <- 3
        nn <- names(dd)[seq(3,length(dd),7)]
        gnn <<- nn #DEBUG-RM
        party <- rep("",length(nn)-1)
        for (j in 1:(length(nn)-1)){
            party[j] <- head(strsplit(nn[j],split=" ")[[1]],1)
        }
        pp <- paste0(party,collapse = ",") #DEBUG
        catmsg(paste0(pp,"  ",cc[i])) #DEBUG
        for (j in seq(9,NCOL(xx),7)){
            yy[,k] <- xx[,j]
            #nn[k-2] <- tail(strsplit(nn[k-2],split=" ")[[1]],1) #use last name
            names(yy)[k] <- party[k-2]
            k <- k+1
        }
        yy$TOTAL <- unlist(xx[,NCOL(xx)])
        idem <- which(names(yy) == "DEM") #Harrison
        irep <- which(names(yy) == "REP") #Graham
        icon <- which(names(yy) == "CON") #Bledsoe
        if (cc[i] == "dorchester"){ #Writein
            iwri <- which(names(yy) == "Write-in")
        }
        else if (cc[i] == "abbeville"){
            iwri <- which(names(yy) == "name")
        }
        else{
            iwri <- which(names(yy) == "Write-In")
        }
        yy <- yy[,c(2,1,NCOL(yy),idem,irep,icon,iwri)]
        names(yy) <- c("COUNTY","AREA","TOTAL","Harrison","Graham",
                       "Bledsoe","Writein")
        yy <- yy[yy$AREA != "Total:",] #delete Total
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    partyzz[4:7] <- c("DEM","REP","CON","WRI")
    write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2020_Senate.csv"))
    write_delim(zz, paste0(data_dir,"SC_2020_Senate.csv"), append = TRUE, col_names = TRUE)
}
createSC_2020_Registered <- function(){
    #input_dir <- "input/"
    #data_dir  <- "data/"
    file <- list.files(paste0(input_dir,"SC/2020/"),"*.xls.xlsx")
    cc <- gsub(".xls.xlsx","",file)
    #cc <- unlist(read_delim(paste0(data_dir,"SC_Counties.csv")," "))
    zz <- NULL
    for (i in 1:length(cc)){
        #for (i in 1:1){
        #catmsg(paste0("START read_excel(",cc[i],") Pres"))
        #dd <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "Registered Voters", skip = 1, n_max = 0) # read names
        xx <- read_excel(paste0(input_dir,"SC/2020/",cc[i],".xls.xlsx"), sheet = "Registered Voters", skip = 0)
        gxx <<- xx #DEBUG-RM
        yy <- data.frame(xx[,c(1,2)]) #AREA,Registered
        yy$COUNTY <- str_to_title(cc[i])
        yy <- yy[,c(3,1,2)] #put COUNTY in front
        gyy1 <<- yy #DEBUG-RM
        names(yy) <- c("COUNTY","AREA","TOTAL")
        yy$DEM <- 1
        yy$REP <- yy$TOTAL - 1
        gyy2 <<- yy #DEBUG-RM
        yy <- yy[yy$AREA != "Failsafe",]
        yy <- yy[yy$AREA != "Provisional",]
        yy <- yy[yy$AREA != "Failsafe Provisional",]
        yy <- yy[yy$AREA != "Total:",] #delete Total
        gyy3 <<- yy #DEBUG-RM
        zz <- rbind(zz,yy)
    }
    partyzz <- names(zz)
    write(paste(partyzz, collapse = " "), paste0(data_dir,"SC_2020_Registered.csv"))
    write_delim(zz, paste0(data_dir,"SC_2020_Registered.csv"), append = TRUE, col_names = TRUE)
}
# TEXAS HOUSE FUNCTION
createTX_House <- function(year,race,subdir){
    yr <- year %% 100
    catmsg(paste0("##### START create",race," #####"))
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    yy <- NULL
    for (i in 1:36){
        xx <- read_csv(paste0(input_dir,"TX/",year,subdir,"/u.s. rep ",i,".csv"))
        # names(xx) <- c("CNTYVTD","NEW***VTDKEY",
        #                "DoggettD_20G_U.S. Rep 35",
        #                "Garcia SharonR_20G_U.S. Rep 35",
        #                "LoeweL_20G_U.S. Rep 35",
        #                "MataI_20G_U.S. Rep 35")
        if (names(xx)[2] == "VTDKEY"){
            istart <- 3
        }
        else{
            istart <- 2
        }
        xx$DIST <- i
        xx$COUNTY <- substring(xx$CNTYVTD,1,3)
        for (i in 1:NROW(xx)){
            xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
            xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
        }
        xx$AREA <- xx$CNTYVTD
        xx$TOTAL <- 0
        itot <- NCOL(xx)
        idem <- irep <- ilib <- ioth1 <- ioth2 <- noth <- 0
        for (j in istart:(itot-4)){
            if(grepl(paste0("D_",yr,"G_"), names(xx)[j])){
                idem <- j
            }
            else if(grepl(paste0("R_",yr,"G_"), names(xx)[j])){
                irep <- j
            }
            else if(grepl(paste0("L_",yr,"G_"), names(xx)[j])){
                ilib <- j
            }
            else if(grepl(paste0("[GIW]_",yr,"G_"), names(xx)[j])){
                if (noth == 0){
                    ioth1 <- j
                    noth <- 1
                }
                else if (noth == 1){
                    ioth2 <- j
                    noth <- 2
                }
                else{
                    print(paste0("### WARNING: INGORING ",names(xx)[j]))
                }
            }
        }
        j <- NCOL(xx)
        if (idem == 0){
            j <- j+1
            idem <- j
            xx$DEM0 <- 0
        }
        if (irep == 0){
            j <- j+1
            irep <- j
            xx$REP0 <- 0
        }
        if (ilib == 0){
            j <- j+1
            ilib <- j
            xx$LIB0 <- 0
        }
        if (ioth1 == 0){
            j <- j+1
            ioth1 <- j
            xx$OTH10 <- 0
        }
        if (ioth2 == 0){
            j <- j+1
            ioth2 <- j
            xx$OTH20 <- 0
        }
        xx <- xx[,c((itot-3):itot,idem,irep,ilib,ioth1,ioth2)]
        names(xx) <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","LIB","OTH1","OTH2")
        yy <- yy <- rbind(yy, xx)
    }
    #zzyy <<- yy #DEBUG-RM
    partyyy <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","LIB","OTH1","OTH2")
    yy$AREA <- sub("^0+", "", substring(yy$AREA,4))
    write(paste(partyyy, collapse = " "), paste0(data_dir,paste0(race,".csv")))
    write_delim(yy, paste0(data_dir,paste0(race,".csv")), append = TRUE, col_names = TRUE)
}
createTX_2018_House <- function(){
    createTX_House(2018,"TX_2018_House","")
}
createTX_2020_House <- function(){
    createTX_House(2020,"TX_2020_House","")
}
createTX_2018_House_210624 <- function(){
    createTX_House(2018,"TX_2018_House_210624","/2106")
}
createTX_2020_House_210624 <- function(){
    createTX_House(2020,"TX_2020_House_210624","/2106")
}
# TEXAS PRECINCT DATA DOWNLOADED IN SEPTEMBER 2021
createTX_2016_President <- function(){
    catmsg("##### START createTX_2016_President #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2016/","president.csv"))
    # names(xx) <- c("CNTYVTD","NEW***VTDKEY","ClintonD_16G_President","TrumpR_16G_President",
    #                "JohnsonL_16G_President","SteinG_16G_President","Write-InW_16G_President")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(8,9,10,3:7)] #was c(7,8,9,2:6)
    names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Johnson","Stein","Writein")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2016_President.csv"))
    write_delim(xx, paste0(data_dir,"TX_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createTX_2018_AG <- function(){
    catmsg("##### START createTX_2018_AG #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2018/","attorney gen.csv"))
    # names(xx) <- c("CNTYVTD","NEW***VTDKEY","PaxtonR_18G_Attorney Gen","NelsonD_18G_Attorney Gen",
    #                "HarrisL_18G_Attorney Gen")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(6,7,8,4,3,5)] #was c(5,6,7,3,2,4)
    names(xx) <- c("COUNTY","AREA","TOTAL","Nelson","Paxton","Harris")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_AG.csv"))
    write_delim(xx, paste0(data_dir,"TX_2018_AG.csv"), append = TRUE, col_names = TRUE)
}
createTX_2018_Governor <- function(){
    catmsg("##### START createTX_2018_Governor #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2018/","governor.csv"))
    # names(xx) <- c("CNTYVTD","NEW***VTDKEY","AbbottR_18G_Governor","ValdezD_18G_Governor",
    #                "TippettsL_18G_Governor")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(6,7,8,4,3,5)] # was c(5,6,7,3,2,4)
    names(xx) <- c("COUNTY","AREA","TOTAL","Valdez","Abbott","Tippetts")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_Governor.csv"))
    write_delim(xx, paste0(data_dir,"TX_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}
createTX_2018_Senate <- function(){
    catmsg("##### START createTX_2018_Senate #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2018/","u.s. sen.csv"))
    # names(xx) <- c("CNTYVTD","NEW***VTDKEY","CruzR_18G_U.S. Sen","O'RourkeD_18G_U.S. Sen",
    #                "DikemanL_18G_U.S. Sen")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(6,7,8,4,3,5)] # was c(5,6,7,3,2,4)
    names(xx) <- c("COUNTY","AREA","TOTAL","ORourke","Cruz","Dikeman")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"TX_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createTX_2020_President <- function(){
    catmsg("##### START createTX_2020_President #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2020/","president.csv"))
    # names(xx) <- c("CNTYVTD","NEW***VTDKEY","BidenD_20G_President","TrumpR_20G_President",
    #                "JorgensenL_20G_President","HawkinsG_20G_President","Write-InW_20G_President")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(8,9,10,3:7)] # was c(7,8,9,2:6)
    names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Jorgensen","Hawkins","Writein")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"TX_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createTX_2020_Senate <- function(){
    catmsg("##### START createTX_2020_Senate #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2020/","u.s. sen.csv"))
    # names(xx) <- c("CNTYVTD","NEW***VTDKEY","CornynR_20G_U.S. Sen","HegarD_20G_U.S. Sen",
    #                "McKennonL_20G_U.S. Sen","CollinsG_20G_U.S. Sen")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(7,8,9,4,3,5,6)] #was c(6,7,8,3,2,4,5)
    names(xx) <- c("COUNTY","AREA","TOTAL","Hegar","Cornyn","McKennon","Collins")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","IND1","IND2")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2020_Senate.csv"))
    write_delim(xx, paste0(data_dir,"TX_2020_Senate.csv"), append = TRUE, col_names = TRUE)
}
# TEXAS PRECINCT DATA DOWNLOADED IN JUNE 2021
createTX_2016_President_210604 <- function(){
    catmsg("##### START createTX_2016_President_210604 #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2016/2106/","president.csv"))
    # names(xx) <- c("CNTYVTD","ClintonD_16G_President","TrumpR_16G_President",
    #                "JohnsonL_16G_President","SteinG_16G_President","Write-InW_16G_President")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(7,8,9,2:6)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Johnson","Stein","Writein")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2016_President_210604.csv"))
    write_delim(xx, paste0(data_dir,"TX_2016_President_210604.csv"), append = TRUE, col_names = TRUE)
}
createTX_2018_AG_210605 <- function(){
    catmsg("##### START createTX_2018_AG_210605 #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2018/2106/","attorney gen.csv"))
    # names(xx) <- c("CNTYVTD","PaxtonR_18G_Attorney Gen","NelsonD_18G_Attorney Gen",
    #                "HarrisL_18G_Attorney Gen")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(5,6,7,3,2,4)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Nelson","Paxton","Harris")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_AG_210605.csv"))
    write_delim(xx, paste0(data_dir,"TX_2018_AG_210605.csv"), append = TRUE, col_names = TRUE)
}
createTX_2018_Governor_210605 <- function(){
    catmsg("##### START createTX_2018_Governor_210605 #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2018/2106/","governor.csv"))
    # names(xx) <- c("CNTYVTD","AbbottR_18G_Governor","ValdezD_18G_Governor",
    #                "TippettsL_18G_Governor")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(5,6,7,3,2,4)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Valdez","Abbott","Tippetts")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_Governor_210605.csv"))
    write_delim(xx, paste0(data_dir,"TX_2018_Governor_210605.csv"), append = TRUE, col_names = TRUE)
}
createTX_2018_Senate_210605 <- function(){
    catmsg("##### START createTX_2018_Senate_210605 #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2018/2106/","u.s. sen.csv"))
    # names(xx) <- c("CNTYVTD","CruzR_18G_U.S. Sen","O'RourkeD_18G_U.S. Sen",
    #                "DikemanL_18G_U.S. Sen")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(5,6,7,3,2,4)]
    names(xx) <- c("COUNTY","AREA","TOTAL","ORourke","Cruz","Dikeman")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2018_Senate_210605.csv"))
    write_delim(xx, paste0(data_dir,"TX_2018_Senate_210605.csv"), append = TRUE, col_names = TRUE)
}
createTX_2020_President_210602 <- function(){
    catmsg("##### START createTX_2020_President_210602 #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2020/2106/","president.csv"))
    # names(xx) <- c("CNTYVTD","BidenD_20G_President","TrumpR_20G_President",
    #                "JorgensenL_20G_President","HawkinsG_20G_President","Write-InW_20G_President")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(7,8,9,2:6)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Jorgensen","Hawkins","Writein")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2020_President_210602.csv"))
    write_delim(xx, paste0(data_dir,"TX_2020_President_210602.csv"), append = TRUE, col_names = TRUE)
}
createTX_2020_Senate_210603 <- function(){
    catmsg("##### START createTX_2020_Senate_210603 #####")
    cc <- read_excel(paste0(input_dir,"all-geocodes-v2020.xlsx"), sheet = "all-geocodes-v2019", skip = 4)
    names(cc) <- c("SummaryLevel","StateCode","CountyCode","SubdivCode","PlaceCode","CityCode","Area")
    cctx <- cc[cc$StateCode == "48",]
    xx <- read_csv(paste0(input_dir,"TX/2020/2106/","u.s. sen.csv"))
    # names(xx) <- c("CNTYVTD","CornynR_20G_U.S. Sen","HegarD_20G_U.S. Sen",
    #                "McKennonL_20G_U.S. Sen","CollinsG_20G_U.S. Sen")
    xx$COUNTY <- substring(xx$CNTYVTD,1,3)
    for (i in 1:NROW(xx)){
        xx$COUNTY[i] <- cctx$Area[cctx$CountyCode == xx$COUNTY[i]]
        xx$COUNTY[i] <- gsub(" County","",xx$COUNTY[i])
    }
    xx$AREA <- xx$CNTYVTD
    xx$TOTAL <- 0
    xx <- xx[,c(6,7,8,3,2,4,5)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Hegar","Cornyn","McKennon","Collins")
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","IND1","IND2")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2020_Senate_210603.csv"))
    write_delim(xx, paste0(data_dir,"TX_2020_Senate_210603.csv"), append = TRUE, col_names = TRUE)
}
# TEXAS PRECINCT DATA OBTAINED FROM TEXAS SOS IN FEBRUARY 2022
read_narrow <- function(
    inputfile, inputsheet="", inputskip=0,
    xcounty, xrace, xcand, xvotes, xarea,
    ycounty, yrace, ycand, yparty, yarea_prefix, zcounty){
    xx <- read_excel(inputfile, sheet = inputsheet, skip = inputskip)
    #xx <- xx[xx[[xcounty]] == ycounty,]
    xx <- xx[xx[[xrace]] == yrace,]
    xx[[xarea]] <- paste0(yarea_prefix, xx[[xarea]])
    xx$Votes <- 0
    for (i in 1:length(xvotes)){
        xx$Votes <- xx$Votes + as.numeric(xx[[xvotes[i]]])
    }
    if (NROW(xx) == 0){
        catmsg(paste0("====> WARNING: ",ycounty," COUNTY had no ",yrace))
        next
    }
    xx$Party <- yparty[length(yparty)]
    for (i in 1:(length(yparty)-1)){
        xx$Party[grep(ycand[i], xx[[xcand]])] <- yparty[i]
    }
    xx <- xx[,c(xcounty, xarea, "Party", "Votes")]
    xx <- xx %>%
        group_by_at(c(xcounty,xarea,"Party")) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread_("Party","Votes")
    xx[[xcounty]] <- zcounty
    xx$TOTAL <- 0
    xx <- xx[c(xcounty,xarea,"TOTAL",yparty)]
    names(xx) <- c("COUNTY","AREA","TOTAL",ycand)
    return(xx)
}
createTX_2020_President_SOS <- function(){
    catmsg("##### START createTX_2020_President_SOS #####")
    
    # Process HARRIS County
    xx <- read_narrow(
        inputfile = paste0(input_dir,"TXsos/2020/HARRIS_COUNTY-2020_NOVEMBER_3RD_GENERAL_ELECTION_1132020-SOS_TotalDetail.xlsx"),
        inputsheet = "Sheet1",
        inputskip = 0,
        xcounty = "political_subdivision",
        xarea = "precinct _number",
        xrace = "race_name",
        xcand = "candidate_name",
        xvotes = c("early_votes...10","election_votes...11"),
        ycounty = "Harris County",
        yarea_prefix = "201",
        yrace = "President and Vice President",
        ycand  = c("Biden","Trump","Jorgensen","Hawkins","Writein"),
        yparty = c("DEM",  "REP",  "LIB",      "GRN",    "WRI"),
        zcounty = "Harris"
    )
    yy <- as.data.frame(xx)
    
    # Process MAVERICK County
    sosfile <- paste0(input_dir,"TXsos/MAVERICK_COUNTY-2020_NOVEMBER_3RD_GENERAL_ELECTION_1132020-PCT. BY PCT. REPORT GENERAL ELECTION 2020.xlsx")
    email <- read_excel(sosfile, sheet = "EARLY VOTING BY MAIL", skip = 0)
    epers <- read_excel(sosfile, sheet = "EARLY VOTING IN PERSON", skip = 0)
    eday  <- read_excel(sosfile, sheet = "ELECTION DAY", skip = 0)
    area <- paste0("32300", names(eday)[3:16])
    rep <- as.numeric(email[2,3:16]) + as.numeric(epers[2,3:16]) + as.numeric(eday[2,3:16])
    dem <- as.numeric(email[3,3:16]) + as.numeric(epers[3,3:16]) + as.numeric(eday[3,3:16])
    lib <- as.numeric(email[4,3:16]) + as.numeric(epers[4,3:16]) + as.numeric(eday[4,3:16])
    grn <- as.numeric(email[5,3:16]) + as.numeric(epers[5,3:16]) + as.numeric(eday[5,3:16])
    wri <- as.numeric(email[6,3:16]) + as.numeric(epers[6,3:16]) + as.numeric(eday[6,3:16])
    xx <- data.frame(area,dem,rep,lib,grn,wri)
    xx$COUNTY <- "Maverick"
    xx$TOTAL <- 0
    xx <- xx[,c(7,1,8,2:6)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Jorgensen","Hawkins","Writein")
    zzxx <<- xx #DEBUG-RM
    zzyy <<- yy #DEBUG-RM
    yy <- rbind(yy,xx)
    zyy2 <<- yy #DEBUG-RM
    yy$AREA <- sub("^0+", "", substring(yy$AREA,4))
    partyyy <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
    write(paste(partyyy, collapse = " "), paste0(data_dir,"TX_2020_President_SOS.csv"))
    write_delim(yy, paste0(data_dir,"TX_2020_President_SOS.csv"), append = TRUE, col_names = TRUE)
}
createTX_2020_Senate_SOS <- function(){
    catmsg("##### START createTX_2020_Senate_SOS #####")
    
    # Process HARRIS County
    xx <- read_narrow(
        inputfile = paste0(input_dir,"TXsos/2020/HARRIS_COUNTY-2020_NOVEMBER_3RD_GENERAL_ELECTION_1132020-SOS_TotalDetail.xlsx"),
        inputsheet = "Sheet1",
        inputskip = 0,
        xcounty = "political_subdivision",
        xarea = "precinct _number",
        xrace = "race_name",
        xcand = "candidate_name",
        xvotes = c("early_votes...10","election_votes...11"),
        ycounty = "Harris County",
        yarea_prefix = "201",
        yrace = "United States Senator", #UPDATE
        ycand  = c("Hegar","Cornyn","McKennon","Collins","Writein"), #UPDATE
        yparty = c("DEM",  "REP",   "LIB",     "GRN",    "WRI"), #UPDATE
        zcounty = "Harris"
    )
    yy <- as.data.frame(xx)
    
    # Process MAVERICK County
    sosfile <- paste0(input_dir,"TXsos/MAVERICK_COUNTY-2020_NOVEMBER_3RD_GENERAL_ELECTION_1132020-PCT. BY PCT. REPORT GENERAL ELECTION 2020.xlsx")
    email <- read_excel(sosfile, sheet = "EARLY VOTING BY MAIL", skip = 0)
    epers <- read_excel(sosfile, sheet = "EARLY VOTING IN PERSON", skip = 0)
    eday  <- read_excel(sosfile, sheet = "ELECTION DAY", skip = 0)
    area <- paste0("32300", names(eday)[3:16])
    rep <- as.numeric(email[8,3:16])  + as.numeric(epers[8,3:16])  + as.numeric(eday[8,3:16])
    dem <- as.numeric(email[9,3:16])  + as.numeric(epers[9,3:16])  + as.numeric(eday[9,3:16])
    lib <- as.numeric(email[10,3:16]) + as.numeric(epers[10,3:16]) + as.numeric(eday[10,3:16])
    grn <- as.numeric(email[11,3:16]) + as.numeric(epers[11,3:16]) + as.numeric(eday[11,3:16])
    wri <- as.numeric(email[12,3:16]) + as.numeric(epers[12,3:16]) + as.numeric(eday[12,3:16])
    xx <- data.frame(area,dem,rep,lib,grn,wri)
    xx$COUNTY <- "Maverick"
    xx$TOTAL <- 0
    xx <- xx[,c(7,1,8,2:6)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Hegar","Cornyn","McKennon","Collins","Writein")
    
    yy <- rbind(yy,xx)
    zyy2 <<- yy #DEBUG-RM
    partyyy <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","GRN","Writein")
    yy$AREA <- sub("^0+", "", substring(yy$AREA,4))
    write(paste(partyyy, collapse = " "), paste0(data_dir,"TX_2020_Senate_SOS.csv"))
    write_delim(yy, paste0(data_dir,"TX_2020_Senate_SOS.csv"), append = TRUE, col_names = TRUE)
}
read_narrow_house <- function(yrace, ycand, yparty){
    xx <- read_narrow(
        inputfile = paste0(input_dir,"TXsos/2020/HARRIS_COUNTY-2020_NOVEMBER_3RD_GENERAL_ELECTION_1132020-SOS_TotalDetail.xlsx"),
        inputsheet = "Sheet1",
        inputskip = 0,
        xcounty = "political_subdivision",
        xarea = "precinct _number",
        xrace = "race_name",
        xcand = "candidate_name",
        xvotes = c("early_votes...10","election_votes...11"),
        ycounty = "Harris County",
        yarea_prefix = "201",
        yrace = yrace,
        ycand  = ycand,
        yparty = yparty,
        zcounty = "Harris"
    )
}
createTX_2020_House_SOS <- function(){
    catmsg("##### START createTX_2020_House_SOS #####")
    sosfile <- paste0(input_dir,"TXsos/MAVERICK_COUNTY-2020_NOVEMBER_3RD_GENERAL_ELECTION_1132020-PCT. BY PCT. REPORT GENERAL ELECTION 2020.xlsx")
    email <- read_excel(sosfile, sheet = "EARLY VOTING BY MAIL", skip = 0)
    epers <- read_excel(sosfile, sheet = "EARLY VOTING IN PERSON", skip = 0)
    eday  <- read_excel(sosfile, sheet = "ELECTION DAY", skip = 0)
    zemail <<- email
    zepers <<- epers
    zeday  <<- eday
    area <- paste0("32300", names(eday)[3:16])
    rep <- as.numeric(email[14,3:16]) + as.numeric(epers[14,3:16]) + as.numeric(eday[14,3:16])
    dem <- as.numeric(email[15,3:16]) + as.numeric(epers[15,3:16]) + as.numeric(eday[15,3:16])
    lib <- as.numeric(email[16,3:16]) + as.numeric(epers[16,3:16]) + as.numeric(eday[16,3:16])
    xx <- data.frame(area,dem,rep,lib)
    xx$DIST <- 23
    xx$COUNTY <- "Maverick"
    xx$TOTAL <- 0
    xx <- xx[,c(5,6,1,7,2:4)]
    names(xx) <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","LIB")
    partyxx <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","LIB")
    xx$AREA <- sub("^0+", "", substring(xx$AREA,4))
    write(paste(partyxx, collapse = " "), paste0(data_dir,"TX_2020_House_SOS.csv"))
    write_delim(xx, paste0(data_dir,"TX_2020_House_SOS.csv"), append = TRUE, col_names = TRUE)
}
createVA_2016_President <- function(){
    catmsg("##### START createVA_2016_President #####")
    xx <- read_csv(paste0(input_dir,"VA/2016/Virginia_Elections_Database__2016_President_General_Election_including_precincts.csv"), col_names = FALSE, skip = 2)
    # names(xx) <- c("County/City","Ward","Pct",
    #                "Hillary R. Clinton","Donald J. Trump",
    #                "Gary Johnson","Evan McMullin","Jill Stein",
    #                "All Others","Total Votes Cast")
    names(xx) <- c("COUNTY","WARD","AREA","Clinton","Trump","Johnson","McMullin","Stein","Other","TOTAL")
    xx <- xx[,c(1,3,10,4:9)]
    xx <- xx[xx$COUNTY != "TOTALS",]
    xx$COUNTY <- str_to_title(xx$COUNTY)
    xx$AREA <- gsub("^[0]+","",xx$AREA)
    xx$AREA <- gsub(" \\([A-Za-z0-9 ]+\\)$","",xx$AREA)
    xx$AREA <- str_to_title(xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","IND","GRN","OTH")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"VA_2016_President.csv"))
    write_delim(xx, paste0(data_dir,"VA_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createVA_2017_Governor <- function(){
    catmsg("##### START createVA_2017_Governor #####")
    xx <- read_csv(paste0(input_dir,"VA/2017/Virginia_Elections_Database__2017_Governor_General_Election_including_precincts.csv"), col_names = FALSE, skip = 2)
    # names(xx) <- c("County/City","Ward","Pct",
    #                "Ralph Shearer Northam","Edward Walter Gillespie",
    #                "Clifford Daniel Hyra","All Others","Total Votes Cast")
    names(xx) <- c("COUNTY","WARD","AREA","Northam","Gillespie","Hyra","Other","TOTAL")
    xx <- xx[,c(1,3,8,4:7)]
    xx <- xx[xx$COUNTY != "TOTALS",]
    xx$COUNTY <- str_to_title(xx$COUNTY)
    xx$AREA <- gsub("^[0]+","",xx$AREA)
    xx$AREA <- gsub(" \\([A-Za-z0-9 ]+\\)$","",xx$AREA)
    xx$AREA <- str_to_title(xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","OTH")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"VA_2017_Governor.csv"))
    write_delim(xx, paste0(data_dir,"VA_2017_Governor.csv"), append = TRUE, col_names = TRUE)
}
createVA_2018_Senate <- function(){
    catmsg("##### START createVA_2018_Senate #####")
    xx <- read_csv(paste0(input_dir,"VA/2018/Virginia_Elections_Database__2018_U_S_Senate_General_Election_including_precincts.csv"), col_names = FALSE, skip = 2)
    # names(xx) <- c("County/City","Ward","Pct",
    #                "Timothy Michael Kaine","Corey Alan Stewart",
    #                "Matthew Joseph Waters","All Others","Total Votes Cast")
    names(xx) <- c("COUNTY","WARD","AREA","Kaine","Stewart","Waters","Other","TOTAL")
    xx <- xx[,c(1,3,8,4:7)]
    xx <- xx[xx$COUNTY != "TOTALS",]
    xx$COUNTY <- str_to_title(xx$COUNTY)
    xx$AREA <- gsub("^[0]+","",xx$AREA)
    xx$AREA <- gsub(" \\([A-Za-z0-9 ]+\\)$","",xx$AREA)
    xx$AREA <- str_to_title(xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","OTH")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"VA_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"VA_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createVA_2020_President <- function(){
    catmsg("##### START createVA_2020_President #####")
    xx <- read_csv(paste0(input_dir,"VA/2020/Virginia_Elections_Database__2020_President_General_Election_including_precincts.csv"), col_names = FALSE, skip = 2)
    # names(xx) <- c("County/City","Ward","Pct",
    #                "Joseph Robinette Biden, Jr","Donald J. Trump",
    #                "Jo Jorgensen","All Others","Total Votes Cast")
    names(xx) <- c("COUNTY","WARD","AREA","Biden","Trump","Jorgensen","Other","TOTAL")
    xx <- xx[,c(1,3,8,4:7)]
    xx <- xx[xx$COUNTY != "TOTALS",]
    xx$COUNTY <- str_to_title(xx$COUNTY)
    xx$AREA <- gsub("^[0]+","",xx$AREA)
    xx$AREA <- gsub(" \\([A-Za-z0-9 ]+\\)$","",xx$AREA)
    xx$AREA <- str_to_title(xx$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","OTH")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"VA_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"VA_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createVA_2021_Governor <- function(){
    catmsg("##### START createVA_2021_Governor #####")
    cc <- read_csv(paste0(data_dir,"VA_COUNTIES.csv"), col_names = TRUE)
    ee <- NULL
    for (i in 1:(dim(cc)[1])){
        jjname <- paste0("https://results.elections.virginia.gov/vaelections/2021%20November%20General/Json/Locality/",cc$COUNTY[i],"/Governor.json")
        jj <- jsonlite::fromJSON(jjname)
        pp <- jj$Precincts
        for (j in 1:length(pp$PrecinctName)){
            nn <- as.data.frame(pp$Candidates[j])
            dem <- nn$Votes[nn$PoliticalParty == "Democratic"]
            rep <- nn$Votes[nn$PoliticalParty == "Republican"]
            lib <- nn$Votes[nn$PoliticalParty == "Liberation"]
            wri <- nn$Votes[nn$PoliticalParty == "Write-In"]
            tot <- dem + rep + lib + wri
            dd <- data.frame(cc$COUNTY[i], pp$PrecinctName[j],tot,dem,rep,lib,wri)
            names(dd) <- c("COUNTY","AREA","TOTAL","McAuliffe","Youngkin","Blanding","WriteIn")
            ee <- rbind(ee, dd)
        }
    }
    ee$COUNTY <- gsub("_"," ",ee$COUNTY)
    ee$COUNTY <- str_to_title(ee$COUNTY)
    ee$COUNTY <- gsub("King & Queen County","King And Queen County",ee$COUNTY) # match prior races
    #ee$AREA <- gsub("^[#]+ ","",ee$AREA) #comment to force into Other
    ee$AREA <- gsub("^[0]+","",ee$AREA)
    ee$AREA <- gsub(" \\([A-Za-z0-9 ]+\\)$","",ee$AREA)
    ee$AREA <- str_to_title(ee$AREA)
    partyxx <- c("COUNTY","AREA","TOTAL","DEM","REP","LIB","WRI")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"VA_2021_Governor.csv"))
    write_delim(ee, paste0(data_dir,"VA_2021_Governor.csv"), append = TRUE, col_names = TRUE)
}
# WISCONSIN PRECINCT DATA
createWI_2016_President_Original <- function(){
    catmsg("##### START createWI_2016_President_Original #####")
    xx0 <- read_excel(paste0(input_dir,"WI/2016/","Ward by Ward Original and Recount President of the United States.xlsx"),
                      sheet = "Sheet1", skip = 1)
    xx <- xx0[,c(1,3,4,6,5,7:21)] #ORIGINAL
    #xx <- xx0[,c(1,3,23,25,24,26:40)] #RECOUNT
    names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Castle","Johnson","Stein","Moorehead","De.La.Fuente","WRI1","WRI2","WRI3","WRI4","WRI5","WRI6","WRI7","WRI8","WRI9","SCATTERING")
    partyxx <- paste0('"',names(xx),'"')
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2016_President_Original.csv"))
    write_delim(xx, paste0(data_dir,"WI_2016_President_Original.csv"), append = TRUE, col_names = TRUE)
}
createWI_2016_President_Recount <- function(){
    catmsg("##### START createWI_2016_President_Recount #####")
    xx0 <- read_excel(paste0(input_dir,"WI/2016/","Ward by Ward Original and Recount President of the United States.xlsx"),
                      sheet = "Sheet1", skip = 1)
    #xx <- xx0[,c(1,3,4,6,5,7:21)] #ORIGINAL
    xx <- xx0[,c(1,3,23,25,24,26:40)] #RECOUNT
    names(xx) <- c("COUNTY","AREA","TOTAL","Clinton","Trump","Castle","Johnson","Stein","Moorehead","De.La.Fuente","WRI1","WRI2","WRI3","WRI4","WRI5","WRI6","WRI7","WRI8","WRI9","SCATTERING")
    partyxx <- paste0('"',names(xx),'"')
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2016_President_Recount.csv"))
    write_delim(xx, paste0(data_dir,"WI_2016_President_Recount.csv"), append = TRUE, col_names = TRUE)
}
createWI_2016_President0 <- function(){
    catmsg("##### START createWI_2016_President #####")
    xx <- read_excel(paste0(input_dir,"WI/2016/","Ward by Ward Report-President_0.xlsx"),
                     sheet = "Ward by Ward Report", skip = 10)
    xx <- xx[,-8] # delete column 8 due to spreadsheet error  
    names(xx) <- c("COUNTY","AREA","TOTAL","Trump","Clinton","Castle","Johnson","Stein","IND1","IND2","IND3","IND4","IND5","IND6","IND7","IND8","IND9","IND10","IND11","SCATTERING")
    lastCOUNTY <- ""
    for (i in 1:NROW(xx)){
        if (is.na(xx$COUNTY[i])){
            xx$COUNTY[i] <- lastCOUNTY
        }
        else{
            lastCOUNTY <- xx$COUNTY[i]
        }
    }
    xx <- xx[!endsWith(xx$AREA,"Totals:"),]
    xx <- xx[!is.na(xx$COUNTY),]
    xx <- xx[,c(1,2,3,5,4,6:20)]
    partyxx <- names(xx)
    partyxx[4:19] <- c("DEM","REP","CON","LIB","WGR","IND1","IND2","IND3","IND4","IND5","IND6","IND7","IND8","IND9","IND10","IND11")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2016_President.csv"))
    write_delim(xx, paste0(data_dir,"WI_2016_President.csv"), append = TRUE, col_names = TRUE)
}
createWI_2018_Governor <- function(){
    catmsg("##### START createWI_2018_Governor #####")
    xx0 <- read_excel(paste0(input_dir,"WI/2018/","Ward by Ward Report-Gen Election-Statewide Constitution Offices.xlsx"),
                      sheet = "Sheet2", skip = 10)
    xx <- xx0[,c(1,2,3,5,4,6:18)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Evers","Walker","Anderson",
                   "White","Turnbull","Enz","Cason","Boucher","Grimek",
                   "William","Hoffman","Turtenwald","Gehler","Davis",
                   "SCATTERING")
    # xx <- xx[xx$AREA != "County Totals",]
    # xx <- xx[xx$AREA != "County Totals:",]
    # xx <- xx[xx$AREA != "Office Totals",]
    xx <- xx[xx$AREA != "Office Totals:",]
    xx <- xx[!grepl("County Totals",xx$AREA,ignore.case = TRUE),]
    xx <- xx[!grepl("Office Totals",xx$AREA,ignore.case = TRUE),]
    for (i in 1:NROW(xx)){
        if (!is.na(xx$COUNTY[i])){
            lastCounty <- xx$COUNTY[i]
        }
        else{
            xx$COUNTY[i] <- lastCounty
        }
    }
    xx$TOTAL <- rowSums(xx[,4:NCOL(xx)], na.rm = TRUE)
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2018_Governor.csv"))
    write_delim(xx, paste0(data_dir,"WI_2018_Governor.csv"), append = TRUE, col_names = TRUE)
}
createWI_2018_Senate <- function(){
    catmsg("##### START createWI_2018_Senate #####")
    xx0 <- read_excel(paste0(input_dir,"WI/2018/","US Senator_WardByWard_withDistricts 2018 General_0.xlsx"),
                      sheet = "Sheet1")
    xx <- xx0[,c(1,3,4,8,7,9:11)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Baldwin","Vukmir","Walters","Schiess","SCATTERING")
    xx$TOTAL <- rowSums(xx[,4:NCOL(xx)], na.rm = TRUE)
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2018_Senate.csv"))
    write_delim(xx, paste0(data_dir,"WI_2018_Senate.csv"), append = TRUE, col_names = TRUE)
}
createWI_2018_SupremeCourt <- function(){
    catmsg("##### START createWI_2018_SupremeCourt #####")
    xx0 <- read_excel(paste0(input_dir,"WI/2018/","Ward Report-4.3.18 Spring Election-Supreme Court.xlsx"),
                      sheet = "Ward by Ward Report", skip = 10)
    xx <- xx0[,c(1,2,3,5,4,6)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Dallet","Screnock",
                   "SCATTERING")
    # xx <- xx[xx$AREA != "County Totals",]
    # xx <- xx[xx$AREA != "County Totals:",]
    # xx <- xx[xx$AREA != "Office Totals",]
    xx <- xx[xx$AREA != "Office Totals:",]
    xx <- xx[!grepl("County Totals",xx$AREA,ignore.case = TRUE),]
    xx <- xx[!grepl("Office Totals",xx$AREA,ignore.case = TRUE),]
    for (i in 1:NROW(xx)){
        if (!is.na(xx$COUNTY[i])){
            lastCounty <- xx$COUNTY[i]
        }
        else{
            xx$COUNTY[i] <- lastCounty
        }
    }
    xx$TOTAL <- rowSums(xx[,4:NCOL(xx)], na.rm = TRUE)
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2018_SupremeCourt.csv"))
    write_delim(xx, paste0(data_dir,"WI_2018_SupremeCourt.csv"), append = TRUE, col_names = TRUE)
}
createWI_2018_House <- function(){
    catmsg("##### START createWI_2018_House #####")
    yy <- NULL
    for (i in 1:8){
        sheetn <- paste0("Sheet",i+1)
        skipn <- 9
        if (i == 1) skipn <- 10
        xx0 <- read_excel(paste0(input_dir,"WI/2018/","Ward by Ward Report-Gen Election-Congress_0.xlsx"),
                          sheet = sheetn, skip = skipn)
        names(xx0)[1:2] <- c("COUNTY","AREA")
        xx0 <- xx0[!grepl("Office Totals",xx0$COUNTY,ignore.case = TRUE),]
        xx0 <- xx0[xx0$AREA != "Office Totals:",]
        xx0 <- xx0[!grepl("County Totals",xx0$AREA,ignore.case = TRUE),]
        if (NCOL(xx0) == 6){
            names(xx0) <- c("COUNTY","AREA","TOTAL","DEM","REP","OTH")
            xx0$IND2 <- 0
            xx0$LAST <- xx0$OTH
            xx0$OTH <- 0
        }
        else if (NCOL(xx0) == 7){
            names(xx0) <- c("COUNTY","AREA","TOTAL","DEM","REP","IND1","OTH")
            xx0$LAST <- xx0$OTH
            xx0$OTH <- 0
        }
        else if (NCOL(xx0) == 8){
            if (is.na(xx0[1,8])) xx0 <- xx0[-8]
            names(xx0) <- c("COUNTY","AREA","TOTAL","DEM","REP","IND1","OTH")
            xx0$LAST <- xx0$OTH
            xx0$OTH <- 0
        }
        else{ # NCOL(xx0) == 8
            if (is.na(xx0[1,8])) xx0 <- xx0[-8]
            names(xx0) <- c("COUNTY","AREA","TOTAL","DEM","REP","IND1","IND2","OTH")
        }
        xx0$DIST <- i
        if (i == 2){
            xx <- xx0[,c(9,1,2,3:8)]
        }
        else{
            xx <- xx0[,c(9,1,2,3,5,4,6:8)]
        }
        names(xx) <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","IND1","IND2","OTH")
        for (i in 1:NROW(xx)){
            if (!is.na(xx$COUNTY[i])){
                lastCounty <- xx$COUNTY[i]
            }
            else{
                xx$COUNTY[i] <- lastCounty
            }
        }
        xx$TOTAL <- rowSums(xx[,4:NCOL(xx)], na.rm = TRUE)
        yy <- yy <- rbind(yy, xx)
    }
    partyyy <- names(yy)
    #partyyy[4:5] <- c("DEM","REP")
    write(paste(partyyy, collapse = " "), paste0(data_dir,"WI_2018_House.csv"))
    write_delim(yy, paste0(data_dir,"WI_2018_House.csv"), append = TRUE, col_names = TRUE)
}
# Assumes DEM and REP listed 1st and 2nd in match, party, and lname
createfile <- function(xx,col,office,match,party,lname,filename){
    names(xx) <- c("COUNTY","AREA","Office","District","TOTAL","Party","Name","Votes")
    xx <- xx[xx$Office == office,]
    # Check District and TOTAL, if necessary
    for (i in 1:NROW(xx)){
        for (j in 1:length(match)){
            if (grepl(match[j],xx$Name[i])){
                xx$Name[i] <- paste0(party[j],"_",lname[j])
                break
            }
        }
    }
    xx <- xx[,c("COUNTY","AREA","Name","Votes")]
    # check for matches first???
    xx <- xx %>%
        group_by(COUNTY,AREA,Name) %>%
        summarize(Votes=sum(Votes))
    xx <- xx %>% spread(Name,Votes)
    xx$TOTAL <- 0
    
    namesxx <- names(xx)
    partyxx <- namesxx
    for (j in 3:(NCOL(xx)-1)){
        partyxx[j] <- head(strsplit(namesxx[j],split="_")[[1]],1) #last segment
        namesxx[j] <- tail(strsplit(namesxx[j],split="_")[[1]],1) #last name
    }
    ii <- c(1,2,NCOL(xx))
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
    for (j in 3:(NCOL(xx)-1)){
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,filename))
    write_delim(xx, paste0(data_dir,filename), append = TRUE, col_names = TRUE)
}
createWI_2016_President <- function(){
    xx <- read_delim(paste0(input_dir,"WI/2016/","20161108__wi__general__ward.txt"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","ward","office","district","total votes","party","candidate","votes")
    office <- "President"
    match <- c("Hillary Clinton","Donald J. Trump","Darrell L. Castle","Gary Johnson",
               "Jill Stein","Monica Moorehead","Rocky Roque De La Fuente","Cherunda Fox",
               "Evan McMullin","Michael A. Maturen","Marshall Schoenke","Chris Keniston",
               "Laurence Kotlikoff","Tom Hoefling","Joseph Maldonado","Emidio Soltysik",
               "Scattering")
    party <- c("DEM","REP","CON","LIB",
               "WGR","IND","IND","IND",
               "IND","IND","IND","IND",
               "IND","IND","IND","IND","")
    lname <- c("Clinton","Trump","Castle","Johnson",
               "Stein","Moorehead","DeLaFuente","Fox",
               "McMullin","Maturen","Schoenke","Keniston",
               "Kotlikoff","Hoefling","Maldonado","Soltysik","Scattering")
    filename <- "WI_2016_President.csv"
    createfile(xx,columns,"President",match,party,lname,filename)
}
createWI_2020_President <- function(){
    catmsg("##### START createWI_2020_President #####")
    xx0 <- read_excel(paste0(input_dir,"WI/2020/","Ward by Ward Report PRESIDENT OF THE UNITED STATES by State Representive District - After Recount.xlsx"),
                      sheet = "Sheet1")
    xx <- xx0[,c(1,3,4,7:19)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Biden","Trump","Blankenship","Jorgensen","Carroll","WRI1","WRI2","WRI3","WRI4","WRI5","WRI6","WRI7","SCATTERING")
    xx$TOTAL <- rowSums(xx[,4:NCOL(xx)], na.rm = TRUE)
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2020_President.csv"))
    write_delim(xx, paste0(data_dir,"WI_2020_President.csv"), append = TRUE, col_names = TRUE)
}
createWI_2020_SupremeCourt <- function(){
    catmsg("##### START createWI_2020_SupremeCourt #####")
    xx0 <- read_excel(paste0(input_dir,"WI/2020/","Ward by Ward Report_Supreme Court.xlsx"),
                      sheet = "Ward by Ward Report", skip = 10)
    xx <- xx0[,c(1,2,3,4:6)]
    names(xx) <- c("COUNTY","AREA","TOTAL","Karofsky","Kelly",
                   "SCATTERING")
    # xx <- xx[xx$AREA != "County Totals",]
    # xx <- xx[xx$AREA != "County Totals:",]
    # xx <- xx[xx$AREA != "Office Totals",]
    xx <- xx[xx$AREA != "Office Totals:",]
    xx <- xx[!grepl("County Totals",xx$AREA,ignore.case = TRUE),]
    xx <- xx[!grepl("Office Totals",xx$AREA,ignore.case = TRUE),]
    for (i in 1:NROW(xx)){
        if (!is.na(xx$COUNTY[i])){
            lastCounty <- xx$COUNTY[i]
        }
        else{
            xx$COUNTY[i] <- lastCounty
        }
    }
    xx$TOTAL <- rowSums(xx[,4:NCOL(xx)], na.rm = TRUE)
    partyxx <- names(xx)
    partyxx[4:5] <- c("DEM","REP")
    write(paste(partyxx, collapse = " "), paste0(data_dir,"WI_2020_SupremeCourt.csv"))
    write_delim(xx, paste0(data_dir,"WI_2020_SupremeCourt.csv"), append = TRUE, col_names = TRUE)
}
createWI_2020_House <- function(){
    catmsg("##### START createWI_2020_House #####")
    yy <- NULL
    for (i in 1:8){
        sheetn <- paste0("Sheet",i+1)
        skipn <- 9
        if (i == 1) skipn <- 10
        xx0 <- read_excel(paste0(input_dir,"WI/2020/","Ward by Ward Report - Representative in Congress.xlsx"),
                          sheet = sheetn, skip = skipn)
        names(xx0)[1:2] <- c("COUNTY","AREA")
        xx0 <- xx0[!grepl("Office Totals",xx0$COUNTY,ignore.case = TRUE),]
        xx0 <- xx0[xx0$AREA != "Office Totals:",]
        xx0 <- xx0[!grepl("County Totals",xx0$AREA,ignore.case = TRUE),]
        if (NCOL(xx0) == 6){
            names(xx0) <- c("COUNTY","AREA","TOTAL","DEM","REP","OTH")
            xx0$LAST <- xx0$OTH
            xx0$OTH <- 0
        }
        xx0$DIST <- i
        xx <- xx0[,c(8,1,2,3:7)]
        names(xx) <- c("DIST","COUNTY","AREA","TOTAL","DEM","REP","IND","OTH")
        for (i in 1:NROW(xx)){
            if (!is.na(xx$COUNTY[i])){
                lastCounty <- xx$COUNTY[i]
            }
            else{
                xx$COUNTY[i] <- lastCounty
            }
        }
        xx$TOTAL <- rowSums(xx[,4:NCOL(xx)], na.rm = TRUE)
        yy <- yy <- rbind(yy, xx)
    }
    partyyy <- names(yy)
    #partyyy[4:5] <- c("DEM","REP")
    write(paste(partyyy, collapse = " "), paste0(data_dir,"WI_2020_House.csv"))
    write_delim(yy, paste0(data_dir,"WI_2020_House.csv"), append = TRUE, col_names = TRUE)
}
# Match using party instead of name if multiple races in state
createfilep <- function(xx,col,office,party,filename){
    xx <- xx[,col]
    names(xx) <- c("COUNTY","AREA","Office","DIST","TOTAL","Party","Name","Votes")
    xx <- xx[xx$Office == office,]
    xx$Party[is.na(xx$Party)] <- "(NA)"
    # Check District and TOTAL, if necessary
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
    xx <- xx[,c("DIST","COUNTY","AREA","Name","Votes")]
    # check for matches first???
    xx <- xx %>%
        group_by(DIST,COUNTY,AREA,Name) %>%
        summarize(Votes=sum(Votes))
    xx$Votes[is.na(xx$Votes)] <- 0
    xx <- xx %>% spread(Name,Votes)
    for (i in 4:NCOL(xx)){
        xx[,i][is.na(xx[,i])] <- 0
    }
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
        if (j != idem & j != irep){
            ii <- c(ii, j)
        }
    }
    xx <- xx[,ii]
    namesxx <- namesxx[ii]
    partyxx <- partyxx[ii]
    names(xx) <- namesxx
    write(paste(partyxx, collapse = " "), paste0(data_dir,filename))
    write_delim(xx, paste0(data_dir,filename), append = TRUE, col_names = TRUE)
}
###############################################################################
# The following uses election data from https://github.com/openelections
###############################################################################
cleanFL_2020 <- function(xx){
    #State-specific changes
    xx$party[xx$party == "GRE"]   <- "GRN"
    xx$party[xx$party == "LBT"]   <- "LIB"
    xx$party[xx$party == "LPF"]   <- "LIB"
    xx$precinct[is.na(xx$precinct)] <- "(NA)"
    # Leave overvotes and undervotes, "ballots cast" not present
    # xx <- xx[!grepl("OVER VOTES", xx$candidate, ignore.case = TRUE),]
    # xx <- xx[!grepl("Overvotes", xx$candidate, ignore.case = TRUE),]
    # xx <- xx[!grepl("UNDER VOTES", xx$candidate, ignore.case = TRUE),]
    # xx <- xx[!grepl("Undervotes", xx$candidate, ignore.case = TRUE),]
    # xx <- xx[!grepl("Unresolved write-in", xx$candidate, ignore.case = TRUE),]
    # xx <- xx[!grepl("Ballots Cast", xx$candidate, ignore.case = TRUE),]
    xx$total <- 0
    return(xx)
}
createFL_2020_State_House <- function(){
    xx <- read_delim(paste0(input_dir,"FL/2020/","20201103__fl__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanFL_2020(xx)
    office <- "State House"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "FL_2020_State_House.csv"
    createfilep(xx,columns,office,party,filename)
}
createFL_2020_State_Senate <- function(){
    xx <- read_delim(paste0(input_dir,"FL/2020/","20201103__fl__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanFL_2020(xx)
    office <- "State Senate"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "FL_2020_State_Senate.csv"
    createfilep(xx,columns,office,party,filename)
}
createFL_2020_President_OE <- function(){
    xx <- read_delim(paste0(input_dir,"FL/2020/","20201103__fl__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanFL_2020(xx)
    office <- "President"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "FL_2020_President_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createFL_2020_House_OE <- function(){
    xx <- read_delim(paste0(input_dir,"FL/2020/","20201103__fl__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanFL_2020(xx)
    office <- "U.S. House"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "FL_2020_House_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createFL_2020_Ballots <- function(){
    xx <- read_delim(paste0(input_dir,"FL/2020/","20201103__fl__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanFL_2020(xx)
    xx$candidate <- "(NA)"
    xx$party     <- "(NA)"
    office <- "Ballots Cast"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "FL_2020_Ballots.csv"
    createfilep(xx,columns,office,party,filename)
}
createFL_2020_Registered_OE <- function(){
    xx <- read_delim(paste0(input_dir,"FL/2020/","20201103__fl__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanFL_2020(xx)
    xx$candidate <- "(NA)"
    xx$party     <- "(NA)"
    office <- "Registered Voters"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "FL_2020_Registered_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
###############################################################################
cleanTX_2020 <- function(xx){
    #State-specific changes
    xx$party[xx$party == "DEMR"]  <- "DEM"
    xx$party[xx$party == "GRE"]   <- "GRN"
    xx$party[xx$party == "GREEN"] <- "GRN"
    xx$party[xx$party == "LBT"]   <- "LIB"
    xx$precinct[is.na(xx$precinct)] <- "PCT"
    xx <- xx[!grepl("OVER VOTES", xx$candidate, ignore.case = TRUE),]
    xx <- xx[!grepl("Overvotes", xx$candidate, ignore.case = TRUE),]
    xx <- xx[!grepl("UNDER VOTES", xx$candidate, ignore.case = TRUE),]
    xx <- xx[!grepl("Undervotes", xx$candidate, ignore.case = TRUE),]
    xx <- xx[!grepl("Unresolved write-in", xx$candidate, ignore.case = TRUE),]
    xx <- xx[!grepl("Ballots Cast", xx$candidate, ignore.case = TRUE),]
    xx$total <- 0
    return(xx)
}
#==============================================================================
createTX_2018_State_House <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2018/","20181106__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    office <- "State Representative"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2018_State_House.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2018_State_Senate <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2018/","20181106__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    office <- "State Senate"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2018_State_Senate.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2018_Governor_OE <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2018/","20181106__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    xx$district <- 0
    office <- "Governor"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2018_Governor_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2018_AG_OE <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2018/","20181106__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    xx$district <- 0
    office <- "Attorney General"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2018_AG_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2018_Senate_OE <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2018/","20181106__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    xx$district <- 0
    office <- "U.S. Senate"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2018_Senate_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2018_House_OE <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2018/","20181106__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    office <- "U.S. House"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2018_House_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2018_RR_Commission <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2018/","20181106__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    office <- "Railroad Commissioner"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2018_RR_Commission.csv"
    createfilep(xx,columns,office,party,filename)
}
#==============================================================================
createTX_2020_State_House <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2020/","20201103__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)

    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    office <- "State Representative"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2020_State_House.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2020_State_Senate <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2020/","20201103__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    office <- "State Senate"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2020_State_Senate.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2020_President_OE <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2020/","20201103__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    xx$district <- 0
    office <- "President"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2020_President_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2020_Senate_OE <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2020/","20201103__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    xx$district <- 0
    office <- "U.S. Senate"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2020_Senate_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2020_House_OE <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2020/","20201103__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    office <- "U.S. House"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2020_House_OE.csv"
    createfilep(xx,columns,office,party,filename)
}
createTX_2020_RR_Commission <- function(){
    xx <- read_delim(paste0(input_dir,"TX/2020/","20201103__tx__general__precinct.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","precinct","office","district","total","party","candidate","votes")
    xx <- cleanTX_2020(xx)
    office <- "Railroad Commissioner"
    party <- c("DEM","REP","LIB","GRN") #include largest parties, rest go into OTHER
    filename <- "TX_2020_RR_Commission.csv"
    createfilep(xx,columns,office,party,filename)
}
###############################################################################
createWI_2018_State_Senate <- function(){
    xx <- read_delim(paste0(input_dir,"WI/2018/","20181106__wi__general__ward.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","ward","office","district","total votes","party","candidate","votes")
    office <- "State Senate"
    party <- c("DEM","REP","LIB","WGR","CON","IND")
    filename <- "WI_2018_State_Senate.csv"
    createfilep(xx,columns,office,party,filename)
}
createWI_2020_State_Senate <- function(){
    xx <- read_delim(paste0(input_dir,"WI/2020/","20201103__wi__general__ward.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","ward","office","district","total votes","party","candidate","votes")
    office <- "State Senate"
    party <- c("DEM","REP","IND","SCATTERING","")
    filename <- "WI_2020_State_Senate.csv"
    createfilep(xx,columns,office,party,filename)
}
createWI_2020_State_Assembly <- function(){
    xx <- read_delim(paste0(input_dir,"WI/2020/","20201103__wi__general__ward.csv"), ',')
    #                  col_types = "cindlD") #char,int,num,dbl,log,date
    # xx <- read_excel(paste0(input_dir,"WI/2016/20161108__wi__general__ward.xlsx"),
    #                  sheet = "Sheet1", skip = 0)
    
    columns <- c("county","ward","office","district","total votes","party","candidate","votes")
    office <- "State Assembly"
    party <- c("DEM","REP","IND","CON","SCATTERING","")
    filename <- "WI_2020_State_Assembly.csv"
    createfilep(xx,columns,office,party,filename)
}
