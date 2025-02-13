library(tidyverse)
library(readxl)

#START PARAMETERS
#links accessible at https://www.pa.gov/agencies/dos/resources/voting-and-elections-resources/voting-and-election-statistics/bulk-election-data.htm.html
vrdef  <- "https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/VoterRegistration_2000_General_ReadMe.txt"
vrdata <- "https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/VoterRegistration_2000_General_Precinct.txt"
erdef  <- "https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/ElectionReturns_2000_General_ReadMe.txt"
erdata <- "https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/ElectionReturns_2000_General_PrecinctRetuns.txt"
state2 <- "pa"
year <- 2000
yr     <- "00"
defyr  <- 16
filename <- paste0(year,"1107__pa__GE__precinct.csv")
#END PARAMETERS

datapath <- "data/"
makepath <- paste0("makedata/",state2,year,"/")
makepath_pre <- paste0(makepath,state2,defyr,"_") #add file prefix

#Download ER Bulk Election Data
destpath <- paste0(makepath,basename(erdata))
download.file(erdata, destfile = destpath, mode = "wb")
#Read ER Bulk Election Data
dd <- read_csv(destpath, col_names = FALSE)

#Read Field, County Code, and Office Code definitions
hh <- as.data.frame(read_table(paste0(makepath_pre,"fields.txt")))
rr <- as.data.frame(read_table(paste0(makepath_pre,"regfields.txt")))
cc <- as.data.frame(read_table(paste0(makepath_pre,"county_code.txt")))
cc$County_Code <- as.numeric(cc$County_Code)
oo <- as.data.frame(read_delim(paste0(makepath_pre,"office_code.txt"),delim = '\t'))

#Convert data to long format with 7 required fields
names(dd) <- hh$Field
ee <- dd[,c("County_Code","Precinct_Code","Candidate_Office_Code","US_Congressional_District",
            "Candidate_Party_Code","Candidate_Last_Name","Vote_Total")]
names(ee) <- c("county","precinct","office","district","party","candidate","votes")
ee$county <- cc$County[ee$county]
ee$precinct <- str_to_title(ee$precinct)
ee$candidate <- str_to_title(ee$candidate)
#Change selected Office Codes for readability
ee$office[ee$office == "USP"] <- "President of the US"
ee$office[ee$office == "USS"] <- "US Senator"
ee$office[ee$office == "USC"] <- "US House"
ee$office[ee$office == "STS"] <- "State Senator"
ee$office[ee$office == "USC"] <- "State House"
ee$office[ee$office == "ATT"] <- "Att General"
ff <- ee

#Download VR Bulk Election Data
destpath <- paste0(makepath,basename(vrdata))
download.file(vrdata, destfile = destpath, mode = "wb")
#Read VR Bulk Election Data
dd <- read_csv(destpath, col_names = FALSE)
#dd <- read_xlsx(destpath) #DEBUG 2012 AND 2016 USES XLSX

#Convert data to long format with 7 required fields
names(dd) <- rr$Field
dd <- dd[dd$County_Code > 0,] #ADD for reg
dd$Precinct <- dd$Precinct_Code
dd$Office <- "Registered"
#dd$Office <- "Registered Voters"
dd$District <- ""

ee <- dd[,c("County_Code","Precinct","Office","District",
            "Party_1_abbreviation","Party_1_abbreviation","Registered_Voters_for_party_1")]
names(ee) <- c("county","precinct","office","district","party","candidate","votes")
ee$county <- cc$County[ee$county]
ff <- rbind(ff,ee)

ee <- dd[,c("County_Code","Precinct","Office","District",
            "Party_2_abbreviation","Party_2_abbreviation","Registered_Voters_for_party_2")]
names(ee) <- c("county","precinct","office","district","party","candidate","votes")
ee$county <- cc$County[ee$county]
ff <- rbind(ff,ee)

ee <- dd[,c("County_Code","Precinct","Office","District",
            "Party_3_abbreviation","Party_3_abbreviation","Registered_Voters_for_party_3")]
names(ee) <- c("county","precinct","office","district","party","candidate","votes")
ee$county <- cc$County[ee$county]
ff <- rbind(ff,ee)

ee <- dd[,c("County_Code","Precinct","Office","District",
            "Party_4_abbreviation","Party_4_abbreviation","Registered_Voters_for_party_4")]
names(ee) <- c("county","precinct","office","district","party","candidate","votes")
ee$county <- cc$County[ee$county]
ff <- rbind(ff,ee)

ee <- dd[,c("County_Code","Precinct","Office","District",
            "Party_5_abbreviation","Party_5_abbreviation","Registered_Voters_for_party_5")]
names(ee) <- c("county","precinct","office","district","party","candidate","votes")
ee$county <- cc$County[ee$county]
ff <- rbind(ff,ee)

#Write long file locally and to data directory
write_csv(ff,paste0(makepath,filename))
write_csv(ff,paste0(datapath,filename))
