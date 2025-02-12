library(tidyverse)
library(readxl) #DEBUG 2016 USES XLSX

#START PARAMETERS
#url accessible at https://www.pa.gov/agencies/dos/resources/voting-and-elections-resources/voting-and-election-statistics/bulk-election-data.html#accordion-b33bb36a11-item-a267be94b7
url <- "https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/VoterRegistration_2016_General_Precinct.xlsx"
datapath <- "data/"
makepath <- "makedata/pa2016/"
makepath_pre <- paste0(makepath,"PA16reg_") #add file prefix
filename <- "20161108__pa__GEreg__precinct.csv"
#END PARAMETERS

#Download 2016 PA Bulk Election Data #UPDATE
destpath <- paste0(makepath,basename(url))
download.file(url, destfile = destpath, mode = "wb")
#Read 2016 PA Bulk Election Data #UPDATE
#dd <- read_csv(destpath, col_names = FALSE)
dd <- read_xlsx(destpath) #DEBUG 2016 USES XLSX
#Read 2016 PA Field, County Code, and Office Code definitions. These were created from file at #UPDATE THIS AND NEXT 2 LINES
# https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/VoterRegistration_2016_General_ReadMeFile.txt
# which is accessible at https://www.pa.gov/agencies/dos/resources/voting-and-elections-resources/voting-and-election-statistics/bulk-election-data.html#accordion-b33bb36a11-item-a267be94b7
hh <- as.data.frame(read_table(paste0(makepath_pre,"fields.txt")))
cc <- as.data.frame(read_table(paste0(makepath_pre,"county_code.txt")))
cc$County_Code <- as.numeric(cc$County_Code)
# oo <- as.data.frame(read_delim(paste0(makepath_pre,"office_code.txt"),delim = '\t')) #CHANGE for reg

#Convert data to long format with 7 required fields
names(dd) <- hh$Field
dd <- dd[dd$County_Code > 0,] #ADD for reg
dd$Precinct <- dd$Precinct_Code
dd$Office <- "Registered"
dd$District <- ""

ee <- dd[,c("County_Code","Precinct","Office","District",
            "Party_1_abbreviation","Party_1_abbreviation","Registered_Voters_for_party_1")]
names(ee) <- c("county","precinct","office","district","party","candidate","votes")
ee$county <- cc$County[ee$county]
ff <- ee

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
