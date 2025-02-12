library(tidyverse)

#START PARAMETERS
#url accessible at https://www.pa.gov/agencies/dos/resources/voting-and-elections-resources/voting-and-election-statistics/bulk-election-data.html#accordion-b33bb36a11-item-d105bc02cf
url <- "https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/bulk-election-data/2024-general-election/vr/vrstat2024_g(9187)_20241223.txt"
datapath <- "data/"
makepath <- "makedata/pa2024/"
makepath_pre <- paste0(makepath,"PA24reg_") #add file prefix
filename <- "20121106__pa__GEreg__precinct.csv"
#END PARAMETERS

#Download 2024 PA Bulk Election Data #UPDATE
destpath <- paste0(makepath,basename(url))
download.file(url, destfile = destpath, mode = "wb")
#Read 2024 PA Bulk Election Data #UPDATE
dd <- read_csv(destpath, col_names = FALSE)
#Read 2024 PA Field, County Code, and Office Code definitions. These were created from file at #UPDATE THIS AND NEXT 2 LINES
# https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/bulk-election-data/2024-general-election/vr/vrstat2024_g_readme.txt
# which is accessible at https://www.pa.gov/agencies/dos/resources/voting-and-elections-resources/voting-and-election-statistics/bulk-election-data.html#accordion-b33bb36a11-item-d105bc02cf
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
