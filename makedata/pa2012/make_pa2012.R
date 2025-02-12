library(tidyverse)

#START PARAMETERS
#url accessible at https://www.pa.gov/agencies/dos/resources/voting-and-elections-resources/voting-and-election-statistics/bulk-election-data.html#accordion-b33bb36a11-item-ca8b45af35
url <- "https://www.pa.gov/content/dam/copapwp-pagov/en/dos/old-website-documents/ElectionReturns_2012_General_PrecinctReturns.txt"
datapath <- "data/"
makepath <- "makedata/pa2012/"
makepath_pre <- paste0(makepath,"PA12_") #add file prefix
filename <- "20121106__pa__GE__precinct.csv"
#END PARAMETERS

#Download 2024 PA Bulk Election Data #UPDATE
destpath <- paste0(makepath,basename(url))
download.file(url, destfile = destpath, mode = "wb")
#Read 2024 PA Bulk Election Data #UPDATE
dd <- read_csv(destpath, col_names = FALSE)
#Read 2024 PA Field, County Code, and Office Code definitions. These were created from file at #UPDATE THIS AND NEXT 2 LINES
# https://www.pa.gov/content/dam/copapwp-pagov/en/dos/resources/voting-and-elections/bulk-data/erstat_2024_g_readme.txt
# which is accessible at https://www.pa.gov/agencies/dos/resources/voting-and-elections-resources/voting-and-election-statistics/bulk-election-data.html#accordion-b33bb36a11-item-ca8b45af35
hh <- as.data.frame(read_table(paste0(makepath_pre,"fields.txt")))
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

#Write long file locally and to data directory
write_csv(ee,paste0(makepath,filename))
write_csv(ee,paste0(datapath,filename))
