library(plyr)
library(RCurl)
library(rjson)
library(dplyr)
library(tidyverse)


#Processing the Open Secrets Raw Data
lobby_bills <- read.csv("Open Secrets Raw Data.txt")

#Create function for counting appearances of something in data
count_appearances <- function(pattern, x) {
  n <- length(x)
  output <- gregexpr(pattern, x)
  returns <- rep(0, n)
  for (i in 1:n) {
    if(length(output[[i]]) == 1) {
      if(output[[i]] == -1) {
        returns[i] <- 0
      } else{
        returns[i] <- length(output[[i]])
      }} else{
        returns[i] <- length(output[[i]])
      }
  }
  return(returns)
}

#Rename columns
names(lobby_bills) <- c("ID", "Code", "Congress", "Bill_No")

#Cleaning the weird data entry
lobby_bills <- lobby_bills %>% 
  mutate(Congress = gsub("[|]", "", Congress), Bill_No = gsub("[|]", "", Bill_No)) %>% 
  mutate(Chamber = substr(Bill_No, 1, 1))

#Grouping to find the number of times lobbied
lobby_appearances <- lobby_bills %>% 
  group_by(ID) %>% 
  dplyr::summarize(Code = toString(unique(Code)), Congress = toString(unique(Congress)), Bill_No = toString(unique(Bill_No)), Chamber = toString(unique(Chamber)), appearances = n())

#This is the key variable: counting the number of unique codes removes duplicate entries so this is the way that we are counting lobbying
lobby_appearances$Code_appearances<- count_appearances(",", lobby_appearances$Code) + 1
lobby_appearances$Congress[lobby_appearances$Congress == "113, 112"] <-  "113"

#more data cleaning and cutting out the bills with incomplete information or that are not h.r. or s. type bills
lobby_appearances <- lobby_appearances %>% 
  filter(Congress != "   ") %>% 
  filter(!(substr(ID, 1, 2) == "ZZ")) %>% 
  mutate(bill_type = gsub("[0-9].*", "", ID), Bill_Num = gsub("[A-Z]*", "", gsub(".*[.]", "", Bill_No))) %>% 
  mutate(Bill_Num = as.numeric(Bill_Num)) %>% 
  filter(bill_type != "h", bill_type != "hamdt", bill_type != "samdt", bill_type != "scon", bill_type != "sj", bill_type != "sr")

dim(lobby_appearances)


#The API has a rate limit of 1000 queries per hour, this function calculates how many queries occurred in the last hour
call_count <- function(times, range = 3600) {
  limit <- Sys.time() - range
  return(sum(times >= limit))
}
#This function makes the system wait for a bit over an hour if there have been too many queries in the last hour
check_calls <- function() {
  if (call_count(call_times) > 950) {
    cat("Rate Limit Approaching, System Pausing", as.character(Sys.time()))
    Sys.sleep(3800)
    cat("Now Proceeding")
  }
}

#limit search to 111th Congress
lobby_data_full <- lobby_appearances
lobby_data <- lobby_data_full %>% 
  filter(Congress == 111, (bill_type == "hr" | bill_type == "s"))

nbills <- nrow(lobby_data)

##IMPORTANT INSTRUCTIONS IN NEXT FEW COMMENTS (until for loop begins)

bills_data <- NULL #readRDS("Lobbied Bills 111th Congress API Info Temporary.rds") : After you run it the first time, replace NULL with this so you don't lose your progress
call_times <- NULL # Delete this after running the first time so you don't lose your query history
#I was instructed by the Library of Congress to not share my API key so I didn't want to include it in the submission. You can get your own API key at this link:  https://gpo.congress.gov/sign-up/
apikey <- "&api_key=[INSERT YOUR API KEY HERE]"

#Goes through each of the bills starting with the one that you left off with last time:
start_val <-  1 #nrow(bills_data) + 1 - after the first time replace the 1 with this

for(i in start_val:nbills) {
  cat(i, ", ", sep = "")
  
  #Creates the url to query the API for this bill
  curr_congress <- lobby_data$Congress[i]
  curr_bill_type <- lobby_data$bill_type[i]
  curr_bill_no <- lobby_data$Bill_Num[i]
  bill_url <- paste0("https://api.congress.gov/v3/bill/", curr_congress, "/", curr_bill_type, "/", curr_bill_no, "?format=json", apikey)
  
  #Makes sure that there actually is a bill with the specified number, will skip if there isn't
  bill_missing <- F
  check_calls()
  bill_data <- tryCatch({
    fromJSON(file = bill_url)
  }, error = function(e) {
    cat("We are missing this bill: ", conditionMessage(e), "\n")
    # Value to be returned in case of an error
    errors <- rep(NA, ncol(bills_data))
    #names(errors) <- names(bills_data)
    bill_missing <- T
    errors
  })
  call_times <- c(call_times, Sys.time())
  if (bill_missing) {
    bills_data[i,] <- bill_data
  } else {
    bill_data <- (as.data.frame(bill_data))
    #Remove unnecessary information
    removing_columns <- c("bill.cboCostEstimates.description", "bill.cboCostEstimates.pubDate", "bill.cboCostEstimates.title", "bill.cboCostEstimates.url", "request.contentType", "request.format", "bill.textVersions.count", "bill.textVersions.url", "request.congress", "request.billNumber", "request.billType", "bill.titles.count", "bill.titles.url", "bill.constitutionalAuthorityStatementText", "bill.relatedBills.url")
    bill_data <- bill_data %>% 
      select_if(!(names(.) %in% removing_columns)) %>% 
      select_if(!(grepl("cboCostEstimates", names(.))))
    
    if(!is.null(bill_data$bill.name)) {
      bill_data <- bill_data %>% 
        rename(policyArea = bill.name)
    }
    
    names(bill_data) <- gsub("^bill[.]", "", names(bill_data))
    
    #Query the API to get more info about Commmittees if it exists and add columns to the data frame as it comes in
    if(!is.null(bill_data$committees.url)) {
      bill_committees_url <- paste0(bill_data$committees.url, apikey)
      check_calls()
      committees_data <- fromJSON(file = bill_committees_url)
      call_times <- c(call_times, Sys.time())
      committees_data <- (as.data.frame(committees_data))
      bill_data$committees <- toString(committees_data[grepl("committees.name", names(committees_data))])
      bill_data$first_committee <- committees_data$committees.name
      bill_data <- bill_data %>% 
        select(-c(committees.url))
    }
    
    #Query the API to get more info about legislative subjects if it exists and add columns to the data frame as it comes in
    if(!is.null(bill_data$subjects.url)) {
      subjects_committees_url <- paste0(bill_data$subjects.url, apikey)
      check_calls()
      subjects_data <- fromJSON(file = subjects_committees_url)
      call_times <- c(call_times, Sys.time())
      
      bill_data$legislativeSubjects <- ifelse(is.null(subjects_data$subjects$legislativeSubjects), NA, toString(unlist(subjects_data$subjects$legislativeSubjects)))
      
      bill_data <- bill_data %>% 
        select(-subjects.url)
    } 
    
    #Query the API to get more info about Sponsors if it exists and add columns to the data frame as it comes in
    if(!is.null(bill_data$sponsors.url)) {
      bill_data <- bill_data %>% 
        select(-sponsors.url)
    }
    
    #Query the API to get more info about Cosponsors if it exists and add columns to the data frame as it comes in
    if(!is.null(bill_data$cosponsors.url)) {
      cosponsors_url <- paste0(bill_data$cosponsors.url, apikey)
      check_calls()
      cosponsors_data <- as.data.frame(fromJSON(file = cosponsors_url))
      call_times <- c(call_times, Sys.time())
      bill_data$cosponsors.names <- toString(cosponsors_data[grepl("fullName", names(cosponsors_data))])
      bill_data <- bill_data %>% 
        select(-cosponsors.url)
    }
    
    #Query the API to get more info about CRS Summaries if it exists and add columns to the data frame as it comes in
    if (!is.null(bill_data$summaries.url)) {
      summaries_url <- paste0(bill_data$summaries.url, apikey)
      check_calls()
      summaries_data <- fromJSON(file = summaries_url)
      call_times <- c(call_times, Sys.time())
      bill_data$summary <- tail(summaries_data$summaries, 1)[[1]]$text
      bill_data$summary <- trimws(gsub("<.*?>", "", bill_data$summary))
      bill_data <- bill_data %>% 
        select(-summaries.url)
    }
    
    #Query the API to get more info about Legislative Actions if it exists and add columns to the data frame as it comes in
    if(!is.null(bill_data$actions.url)) {
      actions_url <- paste0(bill_data$actions.url, apikey)
      check_calls()
      actions_data <- fromJSON(file = actions_url)
      call_times <- c(call_times, Sys.time())
      n <- length(actions_data$actions)
      
      #Create a data frame of all the legislative actions
      text <- rep(NA, n)
      actionCode <- rep(NA, n)
      actionDate <- rep(NA, n)
      sourceSystem <- rep(NA, n)
      type <- rep(NA, n)
      for(i in 1:n) {
        text[i] <- ifelse(is.null(actions_data$actions[[i]]$text), NA, actions_data$actions[[i]]$text)
        actionCode[i] <- ifelse(is.null(actions_data$actions[[i]]$actionCode), NA, actions_data$actions[[i]]$actionCode)
        actionDate[i] <- ifelse(is.null(actions_data$actions[[i]]$actionDate), NA, actions_data$actions[[i]]$actionDate)
        sourceSystem[i] <- ifelse(is.null(actions_data$actions[[i]]$sourceSystem$code), NA, actions_data$actions[[i]]$sourceSystem$code)
        type[i] <- ifelse(is.null(actions_data$actions[[i]]$type), NA, actions_data$actions[[i]]$type)
      }
      
      actions_df <- data.frame(text = text, actionCode = actionCode, actionDate = actionDate, sourceSystem = sourceSystem, type = type)
      
      #Nest the legislative actions
      bill_data$actions <- list(actions_df)
      bill_data <- bill_data %>% 
        select(-actions.url) %>% 
        #Create a variable for the highest action that it took
        mutate(max_type = case_when("BecameLaw" %in% actions_df$type ~ "BecameLaw",
                                    "Veto" %in% actions_df$type ~ "Veto",
                                    "President" %in% actions_df$type ~ "President",
                                    "ResolvingDifferences" %in% actions_df$type ~ "ResolvingDifferences",
                                    "Floor" %in% actions_df$type ~ "FloorConsideration",
                                    "Calendars" %in% actions_df$type ~ "CalendarEntry",
                                    "Discharge" %in% actions_df$type ~ "DischargePetition",
                                    "Committee" %in% actions_df$type ~ "CommitteeConsideration",
                                    "IntroReferral" %in% actions_df$type ~ "Introduced",
                                    T ~ "Other")) %>% 
        mutate(NotUsed = "NotUsed" %in% type, President = "President" %in% type, max_type = ifelse((max_type == "CommitteeConsideration" & any(grepl("report", tolower(text[type == "Committee"])))), "PassedCommittee", max_type))
      
    } else{
      bill_data$actions <- NA
    }
    #remove all URLs
    bill_data <- bill_data %>% 
      select_if(!(grepl("[.]url$", names(.))))
    
    #add all the information from this bill (bill_data) to the rest of the information (bills_data)
    bills_data <- rbind.fill(bills_data, bill_data)
  }
  
  #Store the progress so far in case of a crash
  saveRDS(bills_data, "Lobbied Bills 111th Congress API Info Temporary.rds")
  
  #There is a 1000 call limit per hour, so whenever we get close we pause until we get back down to 750 in the last hour 
  check_calls()
}

#Delete the columns that don't have much data because we can't use those
bills_data_cut <- bills_data %>% 
  select_if(!(colMeans(is.na(.)) > .9))

saveRDS(bills_data_cut, "Lobbied Bills 111th Congress.rds")


## Run this if need to revert. This is sometimes necessary when there is an error because it will create several rows of NAs and you will want to delete those
revert_data <- function(end_point) {
  bills_data <- bills_data[1:end_point,]
  saveRDS(bills_data, "Lobbied Bills 111th Congress API Info Temporary.rds")
  return(dim(readRDS("Lobbied Bills 111th Congress API Info Temporary.rds")))
}
#revert_data(5034)


#bind the two together, since the loop was contructed so that each rows would match a merge is not necessary, cbind works fine
lobby_combined <- cbind(lobby_data, API_bills)

#Save as an RDS
saveRDS(lobby_combined, "Rawdata.rds")

