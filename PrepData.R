lobby_bills_df <- readRDS("Rawdata.rds")
lobby_bills_df <- lobby_bills_df %>% 
  #make type into a factor in the right order
  mutate(max_type = ifelse(max_type %in% c("Veto", "BecameLaw"), "PassedCongress", max_type)) %>% 
  mutate(max_type = factor(max_type, levels = c("Introduced", "CommitteeConsideration", "DischargePetition", "CalendarEntry", "FloorConsideration", "ResolvingDifferences", "PassedCongress")), relatedBills.count = as.numeric(relatedBills.count), subjects.count = as.numeric(subjects.count)) %>% 
  #Replace missing values with None
  mutate(first_committee = ifelse(is.na(first_committee), "None", first_committee), policyArea = ifelse(is.na(policyArea), "None", policyArea)) %>% 
  mutate(sponsors.party = ifelse(sponsors.party == "ID" | sponsors.party == "I", "D", sponsors.party)) %>% 
  mutate(introducedDate = as.Date(introducedDate))

count_vars <- names(lobby_bills_df)[grepl("count", names(lobby_bills_df))]

#Replace missing count values with 0 and make values numeric
for(var in count_vars) {
  lobby_bills_df[,var][is.na(lobby_bills_df[,var])] <- 0
  lobby_bills_df[,var] <- as.numeric(lobby_bills_df[,var])
}
saveRDS(lobby_bills_df, "data.rds")