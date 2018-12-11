
#rerun mall query a few times per year

packages <- c("plyr", "dplyr", "tidyr", "stringr", "splitstackshape", 
              "purrr", "lubridate", "xlsx", "foreach", "data.table") #Do we need all of these?
lapply(packages, library, character.only = TRUE) #opens all packages in list above

if(getwd() != "C:/Users/jhoukal/Desktop/R/Management"){ 
  setwd("C:/Users/jhoukal/Desktop/R/Management")
}

temp = list.files(pattern="*.csv") #use grep wrapper to get rid of csv if needed
list2env(lapply(setNames(temp, make.names(gsub("*.csv$", "", temp))), #creates environment variables based on csv list pulled above
                stringsAsFactors=F, header=T, data.table::fread), envir = .GlobalEnv)

mydata <- MyEngagementSQL[!duplicated(MyEngagementSQL[1]),] #create separate obvious variable for master file; also get rid of duplicate entities (if any)
  
colnames(mydata)[1] <- "ENTITY_NO"
colnames(MyEngagementSF)[1] <- "ENTITY_NO"

#sf data 

MyEngagementSF <- aggregate(Temperature_Metric ~ ENTITY_NO, data=MyEngagementSF, FUN=mean)

mydata$Contact <- ifelse(mydata$ENTITY_NO %in% MyEngagementSF$ENTITY_NO, 1, 0)
mydata$Temp <- ifelse(mydata$ENTITY_NO %in% MyEngagementSF$ENTITY_NO, MyEngagementSF$Temperature_Metric, '')
mydata$Rebate <- ifelse(mydata$ENTITY_NO %in% MyEngagementRebates$`Entity Number`, 1, 0)
mydata$OnlineForm <- ifelse(mydata$ENTITY_NO %in% MyEngagementForms$`SubmittedBy: Entity ID`, 1, 0)

#housekeeping to speed up operations
rm(list = c("temp", "packages", "MyEngagementSF", "MyEngagementRebates", "MyEngagementForms", "MyEngagementSQL"))

#aggregated (averaged using numeric fields) by employer. Removing some non-numeric cols. 
#The '.~EMP_NAME' means we're aggregating (in this case, averaging) all remaining fields at an employer level.

x <- aggregate(.~EMP_NAME, mean, data = 
                 within(mydata, rm(EARNED_STATUS_ADJ, ENTITY_NO, REWARD_STATUS_ADJ, GENDER, ALLIANCE, STATE, EMP_NO, BMI, 
                                   Vitality_Age, Temp)))

#rearrange cols; second row is used for feature evaluation
x <- x[,c(1, 2, 16, 18, 19, 20, 21, 22, 23, 24,
          3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 25, 26, 27)]

#commenting out for now; I don't think we need this. 
#write.csv(x, paste0("./mgmtoutput/employers_", 
#        format(Sys.time(), "%m%d%Y"), ".csv"), row.names = F) #save now before transformations we don't want users to see


for(i in 1:nrow(x)){
   x$features[i] <- length(which(x[i, 11:27]>0))
} #out of 17 total points as of 12052018

#We assume the employer doesn't offer a feature if we register zero engagement (without rounding)

#state data...
states <- mydata %>% group_by(EMP_NAME, toupper(STATE)) %>% dplyr::summarise(Freq=n()) #group state frequency at emp level
states <- states[with(states, order(states$Freq, decreasing = TRUE) ), ]
states <- states[!duplicated(states$EMP_NAME), ] #gets unique (most frequent) value; should be around 491 records

colnames(states) <- c("EMP_NAME", "DOMINANT_STATE", "STATE_FREQ")

#mall data... 
#counts appearance of unique mall factors by company

MyEngagementMall <- ddply(MyEngagementMall, .(MyEngagementMall$EMP_NAME), mutate, count = length(unique(Mall_Factor)))
MyEngagementMall$Mall <- ifelse(MyEngagementMall$count == 1, as.character(MyEngagementMall$Mall_Factor), "Multiple Factors")
MyEngagementMall <- MyEngagementMall[, -(2:5)]
colnames(MyEngagementMall) <- c("EMP_NAME", "MALL")

#return data to main df using left joins
mydata <- left_join(left_join(left_join(
    mydata, x %>% select(EMP_NAME, features), by="EMP_NAME"), #features
      states %>% select(EMP_NAME, DOMINANT_STATE, STATE_FREQ), by="EMP_NAME"), #states
        MyEngagementMall[!duplicated(MyEngagementMall$EMP_NAME), ] %>% select(EMP_NAME, MALL), by="EMP_NAME") #mall

mydata <- mydata[!duplicated(mydata$ENTITY_NO),]          
#save file
data.table(fwrite(mydata, "./mgmtoutput/mgmtdashboard.csv", row.names=F))

#housekeeping
rm(list=ls())

print("Gleep Gloop")
