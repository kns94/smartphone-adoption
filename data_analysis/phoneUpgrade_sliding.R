suppressMessages(require(data.table))
suppressMessages(require(ggplot2))
suppressMessages(require(hashmap))
source('./functions.R')
source('./upgrade_functions.R')
require(streamgraph)
require(zoo)
require(plotly)
source('./phoneAdoption.R')
library(xts)
library(forecast)

imsiraw <- load_log_files('imsis_id_endaga')

#imsiraw <- fread('indonesia_entire.csv')

imsiraw <- id_all

imsiraw[, created_ds:=as.Date(imsiraw$created_ds, format = '%Y-%m-%d')]
imsiraw[, accessed_ds:=as.Date(imsiraw$accessed_ds, format = '%Y-%m-%d')]
#imsiraw = subset(imsiraw, accessed_ds >= as.Date('2016-04-11', format = '%Y-%m-%d'))
#imsiraw = subset(imsiraw, accessed_ds < as.Date('2016-12-01', format = '%Y-%m-%d'))

local <- filter_local_users(imsiraw)
non_local <- filter_non_local_users(imsiraw)
#local = local[accessed_ds >= as.Date('2016-04-11', format = '%Y-%m-%d'), ]
#non_local = non_local[accessed_ds >= as.Date('2016-04-11', format = '%Y-%m-%d'), ]

imsiraw <- filter_local_users(imsiraw)
imsiraw = imsiraw[accessed_ds >= as.Date('2016-04-11', format = '%Y-%m-%d'), ]
#tac_mapping <- fread('tac_mapping.csv')

#Merge the mturk dataset
turk_task <- fread('turk_final.csv', colClasses = 'character')
imsiraw <- merge(imsiraw, turk_task, by.x="fname", by.y = 'fname', all.x=TRUE)

phones <- imsiraw[, list(fname, year), ]
phones <- unique(phones)

imsi_count = 0
shared_imsi = c()

phone_upgrades <- data.frame(matrix(NA, nrow = 0, ncol = 3))
colnames(phone_upgrades) <- c('from', 'to', 'count')
community_transfer <- data.frame(matrix(NA, nrow = 0, ncol = 3))
colnames(community_transfer) <- c('imsi', 'imei', 'age')
upgrade_age <- list()
upgrade_age$`2G_4G` <- c()

unique_imsi <- unique(imsiraw$imsi)

no_year <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(no_year) <- c('fname', 'tac', 'company_name')

#Now, lets look at upgrades - which is phones where the old sim was not logged back.
for(current_imsi in unique_imsi){

    sim_activity <- subset(imsiraw, imsi == current_imsi)
    #Dominant phone per day
    sim_activity <- sim_activity[, list(times_used = max(.N)), by = list(accessed_ds, imeicorrected)]
    sharing = compute_sim_sharing(sim_activity)
    
    imsi_count = imsi_count + 1
    print(imsi_count)
    
    #Checking if the old IMEI was logged back in, if it was then the phone is assumed as a shared phone
    if(length(unique(sim_activity$imeicorrected)) != 1)
    {
      #After it has been established that the phone is not a shared phone, parse tacs
      current_type = F
      if(sharing == F){
        upgrades <- compute_phone_upgrade(current_imsi, imsiraw, sim_activity, phone_upgrades, community_transfer, upgrade_age, no_year)
        phone_upgrades <- upgrades$phone_upgrades
        community_transfer <- upgrades$community_transfer 
        no_year <- upgrades$no_year
        upgrade_age <- upgrades$upgrade_age
      }
      else{
        shared_imsi[length(shared_imsi) + 1] <- current_imsi
      }
    }
}

shared_imsiraw <- imsiraw[imsi %in% shared_imsi, ]

print(no_year)

imsi_count