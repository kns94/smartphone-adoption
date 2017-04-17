suppressMessages(require(data.table))
suppressMessages(require(ggplot2))
suppressMessages(require(hashmap))
source('./functions.R')
source('./upgrade_functions.R')

imsiraw <- load_log_files('imsis_id_endaga')
imsiraw <- filter_local_users(imsiraw)
tac_mapping <- fread('tac_mapping.csv')

imsi_count = 0
shared_imsi = c()
phone_upgrades <- data.frame(matrix(NA, nrow = 0, ncol = 3))
colnames(phone_upgrades) <- c('from', 'to', 'count')
unique_imsi <- unique(imsiraw$imsi)

#Now, lets look at upgrades - which is phones where the old sim was not logged back.
for(current_imsi in unique_imsi){

    sim_activity <- subset(imsiraw, imsi == current_imsi)
    #Dominant phone per day
    sim_activity <- sim_activity[, max(.N), by = list(accessed_ds, imei)]
    sharing = compute_sim_sharing(sim_activity)
    
    imsi_count = imsi_count + 1
    print(imsi_count)
    
    #Checking if the old IMEI was logged back in, if it was then the phone is assumed as a shared phone
    if(length(unique(sim_activity$imei)) != 1)
    {
      #After it has been established that the phone is not a shared phone, parse tacs
      current_type = F
      if(sharing == F){
        
        phone_upgrades <- compute_phone_upgrade(sim_activity, phone_upgrades) 
        #print(phone_upgrades)          

      }
      else{
        shared_imsi[length(shared_imsi) + 1] <- current_imsi
      }
    }
}

imsi_count