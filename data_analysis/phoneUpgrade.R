suppressMessages(require(data.table))
suppressMessages(require(ggplot2))
suppressMessages(require(hashmap))
source('./functions.R')

# Read in the files into a dataframe, this is the untarred, unzipped folder that Shaddi sent out
# remove the 0 byte files, or it will fail
setwd('./imsis_id_endaga/')
files <- list.files(pattern='*.log')

imsiraw <- rbindlist(lapply(files,
                            function(x){fread(x,colClasses=c('character','character','integer','integer','integer'))}))

setnames(imsiraw,c('imsi','imei','accessed','created','is_auth'))

# process the date and time
imsiraw[,created_dt:=as.POSIXct(created, origin="1970-01-01", tz = "GMT")]
imsiraw[,created_ds:=as.Date(created_dt)]
imsiraw[,created_hour:=hour(created_dt)]

imsiraw[,accessed_dt:=as.POSIXct(accessed, origin="1970-01-01", tz = "GMT")]
imsiraw[,accessed_ds:=as.Date(accessed_dt)]
imsiraw[,accessed_hour:=hour(accessed_dt)]

#Sorting dates by creation time
imsiraw <- imsiraw[order(imsiraw$accessed_dt), ]

setwd('../')
tac_mapping <- fread('tac_mapping.csv')

imsiraw$tac <- substr(imsiraw$imei, 1, 8)

feature_feature = 0
feature_3g = 0
feature_4g = 0
threeG_fourG = 0
threeG_threeG = 0
threeG_feature = 0
fourG_threeG = 0
fourG_feature = 0
fourG_fourG = 0
imsi_count = 0
shared_imsi = c()

imsiraw = subset(imsiraw, as.numeric(days_encounter) > 2)
unique_imsi <- unique(imsiraw$imsi)
imsiraw = subset(imsiraw, accessed_dt > as.POSIXct('2016-04-11', format = '%Y-%m-%d'))

#Now, lets look at upgrades - which is phones where the old sim was not logged back.
for(current_imsi in unique_imsi){
    sim_activity <- subset(imsiraw, imsi == current_imsi)
    
    imsi_count = imsi_count + 1
    print(imsi_count)

    phones <- c()
    current_imei = F
    sharing = F

    #Checking if the old IMEI was logged back in, if it was then the phone is assumed as a shared phone
    if(length(unique(sim_activity$imei)) != 1)
    {
      for(i in 1:nrow(sim_activity)){

        if(current_imei == F)
        {
          current_imei = as.character(sim_activity[i, 'imei'])
          phones[length(phones) + 1] <- current_imei
        }

        else{
          new_imei = as.character(sim_activity[i, 'imei'])
          if(new_imei != current_imei){
            if(!(new_imei %in% phones)){
              current_imei = new_imei
              phones[length(phones) + 1] <- current_imei
            }
            else{
              sharing = T
              shared_imsi[length(shared_imsi) + 1] <- current_imsi
              break
            }
          }
        }  
      }

      #After it has been established that the phone is not a shared phone, parse tacs
      current_type = F
      if(sharing == F){
            #phone_upgrade[nrow(phone_upgrade) + 1, c('imsi', 'count')] <- c(current_imsi, length(parsed_imei))
            #phone_upgrade$imei_list[nrow(phone_upgrade)] <- list(parsed_imei)

        #sim_activity = sim_activity[, .N, by = list(imsi, imei)]
        all_tacs = unique(sim_activity$tac)

        for(TAC in all_tacs){
          if(current_type == F){
            current_type = as.character(subset(tac_mapping, tac == TAC)$type_phone)

            if(length(current_type) == 0){
              current_type = F
            }
            else if(current_type == ""){
              current_type = F
            }
          }
          else{
            new_type = as.character(subset(tac_mapping, tac == TAC)$type_phone)

            if(length(new_type) != 0){
              if(new_type != ""){
                if(current_type == 'feature' & new_type == 'feature'){
                  feature_feature = feature_feature + 1
                }
                else if(current_type == 'feature' & new_type == 'threeG'){
                  feature_3g = feature_3g + 1
                }
                else if(current_type == 'feature' & new_type == 'fourG'){
                  feature_4g = feature_4g + 1
                }
                else if(current_type == 'threeG' & new_type == 'feature'){
                  threeG_feature = threeG_feature + 1
                }
                else if(current_type == 'threeG' & new_type == 'threeG'){
                  threeG_threeG = threeG_threeG + 1
                }
                else if(current_type == 'threeG' & new_type == 'fourG'){
                  threeG_fourG = threeG_fourG + 1
                }
                else if(current_type == 'fourG' & new_type == 'feature'){
                  fourG_feature = fourG_feature + 1
                }
                else if(current_type == 'fourG' & new_type == 'threeG'){
                  fourG_threeG = fourG_threeG + 1
                }
                else if(current_type == 'fourG' & new_type == 'fourG'){
                  fourG_fourG = fourG_fourG + 1
                }

                current_type = new_type
              }
            }
          } 
        }
      }
    }
}

paste('feature_feature', feature_feature, sep = ':')
paste('feature_3g', feature_3g, sep = ':')
paste('feature_4g', feature_4g, sep = ':')
paste('threeG_fourG', threeG_fourG, sep = ':')
paste('threeG_threeG', threeG_threeG, sep = ':')
paste('threeG_feature', threeG_feature, sep = ':')
paste('fourG_threeG', fourG_threeG, sep = ':')
paste('fourG_feature', fourG_feature, sep = ':')
paste('fourG_fourG', fourG_fourG, sep = ':')

imsi_count

#phone_upgrade$count <- as.numeric(phone_upgrade$count)
#phone_upgrade = phone_upgrade[order(phone_upgrade$count, decreasing = T), ]

#for(times in unique(phone_upgrade$count)){
#  num <- length(which(phone_upgrade$count == times))
#  cat(paste(num, times, sep = ","))
#  cat('\n')
#}