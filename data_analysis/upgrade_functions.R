#Load log files into a certain format
load_log_files <- function(directory){
    setwd(directory)
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
    setwd('../')

    imsiraw$tac <- substr(imsiraw$imei, 1, 8)
    imsiraw = subset(imsiraw, accessed_dt > as.POSIXct('2016-04-11', format = '%Y-%m-%d'))
    return(imsiraw)
}

#Get local users based on recorded IMSI activity
filter_local_users <- function(imsiraw){
    #imsi raw is expected in a certain format, you have to load the log files using the load file function

    # figure out first time an imsi is observed
    imsiraw[,first_encounter_with_imsi:=min(accessed_ds),imsi]
    # last time the imsi is observed
    imsiraw[,last_encounter_with_imsi:=max(accessed_ds),imsi]
    # Count number of days in between
    imsiraw[, days_encounter:=(last_encounter_with_imsi - first_encounter_with_imsi) + 1]
    #Count number of times connected
    times = imsiraw[, .N, by = list(imsi, accessed_ds)][, .N, by = list(imsi)]
    #Merging both dataframes
    setkey(imsiraw, imsi)
    setkey(times, imsi)
    imsiraw = merge(imsiraw, times)
    #Calculating ratio
    imsiraw[, average_usage:=N/as.integer(days_encounter)]

    unique_imsiraw = imsiraw[, list(imsi, average_usage, days_encounter, first_encounter_with_imsi, last_encounter_with_imsi, N)]
    setkey(unique_imsiraw, 'imsi')
    unique_imsiraw = unique(unique_imsiraw)

    imsiraw = subset(imsiraw, as.numeric(days_encounter) > 3)
    return(imsiraw)
}

#To see if that particular IMSI number is shared among multiple phones
compute_sim_sharing <- function(sim_activity){
    #sim_activity <- subset(imsiraw, imsi == current_imsi)
    phones <- c()
    current_imei = F
    sharing = F

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
              return(T)
            }
          }
        }  
      }
    }
    
    return(F)
}

compute_phone_upgrade <- function(sim_activity, phone_upgrades){
    all_imei = unique(sim_activity$imei)
    current_type = F

    for(IMEI in all_imei){
        TAC = substr(IMEI, 1, 8)

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
                    row = which((phone_upgrades$from == current_type) & (phone_upgrades$to == new_type))
                    #print(current_type)
                    #print(new_type)
                    #print(row)

                    if(length(row) == 0){
                        phone_upgrades[nrow(phone_upgrades) + 1, ] <- c(current_type, new_type, 1)
                    }
                    else{
                        phone_upgrades[row, 'count'] <- as.numeric(phone_upgrades[row, 'count']) + 1
                    }
                    
                    current_type = new_type
                }
            }
        } 
    }

    return(phone_upgrades)
}

plot_sankey <- function(phone_upgrades){
    names(phone_upgrades) <- c('source', 'target', 'value')
    nodes <- data.frame(unique(phone_upgrades$source))
    names(nodes) <- 'name'

    phone_upgrades$source <- match(phone_upgrades$source, nodes$name)
    phone_upgrades$source <- phone_upgrades$source - 1

    phone_upgrades$target <- match(phone_upgrades$target, nodes$name)
    phone_upgrades$target <- phone_upgrades$target - 1

    sankeyNetwork(Links = phone_upgrades, Nodes = nodes, Source = 'source',
             Target = 'target', Value = 'value', NodeID = 'name',
             fontSize = 12, nodeWidth = 30)
}