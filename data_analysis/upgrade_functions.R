source('./functions.R')

#Figure out type of device
compute_specs <- function(imsi_mapped){
    imsi_mapped$network_support = 'feature'
    imsi_mapped[grepl('CDMA',Band), network_support:='threeG']
    imsi_mapped[grepl('UMTS',Band), network_support:='threeG']
    imsi_mapped[grepl('HSDPA',Band), network_support:='threeG']
    imsi_mapped[grepl('LTE', Band), network_support:='fourG']

    return(imsi_mapped)
}

#Figure out how many feature, 3G and 4G phones were connected per day
compute_phones_online <- function(imsi_mapped){
    phone_usage <- imsi_mapped[, list(times = sum(times_accessed)), by = list(accessed_ds, network_support)]
    phone_usage <- phone_usage[order(accessed_ds)]
    total_usage <- phone_usage[, list(total_connections = sum(times)), by = list(accessed_ds)][order(accessed_ds)]
    setkey(phone_usage, accessed_ds)[total_usage, ratio := ((times/total_connections) * 100)]


    streamgraph(data = phone_usage, key = 'network_support', value = 'ratio', date = 'accessed_ds', offset="zero",
     interpolate="linear") %>%
      sg_legend(show=TRUE, label="I- names: ")

    return(phone_usage)
}

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

    #Getting only one instance of IMSI, IMEI per day
    imsiraw <- imsiraw[, list(times_accessed = .N), list(imsi, imei, accessed_ds, created_ds)]

    # Calculate the check digit
    imsiraw[,checkdigit:=checkDigitCalc(imei)]
    # create the corrected imei
    imsiraw[,imeicorrected:=paste(substr(imei,1,14),checkdigit,sep = '')]

    # figure out the weekday
    imsiraw[,dayofweek:=weekdays(accessed_ds, abbreviate = TRUE)]
    # cast day of the week as factor and order them
    imsiraw[,dayofweek:=factor(dayofweek, ordered = TRUE, 
                           levels = c('Sun', 'Sat', 'Fri', 'Thu',  'Wed', 'Tue', 'Mon'))]
    # Add weekend/weekday identifier
    imsiraw[,weekend:=FALSE]
    imsiraw[dayofweek %in% c('Sun','Sat'),weekend:=TRUE]

    imsiraw$tac <- substr(imsiraw$imeicorrected, 1, 8)
    imsiraw = subset(imsiraw, accessed_ds >= as.Date('2016-04-11', format = '%Y-%m-%d'))
    
    #tacdb <- fread('tac_mapping.csv', colClasses = 'character')
    tacdb<-fread('MSRPLUS20161101.txt',colClasses = "character")
    imsi_mapped <- merge(imsiraw, tacdb, by.x="tac", by.y = 'TAC', all.x=TRUE)
    imsi_mapped <- compute_specs(imsi_mapped)
    imsi_mapped[imsi_mapped$network_support == "", 'network_support'] <- NA

    return(imsi_mapped)
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

    imsiraw = subset(imsiraw, as.numeric(days_encounter) > 0)
    return(imsiraw)
}

#To see if that particular IMSI number is shared among multiple phones
compute_sim_sharing <- function(sim_activity){
    #sim_activity <- subset(imsiraw, imsi == current_imsi)
    phones <- c()
    current_imei = F
    sharing = F

    if(length(unique(sim_activity$imeicorrected)) != 1)
    {
      for(i in 1:nrow(sim_activity)){

        if(current_imei == F)
        {
          current_imei = as.character(sim_activity[i, 'imeicorrected'])
          phones[length(phones) + 1] <- current_imei
        }

        else{
          new_imei = as.character(sim_activity[i, 'imeicorrected'])
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

compute_phone_upgrade <- function(IMSI, imsiraw, sim_activity, phone_upgrades, upgrade_age){
    all_imei = unique(sim_activity$imeicorrected)
    current_type = NA
    l <- list()

    for(IMEI in all_imei){
        TAC = substr(IMEI, 1, 8)

        u.row <- which((upgrade_age$imsi == IMSI) & (upgrade_age$imeicorrected == IMEI))
        if(length(u.row) == 0){
            upgrade_age[nrow(upgrade_age) + 1, ] <- c(IMSI, compute_upgrade_age(sim_activity, imsiraw, IMEI))
        }

        if(is.na(current_type)){
            current_type = as.character(subset(imsiraw, tac == TAC)$network_support[1])

            if(is.na(current_type)){
                current_type = NA
            }
        }
        else{
            new_type = as.character(subset(imsiraw, tac == TAC)$network_support[1])

            if(!is.na(new_type)){
                row = which((phone_upgrades$from == current_type) & (phone_upgrades$to == new_type))

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

    l$phone_upgrades <- phone_upgrades
    l$upgrade_age <- upgrade_age

    return(l)
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

#Computing if the phone to which the user upgraded was an old phone or not. If it was an old phone, what is the date difference between
#today and when it was first created!
compute_upgrade_age <- function(sim_activity, imsiraw, IMEI){
    pairing_date <- min(subset(sim_activity, imeicorrected == IMEI)$accessed_ds)
    age <- as.numeric(pairing_date - min(subset(imsiraw, imeicorrected == IMEI)$created_ds))
    return(c(IMEI, as.numeric(age)))
}