source('./functions.R')
require(reshape2)

#Figure out type of device
compute_specs <- function(imsi_mapped){
    imsi_mapped[, network_support:='feature']
    imsi_mapped[grepl('CDMA',Band), network_support:='threeG']
    imsi_mapped[grepl('UMTS',Band), network_support:='threeG']
    imsi_mapped[grepl('HSDPA',Band), network_support:='threeG']
    imsi_mapped[grepl('HSUPA',Band), network_support:='threeG']
    imsi_mapped[grepl('WCDMA',Band), network_support:='threeG']
    imsi_mapped[grepl('HSPA+',Band), network_support:='threeG']
    imsi_mapped[grepl('LTE', Band), network_support:='fourG']

    return(imsi_mapped)
}

#Computing online hours of feature, 3g and 4g phones.
compute_phones_online <- function(imsi_mapped){
    
    average_daily_phone_usage <- imsi_mapped[, list(daily_mean_hours = mean(times_accessed_per_day)), by = list(accessed_ds, accessed_month, accessed_year,
                                                           network_support)][order(accessed_ds)]
    average_monthly_phone_usage <- average_daily_phone_usage[, list(monthly_mean_hours = mean(daily_mean_hours)),
        by = list(accessed_year, accessed_month, network_support)]

    average_monthly_phone_usage[, accessed_ds:= paste(accessed_month, accessed_year, sep = '-')]
    average_monthly_phone_usage$accessed_ds = as.Date(as.yearmon(average_monthly_phone_usage$accessed_ds, format = '%m-%Y'))

    average_monthly_phone_usage <- average_monthly_phone_usage[, c('accessed_ds', 'monthly_mean_hours', 'network_support')]
    average_monthly_phone_usage <- dcast(average_monthly_phone_usage, accessed_ds ~ network_support, value.var = 'monthly_mean_hours')

    p <- plot_ly(average_monthly_phone_usage, x = ~accessed_ds, y = ~feature, type = 'bar', name = 'Feature Phones') %>%
            add_trace(y = ~threeG, name = '3G Phones') %>%
            add_trace(y = ~fourG, name = '4G Phones') %>%
            layout(yaxis = list(title = 'Average of Average Daily Usage'), xaxis = list(title = 'Month'),
             barmode = 'group', title= 'Mean of average daily hours per month in Philippines')

    return(p)
}

#Load log files into a certain format
load_log_files <- function(directory){
    setwd(directory)
    files <- list.files(pattern='*.log')

    #This function requires files to alteast have values. Please remove the 0KB files before processing! 
    imsiraw <- rbindlist(lapply(files,
                                function(x){fread(x,colClasses=c('character','character','integer','integer','integer'))}))

    setnames(imsiraw,c('imsi','imei','accessed','created','is_auth'))
    imsiraw[is_auth >= 1, is_auth:=1]

    # process the date and time
    imsiraw[,created_dt:=as.POSIXct(created, origin="1970-01-01", tz = "GMT")]
    imsiraw[,created_ds:=as.Date(created_dt)]
    imsiraw[,created_hour:=hour(created_dt)]

    imsiraw[,accessed_dt:=as.POSIXct(accessed, origin="1970-01-01", tz = "GMT")]
    imsiraw[,accessed_ds:=as.Date(accessed_dt)]
    imsiraw[,accessed_hour:=hour(accessed_dt)]
    setwd('../')

    #Getting only one instance of IMSI, IMEI per day and saving the count in this variable called times_accessed_per_day
    imsiraw <- imsiraw[, list(times_accessed_per_day = .N), list(imsi, imei, accessed_ds, created_ds, is_auth)]

    #Getting month and year when the phone was accessed
    imsiraw[, accessed_month:=month(accessed_ds)]
    imsiraw[, accessed_year:=year(accessed_ds)]

    # Calculate the check digit
    imsiraw[,checkdigit:=checkDigitCalc(imei)]
    # create the corrected imei
    imsiraw[,imeicorrected:=paste(substr(imei,1,14),checkdigit,sep = '')]

    # figure out the weekday
    #imsiraw[,dayofweek:=weekdays(accessed_ds, abbreviate = TRUE)]
    # cast day of the week as factor and order them
    #imsiraw[,dayofweek:=factor(dayofweek, ordered = TRUE, 
                           levels = c('Sun', 'Sat', 'Fri', 'Thu',  'Wed', 'Tue', 'Mon'))]
    # Add weekend/weekday identifier
    #imsiraw[,weekend:=FALSE]
    #imsiraw[dayofweek %in% c('Sun','Sat'),weekend:=TRUE]

    imsiraw[ ,tac:=substr(imeicorrected, 1, 8)]
    #Having the same time for PH and ID
    #imsiraw = subset(imsiraw, accessed_ds >= as.Date('2016-04-11', format = '%Y-%m-%d'))
    
    #tacdb <- fread('tac_mapping.csv', colClasses = 'character')
    tacdb<-fread('MSRPLUS20161101.txt',colClasses = "character")
    imsi_mapped <- merge(imsiraw, tacdb, by.x="tac", by.y = 'TAC', all.x=TRUE)
    imsi_mapped <- compute_specs(imsi_mapped)
    #imsi_mapped[network_support == "", network_support = NA]

    return(imsi_mapped)
}

#Plot different metrics to get local users
get_inflection_point <- function(imsiraw){

    # figure out first time an imsi is observed
    imsiraw[,first_encounter_with_imsi:=min(accessed_ds),imsi]
    # last time the imsi is observed
    imsiraw[,last_encounter_with_imsi:=max(accessed_ds),imsi]
    # Count number of days in between
    imsiraw[, diff_days:=(last_encounter_with_imsi - first_encounter_with_imsi) + 1]
    #Count number of days an IMSI is connected
    times = imsiraw[, .N, by = list(imsi, accessed_ds)][, list(number_days = .N), by = list(imsi)]
    #Merging both dataframes
    setkey(imsiraw, imsi)
    setkey(times, imsi)
    imsiraw = merge(imsiraw, times)
    #Calculating ratio
    imsiraw[, average_usage:=number_days/as.integer(diff_days)]   

    unique_imsiraw = imsiraw[, list(imsi, average_usage, diff_days, first_encounter_with_imsi, last_encounter_with_imsi, number_days, is_auth)]
    setkey(unique_imsiraw, 'imsi')
    unique_imsiraw = unique(unique_imsiraw)


    #Metric 1 : Number of days active
    days_usage = data.frame(matrix(nrow = 0, ncol = 2))
    names(days_usage) <- c('num_days', 'num_users')
    #days = unique(unique_imsiraw$number_days)
    days = c(c(1 : 10), c(15, 20, 25, 30, 40, 50, 60, 70))

    for(day in days){
        days_usage[nrow(days_usage) + 1, ] <- c(day, nrow(subset(unique_imsiraw, number_days >= day)))
    }
    days_usage <- days_usage[order(days_usage$num_days), ]

    x <- list(
        title = "Number of days of Network-Activity"
    )

    y <- list(
        title = "Number of Sims"
    )
    plot_ly(data = days_usage, x = ~num_days, y = ~num_users) %>%
            layout(xaxis = x, yaxis = y, title = 'Number of days of Network-Activity vs Number of SIMs in Indonesia')


    #Metric 2: Difference of days
    days_encounter = data.frame(matrix(nrow = 0, ncol = 2))
    names(days_encounter) <- c('days', 'num_users')
    #days = unique(unique_imsiraw$diff_days)
    days = c(c(1 : 10), c(15, 20, 25, 30, 40, 50, 60, 70)) 

    for(day in days){
        days_encounter[nrow(days_encounter) + 1, ] <- c(day, nrow(subset(unique_imsiraw, diff_days >= day)))
    }
    days_encounter <- days_encounter[order(days_encounter$days), ]

    x <- list(
        title = "Difference of days of last use and first use"
    )

    y <- list(
        title = "Number of Users"
    )
    plot_ly(data = days_encounter, x = ~days, y = ~num_users) %>%
            layout(xaxis = x, yaxis = y, title = 'Difference of days of last use and first use vs number of users')


    #Metric 3 : Ratio
    unique_imsiraw[,ratio:=round(average_usage, 2)]
    #ratios = unique(unique_imsiraw$ratio)
    ratios = seq(min(t), max(t), by = 0.05)
    setkey(unique_imsiraw, average_usage)

    average_usage = data.frame(matrix(nrow = 0, ncol = 2))
    names(average_usage) <- c('ratio', 'num_users')
    
    for(r in ratios){
        r <- as.numeric(r)
        average_usage[nrow(average_usage) + 1, ] <- c(r, nrow(subset(unique_imsiraw, ratio >= r)))
    }
    average_usage <- average_usage[order(average_usage$ratio), ]

     x <- list(
        title = "Ratio of days of activity per difference of days of last use and first use"
    )

    y <- list(
        title = "Number of Users"
    )    

    plot_ly(data = average_usage, x = ~ratio, y = ~num_users) %>%
            layout(xaxis = x, yaxis = y, title = 'Ratio of usage vs number of users')


}
    
#Get local users based on recorded IMSI activity
filter_local_users <- function(imsiraw){
    #imsi raw is expected in a certain format, you have to load the log files using the load file function

     # figure out first time an imsi is observed
    #imsiraw[,first_encounter_with_imsi:=min(accessed_ds),imsi]
    # last time the imsi is observed
    #imsiraw[,last_encounter_with_imsi:=max(accessed_ds),imsi]
    # Count number of days in between
    #imsiraw[, diff_days:=(last_encounter_with_imsi - first_encounter_with_imsi) + 1]

    #Count number of days an IMSI is connected
    times = imsiraw[, .N, by = list(imsi, accessed_ds)][, list(number_days = .N), by = list(imsi)]
    
    #Merging both dataframes
    setkey(imsiraw, imsi)
    setkey(times, imsi)
    imsiraw = merge(imsiraw, times)
    
    #Calculating ratio
    #imsiraw[, average_usage:=number_days/as.integer(diff_days)]   

    #unique_imsiraw = imsiraw[, list(imsi, average_usage, diff_days, first_encounter_with_imsi, last_encounter_with_imsi, number_days, is_auth)]
    #setkey(unique_imsiraw, 'imsi')
    #unique_imsiraw = unique(unique_imsiraw)

    #Local users are those registered on network ie is_auth > 0 or else having an activity of atleast 10 days on the network
    imsiraw = subset(imsiraw, (is_auth > 0) | (number_days >= 10))
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

get_upgrade_matrix <- function(phone_upgrades){
    phone_upgrades$count <- as.numeric(phone_upgrades$count)
    phone_upgrades$percentage <- as.numeric(phone_upgrades$count/(sum(phone_upgrades$count)) * 100)
    m <- as.matrix(dcast(phone_upgrades, to ~ from, value.var = "percentage", fill=0))[, 2:4]
    row.names(m) <- colnames(m)
}