source('./functions.R')
source('./phoneAdoption.R')
require(reshape2)

#Figure out type of device
compute_specs <- function(imsi_mapped){
    imsi_mapped[, network_support:='2G']
    imsi_mapped[grepl('CDMA',Band, ignore.case = T), network_support:='3G']
    imsi_mapped[grepl('UMTS',Band, ignore.case = T), network_support:='3G']
    imsi_mapped[grepl('HSDPA',Band, ignore.case = T), network_support:='3G']
    imsi_mapped[grepl('HSUPA',Band, ignore.case = T), network_support:='3G']
    imsi_mapped[grepl('WCDMA',Band, ignore.case = T), network_support:='3G']
    imsi_mapped[grepl('HSPA+',Band, ignore.case = T), network_support:='3G']
    imsi_mapped[grepl('LTE', Band, ignore.case = T), network_support:='4G']

    return(imsi_mapped)
}

#Computing online hours of feature, 3g and 4g phones.
compute_phones_online <- function(imsi_mapped){
    
    average_daily_phone_usage <- imsi_mapped[, list(daily_mean_hours = mean(times_accessed_per_day)), by = list(accessed_ds, accessed_month, accessed_year,
                                                           network_support)][order(accessed_ds)]
    average_monthly_phone_usage <- average_daily_phone_usage[, list(monthly_mean_hours = mean(daily_mean_hours)),
        by = list(accessed_year, accessed_month, network_support)]

    num_months <- nrow(unique(average_monthly_phone_usage[, list(accessed_month, accessed_year)]))

    average_yearly_phone_usage <- average_monthly_phone_usage[, list(yearly_hours = sum(monthly_mean_hours)), by = list(network_support)]
    average_yearly_phone_usage[,yearly_mean_hours:=yearly_hours/num_months]

    average_monthly_phone_usage[, accessed_ds:=as.Date(paste(1, accessed_month, accessed_year, sep = '-'), format = '%d-%m-%Y')]

    write.csv(average_monthly_phone_usage, 'paper_plots/average_daily_online_hours_year.csv', row.names = F)

    #average_monthly_phone_usage[, accessed_ds:= paste(accessed_month, accessed_year, sep = '-')]
    #average_monthly_phone_usage$accessed_ds = as.Date(as.yearmon(average_monthly_phone_usage$accessed_ds, format = '%m-%Y'))

    average_monthly_phone_usage <- average_monthly_phone_usage[, c('accessed_ds', 'monthly_mean_hours', 'network_support')]
    average_monthly_phone_usage <- dcast(average_monthly_phone_usage, accessed_ds ~ network_support, value.var = 'monthly_mean_hours')

    p <- plot_ly(average_monthly_phone_usage, x = ~accessed_ds, y = ~`2G`, type = 'bar', name = '2G', marker = list(color = '#d95f02')) %>%
            add_trace(y = ~`3G`, name = '3G', marker = list(color = '#1b9e77')) %>%
            add_trace(y = ~`4G`, name = '4G', marker = list(color = '#7570b3')) %>%
            layout(yaxis = list(title = 'Average Monthly activity', showgrid = FALSE, tickfont = list(size = 35), showline = T, titlefont = list(size =35)),
             xaxis = list(title = "", tickfont = list(size = 35)),
             barmode = 'group', legend = list(font = list(size = 32)), 
             titlefont = list(size = 40), margin = list(l = 80))

    layout(yaxis = list(title = 'Percentage of Phones', showline = TRUE, showgrid = FALSE, 
        titlefont = list(size =35), tick0 = 0, tickfont = list(size = 35)), barmode = 'group',
       xaxis = list(title = "", tickfont = list(size = 35), tick0 = 0, dtick = 0), 
        legend = list(x = 0.75, y = 1, font = list(size = 32)), title = 'Philippines (April 2016 onwards)', 
        titlefont = list(size = 40), margin = list(t = 150, l = 150))

    p <- plot_ly(average_monthly_phone_usage, x = ~network_support, y = ~monthly_mean_hours, type = 'bar', 
        marker = list(color = c('rgb(244,125,124)', 'rgb(153,230,167)',
                                'rgb(149,204,240)'))) %>%
            layout(yaxis = list(title = 'Average hours'), xaxis = list(title = 'Type of Device'),
             barmode = 'group', title= 'Average daily hours of community members in Philippines (April 2016 onwards)')

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
    #                       levels = c('Sun', 'Sat', 'Fri', 'Thu',  'Wed', 'Tue', 'Mon'))]
    # Add weekend/weekday identifier
    #imsiraw[,weekend:=FALSE]
    #imsiraw[dayofweek %in% c('Sun','Sat'),weekend:=TRUE]

    imsiraw[ ,tac:=substr(imeicorrected, 1, 8)]
    #Having the same time for PH and ID
    #imsiraw = subset(imsiraw, accessed_ds >= as.Date('2016-04-11', format = '%Y-%m-%d'))
    
    #tacdb <- fread('tac_mapping.csv', colClasses = 'character')
    tacdb<-fread('MSRPLUS20161101.txt', colClasses = "character")
    imsi_mapped <- merge(imsiraw, tacdb, by.x="tac", by.y = 'TAC', all.x=TRUE)
    
    imsi_mapped <- compute_specs(imsi_mapped)
    imsi_mapped[ ,fname:=paste(`Brand Name`, `Model Name`, `Marketing Name`)]

    #turk <- fread('mturk_results.csv', colClasses = "character")
    #imsi_mapped <- merge(imsi_mapped, turk, by.x="fname", by.y = "fname", all.x=TRUE, allow.cartesian = T)
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

    p <- ggplot(data = days_usage, aes(num_days, num_users)) + geom_point(color = 'blue') + 
      labs(x = "Number of days in network", y = 'Number of users') 

    ggplotly(p)

 
    plot_ly(data = days_usage, x = ~num_days, y = ~num_users) %>%
            layout(xaxis = x, yaxis = y) #title = 'Number of days of Network-Activity vs Number of SIMs in Indonesia')


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
    imsiraw = merge(imsiraw, times, all.x = TRUE)
    
    #Same time as experiment
    #imsiraw <- subset(imsiraw, accessed_ds > as.Date('04-01-2016', format = '%m-%d-%Y'))

    #Calculating ratio
    #imsiraw[, average_usage:=number_days/as.integer(diff_days)]   

    #unique_imsiraw = imsiraw[, list(imsi, tac, number_days, is_auth, year)]
    #setkey(unique_imsiraw, 'imsi')
    #unique_imsiraw = unique(unique_imsiraw)

    #unique_devices = imsiraw[, list(tac, fname)]
    #setkey(unique_devices, tac)
    #unique_devices <- unique(unique_devices)
    #    imsi_mapped <- merge(unique_devices, turk, by.x="fname", by.y = "fname", all.x=TRUE, allow.cartesian = T)


    #Local users are those registered on network ie is_auth > 0 or else having an activity of atleast 10 days on the network
    imsiraw = imsiraw[(is_auth > 0) | (number_days >= 10), ]
    return(imsiraw)
}

#Get local users based on recorded IMSI activity
filter_non_local_users <- function(imsiraw){
    #imsi raw is expected in a certain format, you have to load the log files using the load file function

    #Count number of days an IMSI is connected
    times = imsiraw[, .N, by = list(imsi, accessed_ds)][, list(number_days = .N), by = list(imsi)]
    
    #Merging both dataframes
    setkey(imsiraw, imsi)
    setkey(times, imsi)
    imsiraw = merge(imsiraw, times)
    
    #Local users are those registered on network ie is_auth > 0 or else having an activity of atleast 10 days on the network
    imsiraw = subset(imsiraw, (is_auth == 0) & (number_days < 10))
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

compute_phone_upgrade <- function(IMSI, imsiraw, sim_activity, phone_upgrades, community_transfer, upgrade_age, no_year){
    all_imei = unique(sim_activity$imeicorrected)
    current_type = NA
    current_age = NA
    first_phone = T
    l <- list()

    for(IMEI in all_imei){
        TAC = substr(IMEI, 1, 8)

        ct.row <- which((community_transfer$imsi == IMSI) & (community_transfer$imeicorrected == IMEI))
        if(length(ct.row) == 0){
            community_transfer[nrow(community_transfer) + 1, ] <- c(IMSI, compute_transfer_community(sim_activity, imsiraw, IMEI))
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

                current_date = as.character(imsiraw[tac == TAC & imeicorrected == IMEI, ]$accessed_ds[1])
                #This gives us the manufacturing year from the turk task undertaken
                phone_year = as.character(imsiraw[tac == TAC, ]$year[1])
                full_name = as.character(imsiraw[tac == TAC, ]$fname[1])
                company_name = as.character(imsiraw[tac == TAC, ]$`Manufacturer (or) Applicant`[1])


                if(length(phone_year) == 0 | is.na(phone_year) | phone_year == 'not_found'){
                    if(current_type == '2G' & new_type == '3G'){
                        no_year[nrow(no_year) + 1, ] <- c(full_name, TAC, company_name)
                    }
                }
                else{

                    if(current_type != FALSE & new_type != FALSE){
                        u = paste(current_type, new_type, sep = '_')
                        phone_year = as.POSIXct(phone_year, format = '%m/%d/%Y')
                        diff <- year(current_date) - year(phone_year)
                        upgrade_age[[u]][[length(upgrade_age[[u]]) + 1]] <- diff
                    }
                }
                
                current_type = new_type
            }
        } 
    }

    l$phone_upgrades <- phone_upgrades
    l$community_transfer <- community_transfer
    l$no_year <- no_year
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
compute_transfer_community <- function(sim_activity, imsiraw, IMEI){
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

#Script to understand the rate of phones being shared during the experiment
shared_phones <- function(shared_imsi, imsiraw){
    shared_stats <- subset(imsiraw, imsi %in% shared_imsi)
    plot_normalized_types(shared_stats, 'Types of devices shared in Philippines')
}

#Script to compute ttest of number of online hours in Philippines
compute_ttest_online_hours <- function(imsiraw){
    average_daily_phone_usage <- imsiraw[, list(daily_mean_hours = mean(times_accessed_per_day)), by = list(accessed_ds, 
        network_support)][order(accessed_ds)]

    twoG_hours <- as.data.frame(average_daily_phone_usage[network_support == '2G', c('daily_mean_hours')])
    colnames(twoG_hours) <- c('hours')
    twoG_hours$hours <- round(twoG_hours$hours, 2)

    h_2g <- ggplot(twoG_hours, aes(x = hours)) +
            geom_histogram(aes(y = ..density..), fill="white", binwidth = .2, colour = '#ff7271') +
            stat_function(fun = dnorm, 
                            args = list(mean = mean(twoG_hours$hours), sd = sd(twoG_hours$hours)), 
                            lwd = 2, 
                            col = '#ff7271') + 
            labs(title="Distribution of average daily 2G hours",
                y="Count of hours", x = 'Hours') 

    ggplotly(h_2g)

    threeG_hours <- as.data.frame(average_daily_phone_usage[network_support == '3G', 'daily_mean_hours']$daily_mean_hours)
    colnames(threeG_hours) <- c('hours')
    threeG_hours$hours <- round(threeG_hours$hours, 2)

    h_3g <- ggplot(threeG_hours, aes(x = hours)) +
            geom_histogram(aes(y = ..density..), fill="white", binwidth = .2, colour = '#99e6a7') +
            stat_function(fun = dnorm, 
                            args = list(mean = mean(threeG_hours$hours), sd = sd(threeG_hours$hours)), 
                            lwd = 2, 
                            col = '#99e6a7') + 
            labs(title="Distribution of average daily 3G hours",
                y="Count of hours", x = 'Hours') 
    ggplotly(h_3g)

    fourG_hours <- as.data.frame(average_daily_phone_usage[network_support == '4G', 'daily_mean_hours']$daily_mean_hours)
    colnames(fourG_hours) <- c('hours')
    fourG_hours$hours <- round(fourG_hours$hours, 2)

    h_4g <- ggplot(fourG_hours, aes(x = hours)) +
            geom_histogram(aes(y = ..density..), fill="white", binwidth = .2, colour = '#95ccf0') +
            stat_function(fun = dnorm, 
                            args = list(mean = mean(fourG_hours$hours), sd = sd(fourG_hours$hours)), 
                            lwd = 2, 
                            col = '#95ccf0') + 
            labs(title="Distribution of average daily 4G hours",
                y="Count of hours", x = 'Hours') 

    ggplotly(h_4g)

    #    stat_function(fun=qnorm, args=list(mean=mean(twoG_hours$hours), sd=sd(twoG_hours$hours)))

    #threeG_hours <- as.vector(average_daily_phone_usage[network_support == '3G', 'daily_mean_hours']$daily_mean_hours)
    #fourG_hours <- as.vector(average_daily_phone_usage[network_support == '4G', 'daily_mean_hours']$daily_mean_hours)

    #h <- hist(g, breaks=10, density=10, col='#ff7271', xlab="Accuracy", main="Overall") 
    #xfit<-seq(min(g),max(g),length=40) 
    #yfit<-dnorm(xfit,mean=mean(g),sd=sd(g)) 
    #yfit <- yfit*diff(h$mids[1:2])*length(g) 
    #lines(xfit, yfit, col="black", lwd=2)
}

regression_model <- function(imsiraw){
    #Getting unique phones per month
    phone_count <- imsiraw[, list(accessed_month, accessed_year, network_support, imsi, imeicorrected), ]
    phone_count <- unique(phone_count)

    #Aggregating the monthly count
    phone_count <- phone_count[, list(count = .N), by = list(accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
    phone_count[,date:=paste(1, accessed_month, accessed_year, sep = '-')]
    phone_count$date <- as.Date(phone_count$date, format = '%d-%m-%Y')

    phone_count <- phone_count[date < as.Date('2017-02-01'), ]

    twoG_count <- as.data.frame(phone_count[network_support == '2G', c('accessed_ds', 'count')])
    colnames(twoG_count) <- c('date', 'count')
    twoG_count$date <- as.Date(twoG_count$date, format = '%Y-%m-%d')

    twoG_count$num_date <- as.numeric(twoG_count$date)
    fit_2G <- lm(twoG_count, formula = 'count ~ num_date + I(num_date^2)')
    fitted.value <- fitted(fit_2G)

    p <- ggplot(twoG_count, aes(x = num_date)) + 
        geom_line(aes(y = count), color = "#ff7271") + 
        geom_line(mapping = aes(y = fitted.value)) + 
        labs(title="Growth of 2G Phones in Philippines",
            y="Number of devices", x = 'Date')

    ggplotly(p)

    threeG_count <- as.data.frame(phone_count[network_support == '3G', c('accessed_ds', 'count')])
    colnames(threeG_count) <- c('date', 'count')
    threeG_count$date <- as.Date(threeG_count$date, format = '%Y-%m-%d')

    threeG_count$num_date <- as.numeric(threeG_count$date)
    fit_3G <- lm(threeG_count, formula = 'count ~ num_date + I(num_date^2)')
    fitted.value <- fitted(fit_3G)

    p <- ggplot(threeG_count, aes(x = num_date)) + 
        geom_line(aes(y = count), color = "#008000") + 
        geom_line(mapping = aes(y = fitted.value)) + 
        labs(title="Growth of 3G Phones in Philippines",
            y="Number of devices", x = 'Date')

    ggplotly(p)

    fourG_count <- as.data.frame(phone_count[network_support == '4G', c('date', 'count')])
    #colnames(fourG_count) <- c('date', 'count')
    #fourG_count$date <- as.Date(fourG_count$date, format = '%Y-%m-%d')

    fourG_count$num_date <- as.numeric(fourG_count$date)
    #fit_4G <- lm(fourG_count, formula = 'count ~ num_date + I(num_date^2) + I(num_date^3)')
    fit_quad_4G <- lm(fourG_count, formula = 'count ~ num_date + I(num_date^2)')
    fit_lr_4G <- lm(fourG_count, formula = 'count ~ num_date')
    #fitted_quad.value <- fitted(fit_quad_4G)

    df.prediction <- data.frame(matrix(0, nrow = nrow(fourG_count) + 29, ncol = 2))
    names(df.prediction) <- c('date', 'num_date')

    df.prediction[1:nrow(fourG_count), ] <- fourG_count[, c('date', 'num_date')]
    last_date <- fourG_count[nrow(fourG_count), 'date']

    #seq(from=as.Date(last_date), to=as.Date("2018-12-01"),by='month' )
    df.prediction[nrow(fourG_count):nrow(df.prediction), 'date'] <- seq(from=as.Date(last_date) ,by='month', length.out = 30)
    df.prediction$num_date <- as.numeric(df.prediction$date)
    df.prediction$date <- as.Date(df.prediction$date)

    quad_prediction <- predict(fit_quad_4G, df.prediction, interval = 'confidence')
    lr_prediction <- predict(fit_lr_4G, df.prediction, interval = 'confidence')
    df.prediction$quad_fit <- quad_prediction[,1]
    df.prediction$quad_lwr <- quad_prediction[,2]
    df.prediction$quad_upr <- quad_prediction[,3]
    df.prediction$actual_count <- NA
    df.prediction[1:nrow(fourG_count), 'actual_count'] <- fourG_count$count
    df.prediction$lr_fit <- lr_prediction[,1]
    df.prediction$lr_lwr <- lr_prediction[,2]
    df.prediction$lr_upr <- lr_prediction[,3]
    #p <- ggplot(fourG_count, aes(x.num_date, y.count)) + #geom_line(aes(y = count), color = "#3399FF") + 
    #labs(title="Growth of 4G Phones in Philippines",
    #            y="Number of devices", x = 'Date') + 
    #    geom_smooth(method = 'lm', formula = 'count ~ num_date + I(num_date^2)')

    ggplot(fourG_count, aes(x = date)) + geom_line(aes(y = count))

    p <- ggplot(df.prediction, aes(x = date)) + 
            geom_line(aes(y = quad_fit, colour = 'Predicted 4G growth'), size = 1) + 
            geom_line(aes(y = quad_lwr, color = "95% confidence interval")) +
            geom_line(aes(y = quad_upr), color = "grey") + 
            geom_line(aes(y = actual_count, colour = 'Monthly active users'), size = 1) + 
            geom_linerange(aes(ymin = 340, ymax = 260, x = as.Date('2019-05-01'), color = 'Estimated Market Viability')) +
            #geom_line(aes(y = lr_fit, colour = 'Predicted 4G growth (Linear)')) + 
            scale_x_date(date_breaks = '8 month', labels=date_format("%b-%y"), expand = c(0, 0)) + 
            scale_color_manual(NULL, values=c(`Predicted 4G growth`="black", "95% confidence interval" = 'grey',
             `Monthly active users` = "#3399FF", `Estimated Market Viability` = 'blue')) +  
            labs(y="Active 4G devices per month") + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), 
              axis.title.x=element_blank(), legend.position="top", axis.title.y=element_text(size = 25), 
              axis.text.x = element_text(size = 25), axis.text.y = element_text(size = 25))

    plt <- ggplotly(p) %>%
            layout(legend = list(x = 0.1, y = 0.5, font = list(size = 30)), margin = list(l = 120)) #%>%
            #add_trace(x = c(as.Date('2019-05-01'), as.Date('2019-05-01')), y= c(260, 340), mode = "lines")

    plt


    #p <- ggplot(fourG_count, aes(x = date)) + 
    #    geom_line(aes(y = count), color = "#3399FF") + 
    #    geom_line(mapping = aes(y = fitted)) + 
    #    labs(title="Growth of 4G Phones in Philippines",
    #        y="Number of devices", x = 'Date')

    twoG_ts <- xts(twoG_count[,-1], order.by=twoG_count[,1])
    x.ts = ts(twoG_ts, freq=365, start=c(2016, 305))

    #ts(twoG_count$hours, frequency = 365.25, start =  c(2016, 4, 11))
    #t <- msts(twoG_count$count, seasonal.periods=c(7,365.25), start = c(2016, 4 ,11))
    #t <- ts(twoG_count$count, frequency = 365.25, start =  c(2016, 4))

    fourG_count$num_date <- as.numeric(fourG_count$date)
    fourG_count$num_d2 <- fourG_count$num_date * fourG_count$num_date 
    fit_4G <- lm(fourG_count, formula = 'count ~ date + d2')
    test_df <- fourG_count[, c('num_date', 'num_d2')]
    fg_predictions <- predict(fit_4G, test_df, interval="predict") 

    #last_date <- test_df[nrow(test_df), 'date']
    #next_year <- c(last_date : (last_date + 365))
    #new_test_df <- data.frame(date = next_year, d2 = next_year * next_year)
    #test_df <- rbind(test_df, new_test_df)
    #predict(fit_4G, test_df, interval="predict") 
}

bar_aggregated <- function(local, non_local){
    local_agg <- local[, list(imei, network_support), ]
    local_agg <- unique(local_agg)
    local_agg <- local_agg[, list(ratio = .N/nrow(local_agg) * 100), by = list(network_support)]
    local_agg$community <- 'Local'

    non_local_agg <- non_local[, list(imei, network_support), ]
    non_local_agg <- unique(non_local_agg)
    non_local_agg <- non_local_agg[, list(ratio = .N/nrow(non_local_agg) * 100), by = list(network_support)]
    non_local_agg$community <- 'Non Local'

    agg <- data.frame(matrix(nrow = 0, ncol = 4))
    colnames(agg) <- c('community', '2G', '3G', '4G')

    agg[1, 'community'] <- 'Local'
    agg[1, '2G'] <- as.numeric(local_agg[network_support == '2G', 'ratio'])
    agg[1, '3G'] <- as.numeric(local_agg[network_support == '3G', 'ratio'])
    agg[1, '4G'] <- as.numeric(local_agg[network_support == '4G', 'ratio'])

    agg[2, 'community'] <- 'Non Local'
    agg[2, '2G'] <- as.numeric(non_local_agg[network_support == '2G', 'ratio'])
    agg[2, '3G'] <- as.numeric(non_local_agg[network_support == '3G', 'ratio'])
    agg[2, '4G'] <- as.numeric(non_local_agg[network_support == '4G', 'ratio'])


    p <- plot_ly(agg, x = ~community, y = ~`2G`, type = 'bar', name = '2G', marker = list(color = '#d95f02')) %>%
      add_trace(y = ~`3G`, name = '3G', marker = list(color = '#1b9e77')) %>%
      add_trace(y = ~`4G`, name = '4G', marker = list(color = '#7570b3') ) %>%
      layout(yaxis = list(title = 'Percentage of Phones', showline = TRUE, showgrid = FALSE, 
        titlefont = list(size =35), tick0 = 0, tickfont = list(size = 35)), barmode = 'group',
       xaxis = list(title = "", tickfont = list(size = 35), tick0 = 0, dtick = 0), 
        legend = list(x = 0.75, y = 1, font = list(size = 32)), title = 'Philippines (April 2016 onwards)', 
        titlefont = list(size = 40), margin = list(t = 150, l = 150))

      layout(legend = list(x = 0.1, y = 0.5, font = list(size = 22)), margin = list(a = 250)) #%>%

      layout(legend = list(x = 0.1, y = 0.9))

    ggplot(agg, aes(community, network_support)) +
    geom_bar(aes(ratio), width = 0.4, position = position_dodge(width=0.4), 
        stat="identity") +  
    theme(legend.position="right", legend.title = element_blank(),
    axis.title.x = element_blank(), panel.background = element_blank(), 
    panel.grid.minor = element_blank()) + 
    labs(y = "Number of devices")

    ggplot(agg, aes(community, network_support)) + geom_bar(aes(fill = ratio), position = "dodge", stat="identity") + 
    scale_fill_manual("legend", values = c("A" = "black", "B" = "orange", "C" = "blue"))

    twoG <- data.frame(matrix(0, nrow = 0, ncol = 2))
    colnames(twoG) <- c('type', 'ratio')
    twoG[nrow(twoG) + 1, ] <- c('local', as.numeric(local_agg[network_support == '2G', 'ratio']))
    twoG[nrow(twoG) + 1, ] <- c('non-local', as.numeric(non_local_agg[network_support == '2G', 'ratio']))

    threeG <- data.frame(matrix(0, nrow = 0, ncol = 2))
    colnames(threeG) <- c('type', 'ratio')
    threeG[nrow(threeG) + 1, ] <- c('local', as.numeric(local_agg[network_support == '3G', 'ratio']))
    threeG[nrow(threeG) + 1, ] <- c('non-local', as.numeric(non_local_agg[network_support == '3G', 'ratio']))

    fourG <- data.frame(matrix(0, nrow = 0, ncol = 2))
    colnames(fourG) <- c('type', 'ratio')
    fourG[nrow(fourG) + 1, ] <- c('local', as.numeric(local_agg[network_support == '4G', 'ratio']))
    fourG[nrow(fourG) + 1, ] <- c('non-local', as.numeric(non_local_agg[network_support == '4G', 'ratio']))

    plot_ly(threeG, x = ~type, y = ~ratio, type = 'bar', 
        marker = list(color = c('#7fbf7b'))) %>%
            layout(yaxis = list(title = 'Average hours'), xaxis = list(title = 'Type of Device'),
             barmode = 'group', title= 'Types of devices between Local and Non-Local users')
}