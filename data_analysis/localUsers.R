suppressMessages(require(data.table))
suppressMessages(require(ggplot2))
suppressMessages(require(hashmap))
source('./functions.R')

setwd('./imsis/')
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

#Checkdigit
#imsiraw[,checkdigit:=checkDigitCalc(imei)]
# create the corrected imei
#imsiraw[,imeicorrected:=paste(substr(imei,1,14),checkdigit,sep = '')]

# figure out first time an imsi is observed
imsiraw[,first_encounter_with_imsi:=min(accessed_ds),imsi]
# last time the imsi is observed
imsiraw[,last_encounter_with_imsi:=max(accessed_ds),imsi]
# Count number of days in between
imsiraw[, days_encounter:=(imsiraw$last_encounter_with_imsi - imsiraw$first_encounter_with_imsi)]
#Count number of times connected
times = imsiraw[, .N, keyby=list(accessed_ds, imsi)][, .N, by = imsi]
#Merging both dataframes
setkey(imsiraw, imsi)
setkey(times, imsi)
imsiraw = merge(imsiraw, times)
#Calculating ratio
imsiraw[, average_usage:=N/as.integer(days_encounter)]