require(data.table)
require(ggplot2)
source('./functions.R')
# Read in the files into a dataframe, this is the untarred, unzipped folder that Shaddi sent out
# remove the 0 byte files, or it will fail
setwd('./imsis/')
files <- list.files(pattern='*.log')

imsiraw <- rbindlist(lapply(files,
                            function(x){fread(x,colClasses=c('character','character','integer','integer','integer'))}))

# setwd('~/R/Projects/Philippines_IMEI_IMSI')

setnames(imsiraw,c('imsi','imei','accessed','created','is_auth'))
imsiraw[,.N,.(imsi,imei)][,.N,imei]

# process the date and time
imsiraw[,created_dt:=as.POSIXct(created, origin="1970-01-01", tz = "GMT")]
imsiraw[,created_ds:=as.Date(created_dt)]
imsiraw[,created_hour:=hour(created_dt)]

imsiraw[,accessed_dt:=as.POSIXct(accessed, origin="1970-01-01", tz = "GMT")]
imsiraw[,accessed_ds:=as.Date(accessed_dt)]
imsiraw[,accessed_hour:=hour(accessed_dt)]
# figure out the weekday
imsiraw[,dayofweek:=weekdays(accessed_ds, abbreviate = TRUE)]
# cast day of the week as factor and order them
imsiraw[,dayofweek:=factor(dayofweek, ordered = TRUE, 
                           levels = c('Sun', 'Sat', 'Fri', 'Thu',  'Wed', 'Tue', 'Mon'))]
# Calculate the check digit
imsiraw[,checkdigit:=checkDigitCalc(imei)]
# create the corrected imei
imsiraw[,imeicorrected:=paste(substr(imei,1,14),checkdigit,sep = '')]
# Add weekend/weekday identifier
imsiraw[,weekend:=FALSE]
imsiraw[dayofweek %in% c('Sun','Sat'),weekend:=TRUE]
# make all is_auth > 0 to be 1 since they are all authorized
imsiraw[is_auth>0,is_auth:=1]
# figure out first time an imei is observed
imsiraw[,first_encounter_with_imei:=min(accessed_ds),imeicorrected]
# figure out first time an imsi is observed
imsiraw[,first_encounter_with_imsi:=min(accessed_ds),imsi]



# Check what % of IMEI before launch did not become a user
imsi_prelaunch<-imsiraw[accessed_ds<'2016-01-30']
imsi_postlaunch<-imsiraw[accessed_ds>='2016-01-30']
# determine which IMEIs were around prelaunch
imsi_postlaunch[,prelaunch:=0]
imsi_postlaunch[imeicorrected%in%imsi_prelaunch[is_auth==0,.N,imeicorrected][,imeicorrected],prelaunch:=1]
# save(imsiraw,imsi_postlaunch,imsi_prelaunch,file="~/R/Projects/Philippines_IMEI_IMSI/imsi.RData")


# load(file="~/R/Projects/Philippines_IMEI_IMSI/imsi.RData")
imsiraw[,TAC:=substr(imeicorrected,1,8)]
# location to the tac DB
tacdb<-fread('/Users/emre/R/Projects/Philippines_IMEI_IMSI/MSRPLUS20161101.txt',colClasses = "character")
imsi_mapped <- merge(imsiraw, tacdb, by="TAC", all.x=TRUE)

imsi_mapped<-imsi_mapped[!is.na(`Marketing Name`)]

imsi_mapped[,.N,.(imeicorrected,`Device Type`)][,.N,.(`Device Type`)]

#get mccmnc list
mccmnc<-readHTMLTable('http://www.imei.info/operator-codes/')
mccmnc<-data.table(mccmnc[[1]])
setnames(mccmnc,c('Code','Operator','Country'))
mccmnc <- mccmnc[Code!='Code']
mccmnc[,Code:=gsub(' ','',Code)]


merge(imsiraw[,.N,.(imsi,Code=substr(imsi,1,5))][,.N,Code][order(-N)],mccmnc,by='Code')[order(-N)]
imsiraw[,.N,.(imeicorrected,is_auth)][,.N,is_auth]

imsi_mapped[,.N,.(imeicorrected,`Device Type`)][,.N,.(`Device Type`)][order(-N)]
imsi_mapped[,.N,.(imeicorrected,`Operating System`)][,.N,.(`Operating System`)][order(-N)]
imsi_mapped[`Device Type`=='Smartphone',.N,.(imeicorrected,Band)][,.N,.(Band)][order(-N)]
imsi_mapped[,lte:='N']
imsi_mapped[grepl('LTE',Band),lte:='Y']
imsi_mapped[lte=='N',.N,Band]
imsi_mapped[,threeG:='N']
imsi_mapped[grepl('CDMA',Band),threeG:='Y']
imsi_mapped[grepl('UMTS',Band),threeG:='Y']
imsi_mapped[grepl('HSDPA',Band),threeG:='Y']
imsi_mapped[grepl('HSUPA',Band),threeG:='Y']
imsi_mapped[grepl('Bluetooth',Band),Bluetooth:='Y']

# newcoming lte capable handsets overtime
lte_ts<-imsi_mapped[,.N,.(year(first_encounter_with_imei),month(first_encounter_with_imei),imeicorrected,lte)][,.N,.(year,month,lte)][order(lte,year,month)]
lte_ts<-dcast.data.table(lte_ts,year+month~lte,fun=sum)
lte_ts[,T:=Y+N]
lte_ts[,Y_perc:=Y/T]
# only authorized users
auth_lte_ts<-imsi_mapped[is_auth==1,.N,.(year(first_encounter_with_imei),month(first_encounter_with_imei),imeicorrected,lte)][,.N,.(year,month,lte)][order(lte,year,month)]
auth_lte_ts<-dcast.data.table(auth_lte_ts,year+month~lte,fun=sum)
auth_lte_ts[,T:=Y+N]
auth_lte_ts[,Y_perc:=Y/T]
# all phones
auth_allphones_lte_ts<-imsi_mapped[is_auth==1,.N,.(year(accessed_ds),month(accessed_ds),imeicorrected,lte)][,.N,.(year,month,lte)][order(lte,year,month)]
auth_allphones_lte_ts<-dcast.data.table(auth_allphones_lte_ts,year+month~lte,fun=sum)
auth_allphones_lte_ts[,T:=Y+N]
auth_allphones_lte_ts[,Y_perc:=Y/T]
# all phones including passer byers
allphones_lte_ts<-imsi_mapped[,.N,.(year(accessed_ds),month(accessed_ds),imeicorrected,lte)][,.N,.(year,month,lte)][order(lte,year,month)]
allphones_lte_ts<-dcast.data.table(allphones_lte_ts,year+month~lte,fun=sum)
allphones_lte_ts[,T:=Y+N]
allphones_lte_ts[,Y_perc:=Y/T]

# newcoming three G capable handsets overtime
threeG_ts<-imsi_mapped[,.N,.(year(first_encounter_with_imei),month(first_encounter_with_imei),imeicorrected,threeG)][,.N,.(year,month,threeG)][order(threeG,year,month)]
threeG_ts<-dcast.data.table(threeG_ts,year+month~threeG,fun=sum)
threeG_ts[,T:=Y+N]
threeG_ts[,Y_perc:=Y/T]

# some misc cuts
imsi_mapped[,.N,.(TAC,`Manufacturer (or) Applicant`)][,.N,`Manufacturer (or) Applicant`][order(-N)]
imsi_mapped[,.N,.(TAC,`Marketing Name`)][,.N,`Marketing Name`][order(-N)]
imsi_mapped[,.N,.(TAC,`Brand Name`)][,.N,`Brand Name`][order(-N)]
imsi_mapped[,.N,.(TAC,`Model Name`)][,.N,`Model Name`][order(-N)]
imsi_mapped[,.N,.(TAC,`Device Type`)][,.N,`Device Type`][order(-N)]
imsi_mapped[,.N,.(TAC,`Operating System`)][,.N,`Operating System`][order(-N)]

