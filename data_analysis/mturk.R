library('data.table')
library('zoo')

task <- fread('Batch_2777334_batch_results.csv')
task <- task[, c(28:36), ]

incomplete <- subset(task, Answer.unavail == 'on')
task <- subset(task, Answer.unavail != 'on')
#Some tasks which were marked incomplete did contain year of manufacture
r <- subset(incomplete, Answer.year != '{}')
#Concatenating both datatables
task <- rbind(task, r, fill = T)
#Some complete tasks contain null in years
task <- subset(task, Answer.year != '{}')

task[ ,fname:=paste(Input.brand, Input.model, Input.marketing)]
task$Answer.year <- tolower(task$Answer.year)

#complete.sub$Answer.4g <- tolower(complete.sub$Answer.4g)
#complete.sub$Answer.gsmarena <- tolower(complete.sub$Answer.gsmarena)
#complete.sub$Answer.os <- tolower(complete.sub$Answer.os)
#complete.sub$Answer.screen_size <- tolower(complete.sub$Answer.screen_size)

incorrect = data.frame(matrix(0, nrow = 0, ncol = 3))
correct = data.frame(matrix(0, nrow = 0, ncol = length(task)))
names(correct) <- names(task)

for(name in unique(task$fname)){
  res = subset(task, fname == name)
  res.year <- res$Answer.year

  if(length(unique(res.year)) != 1){
    incorrect[nrow(incorrect) + 1, ] <- c(res$Input.brand[1], res$Input.model[1],
                                        res$Input.marketing[1])
  }
  else{
    correct[nrow(correct) + 1, ] <- res[1, ]
  }
}

#Merge tac database with the correct values filtered
merge_tacdb <- function(correct){
    tacdb<-fread('MSRPLUS20161101.txt',colClasses = "character")
    tacdb[ ,fname:=paste(`Brand Name`, `Model Name`, `Marketing Name`)]
    setkey(tacdb, fname)

    correct <- data.table(correct)
    setkey(correct, fname)
    correct <- merge(correct, tacdb, by.x="fname", by.y = 'fname', all.x=TRUE)
    years <- correct[, c('TAC', 'Answer.year', 'fname')]
    years <- data.table(years)
    names(years) <- c('TAC', 'year', 'fname')
    setkey(years, TAC, year)
    years <- unique(years)
    return(years)
}

#Clean years from this data frame of years
clean_years <- function(years){

    #Removing ,.
    years$year <- gsub(',', '', years$year)
    years$year <- gsub('\\.', '', years$year)

    #First parse: month-year format
    years[ ,modified:=as.yearmon(years$year, format = '%b-%y')]

    #Second parse: only year
    years[is.na(modified), modified:=as.yearmon(as.Date(year, format = '%Y'))]

    #If there's a term 'discontinued' - replace it with NA
    years[grepl('discontinued', year), modified:=NA]

    #If there's a term 'cant find' - replace it with NA
    years[grepl('can\'t find', year), modified:=NA]

    #For elements which have prefixes like 'realeased in'
    t <- strsplit(years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), year], ' ')
    ele <- lapply(t, tail, n = 2L)
        
    for(i in seq_along(ele)){
        ele[[i]] <- paste(ele[[seq_along(ele)[i]]], collapse = ' ')
    }
    ele <- unlist(ele)
    years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), year:=ele]
    
    #Year month format
    years[is.na(modified), modified:=as.yearmon(as.Date(year, format = '%Y %B'))]
    years[is.na(modified), modified:=as.yearmon(year, format = '%Y %B')]

    #Remove leading and trailing white space
    formatted = trimws(years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), year], 
        which = c("both"))
    years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), year:=formatted]
    
    #MonthYear (without space)
    years[is.na(modified), modified:=as.yearmon(year, format = '%b%Y')]

    #replace o with 0
    replaced <- gsub('o', '0', years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), year])
    years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), year:=replaced]
    #mm/year format
    replacement <- as.yearmon(years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), year]
        , format = '%m/%Y')
    years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), modified:=replacement]

    #years[is.na(modified) & !grepl('discontinued', year, ignore.case = T) & !grepl("can't find", year, ignore.case = T), year]
    return(years)
}

correct <- data.table(correct)
correct <- correct[, c('fname', 'Answer.year'), ]
names(correct) <- c('fname', 'year')

years <- clean_years(correct)
years$modified <- as.character(years$modified)
years[is.na(modified), modified:='not_found']

task <- task1[, c('fname', 'modified'), ]
names(task) <- c('fname', 'year')

write.csv(years, 'tac_results_1.csv', row.names = F)


##################### This is for the second parse #####################################

task2 <- fread('mturk_parse_2.csv', colClasses = 'character')

task2[ ,fname:=paste(Input.brand, Input.model, Input.marketing)]
task2$Answer.year <- tolower(task2$Answer.year)

task2 <- task2[, c('fname', 'Answer.year'), ]

#Replacing non-phone devices with the term incorrect
task2[grepl('not a phone', Answer.year, ignore.case = T), Answer.year:='incorrect']
task2[grepl('VHF', Answer.year, ignore.case = T), Answer.year:='incorrect']
task2[grepl('HBV', Answer.year, ignore.case = T), Answer.year:='incorrect']
#Replacing values not_found
task2[grepl('no', Answer.year, ignore.case = T), Answer.year:='not_found']
task2[grepl('only', Answer.year, ignore.case = T), Answer.year:='not_found' ]
#Filter empty values
task2 <- subset(task2, Answer.year != '{}')

#task2 <- merge_tacdb(task2)
names(task2) <- c('fname', 'year')
task2 <- unique(task2)
task2 <- clean_years(task2)

replace_0 <- gsub('0', 'o', task2[is.na(modified), year])
task2[is.na(modified), year:=replace_0]

task2$modified <- as.character(task2$modified)
task2[year == 'incorrect', modified:='not_phone']
task2[year == 'not_found', modified:='not_found']

task2 <- task2[, c('fname', 'modified'), ]
names(task2) <- c('fname', 'year')

write.csv(task2, 'tac_results_2.csv', row.names = F)

########################### Task 3 #####################################

task3 <- fread('mturk_parse_3.csv', colClasses = 'character')
task3[ ,fname:=paste(Input.brand, Input.model, Input.marketing)]
task3$Answer.year <- tolower(task3$Answer.year)
task3 <- task3[, c('fname', 'Answer.year'), ]

task3 <- subset(task3, Answer.year != '{}')
task3 <- subset(task3, Answer.year != '')
task3 <- subset(task3, Answer.year != ' ')

task3[grepl('not a phone', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('GPS', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('USB', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('modem', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('router', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('module', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('lawnmower', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('batteries', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('USB', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('camera', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('speaker', Answer.year, ignore.case = T), Answer.year:='incorrect']
task3[grepl('remote', Answer.year, ignore.case = T), Answer.year:='incorrect']

task3[grepl('not found', Answer.year, ignore.case = T), Answer.year:='not_found']
task3[grepl('info found', Answer.year, ignore.case = T), Answer.year:='not_found']

#months <- month.abb[1:12]
#months = paste(months, collapse = '|')

ym <- task3[grepl(months, Answer.year, ignore.case = T), ]
f1 <- task3[!grepl(months, Answer.year, ignore.case = T), ]
f2 <- f1[!Answer.year == 'not_found', ]
f3 <- f2[!Answer.year == 'incorrect']
y <- f3[which(!is.na(as.numeric(f3$Answer.year))), ]
ym <- rbind(ym, y)
#
f3 <- f3[which(is.na(as.numeric(f3$Answer.year))), ]
f3[,Answer.year:='incorrect']
#
task3 <- rbind(ym, f3)

#task3 <- merge_tacdb(task3)
names(task3) <- c('fname', 'year')
task3 <- unique(task3)
task3 <- clean_years(task3)

replace_0 <- gsub('0', 'o', task3[is.na(modified), year])
task3[is.na(modified), year:=replace_0]

task3$modified <- as.character(task3$modified)
task3[year == 'incorrect', modified:='not_phone']
task3[year == 'not_found', modified:='not_found']
task3[is.na(modified), modified:='not_phone']

task3 <- task3[, c('fname', 'year'), ]
names(task3) <- c('fname', 'year')

#write.csv(task3, 'tac_results_3.csv', row.names = F)

tasks <- rbind(task, task2, task3)
tasks <- unique(tasks)

names <- setdiff(unique(tasks$fname), NA)

i = 0
for(name in names){
    rec <- subset(tasks, fname == as.character(name))
    i = i + 1

    if(i == 10){
        break
    }

    if(nrow(rec) != 1){

        print(rec)
        #nrec <- subset(rec, year != 'not_found' & year != 'incorrect')
        #if(nrow(nrec) != 1){
        #    print(nrec)
        #    break
        #}
    }
}

tasks[year == 'incorrect', year:='not_phone']
nf <- tasks[!grepl(months, year, ignore.case = T), ]

ym <- tasks[grepl(months, year, ignore.case = T), ]
ym <- ym[grepl('2017 may (expected)', year, ignore.case = T), year:='2017 may']
ym <- ym[grepl('2009 august | 2008 july', year, ignore.case = T), year:='2008 July']

change <- as.yearmon(ym$year, format = c('%b %Y'))
ym[,modified:=change]
reverse_change <- as.yearmon(ym[is.na(modified), year], format = c('%Y %b'))
ym[is.na(modified), modified:=reverse_change]
ym <- ym[, c('fname', 'modified'), ]
ym <- unique(ym)

ym$modified <- year(ym$modified)
ym <- unique(ym)
names(ym) <- c('fname', 'year')

ym <- rbind(ym, nf)

#2009 august | 2008 july

##Checking for unique ym

for(name in unique(ym$fname)){
    rec <- subset(ym, fname == name)

    if(nrow(rec) != 1){
        if('not_phone' %in% rec$year | 'not_found' %in% rec$year){
            y <- rec[!grepl('not_found|not_phone', year), year]
            #y <- year(as.yearmon(y, format = '%Y'))
            y <- as.numeric(y)
            ym[fname == name, year:=y]
        }
    }
}

ym <- unique(ym)

#Taking true value from 3rd task
for(name in unique(ym$fname)){
    rec <- subset(ym, fname == name)

    if(nrow(rec) != 1){
        
        tv <- subset(task3, fname == name)
        yr <- tv$year

        if(length(yr) > 0){
            ym[fname == name, year:=yr]
        }
    }
}

ym <- unique(ym)

for(name in unique(ym$fname)){
    rec <- subset(ym, fname == name)

    if(nrow(rec) != 1){
        print(rec)
    }
}

ym[fname == 'INNJOO Halo Halo', year:='2015']
ym[fname == 'Samsung SM-J700F GALAXY J7 SM-J700F', year:='2016']
ym[fname == 'Not Known SHW-M180S SHW-M180S', year:='2012']


######################################################################################3
#Parse 4

task4 <- fread('mturk_parse_4.csv')
task4[ ,fname:=paste(Input.brand, Input.model, Input.marketing)]
task4$Answer.year <- tolower(task4$Answer.year)
task4 <- task4[, c('fname', 'Answer.year'), ]

task4 <- subset(task4, Answer.year != '{}')
task4 <- subset(task4, Answer.year != '')
task4 <- subset(task4, Answer.year != ' ')

task4[grepl('not a phone', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('GPS', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('USB', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('modem', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('router', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('module', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('lawnmower', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('batteries', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('USB', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('camera', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('speaker', Answer.year, ignore.case = T), Answer.year:='incorrect']
task4[grepl('remote', Answer.year, ignore.case = T), Answer.year:='incorrect']

task4[grepl('not found', Answer.year, ignore.case = T), Answer.year:='not_found']
task4[grepl('info found', Answer.year, ignore.case = T), Answer.year:='not_found']


ym <- task4[grepl(months, Answer.year, ignore.case = T), ]
f1 <- task4[!grepl(months, Answer.year, ignore.case = T), ]
f2 <- f1[!Answer.year == 'not_found', ]
f3 <- f2[!Answer.year == 'incorrect']
y <- f3[which(!is.na(as.numeric(f3$Answer.year))), ]
ym <- rbind(ym, y)
#
f3 <- f3[which(is.na(as.numeric(f3$Answer.year))), ]
f3[,Answer.year:='incorrect']
#
task4 <- rbind(ym, f3)

#task3 <- merge_tacdb(task3)
names(task4) <- c('fname', 'year')
task4 <- unique(task4)
task4 <- clean_years(task4)

replace_0 <- gsub('0', 'o', task3[is.na(modified), year])
task3[is.na(modified), year:=replace_0]

task3$modified <- as.character(task3$modified)
task3[year == 'incorrect', modified:='not_phone']
task3[year == 'not_found', modified:='not_found']
task3[is.na(modified), modified:='not_phone']

task3 <- task3[, c('fname', 'year'), ]
names(task3) <- c('fname', 'year')