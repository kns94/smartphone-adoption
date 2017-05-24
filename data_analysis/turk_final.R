task4 <- fread('mturk_parse_4.csv')
task4 <- clean_df(task4)

sub_task4 <- task4[, list(fname, modified)]
names(sub_task4) <- c('fname', 'year')

final <- sub_task4

########################################## Now looking at task-3 ##############################

task3 <- fread('mturk_parse_3.csv', colClasses = 'character')
task3 <- clean_df(task3)

sub_task3 <- task3[, list(fname, modified)]
names(sub_task3) <- c('fname', 'year')

final <- fill_missing(final, sub_task3)
final <- fill_not_known(final, sub_task3)

#################################### Now looking at task - 2 #############################

task2 <- fread('mturk_parse_2.csv', colClasses = 'character')
task2 <- clean_df(task2)

sub_task2 <- task2[, list(fname, modified)]
names(sub_task2) <- c('fname', 'year')

final <- fill_missing(final, sub_task2)
final <- fill_not_known(final, sub_task2)

#################################### Now looking at task - 1 #################################

task1 <- fread('Batch_2777334_batch_results.csv')
task1 <- clean_df(task1)

sub_task1 <- task1[, list(fname, modified)]
names(sub_task1) <- c('fname', 'year')

sub_task1 <- remove_duplicates(sub_task1)

final <- fill_missing(final, sub_task1)
final <- fill_not_known(final, sub_task1)

final$year <- as.Date(as.yearmon(final$year, format = '%b %Y'))
final$year <- as.character(final$year)
final[is.na(year), year:='not_found']

write.csv(final, 'turk_final.csv', row.names = F)

##############################################################################################

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

#Same function to clean df
clean_df <- function(df){
    df[ ,fname:=paste(Input.brand, Input.model, Input.marketing)]
    df$Answer.year <- tolower(df$Answer.year)
    df <- df[, list(fname, Answer.year), ]
    df <- unique(df)

    months <- month.abb[1:12]
    months = paste(months, collapse = '|')

    df$answered = F
    df[grepl(months, Answer.year, ignore.case = T), answered:=T]
    df[which(!is.na(as.numeric(Answer.year))), answered:=T]

    names(df) <- c('fname', 'year', 'answered')
    df <- clean_years(df)

    #Checking to see if year has some value but modified returned true
    df[is.na(modified) & answered == T, ]

    #Replacing modified with not_found if it has NA
    df$modified <- as.character(df$modified)
    df[is.na(modified), modified:='not_found']

    return(df)
}

#Compare dataframes - This function is to see if a value is found in a subtask, but not found in the final data frame
fill_missing <- function(final, sub_task){
    unanswered <- final[year == 'not_found', ]

    for(i in 1:nrow(unanswered)){
        f1 <- as.character(unanswered[i, 'fname'])

        another_val = sub_task[fname == f1, ]
        another_val <- unique(another_val)

        if(nrow(another_val) != 0){

            if(nrow(another_val) == 1){
            
                yr = as.character(another_val$year)
                if(yr != 'not_found'){
                
                    final[fname == f1, year:=yr]
                }
            }
            else{
                print(another_val)
            }
        }
    }
    return(final)
}

#This is to see if the entire record is not found in final dataframe
fill_not_known <- function(final, sub_task){

    for(name in unique(sub_task$fname)){
        another_val = final[fname == name, ]

        if(nrow(another_val) == 0){

            new_name <- sub_task[fname == name, ]$fname[1]
            new_year <- sub_task[fname == name, ]$year[1]

            final <- data.table(rbind(final, data.frame(fname = new_name, year = new_year)))
        }
    }

    return(final)
}

remove_duplicates <- function(task1){

    new <- data.frame(matrix(nrow = 0, ncol = 2))
    colnames(new) <- c('fname', 'year')
    new <- data.table(new)

    for(name in unique(task1$fname)){
      res = subset(task1, fname == name)
      res.year <- res$Answer.year

      if(length(unique(res.year)) != 1){
        
        some_value <- res[year != 'not_found', ]

        if(nrow(some_value) != 0){
            yr <- some_value$year[1]
            new <- data.table(rbind(new, data.frame(fname = name, year = yr)))
        }
        else{
            new <- data.table(rbind(new, data.frame(fname = name, year = 'not_found')))   
        }
      }
      else{
        new <- data.table(rbind(new, data.frame(fname = name, year = as.character(res.year[1]))))
      }
    }

    return(new)
}