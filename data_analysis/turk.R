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

months <- month.abb[1:12]
months = paste(months, collapse = '|')

names(task4) <- c('fname', 'year')
with_months <- task4[grepl(months, year, ignore.case = T), ]
with_months <- clean_years(with_months)
with_months$modified <- year(with_months$modified)
with_months$modified <- as.character(with_months$modified)
with_months[is.na(modified), modified:='not_phone']
with_months <- with_months[, c('fname', 'modified'), ]
names(with_months) <- c('fname', 'year')

without_months <- task4[!grepl(months, year, ignore.case = T), ]
already_parsed <- without_months[year == 'not_phone' | year == 'not_found' | year == 'incorrect', ]

result <- rbind(with_months, already_parsed)

without_months <- without_months[year != 'not_phone' & year != 'not_found' & year != 'incorrect', ]
months <- without_months[c(9, 10, 17, 29, 58, 83, 87, 90, 109, 113, 140), ]
months[1, 'year'] <- '2015'
months[c(8, 9), 'year'] <- '2013'
months[c(2, 4), 'year'] <- '2012'

result <- rbind(result, months)

find_missing <- without_months[-c(9, 10, 17, 29, 58, 83, 87, 90, 109, 113, 140), ]