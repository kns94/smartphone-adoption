suppressMessages(require(data.table))
suppressMessages(require(ggplot2))
suppressMessages(require(hashmap))
require('ggplot2')
require('XML')
require('plotly')
source('./functions.R')
source('./upgrade_functions.R')

imsiraw <- load_log_files('imsis')
imsiraw <- filter_local_users(imsiraw)

phone_ts <- imsiraw[, .N, key = list(accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
names(phone_ts) <- c('month', 'year', 'network_support', 'N')
temp <- phone_ts[, j = list(total_phones = sum(N)), by = list(month, year)]
phone_ts <- setkey(phone_ts, month, year)[temp, percentage := ((N/total_phones) * 100)]
phone_ts[, date := paste(month, year, sep = '-')]
phone_ts <- phone_ts[order(year, month)]
phone_ts$date = as.Date(as.yearmon(phone_ts$date, format = '%m-%Y'))

streamgraph(data = phone_ts, key = 'network_support', value = 'percentage', date = 'date', offset="zero",
    interpolate="linear") %>%
    sg_legend(show=TRUE, label="Network Support: ")