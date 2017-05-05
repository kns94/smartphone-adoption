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

# Purpose of this code is to plot percentage of feature, 3g, 4g phones per month

phone_ts <- imsiraw[, .N, key = list(accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
names(phone_ts) <- c('month', 'year', 'network_support', 'N')
temp <- phone_ts[, j = list(total_phones = sum(N)), by = list(month, year)]
phone_ts <- setkey(phone_ts, month, year)[temp, percentage := ((N/total_phones) * 100)]
phone_ts[, date := paste(month, year, sep = '-')]
phone_ts <- phone_ts[order(year, month)]
phone_ts$date = as.Date(as.yearmon(phone_ts$date, format = '%m-%Y'))

sg <- streamgraph(data = phone_ts, key = 'network_support', value = 'percentage', date = 'date', offset="zero",
    interpolate="linear") %>% 
 # Set the X axis ticks to be 1 per year
 sg_axis_x(tick_interval=3, tick_units="month", tick_format = '%m-%Y') %>% 
 # create a legend
 sg_legend(show = FALSE) %>% 
 # Add a title
 sg_title(title="Adoption rate of phones in Philippines (April 2016 onwards)") %>% 
 # Render the HTML
 html_print()