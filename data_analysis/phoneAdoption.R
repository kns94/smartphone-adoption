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

phone_ts <- imsiraw[, .N, key = list(month(accessed_dt), year(accessed_dt), type_phone)][order(year, month)]
temp <- phone_ts[, j = list(total_phones = sum(N)), by = list(month, year)]
phone_ts <- setkey(phone_ts, month, year)[temp, percentage := ((N/total_phones) * 100)]
phone_ts[, date := paste(month, year, sep = '-')]
phone_ts <- phone_ts[order(year, month)]

babynames %>%
  filter(grepl("^I", name)) %>%
  group_by(year, name) %>%
  tally(wt=n) %>%
  streamgraph("name", "n", "year", offset="zero", interpolate="linear") %>%
  sg_legend(show=TRUE, label="I- names: ")