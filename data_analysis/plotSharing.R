shared_imsiraw <- imsiraw[imsi %in% shared_imsi, ]

#Purpose of this code is to plot percentage of feature, 3g, 4g phones per month
plot_shared_normalized_types <- function(imsiraw, shared_imsiraw, title_text){

    #Grouping unique devices per month
    ts1 <- imsiraw[, .N, key = list(imeicorrected, accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
    complete_phone_ts <- ts1[, .N, key = list(accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]

    #Count shared unique devices
    ts2 <- shared_imsiraw[, .N, key = list(imeicorrected, accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
    shared_phone_ts <- ts2[, .N, key = list(accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
    shared_phone_ts$percentage = 0
    shared_phone_ts$total_phones = 0

    #Ratio per month
    for(i in 1:nrow(shared_phone_ts)){
        m <- as.character(shared_phone_ts[i, 'accessed_month'])
        y <- as.character(shared_phone_ts[i, 'accessed_year'])
        n <- as.character(shared_phone_ts[i, 'network_support'])
        shared_count <- as.numeric(shared_phone_ts[i, 'N'])
        complete_count <- as.numeric(complete_phone_ts[accessed_month == m & accessed_year == y & network_support == n, 'N'])

        shared_phone_ts[i, 'percentage'] <- shared_count/complete_count * 100
        shared_phone_ts[i, 'total_phones'] <- complete_count
    }

    shared_phone_ts[, date := paste(accessed_month, accessed_year, sep = '-')]
    shared_phone_ts$date = as.Date(as.yearmon(shared_phone_ts$date, format = '%m-%Y'))

    p <- ggplot(data=shared_phone_ts,
       aes(x=date, y=percentage, colour=network_support)) +
       geom_line()

    ggplotly(p) %>%
    layout(legend = list(font = list(size = 35)), margin = list(l = 150, t = 150), titlefont = list(size = 40),
        yaxis = list(title = 'Percentage of shared devices', showgrid = FALSE, tickfont = list(size = 35), showline = T, titlefont = list(size =35)),
        xaxis = list(title = "", tickfont = list(size = 35)), title = 'Indonesia')

    layout(yaxis = list(title = 'Average Monthly activity', showgrid = FALSE, tickfont = list(size = 35), showline = T, titlefont = list(size =35)),
             xaxis = list(title = "", tickfont = list(size = 35)),
             barmode = 'group', legend = list(font = list(size = 32)), 
             titlefont = list(size = 40), margin = list(l = 80))
    

    shared_agg <- data.frame(matrix(0, nrow = 0, ncol = 4))
    colnames(shared_agg) <- c('date', '2G', '3G', '4G')

    for(dt in unique(shared_phone_ts$date)){
        rec = shared_phone_ts[date == as.Date(dt), ]
        twoG_perc = as.numeric(rec[network_support == '2G', ]$percentage)
        threeG_perc = as.numeric(rec[network_support == '3G', ]$percentage)
        fourG_perc = as.numeric(rec[network_support == '4G', ]$percentage)
        if(length(fourG_perc) == 0){
            fourG_perc = 0
        }
        shared_agg[nrow(shared_agg) + 1, ] <- c(dt, twoG_perc, threeG_perc, fourG_perc)
    }
    shared_agg$date <- as.Date(shared_agg$date)

    write.csv(shared_phone_ts, 'paper_plots/PH_shared_last_year.csv', row.names = F)

    #Average of ratio per month
    num_months <- nrow(unique(shared_phone_ts[, list(accessed_month, accessed_year)]))
    shared_phone_ts <- shared_phone_ts[, list(total_avg = sum(percentage)), by = list(network_support)]
    shared_phone_ts[ ,total_avg:=total_avg/num_months]

    p <- plot_ly(shared_phone_ts, x = ~network_support, y = ~total_avg, type = 'bar', 
        marker = list(color = c("#d95f02", "#1b9e77", "#7570b3"))) %>%
            layout(yaxis = list(title = 'Percentage of shared devices', showline = TRUE, showgrid = FALSE, 
        titlefont = list(size =35), tick0 = 0, tickfont = list(size = 35)), xaxis = list(title = '', 
        tickfont = list(size = 35)),
             barmode = 'group', title= 'Philippines', 
             titlefont = list(size = 40), margin = list(t = 150, l = 150))

    p

    p <- ggplot(shared_agg, aes(x = date)) + 
         geom_line(aes(y = `2G`)) +
         geom_line(aes(y = `3G`)) + 
         geom_line(aes(y = `4G`))
 
    plot_ly(shared_agg, x = ~date, y = ~`2G`, type = 'line', name = '2G', marker = list(color = '#d95f02')) %>%
            add_trace(y = ~`3G`, name = '3G', marker = list(color = '#1b9e77')) %>%
            add_trace(y = ~`4G`, name = '4G', marker = list(color = '#7570b3')) %>%
            layout(yaxis = list(title = 'Average Monthly activity', showgrid = FALSE, tickfont = list(size = 35), showline = T, titlefont = list(size =35)),
             xaxis = list(title = "", tickfont = list(size = 35)),
             barmode = 'group', legend = list(font = list(size = 32)), 
             titlefont = list(size = 40), margin = list(l = 80))

    #complete_phone_ts <- imsiraw[, .N, key = list(imei, accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
    #shared_phone_ts <- shared_imsiraw[, .N, key = list(accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]

    #names(complete_phone_ts) <- c('month', 'year', 'network_support', 'N')
    #temp <- complete_phone_ts[, j = list(total_phones = sum(N)), by = list(month, year)]
    #complete_phone_ts <- setkey(complete_phone_ts, month, year)[temp, percentage := ((N/total_phones) * 100)]
    #complete_phone_ts[, date := paste(month, year, sep = '-')]
    #complete_phone_ts <- phone_ts[order(year, month)]
    #complete_phone_ts$date = as.Date(as.yearmon(complete_phone_ts$date, format = '%m-%Y'))

    #sg <- streamgraph(data = complete_phone_ts, key = 'network_support', value = 'percentage', date = 'date', offset="zero",
    #    interpolate="linear") %>% 
     # Set the X axis ticks to be 1 per year
    # sg_axis_x(tick_interval=3, tick_units="month", tick_format = '%m-%Y') %>% 
     # create a legend
    # sg_legend(show = FALSE) %>% 
     # Add a title
    # sg_title(title=title_text) %>% 
     # Render the HTML
    # html_print()
}

