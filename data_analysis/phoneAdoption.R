require('plotly')
require('htmltools')
require('scales')

# Purpose of this code is to plot percentage of feature, 3g, 4g phones per month
plot_normalized_types <- function(imsiraw, title_text){

    phone_ts <- imsiraw[, .N, key = list(imeicorrected, accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
    phone_ts <- phone_ts[, .N, key = list(accessed_month, accessed_year, network_support)][order(accessed_year, accessed_month)]
    names(phone_ts) <- c('month', 'year', 'type', 'N')
    
    temp <- phone_ts[, j = list(total_phones = sum(N)), by = list(month, year)]
    phone_ts <- setkey(phone_ts, month, year)[temp, percentage := ((N/total_phones) * 100)]
    phone_ts[, date := paste(month, year, sep = '-')]
    phone_ts <- phone_ts[order(year, month)]
    phone_ts$date = as.Date(as.yearmon(phone_ts$date, format = '%m-%Y'))

    phone_ts <- phone_ts[, c('type', 'percentage', 'date', 'N')]

    data <- data.frame(phone_ts)

    p <- ggplot(data, aes(x=date, y=percentage, fill=type)) + geom_area() + labs(y = 'Distribution of Devices',
     fill = 'Phone Types') + scale_color_manual(values=c("#d95f02", "#1b9e77", "#7570b3"))

    data$type <- as.factor(data$type)

    data[which(data$type == '2G'), 'c'] = "#d95f02"
    data[which(data$type == '3G'), 'c'] = "#1b9e77"
    data[which(data$type == '4G'), 'c'] = "#7570b3"

    p <- ggplot() + geom_area(aes(y = percentage, x = date, fill = type), data = data,
                           stat="identity") + 
    scale_fill_manual(values = c("#d95f02", "#1b9e77", "#7570b3")) + 
    labs(y = '% of mobile phones in community', x = "", title = 'Philippines (April 2016 onwards)') + 
    scale_x_date(date_breaks = '2 month', labels=date_format("%b'%y"), expand = c(0, 0.5)) + 
    scale_y_continuous(expand = c(0, 0)) + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"), 
              axis.title.x=element_blank(), axis.title.y=element_text(size=30, hjust = 0.5), 
              axis.text.x = element_text(size = 30), axis.text.y = element_text(size = 30),
               legend.title=element_blank(), title = element_text(size = 20))

    plt <- ggplotly(p) %>%
    layout(legend = list(font = list(size = 35)), margin = list(l = 150, t = 150), titlefont = list(size = 40))

    
    
    plt
    
    l <- list()
    l$plt <- p 
    l$data <- data

    return(l)
}

