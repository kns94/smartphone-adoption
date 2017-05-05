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

    #sg <- streamgraph(data = phone_ts, key = 'network_support', value = 'percentage', date = 'date', offset="zero",
    #    interpolate="linear") %>% 
     # Set the X axis ticks to be 1 per year
    # sg_axis_x(tick_interval=3, tick_units="month", tick_format = '%m-%Y') %>% 
     # create a legend
    # sg_legend(show = FALSE) %>% 
     # Add a title
    # sg_title(title=title_text) %>% 
     # Render the HTML
    # html_print()

    phone_ts <- phone_ts[, c('type', 'percentage', 'date', 'N')]

    data <- data.frame(phone_ts)

    #p <- ggplot(data, aes(x=date, y=percentage, fill=type)) + geom_area() + labs(title = title_text, x = 'Time', 
    #    y = 'Distribution of Devices', fill = 'Phone Types') + scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))

    data$type <- as.factor(data$type)

    #p <- ggplot(data, aes(x=date, y=percentage, fill=type)) +
    #geom_area(colour="black", size=.2, alpha=.4) +
    #scale_fill_brewer(palette="Greens", breaks=rev(levels(data$type))) + 
    #labs(title = 'Adoption Rate in Indonesia', x = 'Time', 
    #    y = 'Distribution of Devices', fill = 'Types') 

    p <- ggplot(data, aes(x=date, y=percentage, fill=type)) +
    geom_area(colour="black", size=.2, alpha=.4) +
    #scale_fill_brewer('red', 'blue', 'green') + 
    scale_x_date(breaks = pretty_breaks(10)) + 
    labs(title = title_text, x = 'Time', 
        y = 'Distribution of Devices', fill = 'Types')

    p <- ggplotly(p)
    
    l <- list()
    l$plt <- p 
    l$data <- data

    return(l)
}

