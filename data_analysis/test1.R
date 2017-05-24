########################### Indonesia ########################
    
    #Non-local

    #Entire

imsiraw <- id_all
nonlocal_imsiraw_entire <- filter_non_local_users(imsiraw)
title_text <- 'Adoption rate of non-locals in Indonesia (August 2014 onwards)'
value <- plot_normalized_types(nonlocal_imsiraw_entire, title_text)
plt <- value$plt
data <- value$data
write.csv(data, 'ID_nonlocal_entire_duration.csv', row.names <- F)

    #Last year
    
imsiraw <- id_all
nonlocal_imsiraw_entire <- filter_non_local_users(imsiraw)
nonlocal_imsiraw_last_year <- subset(nonlocal_imsiraw_entire, accessed_ds > as.Date('04-01-2016', format = '%m-%d-%Y'))
title_text <- 'Adoption rate of non-locals in Indonesia (April 2016 onwards)'
value <- plot_normalized_types(nonlocal_imsiraw_last_year, title_text)
plt <- value$plt
data <- value$data
write.csv(data, 'ID_nonlocal_last_year.csv', row.names <- F)

    #Local

imsiraw <- id_all
local_imsiraw_entire <- filter_local_users(imsiraw)
local_imsiraw_last_year <- subset(local_imsiraw_entire, accessed_ds > as.Date('04-01-2016', format = '%m-%d-%Y'))
title_text <- 'Adoption rate of locals in Indonesia (April 2016 onwards)'
value <- plot_normalized_types(local_imsiraw_last_year, title_text)
plt <- value$plt
data <- value$data
write.csv(data, 'ID_local_last_year.csv', row.names <- F)

    #Last year

imsiraw <- id_all
local_imsiraw_entire <- filter_local_users(imsiraw)
title_text <- 'Adoption rate of locals in Indonesia (August 2014 onwards)'
value <- plot_normalized_types(local_imsiraw_entire, title_text)
plt <- value$plt
data <- value$data
write.csv(data, 'ID_local_entire_duration.csv', row.names <- F)


################### Philippines #########################

imsiraw <- ph_all
nonlocal_imsiraw <- filter_non_local_users(imsiraw)
title_text <- 'Adoption rate of non-locals in Philippines (April 2016 onwards)'
plot <- plot_normalized_types(nonlocal_imsiraw, title_text)$plt
data <- plot_normalized_types(nonlocal_imsiraw, title_text)$data
write.csv(data, 'PH_nonlocal_last_year.csv', row.names = F)

imsiraw <- ph_all
local_imsiraw <- filter_local_users(imsiraw)
title_text <- 'Adoption rate of locals in Philippines (April 2016 onwards)'
plot <- plot_normalized_types(local_imsiraw, title_text)$plt
data <- plot_normalized_types(local_imsiraw, title_text)$data
write.csv(data, 'PH_local_last_year.csv', row.names = F)