
feature_feature = 0
feature_3g = 0
feature_4g = 0
threeG_fourG = 0
threeG_threeG = 0
threeG_feature = 0
fourG_threeG = 0
fourG_feature = 0
fourG_fourG = 0
imsi_count = 0


id <- dcast(id, date ~ type_phone)
id$date <- as.Date(as.yearmon(id$date, format = '%m-%Y'))

nc <- checking[is.na(year), ]
imsiraw <- load_log_files('imsis_id_endaga')

for(name in unique(nc$fname)){
  rec <- task3[fname == name, ]
  
  if(nrow(rec) != 0){
    print(rec)
  }
}
