checkDigitCheck <- function(imeilist){
  l <- c()
  for(imei in imeilist){
    # Check whether the imei is not numeric or null
    if(is.na(suppressWarnings(as.numeric(imei))) | is.null(imei)){
      l <- c(l, 0)
    } else {
      barcode <- substr(imei,1,nchar(imei)-1)
      checkdigit <- substr(imei,nchar(imei),nchar(imei))
      lenbar <- nchar(barcode)
      
      evensum <- 0
      oddsum <- 0
      
      # Odd digits, if greater than 9 then add the digits together (this only works when the multiplier is 2) 
      for(i in seq(2, lenbar, by=2)){
        digsum<-as.numeric(substr(barcode,i,i))*2
        if(digsum>9){digsum <- digsum - 9}
        evensum <- evensum + digsum
      }
      
      # Sum the even digits 
      for(i in seq(1, lenbar, by=2)){
        oddsum <- oddsum + as.numeric(substr(barcode,i,i))
      }
      
      allsum = evensum + oddsum
      valid <- 0.0
      if(checkdigit == (10 - (allsum %% 10))%%10) valid <- 1.0
      l <- c(l, valid)
    }
  }
  return(l)
}

checkDigitCalc <- function(imeilist){
  l <- c()
  for(imei in imeilist){
    # Check whether the imei is not numeric or null
    if(is.na(suppressWarnings(as.numeric(imei))) | is.null(imei)){
      l <- c(l, 0)
    } else {
      barcode <- substr(imei,1,nchar(imei)-1)
      checkdigit <- substr(imei,nchar(imei),nchar(imei))
      lenbar <- nchar(barcode)
      
      evensum <- 0
      oddsum <- 0
      
      # Odd digits, if greater than 9 then add the digits together (this only works when the multiplier is 2) 
      for(i in seq(2, lenbar, by=2)){
        digsum<-as.numeric(substr(barcode,i,i))*2
        if(digsum>9){digsum <- digsum - 9}
        evensum <- evensum + digsum
      }
      
      # Sum the even digits 
      for(i in seq(1, lenbar, by=2)){
        oddsum <- oddsum + as.numeric(substr(barcode,i,i))
      }
      
      allsum = evensum + oddsum
      checkdigit <- (10 - (allsum %% 10))%%10
      l <- c(l, checkdigit)
    }
  }
  return(l)
}