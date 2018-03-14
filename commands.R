rm(list = ls())
data[!complete.cases(data),] # to find rows of na values
#install.packages(lubridate)
#library(lubridate)
month(data$DATE)<-month(data$DATE)-1  
t<- Reduce(function(x, y) merge(x, y, all=TRUE), list(exchange_rate_data,oil_price_data, import_price_index_us, interest_rate_canada, 
                                                      interest_rate_us, cpi_ca, cpi_us, employment_change_canada, Non_farm_Payrolls, 
                                                      Manufacturing_index_US, Non_Manufacturing_index_US, Raw_Material_Price_Index_CA, 
                                                      unemployment_rate_canada, unemployment_rate_us, ppi_industry_US, ppi_commodity_US  ))
# with(beaver, tapply(temp, activ, shapiro.test)
     
# split(das, cut(das$anim, 3))
# t3<- split(t2, cut(t2$Exchange_Rate, 5))
num_to_cat <- function(x) {
   cut(x,5, labels = c("Very Low", "Low", "Medium", "High", "Very High"))
}
z<-lapply(t2[-1],num_to_cat)
out_treat<- function(f){
  a <- round(quantile(f,.25)- 1.5* IQR(f), digits = 1)
  b <- round(quantile(f,.75)+ 1.5* IQR(f), digits = 1)
  percentile<- ecdf(f)
  e<- percentile(a)
  d<- percentile(b)
  round(squish(f, quantile(f, c(e,d))), digits = 1)
  
}
t3<-lapply(t2[-1],out_treat)
#a <- round(quantile(t2$`cpi_CA(%)`,.25)- 1.5* IQR(t2$`cpi_CA(%)`), digits = 1)
#b <- round(quantile(t2$`cpi_CA(%)`,.75)+ 1.5* IQR(t2$`cpi_CA(%)`), digits = 1)
#percentile<- ecdf(t2$`cpi_CA(%)`)
#e<- percentile(a)
#d<- percentile(b)
#zzz<- round(squish(t2$`cpi_CA(%)`, quantile(t2$`cpi_CA(%)`, c(e,d))), digits = 1)
#install.packages("scales")
#library(scales)
#install.packages("gtools")
#library(gtools)
#quantcut(x, q=seq(0,1,by=0.20), na.rm=TRUE)
labels<- c("Very Low", "Low", "Medium", "High","Very High")
zzzz<- quantcut(t2$`cpi_CA(%)` , q=seq(0,1,by=0.20), na.rm=TRUE, labels = labels)
par(mfrow= c(2,2))
