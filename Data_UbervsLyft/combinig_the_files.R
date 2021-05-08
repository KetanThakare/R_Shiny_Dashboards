library(tidyr)
library(reshape)
library(lubridate)
library(dplyr)
pickup_aug <- read.csv("C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/AVN_TNC_PickUp_Aug_2016.csv")
pickup_sept <- read.csv("C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/AVN_TNC_PickUp_Sept_2016.csv")
pickup_jul <- read.csv("C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/AVN_TNC_PickUp_Jul_2016.csv")
pickup_nov <- read.csv("C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/AVN_TNC_PickUp_Nov_2016.csv")
pickup_dec <- read.csv("C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/AVN_TNC_PickUp_Dec_2016.csv")
pickup_jun <- read.csv("C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/AVN_TNC_PickUp_Jun_2016.csv")
pickup_oct <- read.csv("C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/AVN_TNC_PickUp_Oct_2016.csv")

MyMerge       <- function(x, y) {
  df            <- rbind(x,
                         y)
  return(df)
}
pickup_combined <- Reduce(MyMerge, list(pickup_jun, pickup_jul, pickup_aug, pickup_sept, pickup_oct, pickup_nov, pickup_dec))


pickup_combined <- separate(pickup_combined, Date, into = c("Date", "Hour", "AM/PM"), sep = " ")

pickup_combined$Date <- as.Date(pickup_combined$Date, format = "%m/%d/%y")

#pickup_combined$Date <- parse_date_time(pickup_combined$Date, c("%m/%d/%y"), exact = TRUE)

#pickup_combined$Date <- parse_date_time(pickup_combined$Date, c("%m/%d/%y %I:%M %p"), exact = TRUE)
pickup_combined <- pickup_combined[!(pickup_combined$Service_Provider == "Wingz"),]
View(pickup_combined)
str(pickup_combined)


#pickup_combined$Hour <- NULL
#pickup_combined$`AM/PM`<- NULL

news = TotalPUbyDate[!(TotalPUbyDate$Service_Provider == "Wingz"),]
View(news)
str(news)
#write.csv(pickup_combined, "C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/pickup_combined.csv")

TotalPUbyDate = pickup_combined %>% group_by(Date, Service_Provider) %>% dplyr::count()
TotalPUbyDate = dplyr::rename(pickup_combined, Total_Pickups = n )
View(TotalPUbyDate)
write.csv(TotalPUbyDate, "C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/TotalPUbyDate.csv")
TotalPUbyDate = pickup_combined %>% group_by(Service_Provider) %>% dplyr::count()

TotalPUbyDay = pickup_combined %>% group_by(month=floor_date(Date, "day"), Service_Provider) %>% dplyr::count()
TotalPUbyDay <- spread(TotalPUbyDay, key = Service_Provider, value = n)
View(TotalPUbyDay)
TotalPUbyDay <- replace_na(TotalPUbyDay, list(Lyft = 0))
TotalPUbyDay <- dplyr::rename(TotalPUbyDay, t_day = day)
write.csv(TotalPUbyDay, "C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/TotalPUbyDay.csv")

TotalPUbyMonth = pickup_combined %>% group_by(month=floor_date(Date, "month"), Service_Provider) %>% dplyr::count()
TotalPUbyMonth <- spread(TotalPUbyMonth, key = Service_Provider, value = n)
TotalPUbyMonth$Month = c("June '16","July '16","August '16","September '16","October '16","November '16","December '16")
TotalPUbyMonth$month = TotalPUbyMonth$Month
TotalPUbyMonth$Month = NULL
TotalPUbyMonth = dplyr::rename(TotalPUbyMonth, Month = month)
View(TotalPUbyMonth_fc)
write.csv(TotalPUbyMonth, "C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/TotalPUbyMonth.csv")


TotalPUbyMonth_fc = pickup_combined %>% group_by(month=floor_date(Date, "month"), Service_Provider) %>% dplyr::count()
View(TotalPUbyMonth_fc)
TotalPUbyMonth_fc$month = c("June '16","June '16","July '16","July '16","August '16","August '16","September '16","September '16","October '16","October '16","November '16","November '16","December '16","December '16")
TotalPUbyMonth_fc <- dplyr::rename(TotalPUbyMonth_fc, Month = month)
write.csv(TotalPUbyMonth_fc, "C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/TotalPUbyMonth_fc.csv")

TotalPUbyWeek = pickup_combined %>% group_by(week=floor_date(Date, "week"), Service_Provider) %>% dplyr::count()
TotalPUbyWeek = spread(TotalPUbyWeek, key = Service_Provider, value = n)
write.csv(TotalPUbyWeek, "C:/Users/admin/Desktop/EeD/R/Shiny_UbervsLyft/Data/TotalPUbyWeek.csv")
View(TotalPUbyWeek)

mean(TotalPUbyMonth$Lyft)



