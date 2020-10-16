seqDays<-function(from, to, by = 1){
  fromDate <- as.Date(as.character(from), "%Y%m%d")
  toDate <- as.Date(as.character(to), "%Y%m%d")
  s <- seq(fromDate, toDate, "days")
  newDates <- str_replace_all(s, "-", "")
  return(newDates)
}

function(d, n) {
  date <- as.Date(as.character(d), "%Y%m%d")
  newDate <- date + n
  stringNewDate <- str_replace_all(newDate, "-", "")
  return(stringNewDate)
}


seq(as.Date("2018/1/1"), as.Date("2018/31/12"), "month")

as.Date("2018/1/1")

sub("-","", format(as.Date("2018/1/1"), "%Y-%m"))


seq()
inicioBacktest<-20181101

#Convert yyyymmdd string to Date class in R
#to convert these dates with format YYYYMMDD to a Date class.

dates <- data.frame(Date = c("20130707", "20130706", "20130705", "20130704"))
#Metodo 1
as.Date(dates[["Date"]], "%Y%m%d")
#Metodo 2
dates<-as.Date(as.character(dates),format="%Y%m%d")

fecha_mora<-c("20171101")

#Secuencia a incluir en el analisis

fecha<-201812

sub("-","", format(seq(as.Date(paste0(fecha,"01"), "%Y%m%d"), length = 12, by = "+1 months"), "%Y-%m"))

#Completar último día
sub("-","", format(seq(as.Date(paste0(fecha,"01"), "%Y%m%d"), length = 12, by = "-1 months")-1, "%Y-%m"))

sub("-","", format(seq(as.Date(paste0(fecha,"01"), "%Y%m%d"), length = 6, by = "-1 months")-1, "%Y-%m"))
sub("-","", format(seq(as.Date(paste0(fecha,"01"), "%Y%m%d"), length = 3, by = "-1 months")-1, "%Y-%m"))

#Sin último día del mes
sub("-","", format(seq(as.Date(paste0(fecha,"01"), "%Y%m%d"), length = 12, by = "-1 months"), "%Y-%m"))
sub("-","", format(seq(as.Date(paste0(fecha,"01"), "%Y%m%d"), length = 6, by = "-1 months"), "%Y-%m"))
sub("-","", format(seq(as.Date(paste0(fecha,"01"), "%Y%m%d"), length = 3, by = "-1 months"), "%Y-%m"))

ll <- seq(as.Date(paste0(fecha,"01"), "%Y%m%d"), as.Date("2018/12/1"), by = "-1 month")




finBacktest<-20181130

from<-20181101
to<-20181130

testingPeriod <- seqDays(inicioBacktest, finBacktest, by = 1)


## first days of years
seq(as.Date(inicioBacktest), as.Date("1999/1/10"), "days")


seq(as.Date("2017/11/1"), as.Date("2018/8/1"), "month")

#Funcion de popular

seqDays <- function(from, to, by = 1){
  fromDate <- as.Date(as.character(from), "%Y%m%d")
  toDate <- as.Date(as.character(to), "%Y%m%d")
  s <- seq(fromDate, toDate, by = by %+% " days")
  newDates <- str_replace_all(s, "-", "")
  return(newDates)
}

dayAddInteger <- function(d, n) {
  date <- as.Date(as.character(d), "%Y%m%d")
  newDate <- date + n
  stringNewDate <- str_replace_all(newDate, "-", "")
  return(stringNewDate)
}

predictPeriodo <- 20180717
dailyPeriodToExpand <- predictPeriodo
dailyPeriodsToProcess <- seqDays(dayAddInteger(min(dailyPeriodToExpand), -120), dailyPeriodToExpand)

seqMonth <- function(from, to, by = 1){
  fromDate <- as.Date(from %+% "01", "%Y%m%d")
  toDate <- as.Date(to %+% "01", "%Y%m%d")
  s <- seq(fromDate, toDate, by = by %+% " months")
  return(format(s, "%Y%m"))
}


monthlyPeriodsToExpand <- c(unique(monthAddInteger(str_sub(dailyPeriodToExpand, 1, 6), -2)),
                            unique(monthAddInteger(str_sub(dailyPeriodToExpand, 1, 6), -1)))

