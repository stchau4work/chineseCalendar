############################
# Get Chinese New Year from 1901 to 2100
# Reimplement from Dr. Herong Yang  http://www.herongyang.com/year/Program-Chinese-Calendar-Program-in-Java.html
# Author: Steven Chau 
# Date: 2017-11-05
###########################

rm(list=ls())
lapply(c("bitops","data.table"), require, character.only = TRUE)

# Base date: 01-Jan-1901, 4598/11/11 in Chinese calendar
baseYear <- 1901
baseMonth <- 1
baseDate <- 1
baseIndex <- 0
baseChineseYear <- 4598-1
baseChineseMonth <- 11
baseChineseDate <- 11

#Chinese month map, 2 bytes per year, from 1900 to 2100, 402 bytes.
#The first 4 bits represents the leap month of the year.
#The rest 12 bits are flags indicate if the corresponding month
#is a 29-day month. 2 bytes are stored in low-high order.
chineseMonths<-c(
  0x00,0x04,0xad,0x08,0x5a,0x01,0xd5,0x54,0xb4,0x09,0x64,0x05,0x59,0x45,
  0x95,0x0a,0xa6,0x04,0x55,0x24,0xad,0x08,0x5a,0x62,0xda,0x04,0xb4,0x05,
  0xb4,0x55,0x52,0x0d,0x94,0x0a,0x4a,0x2a,0x56,0x02,0x6d,0x71,0x6d,0x01,
  0xda,0x02,0xd2,0x52,0xa9,0x05,0x49,0x0d,0x2a,0x45,0x2b,0x09,0x56,0x01,
  0xb5,0x20,0x6d,0x01,0x59,0x69,0xd4,0x0a,0xa8,0x05,0xa9,0x56,0xa5,0x04,
  0x2b,0x09,0x9e,0x38,0xb6,0x08,0xec,0x74,0x6c,0x05,0xd4,0x0a,0xe4,0x6a,
  0x52,0x05,0x95,0x0a,0x5a,0x42,0x5b,0x04,0xb6,0x04,0xb4,0x22,0x6a,0x05,
  0x52,0x75,0xc9,0x0a,0x52,0x05,0x35,0x55,0x4d,0x0a,0x5a,0x02,0x5d,0x31,
  0xb5,0x02,0x6a,0x8a,0x68,0x05,0xa9,0x0a,0x8a,0x6a,0x2a,0x05,0x2d,0x09,
  0xaa,0x48,0x5a,0x01,0xb5,0x09,0xb0,0x39,0x64,0x05,0x25,0x75,0x95,0x0a,
  0x96,0x04,0x4d,0x54,0xad,0x04,0xda,0x04,0xd4,0x44,0xb4,0x05,0x54,0x85,
  0x52,0x0d,0x92,0x0a,0x56,0x6a,0x56,0x02,0x6d,0x02,0x6a,0x41,0xda,0x02,
  0xb2,0xa1,0xa9,0x05,0x49,0x0d,0x0a,0x6d,0x2a,0x09,0x56,0x01,0xad,0x50,
  0x6d,0x01,0xd9,0x02,0xd1,0x3a,0xa8,0x05,0x29,0x85,0xa5,0x0c,0x2a,0x09,
  0x96,0x54,0xb6,0x08,0x6c,0x09,0x64,0x45,0xd4,0x0a,0xa4,0x05,0x51,0x25,
  0x95,0x0a,0x2a,0x72,0x5b,0x04,0xb6,0x04,0xac,0x52,0x6a,0x05,0xd2,0x0a,
  0xa2,0x4a,0x4a,0x05,0x55,0x94,0x2d,0x0a,0x5a,0x02,0x75,0x61,0xb5,0x02,
  0x6a,0x03,0x61,0x45,0xa9,0x0a,0x4a,0x05,0x25,0x25,0x2d,0x09,0x9a,0x68,
  0xda,0x08,0xb4,0x09,0xa8,0x59,0x54,0x03,0xa5,0x0a,0x91,0x3a,0x96,0x04,
  0xad,0xb0,0xad,0x04,0xda,0x04,0xf4,0x62,0xb4,0x05,0x54,0x0b,0x44,0x5d,
  0x52,0x0a,0x95,0x04,0x55,0x22,0x6d,0x02,0x5a,0x71,0xda,0x02,0xaa,0x05,
  0xb2,0x55,0x49,0x0b,0x4a,0x0a,0x2d,0x39,0x36,0x01,0x6d,0x80,0x6d,0x01,
  0xd9,0x02,0xe9,0x6a,0xa8,0x05,0x29,0x0b,0x9a,0x4c,0xaa,0x08,0xb6,0x08,
  0xb4,0x38,0x6c,0x09,0x54,0x75,0xd4,0x0a,0xa4,0x05,0x45,0x55,0x95,0x0a,
  0x9a,0x04,0x55,0x44,0xb5,0x04,0x6a,0x82,0x6a,0x05,0xd2,0x0a,0x92,0x6a,
  0x4a,0x05,0x55,0x0a,0x2a,0x4a,0x5a,0x02,0xb5,0x02,0xb2,0x31,0x69,0x03,
  0x31,0x73,0xa9,0x0a,0x4a,0x05,0x2d,0x55,0x2d,0x09,0x5a,0x01,0xd5,0x48,
  0xb4,0x09,0x68,0x89,0x54,0x0b,0xa4,0x0a,0xa5,0x6a,0x95,0x04,0xad,0x08,
  0x6a,0x44,0xda,0x04,0x74,0x05,0xb0,0x25,0x54,0x03
)

`%+=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 + e2))
`%-=%` = function(e1,e2) eval.parent(substitute(e1 <- e1 - e2))

daysInGregorianMonth <- function(gregorianYear,gregorianMonth){
  date <- seq(as.Date(paste0(gregorianYear,'-',gregorianMonth,'-','01')),length.out=2, by='month')[2]
  as.numeric(unlist(strsplit(as.character(date-1),"-"))[3])
}

isGeogorianLeapYear <- function(gregorianYear){
  isLeap<-F
  if(gregorianYear%%4 == 0) isLeap <-T
  if(gregorianYear%%100 ==0) isLeap <-F
  if(gregorianYear%%400 ==0) isLeap <-T
  isLeap
}

nextChineseMonth <-function(y,m) {
  n <- abs(m) + 1 # normal behavior
  if (m>0) {
    # need to find out if we are in a leap year or not
    index <- y - baseChineseYear + baseIndex
    v <- chineseMonths[2*index+1+1]
    v <- bitAnd((bitShiftR(v,4)),0x0F)
    if (v==m) n <- -m # next month is a leap month
  }
  if (n==13) n <- 1 #roll into next year
  n
}

bigLeapMonthYears <- c(
  # The leap months in the following years have 30 days
  6, 14, 19, 25, 33, 36, 38, 41, 44, 52, 
  55, 79,117,136,147,150,155,158,185,193
)

daysInChineseMonth <- function(y,m) {
  # Regular month: m > 0
  # Leap month: m < 0
  index <- y - baseChineseYear + baseIndex
  v <- 0
  l <- 0
  d <- 30 # normal month
  if (1<=m && m<=8) {
    v <- chineseMonths[2*index+1]
    l <- m - 1
    if (bitAnd((bitShiftR(v,l)),0x01)==1 ) d <- 29
  } else if (9<=m && m<=12) {
    v <- chineseMonths[2*index+1+1]
    l <- m - 9
    if ( bitAnd((bitShiftR(v,l)),0x01)==1 ) d <- 29
  } else { # leap month
    v <- chineseMonths[2*index+1+1]
    v <- bitAnd((bitShiftR(v,4)),0x0F)
    if (v!=abs(m)) {
      d <- 0 # wrong m specified
    } else {
      d <- 29
      for (i in (1:length(bigLeapMonthYears))) {
        if (bigLeapMonthYears[i]==index) {
          d <- 30
          break
        }
      }
    }
  }
  return(d)
}

getChinesCalendarDate <- function(date){
  try(if(!is.double(date)) stop("invalid date input"))
  gregorianYear <- as.numeric(substr(as.character(date),0,4))
  gregorianMonth <- as.numeric(substr(as.character(date),6,7))
  gregorianDate <- as.numeric(substr(as.character(date),9,10))
  if(gregorianYear<1901 || gregorianYear>2100) 
    stop("date must within 1901 and 2100")
  startYear <- baseYear
  startMonth <- baseMonth
  startDate <- baseDate
  chineseYear <- baseChineseYear
  chineseMonth <- baseChineseMonth
  chineseDate <- baseChineseDate
  #switching to second base year to reduce calculation time
  #second base date: 01-Jan-2000, 4697/11/25 in Chinese calendar
  if(gregorianYear>=2000){
    startYear <- baseYear + 99
    startMonth <- 1
    startDate <- 1
    chineseYear <- baseChineseYear +99
    chineseMonth <- 11
    chineseDate <- 25
  }
  # Calculating the number of days 
  #    between the start date and the current date
  # The following algorithm only works 
  #    for startMonth = 1 and startDate = 1
  daysDiff <- 0
  if(gregorianYear>startYear){
    for (i in (startYear:(gregorianYear-1))){
      daysDiff %+=% 365
      if(isGeogorianLeapYear(i))
        daysDiff %+=% 1
    }
   }
  if(gregorianMonth>startMonth){
    for (i in (startMonth:(gregorianMonth-1))){
      daysDiff %+=% daysInGregorianMonth(gregorianYear,i)
    }
  }
  daysDiff %+=% (gregorianDate-startDate)
  # Adding that number of days to the Chinese date
  # Then bring Chinese date into the correct range.
  # one Chinese month at a time
  chineseDate %+=% daysDiff
  lastDate <- daysInChineseMonth(chineseYear, chineseMonth)
  nextMonth <- nextChineseMonth(chineseYear, chineseMonth)
  while (chineseDate>lastDate) {
    if (abs(nextMonth)<abs(chineseMonth)) chineseYear %+=% 1
    chineseMonth <- nextMonth
    chineseDate %-=% lastDate
    lastDate <- daysInChineseMonth(chineseYear, chineseMonth)
    nextMonth <- nextChineseMonth(chineseYear, chineseMonth)
  }
  calendar <- paste0(chineseYear,'-',chineseMonth,'-',chineseDate)
  return(calendar)
}

# return the CNY for a given gregorian year
getChineseNewYear <- function(year){
  t <-c()
  firstDay <- as.Date(paste0(as.character(year),"-01-01"))
  t$gregorian <- seq(firstDay, length.out=61, by='day')
  #search for the first 61 days for the CNY
  for(i in t$g){
    d <- getChinesCalendarDate(as.Date(i, origin="1970-01-01"))
    t$chinese <- c(t$chinese,d)
  }
  t <- as.data.table(t)
  t[,cyear:=as.numeric(substr(chinese,0,4))]
  t[,cyear_lag:=shift(cyear,1,type="lag")]
  t[!is.na(cyear_lag),diff:=cyear-cyear_lag]
  as.character(t[diff>0]$gregorian)
}

# return CNY for multiple gregorian years
getChineseNewYears <- function(from,to){
  cny<-c()
  while(from<=to){
    cny <- c(cny,getChineseNewYear(from))
    from %+=% 1
  }
  cny
}

# return a list of HK CNY holidays with a given gregorian year
getHKChineseNewYearHolidays <- function(year){
  d<-as.Date(getChineseNewYear(year))
  if(weekdays(d) %in% c("Friday","Saturday","Sunday"))
    seq(d, length.out=4, by='day')
  else
    seq(d, length.out=3, by='day')
}


##############################################################
# http://www.chinesenewyears.info/chinese-new-year-calendar.php
# Testing getChineseNewYear 
getChineseNewYear(2000) == as.Date("2000-02-05")
getChineseNewYear(2001) == as.Date("2001-01-24")
getChineseNewYear(2002) == as.Date("2002-02-12")
getChineseNewYear(2003) == as.Date("2003-02-01")
getChineseNewYear(2004) == as.Date("2004-01-22")
getChineseNewYear(2005) == as.Date("2005-02-09")
getChineseNewYear(2006) == as.Date("2006-01-29")
getChineseNewYear(2007) == as.Date("2007-02-18")
getChineseNewYear(2008) == as.Date("2008-02-07")
getChineseNewYear(2009) == as.Date("2009-01-26")
getChineseNewYear(2010) == as.Date("2010-02-14")
getChineseNewYear(2011) == as.Date("2011-02-03")
getChineseNewYear(2012) == as.Date("2012-01-23")
getChineseNewYear(2013) == as.Date("2013-02-10")
getChineseNewYear(2014) == as.Date("2014-01-31")
getChineseNewYear(2015) == as.Date("2015-02-19")
getChineseNewYear(2016) == as.Date("2016-02-08")
getChineseNewYear(2017) == as.Date("2017-01-28")
getChineseNewYear(2018) == as.Date("2018-02-16")
getChineseNewYear(2019) == as.Date("2019-02-05")
getChineseNewYear(2020) == as.Date("2020-01-25")

# Testing getChineseNewYears 
getChineseNewYears(2020,2025) == c(as.Date("2020-01-25"),as.Date("2021-02-12"),as.Date("2022-02-01"),as.Date("2023-01-22"),as.Date("2024-02-10"),as.Date("2025-01-29"))

# Testing HK Chinese New Year Holidays
# https://www.timeanddate.com/calendar/?year=2017&country=42
getHKChineseNewYearHolidays(2017) ==c(as.Date("2017-1-28"),as.Date("2017-1-29"),as.Date("2017-1-30"),as.Date("2017-1-31"))
getHKChineseNewYearHolidays(2018) ==c(as.Date("2018-2-16"),as.Date("2018-2-17"),as.Date("2018-2-18"),as.Date("2018-2-19"))
getHKChineseNewYearHolidays(2019) ==c(as.Date("2019-2-5"),as.Date("2019-2-6"),as.Date("2019-2-7"))
getHKChineseNewYearHolidays(2020) ==c(as.Date("2020-1-25"),as.Date("2020-1-26"),as.Date("2020-1-27"),as.Date("2020-1-28"))
getHKChineseNewYearHolidays(2021) ==c(as.Date("2021-2-12"),as.Date("2021-2-13"),as.Date("2021-2-14"),as.Date("2021-2-15"))





