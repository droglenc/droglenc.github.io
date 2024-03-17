library(tidyverse)
library(lubridate)
library(patchwork)

theme_set(theme_minimal())

dumdate <- as.Date("1-Jan-2025",format="%d-%b-%Y")
inclr <- "darkblue"
outclr <- "darkred"
icefillclr <- FSA::col2rgbt("#95d0fc",0.35)
main_lw <- 1.5
main_clr <- "gray30"
yr_breaks10 <- seq(1800,2100,10)
yr_breaks5 <- seq(1800,2100,5)

## Make date breaks based on days since Jan-1 (of winter year)
byr <- 2001
date_breaks1 <- make_date(year=c(rep(byr,2),rep(byr+1,5)),
                          month=c(11,12,1,2,3,4,5),day=1)
date_labels1 <- month(date_breaks1,label=TRUE)
date_breaks1 <- as.numeric(date_breaks1-make_date(byr+1,1,1))

date_breaks2 <- make_date(year=c(rep(byr,4),rep(byr+1,10)),
                          month=rep(c(11,12,1,2,3,4,5),each=2),
                          day=rep(c(1,15),times=7))
date_labels2 <- paste(month(date_breaks2,label=TRUE),day(date_breaks2))
date_breaks2 <- as.numeric(date_breaks2-make_date(byr+1,1,1))


prepIceData <- function(sheet,fn,skip=3) {
  tmp <- readxl::read_excel(fn,sheet=sheet,skip=skip,
                            col_types=c("text","date","date")) %>%
    mutate(lake=sheet,
           fyr=as.numeric(stringr::str_extract(season,"[^-]+")),
           wyr=fyr+1,
           fleapyr=leap_year(fyr),
           wleapyr=leap_year(wyr),
           inYr=ifelse(month(iceIn)>6,fyr,wyr),
           inDate=make_date(inYr,month(iceIn),day(iceIn)),
           inDay=as.numeric(inDate-make_date(wyr,1,1)),
           outYr=ifelse(month(iceOut)<6,wyr,fyr),
           outDate=make_date(outYr,month(iceOut),day(iceOut)),
           outDay=as.numeric(outDate-make_date(wyr,1,1)),
           daysIced=as.numeric(outDay-inDay)) %>%
    select(lake,season,fyr,wyr,fleapyr,wleapyr,
           inDate,inYr,inDay,
           outDate,outYr,outDay,daysIced)
  ## Identify years with two recordings (ice on, then off, and then on again) ...
  ##   keep the 2nd recording (so delete row before duplicate was identified)
  dups <- which(duplicated(tmp$season))-1
  if (length(dups)>=1) tmp <- tmp[-dups,]
  ## Return data frame
  data.frame(tmp)
}

sumIceData <- function(d) {
  dumdate <- as.Date("1-Jan-2025",format="%d-%b-%Y")
  tmp <- d %>%
    summarize(inAvgDay=round(mean(inDay,na.rm=TRUE),0),
              inEarlyDay=min(inDay,na.rm=TRUE),
              inLateDay=max(inDay,na.rm=TRUE),
              outAvgDay=round(mean(outDay,na.rm=TRUE),0),
              outEarlyDay=min(outDay,na.rm=TRUE),
              outLateDay=max(outDay,na.rm=TRUE),
              durAvg=round(mean(daysIced,na.rm=TRUE),0),
              durShort=min(daysIced,na.rm=TRUE),
              durLong=max(daysIced,na.rm=TRUE)) %>%
    mutate(lake=d$lake[1],
           inAvgDate=format(dumdate + inAvgDay,format="%b %e"),
           inEarlyDate=format(dumdate + inEarlyDay,format="%b %e"),
           inLateDate=format(dumdate + inLateDay,format="%b %e"),
           outAvgDate=format(dumdate + outAvgDay,format="%b %e"),
           outEarlyDate=format(dumdate + outEarlyDay,format="%b %e"),
           outLateDate=format(dumdate + outLateDay,format="%b %e")) %>%
    select(lake,inAvgDay,inAvgDate,inEarlyDay,inEarlyDate,inLateDay,inLateDate,
           outAvgDay,outAvgDate,outEarlyDay,outEarlyDate,outLateDay,outLateDate,
           durAvg,durShort,durLong)
  tmp
}

## For converting dat files from https://nsidc.org/data/lake_river_ice/freezethaw.html
##   to a CSV file that I put in the IceDates.xlsx file.
parseNSIDC <- function(fnout,fnin="liag_ice.dat") {
  tmp <- readr::read_tsv(fnin,na=c("","NA","-999")) %>%
    mutate(iceIn=make_date(iceon_year,iceon_month,iceon_day),
           iceIn=paste(month(iceIn,abbr=TRUE),day(iceIn),sep="-"),
           iceOut=make_date(iceoff_year,iceoff_month,iceoff_day),
           iceOut=paste(month(iceOut,abbr=TRUE),day(iceOut),sep="-")) %>%
    select(season,iceIn,iceOut)
  write.csv(tmp,fnout,quote=FALSE,row.names=FALSE)
}
