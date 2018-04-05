install.packages(c("ggplot","magrittr","lubridate","dplyr","glmnet"))
library(ggplot2); library(magrittr); library(lubridate) ; library(dplyr)

# extra functions

extract_numerics <- function(x){
  strsplit(x," ") %>% lapply(function(x){
    nums <- which(!is.na(as.numeric(x)))
    return(as.numeric(x[nums]))
  }) %>% unlist
}

number_of_days <- function(x,y){
  c(lubridate::days(x-y) %>% as.numeric())/(60*60*24)
}


xml <- xml2::read_html("clds.html")

attendance <- rvest::html_text(rvest::html_nodes(xml,".avatarRow--attendingCount"))[1:26] %>% extract_numerics()
dates <- rvest::html_text(rvest::html_nodes(xml,".eventTimeDisplay-startDate"))[1:26]
title <-rvest::html_text(rvest::html_nodes(xml,".eventCardHead--title"))[1:26]
comments <- rvest::html_text(rvest::html_nodes(xml,".eventCard--expandedInfo-comments"))[1:26] %>% extract_numerics()

meetup_dates <- as.Date(dates,"%A, %B %d, %Y, %I:%M %p")
day <- strptime(dates,"%A, %B %d, %Y, %I:%M %p")

meetup_data <- data.frame("Meetup"=title,
                          "Date"=meetup_dates,
                          "Attendance"=attendance,
                          "WeekDay"=weekdays(meetup_dates),
                          "Comments"= comments)


cd <- data.table::fread("clds.txt")

dates <- cd$JoinedGroup
tabs <- table(dates)

df <- data.frame("Date"=lubridate::ymd(names(tabs)),"Members"=as.numeric((tabs)))
df2 <- df[order(df$Date),]
df2$Members <- cumsum(df2$Members)


## let's now add some extra covariates
full_data <- dplyr::left_join(df2,meetup_data,by="Date")
write.csv(full_data,"clds.txt",row.names = FALSE)
