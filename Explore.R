library(devtools)

install_github("ahalterman/phoxy")

library(phoxy)
library(dplyr)
library(magrittr)
library(data.table)
library(lubridate)
library(ggplot2)

download_phoenix("./Phoenix")
events <- data.table(ingest_phoenix("./Phoenix"))

# Count by Year
temp <- events %>%
  group_by(Year) %>%
  summarize(Count=n())
ggplot(temp, aes(Year, Count)) + geom_bar(stat="identity")
ggplot(temp, aes(Year, log10(Count))) + geom_bar(stat="identity") + labs(y="Count Order of Magnitude")

# Count by Month
temp <- events %>%
  group_by(Year, Month) %>%
  summarize(Count=n()) %>%
  mutate(Date = Year+(Month-1)/12) %>%
  filter(Date > 2014) %>%
  arrange(Date)
ggplot(temp, aes(Date, Count)) + geom_bar(stat="identity")
ggplot(temp, aes(Date, log10(Count))) + geom_bar(stat="identity") + labs(y="Count Order of Magnitude")

# Count by Day
temp <- events %>%
  group_by(Date) %>%
  filter(Date > 20140700) %>%
  summarize(Count=n()) %>%
  arrange(Date)
ggplot(temp, aes(lubridate::ymd(Date), Count)) + geom_point() + labs(x="Date")

# Count by Day of Week
weekdays<-c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
temp <- events %>%
  filter(Date > 20140700) %>%
  mutate(wDay=wday(ymd(Date)), Days=weekdays[wDay]) %>%
  group_by(wDay) %>%
  summarize(Count=n())
ggplot(temp, aes(weekdays[wDay], Count)) + geom_bar(stat="identity") + labs(x="Week Day") + scale_x_discrete(limits=weekdays)

# Counts of Event Roots Over Time
temp <- events %>%
  filter(Date > 20140700) %>%
  group_by(Date, EventRootCode) %>%
  summarize(Count=n())
ggplot(temp, aes(ymd(Date), Count, color=EventRootCode)) + geom_line()

# Counts of Quad Classes Over Time
CAMEO <- read.csv("CAMEO.csv")
temp <- events %>%
  filter(Date > 20140700, QuadClass > 0) %>%
  group_by(Date) %>%
  summarize(DailyCount=n()) %>%
  group_by(QuadClass) %>%
  mutate(QCF=n()/DailyCount)
ggplot(temp, aes(ymd(Date), Count, color=QuadClass)) + geom_line()

