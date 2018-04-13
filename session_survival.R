#ocf_sessions <- read.csv("~/remote/ocf_boc/query_result_2018-03-13T20_52_33.412Z.csv", stringsAsFactors=FALSE, na.strings = "")
sessions <- read.csv("~/remote/ocf_boc/session_duration_public.csv", stringsAsFactors=FALSE, na.strings = "NULL")
staff_sessions <- read.csv("~/remote/ocf_boc/staff_session_duration_public.csv", stringsAsFactors=FALSE,na.strings = "NULL")

library(dplyr)
library(lubridate)
library(ggplot2)

#nonstaff_sessions

sessions = sessions %>% 
  filter(complete.cases(sessions)) %>%
  mutate(duration = time_length(interval(start = start, end = end), unit="minute"))

staff_sessions = staff_sessions %>% 
  filter(complete.cases(staff_sessions)) %>%
  mutate(duration = time_length(interval(start = start, end = end), unit="minute"))

public_sessions = sessions %>% anti_join(staff_sessions, by="id")

######
######
top=240

par(mfrow=c(1,1))
time_intervals = seq(0, 100, 1)
densitys_x = data.frame()
densitys_y = data.frame()
priors = numeric()
plot(density(public_sessions$duration[public_sessions$duration < top & public_sessions$duration > 0]),
     "Densities")
for(i in 1:length(time_intervals)) {
  time = time_intervals[i]
  sessions_durations = public_sessions$duration[public_sessions$duration < top & public_sessions$duration > time]
  temp = density(sessions_durations)
  lines(temp$x - time, temp$y, col=rgb(time/100, 1-time/100, 0))
  priors[i] = length(sessions_durations)
}

  time_intervals = seq(0, 100, 1)
  densitys_x = data.frame()
  densitys_y = data.frame()
  priors = numeric()
  plot(density(staff_sessions$duration[staff_sessions$duration < top & staff_sessions$duration > 0]),
       "Densities")
  for(i in 1:length(time_intervals)) {
    time = time_intervals[i]
    sessions_durations = staff_sessions$duration[staff_sessions$duration < top & staff_sessions$duration > time]
    temp = density(sessions_durations)
    lines(temp$x - time, temp$y, col=rgb(time/100, 1-time/100, 0))
    priors[i] = length(sessions_durations)
  }
  
sp18 = public_sessions %>% filter(date(start) >= "2018-01-16") %>% arrange(desc(end))
wait_times = as.numeric(difftime(sp18$end[1:(length(sp18$end)-1)], sp18$end[2:length(sp18$end)])) 
sp18_wait = sp18 %>% 
  mutate(wait_time = c(0, as.numeric(difftime(sp18$end[1:(length(sp18$end)-1)], sp18$end[2:length(sp18$end)])))) %>%
  mutate(weekday = weekdays(date(start))) %>%
  mutate(hour = hour(end))

View(sp18_wait %>% group_by(hour, weekday) %>% summarise(mean=mean(wait_time), 
                                                         median=median(wait_time), 
                                                         var=var(wait_time)))
####
get_time = function(time_stamp) {
  # given time stamp, extract the time down to minutes 
  return(substr(time_stamp, 12, 16))
}

####
time_int = "10 minutes"
interval2 = "10 min"
begin_date = as.POSIXct(as.Date(min(sp18$start)), tz='America/Los_Angeles') + 8*60*60
end_date  = as.POSIXct(as.Date(max(sp18$end)), tz='America/Los_Angeles')+ 8*60*60 
sp18_time_seq = seq.POSIXt(as.POSIXct("2018/01/15"), as.POSIXct("2018/01/22"), by = interval2,tz='America/Los_Angeles')
####
sp18_hour = sp18_wait %>% 
  filter(hour >= 9 & hour < 18) %>%
  mutate(time = get_time(round_date(as.POSIXct(end), unit=time_int)))

sp18_end =  sp18_hour %>%
  group_by(time, weekday) %>% 
  summarise(mean=mean(wait_time), 
            median=median(wait_time),
            q1=quantile(wait_time, 0.25),
            q3=quantile(wait_time, 0.75),
            n=n())

sp18_util = data.frame(date=date(sp18_time_seq), 
                       hour = hour(sp18_time_seq), 
                       time = get_time(sp18_time_seq)) %>% 
  mutate(weekday = weekdays(date)) %>%
  filter(hour >= 9 & hour < 18) %>%
  select(time, weekday) %>%
  mutate(time=as.character(time))

sp18_waittime = sp18_util %>% 
  left_join(sp18_end, by=c("time", "weekday")) %>%
  mutate(mean = ifelse(is.na(mean), 0, mean)) %>%
  mutate(median = ifelse(is.na(median), 0, median)) %>%
  mutate(n = ifelse(is.na(n), 0, n))
####
ggplot(sp18_waittime 
       %>% filter(!(weekday %in% c("Saturday", "Sunday")))) +
  geom_line(aes(x=time,y=median,group=weekday, color=weekday)) +
  ylim(0, 150)

ggplot(sp18_waittime 
       %>% filter((weekday %in% c("Monday", "Wednesday")))) +
  geom_line(aes(x=time,y=median,group=weekday, color=weekday)) +
  geom_line(aes(x=time,y=mean,group=weekday, color=weekday)) +
  ylim(0, 100)+ 
  theme(axis.text.x = element_text(angle=90))

ggplot(sp18_waittime 
       %>% filter((weekday %in% c("Tuesday", "Thursday")))) +
  geom_line(aes(x=time,y=median,group=weekday, color=weekday)) +
  ylim(0, 100)+ 
  theme(axis.text.x = element_text(angle=90))

#, "Thursday"
day_of_week = "Wednesday"
ggplot(sp18_waittime 
       %>% filter((weekday %in% c(day_of_week))), aes(x=time)) +
  geom_line(aes(y=median,group=weekday, color=weekday)) +
  geom_errorbar(aes(ymin=q1, ymax=q3, colour="black")) +
  ylim(0, 100) + 
  theme(axis.text.x = element_text(angle=90)) +
  geom_text(aes(y=median, label=median)) +
  geom_text(aes(y=q1, label=q1)) +
  geom_text(aes(y=q3, label=q3)) 

ggplot(sp18_waittime 
       %>% filter((weekday %in% c("Saturday", "Sunday")))) +
  geom_line(aes(x=time,y=median,group=weekday, color=weekday))
####
# visualizing wait time distributions
####
x = aggregate(wait_time~time + weekday, sp18_hour %>% filter(weekday=="Monday" & wait_time < 1000), I)

par(mfrow=c(4,4), mar=c(2,6,2,2))
for (i in seq(1, 55, 1)) {
  index = i
  hist(x[index,3][[1]], breaks=length(x[index,3][[1]])/2,
       xlim=c(0, 200),
       prob=T,
       main = paste(x[index,2], x[index,1]),
       xlab = "wait time in seconds")
  abline(v=median(x[index,3][[1]]), col="blue", lwd=2)
  abline(v=mean(x[index,3][[1]]), col="red", lwd=2 )
  legend(x = "topright", # location of legend within plot area
         c("Median", "Mean"),
         col = c("royalblue", "red"),
         lwd = c(2,2)
         )
}
