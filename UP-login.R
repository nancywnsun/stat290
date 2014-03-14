#install.packages("RCurl")
#install.packages("RJSONIO")

library(RCurl)
library(RJSONIO)
##take in sample account information
source("accountinfo.R")

#Connect to Jawbone UP API
connectUP.j = postForm(uri = "https://jawbone.com/user/signin/login", .params=list(email=username, pwd = pw, service =service),.checkParams = TRUE)

connectUP<-fromJSON(connectUP.j)

access_token <- connectUP$token
access_token

#obtain sleep data
sleep.j<-getURL(url=paste("https://jawbone.com/nudge/api/users/@me/sleeps?limit=0"),httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
sleep <- fromJSON(sleep.j)
#construct vector from JSON
#df<-as.data.frame(unlist(sleep$data$items[[1]]))

names(sleep$data$items[[1]])
s_size<-1:length(sleep$data$items)

#******extract key sleep data******
#label explanation source: https://jawbone.com/up/developer/endpoints/sleeps

s_xid <- lapply(s_size, function(i) sleep$data$items[[i]]$xid) #unique id of this specific event
s_date <- lapply(s_size, function(i) sleep$data$items[[i]]$date) #Date when this sleep was created factoring in user timezone, formatted as YYYYMMDD
s_subtype <- lapply(s_size, function(i) sleep$data$items[[i]]$sub_type) #Type of sleep. 0=normal, 1=power_nap, 2=nap
s_duration <- lapply(s_size, function(i) sleep$data$items[[i]]$details$duration) #lenght of sleep in seconds
s_asleep_time <- lapply(s_size, function(i) sleep$data$items[[i]]$details$asleep_time) #Epoch timestamp when the user fell asleep.
s_awake <- lapply(s_size, function(i) sleep$data$items[[i]]$details$awake) #Epoch timestamp when the user wake up.
s_deep <- lapply(s_size, function(i) sleep$data$items[[i]]$details$sound) #Total deep/sound sleep time, in seconds.
s_light <- lapply(s_size, function(i) sleep$data$items[[i]]$details$light) #Total light sleep time, in seconds.
s_quality <- lapply(s_size, function(i) sleep$data$items[[i]]$details$quality) #Sleep quality for the night. Based on a JAWBONE proprietary formula of light and deep sleep vs wake time.
s_rem <- lapply(s_size, function(i) sleep$data$items[[i]]$details$rem) #rem is not in used in Jawbone UP yet

#obtain move data
move.j<-getURL(url=paste("https://jawbone.com/nudge/api/users/@me/moves?limit=0"),httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
move <- fromJSON(move.j)

m_size <- 1:length(move$data$items)
move$data$size
names(move$data$items[[1]]$details)

#extract key activity data
m_xid <- lapply(m_size, function(i) move$data$items[[i]]$xid) #unique id of this specific event
m_date <- lapply(m_size, function(i) move$data$items[[i]]$date) #Date when this move was created factoring in user timezone, formatted as YYYYMMDD
m_total_calaries <- lapply(m_size, function(i) move$data$items[[i]]$detail$calaries) #Total calories burned. This is computed by this formula: wo_calories+bg_calories+bmr_day / 86400 * active_time
m_distance <- lapply(m_size, function(i) move$data$items[[i]]$detail$distance)
m_distance_km <- lapply(m_size, function(i) move$data$items[[i]]$detail$km)
m_distance_inactive_time <- lapply(m_size, function(i) move$data$items[[i]]$detail$inactive_time)
m_distance_lcat <- lapply(m_size, function(i) move$data$items[[i]]$detail$longest_active)
m_distance_lcit <- lapply(m_size, function(i) move$data$items[[i]]$detail$longest_idle)
m_steps <- lapply(m_size, function(i) move$data$items[[i]]$detail$steps)
m_calories <- lapply(m_size, function(i) move$data$items[[i]]$detail$bg_calories)
m_workout_count <- lapply(m_size, function(i) move$data$items[[i]]$detail$wo_count)
m_workout_time <- lapply(m_size, function(i) move$data$items[[i]]$detail$wo_time)

#obtain goal data
goal.j<-getURL(url=paste("https://jawbone.com/nudge/api/users/@me/goals"),httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
goal <- fromJSON(goal.j)

names(goal$data)

#extract goal data
sleep_goal <- goal$data$sleep_total
step_goal <- goal$data$move_steps










#sleep.df <- as.data.frame(matrix(unlist(sleep.ul),nrow=38,byrow=T))
#sleep.df <- do.call(rbind.data.frame,sleep.ul)

#indrect csv import from Jawbone Website
sample<-read.table("2014.csv",header=TRUE,sep=",")


