#install.packages("RCurl")
#install.packages("RJSONIO")

library(RCurl)
library(RJSONIO)
##take in sample account information
#source("accountinfo.R")

jawboneActivities <- setRefClass(Class = "jawboneActivities",
                                fields = list(username = "character",
                                              password = "character",
                                              sleep = "vector",
                                              move = "vector",
                                              workout = "vector",
                                              goal = "vector",
                                              access_token = "character"
                                ))

jawboneActivities$methods(
  initialize = function(...) {
    sleep <<- vector("list")
    move <<- vector("list")
    workout <<- vector("list")
    goal <<- vector("list")
    access_token <<- character()
    callSuper(...)
  })

jawboneActivities$methods(
  connectJU = function() {
    "Login to Jawbone Connect"
    
    #Connect to Jawbone UP API via postForm
    
    connectUP.j = postForm(uri = "https://jawbone.com/user/signin/login", .params=list(email=username, pwd = password, service = "nudge"),.checkParams = TRUE)
    connectUP<-fromJSON(connectUP.j)
    access_token <<- connectUP$token
    
    # Status report
    if (length(connectUP$error)!=0) stop("Connection to JawboneUP failed - validation")
    else {
      cat("User", username, "successfully connected to Jawbone UP", "\n")
      #return(access_token)
    }
  })

jawboneActivities$methods(
  retrieveJUsleepList = function() {
    
    if(is.na(access_token)) stop("Not connected to Jawebone UP. Please call function connectJU")
    else{
    print("OK start get URL")  
    sleep.j<-getURL(url=paste("https://jawbone.com/nudge/api/users/@me/sleeps?limit=0"),httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
    print("ok geturl")
    sleep.parsed <- fromJSON(sleep.j)
    print("ok parsed")

    #names(sleep.parsed$data$items[[1]])
    s_size<-1:length(sleep.parsed$data$items) #number of all the sleep activities recorded
    print("ok size")
    #******extract key sleep data******
    #label explanation source: https://jawbone.com/up/developer/endpoints/sleeps

    s_xid <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$xid) #unique id of this specific event
    print("ok xid")
    s_date <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$date) #Date when this sleep was created factoring in user timezone, formatted as YYYYMMDD
    s_subtype <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$sub_type) #Type of sleep. 0=normal, 1=power_nap, 2=nap
    s_duration <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$duration) #lenght of sleep in seconds
    s_asleep_time <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$asleep_time) #Epoch timestamp when the user fell asleep.
    s_awake <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$awake) #Epoch timestamp when the user wake up.
    s_deep <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$sound) #Total deep/sound sleep time, in seconds.
    s_light <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$light) #Total light sleep time, in seconds.
    s_quality <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$quality) #Sleep quality for the night. Based on a JAWBONE proprietary formula of light and deep sleep vs wake time.
    s_rem <- lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$rem) #rem is not in used in Jawbone UP yet
    
    sleep <<- list("s_xid"=s_xid,"s_date"=s_date,"s_subtype"=s_subtype,"s_duration"=s_duration,
                  "s_asleep_time"=s_asleep_time,"s_awake"=s_awake,"s_deep"=s_deep,"s_light"=s_light,"s_quality"=s_quality,"s_rem"=s_rem)
    #return(sleep)
    }
},

  retrieveJUmoveList = function() {
    
    #obtain move data
    move.j<-getURL(url=paste("https://jawbone.com/nudge/api/users/@me/moves?limit=0"),httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
    move.parsed <- fromJSON(move.j)
    
    m_size <- 1:length(move.parsed$data$items)
    #move$data$size
    #names(move$data$items[[1]]$details)
    
    #******extract key activity data******
    #label explanation source: https://jawbone.com/up/developer/endpoints/moves
    
    m_xid <- lapply(m_size, function(i) move.parsed$data$items[[i]]$xid) #unique id of this specific event
    m_date <- lapply(m_size, function(i) move.parsed$data$items[[i]]$date) #Date when this move was created factoring in user timezone, formatted as YYYYMMDD
    m_total_calaries <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$calaries) #Total calories burned. This is computed by this formula: wo_calories+bg_calories+bmr_day / 86400 * active_time
    m_distance <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$distance)
    m_distance_km <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$km)
    m_inactive_time <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$inactive_time)
    m_longest_active <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$longest_active)
    m_longest_idle <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$longest_idle)
    m_steps <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$steps)
    m_calories <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$bg_calories)
    m_workout_count <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$wo_count)
    m_workout_time <- lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$wo_time)
    
    move <<- list("m_xid"=m_xid,"m_date"=m_date,"m_total_calaries"=m_total_calaries,"m_distance"=m_distance,
                  "m_distance_km"=m_distance_km,"m_inactive_time"=m_inactive_time,"m_longest_active"=m_longest_active,
                  "m_longest_idle"=m_longest_idle,"m_steps"=m_steps,"m_calories"=m_calories,"m_workout_count"=m_workout_count,
                  "m_workout_time"=m_workout_time)
  },

    retrieveJUworkoutList = function() {
      
      #obtain workout data
      workout.j<-getURL(url=paste("https://jawbone.com/nudge/api/users/@me/workouts?limit=0"),httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
      workout.parsed <- fromJSON(workout.j)
      
      wo_size <- 1:length(workout.parsed$data$items)
      names(workout.parsed$data$items[[1]]$details)
      names(workout.parsed$data$item[[1]])
      
      #******extract key activity data******
      #label explanation source: https://jawbone.com/up/developer/endpoints/moves
      
      wo_xid <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$xid) #unique id of this specific event
      wo_date <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$date) #Date when this move was created factoring in user timezone, formatted as YYYYMMDD
      wo_type <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$sub_type) #Workout type, see Type table below.
      wo_steps <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$steps) #Number of steps during this workout. Note steps will only be provided if the user was wearing the physical UP band during workout.
      wo_time <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$time) #Total time for this workout, in seconds.
      wo_distance_m <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$meters) #Total distance travelled, in meters.
      wo_intensity <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$intensity) #Intensity of the workout as selected by the user or 3rd party app. 1 = easy, 2 = moderate, 3 = intermediate, 4 = difficult, 5 = hard
      wo_calories <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$calories) #Total calories burned during workout.
      wo_bmr <- lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$bmr) #Basal metabolic rate during workout.
      
      workout <<- list("wo_xid"=wo_xid,"wo_date"=wo_date,"wo_type"=wo_type,"wo_steps"=wo_steps,"wo_time"=wo_time,
                       "wo_distance_m"=wo_distance_m,"wo_intensity"=wo_intensity,"wo_calories"=wo_calories,
                       "wo_bmr"=wo_bmr)
    },

  retriveJUgoalList = function() {
    #obtain goal data
    goal.j<-getURL(url=paste("https://jawbone.com/nudge/api/users/@me/goals"),httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
    goal.parsed <- fromJSON(goal.j)
    
    names(goal$data)
    
    #extract goal data
    sleep_goal <- goal.parsed$data$sleep_total
    step_goal <- goal.parsed$data$move_steps
    
    goal <<- list("sleep_goal"=sleep_goal,"step_goal"=step_goal)
  })
    
    
    
