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
      return(access_token)
    }
  })

jawboneActivities$methods(
  retrieveJUsleepList = function() {
    
    if(is.na(access_token)) stop("Not connected to Jawebone UP. Please call function connectJU")
    else{
      
    sleep.j<-getURL(url=paste("https://jawbone.com/nudge/api/users/@me/sleeps?limit=0"),httpheader=c("x-nudge-token"=access_token)) #,verbose=TRUE
    sleep.parsed <- fromJSON(sleep.j)

    #names(sleep.parsed$data$items[[1]])
    s_size<-1:length(sleep.parsed$data$items) #number of all the sleep activities recorded
    #******extract key sleep data******
    #label explanation source: https://jawbone.com/up/developer/endpoints/sleeps

    s_xid <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$xid)) #unique id of this specific event
    s_date <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$date)) #Date when this sleep was created factoring in user timezone, formatted as YYYYMMDD
    s_subtype <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$sub_type)) #Type of sleep. 0=normal, 1=power_nap, 2=nap
    s_duration <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$duration)) #lenght of sleep in seconds
    s_asleep_time <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$asleep_time)) #Epoch timestamp when the user fell asleep.
    s_awake <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$awake)) #Epoch timestamp when the user wake up.
    s_deep <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$sound)) #Total deep/sound sleep time, in seconds.
    s_light <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$light)) #Total light sleep time, in seconds.
    s_quality <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$quality)) #Sleep quality for the night. Based on a JAWBONE proprietary formula of light and deep sleep vs wake time.
    s_rem <- unlist(lapply(s_size, function(i) sleep.parsed$data$items[[i]]$details$rem)) #rem is not in used in Jawbone UP yet

    str(s_quality)
    str(s_duration)
    print(s_quality)
    
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
    
    m_xid <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$xid)) #unique id of this specific event
    m_date <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$date)) #Date when this move was created factoring in user timezone, formatted as YYYYMMDD
    m_distance <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$distance))
    m_distance_km <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$km))
    m_inactive_time <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$inactive_time))
    m_longest_active <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$longest_active))
    m_longest_idle <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$longest_idle))
    m_steps <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$steps))
    m_calories <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$calories))
    m_workout_count <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$wo_count))
    m_workout_time <- unlist(lapply(m_size, function(i) move.parsed$data$items[[i]]$detail$wo_time))
    
    move <<- list("m_xid"=m_xid,"m_date"=m_date,"m_distance"=m_distance,
                  "m_distance_km"=m_distance_km,"m_inactive_time"=m_inactive_time,"m_longest_active"=m_longest_active,
                  "m_longest_idle"=m_longest_idle,"m_steps"=m_steps,"m_calories"=m_calories,"m_workout_count"=m_workout_count,
                  "m_workout_time"=m_workout_time)
    
    print(m_calories)
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
      
      wo_xid <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$xid)) #unique id of this specific event
      wo_date <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$date)) #Date when this move was created factoring in user timezone, formatted as YYYYMMDD
      wo_type <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$sub_type)) #Workout type, see Type table below.
      wo_steps <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$steps)) #Number of steps during this workout. Note steps will only be provided if the user was wearing the physical UP band during workout.
      wo_time <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$time)) #Total time for this workout, in seconds.
      wo_distance_m <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$meters)) #Total distance travelled, in meters.
      wo_intensity <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$intensity)) #Intensity of the workout as selected by the user or 3rd party app. 1 = easy, 2 = moderate, 3 = intermediate, 4 = difficult, 5 = hard
      wo_calories <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$calories)) #Total calories burned during workout.
      wo_bmr <- unlist(lapply(wo_size, function(i) workout.parsed$data$items[[i]]$details$bmr)) #Basal metabolic rate during workout.
      
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
  },
  
  createJUsleep = function(s_startTime="character",s_endTime="character"){
    #timezone is the local computer timezone
    #time format:"2014-3-15 12:00:00"
    s_startTime<-as.POSIXct(s_startTime)
    s_endTime <- as.POSIXct(s_endTime)
    cat("The sleep starts at",format(s_startTime), "and ends at",format(s_endTime),"\n")
    createSleep.j <- postForm(uri = "https://jawbone.com/nudge/api/users/@me/sleeps",
                            .opts=list(httpheader=c("x-nudge-token"= access_token)), #,verbose=TRUE)
                            .params=list(time_created=as.integer(s_startTime),
                                         time_completed=as.integer(s_endTime)),
                            style="POST", .checkParams = TRUE)
    createSleep.parsed <- fromJSON(createSleep.j)
    print(createSleep.parsed$meta$message == "Created")
    print(createSleep.parsed)
    if(createSleep.parsed$meta$message == "Created"){
      cat("The sleep event",createSleep.parsed$data$xid, "(", 
          format(as.POSIXct(createSleep.parsed$data$details$awake_time,origin="1970-01-01")),
          "-", format(as.POSIXct(createSleep.parsed$data$details$asleep_time,origin="1970-01-01")),
          ") is successfully created.")
      return(.self$retrieveJUsleepList())
    }
    else
      return(print("Failed to create a sleep event. Please check the input format or try reconnect to Jawbone UP."))
  },

deleteJUsleep= function(s_xid="character"){
    deleteSleep.j <- httpDELETE(url = paste("https://jawbone.com/nudge/api/sleeps/",s_xid,sep=""), 
                                httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
    deleteSleep.parsed <- fromJSON(deleteSleep.j)
    print(deleteSleep.parsed$meta$message)
    if(deleteSleep.parsed$meta$message=="OK"){
      cat("The sleep event(",s_xid,")is successfully deleted.")
      return(.self$retrieveJUsleepList())
    }
    else
        print("Failed to delete a sleep event. Please check the input format or try reconnect to Jawbone UP.")
    },

createJUworkout = function(wo_startTime="character",wo_endTime="character",wo_subType="numeric",
                           wo_calories="numeric",wo_distance="numeric",wo_intensity="integer"){
  #required parameters: startTime, endTime, and intensity
  #timezone is the local computer timezone
  #time format:"2014-3-15 12:00:00"
  print(wo_startTime)
  wo_startTime<-as.POSIXct(wo_startTime)
  print(wo_startTime)
  wo_endTime <- as.POSIXct(wo_endTime)
  cat("The workout starts at",format(wo_startTime), "and ends at",format(wo_endTime),"\n")
  if(missing(wo_calories)) wo_calories <- 0
  if(missing(wo_distance)) wo_distance <- 0
  if(missing(wo_subType))  wo_subType <- 0
  createWorkout.j <- postForm(uri = "https://jawbone.com/nudge/api/users/@me/workouts",
                            .opts=list(httpheader=c("x-nudge-token"= access_token)), #, verbose=TRUE
                            .params=list(time_created=as.integer(wo_startTime),
                                         time_completed=as.integer(wo_endTime),
                                         subtype=wo_subType,
                                         calories=wo_calories,
                                         distance=wo_distance,
                                         intensity=wo_intensity),
                            style="POST", .checkParams = TRUE)
  createWorkout.parsed <- fromJSON(createWorkout.j)
  print(createWorkout.parsed$meta$message)
  #print(createWorkout.parsed)
  if(createWorkout.parsed$meta$message == "Created"){
    cat("The workout event",createWorkout.parsed$data$xid, "(", 
        format(as.POSIXct(createWorkout.parsed$data$time_created,origin="1970-01-01")),
        "-", format(as.POSIXct(createWorkout.parsed$data$time_completed,origin="1970-01-01")),
        ") is successfully created.")
    return(.self$retrieveJUworkoutList())
  }
  else
    return(print("Failed to create a workout event. Please check the input format or try reconnect to Jawbone UP."))
},

deleteJUworkout= function(wo_xid="character"){
  deleteWorkout.j <- httpDELETE(url = paste("https://jawbone.com/nudge/api/workouts/",wo_xid,sep=""), 
                              httpheader=c("x-nudge-token"=access_token),verbose=TRUE)
  deleteWorkout.parsed <- fromJSON(deleteWorkout.j)
  print(deleteWorkout.parsed$meta$message)
  if(deleteWorkout.parsed$meta$message=="OK"){
    cat("The workout event(",wo_xid,")is successfully deleted.")
    return(.self$retrieveJUworkoutList())
  }
  else
    print("Failed to delete a workout event. Please check the input format or try reconnect to Jawbone UP.")
},

sleepQualvsQuanGraph = function(){
  .self$retrieveJUsleepList()
  sleep.df <- as.data.frame(.self$sleep, stringsAsFactors=FALSE)
  sleep.df <- sleep.df[!sleep.df$s_quality==0,]
  print(sleep.df)
  sleep.qualquan <- ggplot(sleep.df,aes(x=s_duration/3600,y=s_quality))+geom_point()+
      labs(x = "Hours of sleep", y = "Sleep quality")
  sleep.qualquan + facet_wrap(~s_subtype, scales = "free_y")
},

sleepDurationGraph = function(){
  .self$retrieveJUsleepList()
  sleep.df <- as.data.frame(.self$sleep, stringsAsFactors=FALSE)
  date.str<-unlist(lapply(sleep.df$s_date,as.character))
  date.str<-as.Date(date.str,"%Y%m%d")
  sleep.df <- data.frame(sleep.df,date.str)
  sleep.duration <- ggplot(sleep.df, aes(x=date.str, y=s_duration/3600))+geom_point()+
    labs(x = "Date", y = "Hours of sleep") + scale_x_date(labels = date_format("%m-%Y"))
  sleep.duration + facet_wrap(~s_subtype, scales = "free_y")
},

sleepQualityGraph = function(){
  .self$retrieveJUsleepList()
  sleep.df <- as.data.frame(.self$sleep, stringsAsFactors=FALSE)
  date.str<-unlist(lapply(sleep.df$s_date,as.character))
  date.str<-as.Date(date.str,"%Y%m%d")
  sleep.df <- data.frame(sleep.df,date.str)
  sleep.quality <- ggplot(sleep.df, aes(x=date.str, y=s_quality))+geom_point()+
    labs(x = "Date", y = "Sleep quality") +
    scale_x_date(labels = date_format("%m-%Y"))
  sleep.quality + facet_wrap(~s_subtype, scales = "free_y")
},

moveStepGraph = function(){
  .self$retrieveJUmoveList()
  move.df <- as.data.frame(.self$move, stringsAsFactors=FALSE)
  date.str<-unlist(lapply(sleep.df$s_date,as.character))
  date.str<-as.Date(date.str,"%Y%m%d")
  move.df <- data.frame(move.df,date.str)
  move.step <- ggplot(move.df, aes(x=date.str, y=m_steps))+geom_line()+  #make it a line trend
    labs(x = "Date", y = "Steps taken") +
    scale_x_date(labels = date_format("%m-%Y"))
  
  #add calorie line; add a few more lines
}
)

