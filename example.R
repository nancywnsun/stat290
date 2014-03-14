source("UP-connect.R")

test<-jawboneActivities$new(username="",password="")

test$connectJU()

test.sleep<-test$retrieveJUsleepList()

test.sleep.df<-t(do.call(rbind.data.frame,test.sleep))

test.sleep$s_awake

test.move<-test$retrieveJUmoveList()

test.move.df<-t(do.call(rbind.data.frame,test.move))

test.goal<-test$retriveJUgoalList()
test.goal

test.workout <- test$retrieveJUworkoutList()

test.workout.df <- t(do.call(rbind.data.frame,test.workout))
