source("UP-connect.R")

test<-jawboneActivities$new(username="nancywnsun@gmail.com",password="RforUP")

test$connectJU()

#retrieve data

test.sleep<-test$retrieveJUsleepList()
test.move<-test$retrieveJUmoveList()
test.goal<-test$retriveJUgoalList()
test.workout <- test$retrieveJUworkoutList()

#retrieve (convert) data frame
workout.df<-as.data.frame(test.workout,stringsAsFactors=FALSE)
sleep.df<-as.data.frame(test.sleep,stringsAsFactors=FALSE)
move.df<-as.data.frame(test.move,stringsAsFactors=FALSE)
goal.df<-as.data.frame(test.goal,stringsAsFactors=FALSE)


test$createJUsleep(s_startTime ="2014-2-1 22:00:00",s_endTime="2014-2-2 7:30:00")

test$deleteJUsleep(s_xid="ZG2XzMlYocrFZ5ZTxCnEGQ")

test$createJUworkout(wo_startTime ="2014-3-17 16:00:00",wo_endTime="2014-3-17 17:00:00",
                      wo_intensity=5,wo_subType=2)

test$deleteJUworkout(wo_xid="ZG2XzMlYocrPPnUrPVV9Dg")

test$sleepQualvsQuanGraph()
test$sleepDurationGraph()
test$sleepQualityGraph()

test$moveStepGraph()



#come up with data analysis stuff

1   walk
2	 run
3	 lift weights
4	 cross train
5	 nike training **
  6	 yoga
7	 pilates
8	 body weight exercise **
  9	 crossfit **
  10	 p90x **
  11	 zumba **
  12	 trx **
  13	 swim
14	 bike
15	 elliptical
16	 bar method **
  17	 kinect exercises **
  18	 tennis
19	 basketball
20	 golf **
  21	 soccer
22	 ski snowboard
23	 dance
24	 hike
25	 cross country skiing
26	 stationary bike
27	 cardio
28	 game

