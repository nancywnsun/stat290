#install.packages("RCurl")
#install.packages("rjson")

library(RCurl)
library(rjson)

username <- ""
pw <- ""
service <- "nudge"

test = getURL(url = paste("https://jawbone.com/user/signin/login", "?email=",username, "&pwd=", pw, "&service=",service, sep=""), verbose=TRUE)

parsed<-fromJSON(test)

access_token <- parsed$token
access_token

#sleeps<-getURL(url=paste("https://jawbone.com/nudge/api/v.1.33/users/@me/sleeps","?x-nudge-token=", access_token, sep=""))

sleeps<-getURL(url="https://jawbone.com/nudge/api/v.1.33/users/@me/sleeps",httpheader=c("x-nudge-token"=access_token),verbose=TRUE)

parsed_sleeps <- fromJSON(sleeps)

paste("x-nudge-token=",access_token)

access_token, "&api_key=", api_key, sep=""))

sample<-read.table("2014.csv",header=TRUE,sep=",")
