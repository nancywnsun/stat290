#install.packages("RCurl")
#install.packages("rjson")

library(RCurl)
library(rjson)

source("accountinfo.R")

#postForm
test2 = postForm(uri = "https://jawbone.com/user/signin/login", .params=list(email=username, pwd = pw, service =service),.checkParams = TRUE)
test2

parsed<-fromJSON(test2)

access_token <- parsed$token
access_token
parsed

sleeps<-getURL(url="https://jawbone.com/nudge/api/v.1.33/users/@me/sleeps",httpheader=c("x-nudge-token"=access_token),verbose=TRUE)

parsed_sleeps <- fromJSON(sleeps)

#paste("x-nudge-token=",access_token)

sample<-read.table("2014.csv",header=TRUE,sep=",")
