library(RCurl)
library(rjson)
library(pbapply)
library(plyr)
library(ggplot2)
library(lubridate)

  apikey <- 'missing'
  group  <- 'Data-Science-London' 
  depth  <- 1

  #uncover the dynamics of a meetup group
  #depth=1 default, only retrieves events and rsvps relative to the group
  #depth=2 retrieves members profiles with the list of their topics and groups
  if(apikey == 'missing') {
    cat('First check http://www.meetup.com/meetup_api/docs/ to get your API key\n\n')
  }
  ####################
  #add throttling and error checking
  #use $meta$count to insure we got everything back
  getrsvps <- function(eventid) {
    results <- 200
    offset <- 0
    #request and increment offset until we get a page with less than 200 items
    while(results%%200==0) {
      url     <- paste('https://api.meetup.com/2/rsvps?&sign=true&event_id=',eventid,'&page=200&offset=',offset,'&key=',apikey,sep="")
      if(offset == 0) {
        tmp <- fromJSON(getURL(url)) }
      else {
        tmp$results <- c(tmp$results,fromJSON(getURL(url))$results)
      }
      results <- length(tmp$results)
      offset  <- offset + 1
    }
    return(tmp)
  }
  getprofile <- function(mid) {
    url <- paste('https://api.meetup.com/2/members?&sign=true&member_id=',mid,'&key=',apikey,sep="")
    return(fromJSON(getURL(url)))
  }
  getgroups <- function(mid) {
    url <- paste('https://api.meetup.com/2/groups?&sign=true&member_id=',mid,'&key=',apikey,sep="")
    return(fromJSON(getURL(url)))
  }
  epoch2UTC <- function(t) {as.POSIXct(t, format="%Y-%m-%d",origin="1970-01-01")}
  ####################
  
  cat('Retrieving all events in',group,'\n')
  url         <- paste('https://api.meetup.com/2/events?&sign=true&status=upcoming,past&group_urlname=',group,'&key=',apikey,sep="")
  ejson       <- fromJSON(getURL(url))
  e           <- ldply(ejson$results,data.frame)
  
  e$created     <- e$created/1000
  e$time        <- e$time/1000
  e$UTC.created <- epoch2UTC(e$created)
  e$UTC.time    <- epoch2UTC(e$time)
  e$cwday       <- factor(wday(e$UTC.created),1:7,labels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat'))
  e$ctimeofday  <- hour(e$UTC.created) + minute(e$UTC.created)/60
  
  #capacity what percentage of all spots have been assigned
  e$capacity    <- 100 * e$yes_rsvp_count / e$rsvp_limit
  #how many actually showed up
  e$shownup     <- 100 * e$headcount / e$yes_rsvp_count
  
  
  cat('Retrieving all rsvps from',length(levels(e$id)),'meetup(s) in',group,'\n')
  #build a list of all rsvps of all meetups from that group
  rsvpjson <- pblapply( levels(e$id), getrsvps )
  #keep only the results, remove the metadata list
  rsvpl    <- sapply(rsvpjson,function(x) {x$results})
  
  #then cast to data.frame
  #ldply does not seem to be able to cope with unbalanced nested lists
  #rsvp <- ldply(rsvpl,data.frame) #won't work
  #I'm not using ldply correctly?
  cat('Converting json to data.frame\n')
  rsvp <- ldply(rsvpl[[1]],data.frame)
  for(i in 2:length(rsvpl)) {
    rsvp <- merge(rsvp , ldply(rsvpl[[i]],data.frame),all=TRUE )
    #checksum
    #cat(dim(rsvp),length(rsvpl[[i]]),dim(rsvp)[1] - length(rsvpl[[i]]) ,'\n')
  }
  
  #order data.frame by rsvp creation
  rsvp <- rsvp[order(rsvp$created),]
  
  #remove milliseconds
  rsvp$created    <- rsvp$created / 1000
  rsvp$event.time <- rsvp$event.time / 1000
  rsvp$mtime      <- rsvp$mtime / 1000
  #convert epoch to UTC
  rsvp$UTC.created <- epoch2UTC(rsvp$created)
  rsvp$UTC.event.time <- epoch2UTC(rsvp$event.time)
  rsvp$UTC.mtime <- epoch2UTC(rsvp$mtime)
  #adjust some classes
  rsvp$member.member_id <- factor(rsvp$member.member_id)
  rsvp$venue.id <- factor(rsvp$venue.id)
  rsvp$group.id <- factor(rsvp$group.id)
  rsvp$rsvp_id <- factor(rsvp$rsvp_id)
  #how many days the member rsvp'ed before the event
  rsvp$days.to.event <- (rsvp$event.time - rsvp$created) / 86400
  #weekday and time of day to group
  rsvp$wday <- factor(wday(rsvp$UTC.created),1:7,labels=c('Sun','Mon','Tue','Wed','Thu','Fri','Sat'))
  rsvp$timeofday <- hour(rsvp$UTC.created) + minute(rsvp$UTC.created)/60
  #how close to the event members switch from "yes" to "no"
  rsvp$mb4event  <- rsvp$event.time - rsvp$mtime
  #ignore when creation time = mtime, i.e. member did not change response
  rsvp$mb4event[rsvp$created == rsvp$mtime] <- NA
  #ignore waitlist -> yes changes, only keep yes -> no
  rsvp$mb4event[rsvp$response == "yes"]     <- NA
  #how many swicthed from yes to no
  e$switch2no <- aggregate(rsvp$mb4event,list(rsvp$event.id),function(x) {length(which(!is.na(x)))})$x
  
  #aggregate events stats from rsvp to e
  e$totalrsvp    <- as.data.frame(table(rsvp$event.id))$Freq
  e$guests       <- aggregate(rsvp$guests,list(rsvp$event.id),sum)$x
  
  #time of the last accepted "yes" rsvp
  rsvpyes        <- subset(rsvp,response=="yes")
  e$firstyes     <- tapply(rsvpyes$created,rsvpyes$event.id,min)
  e$lastyes      <- tapply(rsvpyes$created,rsvpyes$event.id,max)
  e$days.to.full <- (e$lastyes - e$firstyes) / 86400
  #how many days elapsed from first "yes" rsvp to last "yes" rsvp
  e$days.to.full <- aggregate(rsvpyes$days.to.event,list(rsvpyes$event.id),max)$x
  e$to.full      <- paste(trunc(e$days.to.full),'days',trunc((e$days.to.full%%1) * 24),'hours',trunc((((e$days.to.full%%1)*24)%%1)*60),'mins')
  
  
  # ratio of new vs repeat members
  #count first timers, i.e. event members not found in previous events
  members.by.event <- tapply(rsvp$member.member_id,rsvp$event.id,list)
  e$returning[1] <- 0
  for(i in 2:dim(e)[1]) {
    e$returning[i] <- length(which(members.by.event[[i]] %in% unlist(members.by.event[c(1:i-1)]) == FALSE))
  }
  
  ## calculating returning members
  #when rsvp is sorted by member if the previous row memberid is identical then it is a returning memberid
  #possible error: it needs to be sorted by memberid first then eventid (otherwise a later event rather than the first event will flag as new member)
  #order by memberid
  rsvp <- rsvp[order(rsvp$member.member_id),]
  #cast memberid as double so we can diff each memberid in the dataframe
  rsvp$mid <- as.double(as.character(rsvp$member.member_id))
  rsvp$mid <- c(1,diff(rsvp$mid))
  #first assume every member is new
  rsvp$returning <- 0
  #but if the previous row memberid is identical then it becomes a returning memberid
  rsvp$returning[rsvp$mid == 0] <- 1
  #reorder by rsvp created
  rsvp <- rsvp[order(rsvp$created),]
  rsvp$mid <- NULL
  
  #order members by most rsvps
  #careful members could have identical names therefore add the member id
  toprsvp <- as.data.frame(table(paste(rsvp$member.name,rsvp$member.member_id)))
  toprsvp <- toprsvp[order(toprsvp$Freq,decreasing=TRUE),]
  names(toprsvp) <- c('Name','rsvp')
  
  #extract upcoming event waiting list
  waitlist <- subset(rsvp,response =="waitlist" & as.numeric(event.id)==max(as.numeric(event.id)))
  #flag members who previously canceled within 6 hours of the event
  yes2no <- subset(rsvp,rsvp$mb4event < 6*3600 & !is.na(rsvp$mb4event))
  serialyes2no <- as.data.frame(summary(yes2no$member.name))
  names(serialyes2no) <- c('canceled')
  #member canceled within 6 hours of the events multiple times!!!
  serialy2n <- rownames(subset(serialyes2no, canceled > 1))
  #are there any serial last minute dropouts that rsvp "yes" for the upcoming event?
  about2fail <- serialy2n[which(serialy2n %in% rsvp$member.name[as.numeric(rsvp$event.id)==max(as.numeric(rsvp$event.id))])]
  
  #let's go deeper
  if(depth==2) {
    #build a list of all group members profiles and all the groups they subscribed to
    #throttle + errorcheck this
    #use levels of member_id factor to deduplicate and poll the same id only once
    cat('Retrieving',length(levels(rsvp$member.member_id)),'members profiles with',dim(rsvp)[1],'rsvps sent to',group,'\n')
    membersjson <- pblapply(levels(rsvp$member.member_id), getprofile)
    members     <- sapply(membersjson,function(x) {x$results})
  
    cat('Retrieving subscribed groups from',length(levels(rsvp$member.member_id)),'members in',group,'\n')
    groupsjson  <- pblapply(levels(rsvp$member.member_id), getgroups)
    mgroups     <- sapply(groupsjson,function(x) {x$results})
    
    #for each member extract all topics subscribed
    topics <- lapply(members, function(x) {
                                sapply(x$topics, function(y){c(y$urlkey)}) 
                })
    #for each member extract all groups subscribed
    groups <- lapply(mgroups, function(x) {
                                sapply(x, function(y) {y$name})
                })
    
    #subscribed topics list sorted by most popular
    dtopics <- as.data.frame(table(unlist(topics)))
    dtopics <- dtopics[order(dtopics$Freq,decreasing=TRUE),]
    
    #subscribed groups list sorted by most popular
    dgroups <- as.data.frame(table(unlist(groups)))
    dgroups <- dgroups[order(dgroups$Freq,decreasing=TRUE),]
    
    group.attr <- c("lon","visibility","organizer","link","state","join_mode","who","country","city","id","category","topics","timezone","group_photo","created","description","name","rating","urlname","lat","members")
    
    #members cities 
    city <- sapply(members, function(x) { x$city } )
    
    #mine bio
    bio  <- sapply(members, function(x) { x$bio } )
  } 
  
  if(depth==3) {
    #extract other_services links for deeper mining
  }
  
  ###### CLEANUP BEFORE SAVE
  #no key stealing plz :)
  rm(apikey)
  #the API key is also contained in the metadata json replies
  ejson$meta <- NULL
  rsvpjson$meta  <- NULL
  if(depth > 1) {
    membersjson$meta <- NULL
    groupsjson$meta <- NULL
  }
  ###### SAVE
  cat('Saving all objects in',paste(group,".meetup.Rdata and csv files\n",sep=""))
  #save all objects to load later in .Rmd
  save.image(file=paste(group,".meetup.Rdata",sep=""))
  #export to csv for other users and tools
  write.csv(e,file=paste(group,".events.csv",sep=""),row.names=FALSE)
  write.csv(rsvp,file=paste(group,".rsvp.csv",sep=""),row.names=FALSE)
