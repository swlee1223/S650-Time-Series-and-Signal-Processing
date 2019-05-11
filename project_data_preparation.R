# setwd('C:\\Users\\swlee\\OneDrive\\Desktop\\2019Spr_MSDS\\Time Series\\youtube-5000-channels-videos-daily-count-every-3h')

# install.packages('RSQLite')

library(RSQLite)

filename <- "0korean_youtube_data.db"
sqlite.driver <- dbDriver("SQLite")
db <- dbConnect(sqlite.driver,
                dbname = filename)
dbListTables(db)
dbListFields(db, "ch_Count")


ch_Count<-dbReadTable(db, "ch_Count")
ch_feature<-dbReadTable(db, "ch_feature")
vi_Count<-dbReadTable(db, "vi_count")
vi_feature<-dbReadTable(db, "vi_feature2")

str(ch_Count)

### buzzbean11
id='UCSHVH_AWVUc-C8-D8mh8W6A'
chcount=subset(ch_Count, ch_id==id)
viid=subset(vi_feature$vi_id, vi_feature$ch_id==id)


vicount<-vi_Count[which(vi_Count$vi_id %in% viid),]
vicount_time<-vicount[order(vicount$Time),]
vicount_video<-vicount[order(vicount$vi_id),]


vicount_time<-ddsg_vdcount_timesort[-which(!vicount_time$Time %in% vicount$Time),]
which(!vicount_time$Time %in% vicount$Time)

##### need to impute for the time gap!!!

vicount_avg<-aggregate(vicount_time[, 3:5], list(vicount_time$Time), mean)
plot(vicount_video$likeCount, xlab='Video', type='l', ylab='Like Count', main='Like Count per time')
plot(vicount_video$dislikeCount, xlab='Video', type='l', ylab='Dislike Count', main='Dislike Count per time')

vicount_sum<-aggregate(vicount_time[, 3:5], list(vicount_time$Time), sum)
colnames(vicount_sum)[1]<-"Time"

which(!vicount_sum$Time %in% chcount$Time)
likecount<-vicount_sum$likeCount
dislikecount<-vicount_sum$dislikeCount
cmtcount<-vicount_sum$commentCount
subcount<-chcount$subscriberCount
viewcount<-as.numeric(chcount$viewCount)

subcount<-append(subcount, (subcount[164] + subcount[165])/2, after = 164)
viewcount<-append(viewcount, (viewcount[164] + viewcount[165])/2, after = 164)

t<-seq(1:length(subcount))
data<-data.frame(subcount, likecount, dislikecount, cmtcount, viewcount, t)
write.csv(data, 'data.csv', row.names =F)

