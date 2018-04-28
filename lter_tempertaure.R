
# -----------------------------------------------------------------------
# Moorea Coral Bleaching 
# Temperature patterns
# -----------------------------------------------------------------------

# rm(list=ls())
home <- getwd()

# Initialization ----------------------------------------------------------
library(lubridate)
library(dplyr)
library(tidyr)

# Data import -------------------------------------------------------------
# http://mcrlter.msi.ucsb.edu/cgi-bin/showDataset.cgi?docid=knb-lter-mcr.1035
# A continuous time series of water temperature is measured with bottom-mounted thermistors at six sites around the shores of Moorea, on the fringing reef, backreef, and forereef. The forereef temperature is recorded at 10, 20, 30 and 40 m, the backreef at 1 m and the fringing reef at 1 and 5 m. SBE 39s are deployed to the forereef and backreef. Onset HOBOs are deployed on the fringing reef. Temperature data are processed and resampled to a 20 minute time step.

lter <- read.csv(file.path(home,'data',paste('MCR_LTER','01','_BottomMountThermistors_20171129.csv',sep='')))
for(i in c('02','03','04','05','06')){
  lter <- rbind(lter,read.csv(file.path(home,'data',paste('MCR_LTER',i,'_BottomMountThermistors_20171129.csv',sep=''))))
}
head(lter)

# format time and date
lter$time_use <- ymd_hms(lter$time_local)
lter$day <- format(lter$time_use, '%Y-%m-%d')

### fix LTER 01 backreef metadata
lter$sensor_depth_m[c(lter$site=='LTER01' & lter$reef_type_code=='BAK' & lter$sensor_depth_m==2)] <- 1

# median temperature by day (and filter out forereef)
lter.day <- lter %>% group_by(site,reef_type_code,sensor_type,sensor_depth_m,day) %>% filter(reef_type_code=='BAK' | reef_type_code=='FRI') %>% summarise(temp_c = median(temperature_c)) %>% ungroup()
lter.day$day <- ymd(lter.day$day)

# data structure ----------------------------------------------------------
samples <- lter.day %>% group_by(site,reef_type_code,sensor_type,sensor_depth_m) %>% filter(reef_type_code=='BAK'| reef_type_code=='FRI') %>% summarise('start'=min(day),'end'=max(day), n=length(day)) %>% print(n=46)

# filter to include only time series we are interested in
samples <- filter(samples,  
                            # site== 'LTER01' & reef_type_code=='BAK' & sensor_depth_m==1 | ##missing data during sampling
                            site== 'LTER02' & reef_type_code=='BAK' & sensor_depth_m==2 |
                            site== 'LTER03' & reef_type_code=='BAK' & sensor_depth_m==2 |
                            site== 'LTER04' & reef_type_code=='BAK' & sensor_depth_m==2 |
                            # site== 'LTER05' & reef_type_code=='BAK' & sensor_depth_m==2 | ##missing data during sampling
                            site== 'LTER06' & reef_type_code=='BAK' & sensor_depth_m==2 |
                            reef_type_code=='FRI' & sensor_depth_m==1)

# create an index for joining with data
samples$ind <- paste(samples$site, samples$reef_type_code, samples$sensor_depth_m, sep='_')
lter.day$ind <- paste(lter.day$site, lter.day$reef_type_code, lter.day$sensor_depth_m, sep='_')
lter.day.sub <- lter.day[lter.day$ind %in% samples$ind,] # select only data corresponding to metadata in samples
lter.day.sub$ind <- as.factor(lter.day.sub$ind)

# subset date range so consistent across sites
lter.day.sub <- filter(lter.day.sub, day >= '2009-12-31' & day <= '2016-12-31') 

# check metadata again and plot
lter.day.sub %>% group_by(site,reef_type_code,sensor_type,sensor_depth_m) %>% filter(reef_type_code=='BAK'| reef_type_code=='FRI') %>% summarise('start'=min(day),'end'=max(day), n=length(day)) %>% print(n=46)

par(mfrow=c(5,2),mar=c(2,2,1,1),mgp=c(1,.5,0))
for(i in 1:10){
  plot(lter.day.sub$day[lter.day.sub$ind==levels(lter.day.sub$ind)[i]],(lter.day.sub$temp_c[lter.day.sub$ind==levels(lter.day.sub$ind)[i]]),ylab="",xlab="",ylim=c(24,30),main=levels(lter.day.sub$ind)[i])
  abline(v=ymd('2016-05-08'),col='blue',lwd=2)
}

# note missing data in some cases, need to fill in NA rows for missing dates
## create a dataframe with the full date range
time.seq <- data.frame(day=unique(lter.day.sub$day),ind=seq(1:length(unique(lter.day.sub$day))))
time.seq$year <- year(time.seq$day)
time.seq$week.num <- week(time.seq$day)
time.seq.week <- time.seq %>% group_by(week.num,year) %>% summarise(day=min(day)) %>% ungroup()

## for each site/hab join with full date range and fill in NAs
alldays <- lter.day.sub %>% select(site,reef_type_code,sensor_depth_m,temp_c,day) %>% filter(site==samples$site[1] & reef_type_code==samples$reef_type_code[1] & sensor_depth_m==samples$sensor_depth_m[1])
alldays <- left_join(time.seq[c('day')],alldays,by='day')
alldays$site[is.na(alldays$site)] <- samples$site[1]
alldays$reef_type_code[is.na(alldays$reef_type_code)] <- samples$reef_type_code[1]
alldays$sensor_depth_m[is.na(alldays$sensor_depth_m)] <- samples$sensor_depth_m[1]
alldays <- alldays[with(alldays, order(day)),]
for(i in 2:10){
  temp <- lter.day.sub %>% select(site,reef_type_code,sensor_depth_m,temp_c,day) %>% filter(site==samples$site[i] & reef_type_code==samples$reef_type_code[i] & sensor_depth_m==samples$sensor_depth_m[i])
  temp <- left_join(time.seq[c('day')],temp,by='day')
  temp$site[is.na(temp$site)] <- samples$site[i]
  temp$reef_type_code[is.na(temp$reef_type_code)] <- samples$reef_type_code[i]
  temp$sensor_depth_m[is.na(temp$sensor_depth_m)] <- samples$sensor_depth_m[i]
  temp <- temp[with(temp, order(day)),]
  alldays <- rbind(alldays, temp)
}
str(alldays)

# check metadata again, should have same date range and number of rows
alldays %>% group_by(site,reef_type_code,sensor_depth_m) %>%  summarise('start'=min(day),'end'=max(day), n=length(day)) %>% print(n=46)

alldays$year <- year(alldays$day)
alldays$week.num <- week(alldays$day)

lter.week <- alldays %>% group_by(site,reef_type_code,sensor_depth_m,year,week.num) %>% summarise(temp_c = mean(temp_c)) %>% ungroup()
lter.week <- lter.week[with(lter.week, order(site,reef_type_code,sensor_depth_m,year,week.num)),]

# calculate cumulative thermal stress -------------------------------------

# From Pratchett et al. 2013:
# The temperature time-series for the 4×4 km pixel closest to the centre of the two
# study sites was extracted to determine the summer average
# temperature [summer periods = Feb–Apr] . The threshold for thermal stress was defined as
# the maximum monthly mean (MMM = 29.0 °C [50]) of the dataset and accumulated heat stress (in °C-weeks) was
# calculated for all temperatures exceeding the MMM value.

lter.week$hotspot <- lter.week$temp_c - 29
lter.week$hotspot[lter.week$hotspot < 0] <- 0

lter.week$cumstress <- NA

# 1 week running sum 
out <- lter.week %>% filter(site==samples$site[1] & reef_type_code==samples$reef_type_code[1] & sensor_depth_m==samples$sensor_depth_m[1])
for(i in 13:nrow(out)){
  out$cumstress[i] <- sum(out$hotspot[(i-12):i],na.rm=T)
}
for(k in c(2:10)){
  temp <- lter.week %>% filter(site==samples$site[k] & reef_type_code==samples$reef_type_code[k] & sensor_depth_m==samples$sensor_depth_m[k])
      for(i in 13:nrow(temp)){
        temp$cumstress[i] <- sum(temp$hotspot[(i-12):i],na.rm=F)
      }
  out <- rbind(out, temp)
}
str(out)
out <- left_join(out, time.seq.week, by=c('year','week.num'))

plot(out$day,out$cumstress)


# plot by station ---------------------------------------------------------

samples.1 <- filter(samples, reef_type_code=='FRI' )
par(mfrow=c(3,2),mar=c(2,2,1,1),mgp=c(1,.5,0),oma=c(0,2,2,0))
for(k in 1:nrow(samples.1)){
  temp <- out %>% filter(site==samples.1$site[k] & reef_type_code==samples.1$reef_type_code[k] & sensor_depth_m==samples.1$sensor_depth_m[k])
  plot(temp$day,temp$cumstress,ylab="",xlab="",ylim=c(0,10),main=paste(samples.1$site[k]),type='n')
  abline(v=ymd('2016-05-08'),col=rgb(0,0,255,100,max=255),lwd=3)
  points(temp$day,temp$cumstress,type='l',lwd=1.5)
}
mtext('Cumulative heat stress',side=2,outer=T)
mtext('Fringing reef, depth=1m',side=3,outer=T)


samples.1 <- filter(samples, reef_type_code=='BAK')
par(mfrow=c(3,2),mar=c(2,2,1,1),mgp=c(1,.5,0),oma=c(0,2,2,0))
for(k in 1:nrow(samples.1)){
  temp <- out %>% filter(site==samples.1$site[k] & reef_type_code==samples.1$reef_type_code[k] & sensor_depth_m==samples.1$sensor_depth_m[k])
  plot(temp$day,temp$cumstress,ylab="",xlab="",ylim=c(0,10),main=paste(samples.1$site[k]),type='n')
  abline(v=ymd('2016-05-08'),col=rgb(0,0,255,100,max=255),lwd=3)
  points(temp$day,temp$cumstress,type='l',lwd=1.5)
}
mtext('Cumulative heat stress',side=2,outer=T)
mtext('Back reef, depth=2m',side=3,outer=T)


# correlations among sites ------------------------------------------------
# cor.out <- matrix(nrow=12,ncol=12)
# 
# samples.2 <- filter(samples, sensor_depth_m < 3)
# # samples.2 <- samples.2[-7,]
# samples.2 <- filter(samples.2, reef_type_code=='BAK' & sensor_depth_m==2 | reef_type_code=='FRI' & sensor_depth_m==1)
# 
# for(k in c(1:12)){
#   for(j in c(1:12)){
#     t1 <- out %>% filter(site==samples.2$site[k] & reef_type_code==samples.2$reef_type_code[k] & sensor_depth_m==samples.2$sensor_depth_m[k]) %>% select(ind,date,cumstress)
#     t2 <- out %>% filter(site==samples.2$site[j] & reef_type_code==samples.2$reef_type_code[j] & sensor_depth_m==samples.2$sensor_depth_m[j]) %>% select(ind,date,cumstress)
#     
#     tt <- left_join(t1,t2,by='date')
#     try(cor.out[k,j] <- cor(tt$cumstress.x,tt$cumstress.y,use='complete.obs'))
#   }
# }
# 
# temp <- cor.out
# temp[temp > 0.9] <- NA
# temp



# link to rest of the data ------------------------------------------------

data <- read.csv(file.path(home,'data','Poc_Acrop_bleaching_all_data.csv'))
str(data)
unique(data$LTER_site)
unique(data$Habitat)

# link to cumstress on May 13, 2016
out$LTER_site <- NA
out$LTER_site[out$site=='LTER01'] <- 'LTER_1'
out$LTER_site[out$site=='LTER02'] <- 'LTER_2'
out$LTER_site[out$site=='LTER03'] <- 'LTER_3'
out$LTER_site[out$site=='LTER04'] <- 'LTER_4'
out$LTER_site[out$site=='LTER05'] <- 'LTER_5'
out$LTER_site[out$site=='LTER06'] <- 'LTER_6'

out$Habitat <- NA
out$Habitat[out$reef_type_code=='BAK'] <- 'Lagoon'
out$Habitat[out$reef_type_code=='FRI'] <- 'Fringe'

str(out)
out.may13 <- subset(out, day==ymd('2016-05-13'))
out.may13 <- rbind(out.may13,out.may13[2,])
out.may13[11,1] <- 'LTER01'
out.may13[11,10] <- 'LTER_1'
out.may13 <- rbind(out.may13,out.may13[9,])
out.may13[12,1] <- 'LTER05'
out.may13[12,10] <- 'LTER_5'

data <- left_join(data,out.may13[c('LTER_site','Habitat','cumstress')],by=c('LTER_site','Habitat'))
str(data)
write.csv(data, file.path(home,'data','Poc_Acrop_bleaching_all_data_wTemp.csv'),row.names=F)
