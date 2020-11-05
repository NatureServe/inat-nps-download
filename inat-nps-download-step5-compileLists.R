#setwd('C:\\Users\\michael_lee\\Documents\\NPS_Bioblitz\\testClean')

library(dplyr) #general R-ing
library(tidyr)

####################################
## merge datasets:
####################################
   

 
##get list of species:

spp.all <- read.csv("spp.ids.csv")

irank <- read.csv("irank_data.csv")

irank.unique <- irank %>% filter(!is.na(irank.to.report), irank.to.report!="") %>% group_by(taxonName) %>%
    summarise(irank=first(irank.to.report), .groups="keep") %>% rename(scientific.name=taxonName)

spp.all <- left_join(spp.all,irank.unique,by="scientific.name") 

spp.iRank <- spp.all %>% select(scientific.name,irank) %>% rename(spp=scientific.name)

spp.usda.status <- read.csv("usda_exotic_status_x_SN.csv") %>% rename(spp=SN)

spp.iRank.L48I <- inner_join(spp.iRank,spp.usda.status,by="spp")

## collect data:
summ.f <- list.files(path = "data", pattern = paste("[A-Z][A-Z][A-Z][A-Z]","_[0-9]+_radii.csv",sep=""))

cat('looking in ',length(summ.f),'files\n')

all.data <- NULL
all.radii <- NULL
ok.count <- 0

for (i in 1:length(summ.f)) {
  ###tryCatch({

  ##pull in data if there are any
       radii.file <-   paste('data/',summ.f[i],sep='')
       data.file <- gsub("radii","raw_data",radii.file)
       if (!file.exists(data.file)) {
          
            cat('cant find ',data.file,'\n')
          
        } else {
          cat(' exists: ' , data.file, '\n')  
          one.data <- read.csv(data.file)
          one.radii <- read.csv(radii.file)
          if (length(all.radii)==0) { all.radii <- one.radii } else {all.radii <- bind_rows(all.radii,one.radii) }         

          #park name would be nice in the data!
          #eliminate rest of name from file, could use stringr to extract too
          one.data$park <-  gsub("_[0-9]+_raw_data.csv","",gsub("data/","",data.file))

          ok.count <- ok.count + 1
          if (length(all.data)==0) {
            all.data <- one.data
          } else {
            all.data <- bind_rows(all.data, one.data)
          }## have data
        } ## have data file
  
 ### }   #end try 
### , error=function(e){
   ###      cat(i,' could not be found and so its # of species with data:',spp.with.data,one.summ.spp.with.data$spp[1],summ.f[i],'\n')
   ###  }) 
 
}
cat(ok.count, ' files ok\n')
cat(nrow(all.data), 'datums \n')
cat(nrow(all.radii), 'radii \n')

##names(rad.ALL)

side.project <- FALSE 
##side.project <- TRUE
if (side.project == TRUE) {
  
  park.watch.list <- read.csv("park_watch_list2.csv")   %>% select(park, scientific.name) %>% rename(taxon.name=scientific.name)
   
  all.data.watch.list <- inner_join(park.watch.list,all.data, by=c("park","taxon.name") ) 

  all.data.summ <-   all.data.watch.list %>% group_by(park, taxon.name) %>% summarise(countOcc=n(),
   avgLat=mean(latitude), avgLong=mean(longitude), mdnLat=median(latitude), mdnLong=median(longitude))

  all.data.find.diff <- all.data.summ %>% filter(countOcc>20)
  
  all.data.pairwise <- inner_join(all.data.find.diff , all.data.find.diff, by="park") %>% 
     mutate(distDeg=((avgLat.x-avgLat.y)^2+(avgLong.x-avgLong.y)^2)^0.5)

  all.data.pairwise.far <-   all.data.pairwise %>% filter(distDeg>2, park=="NISI", taxon.name.x=="Acer palmatum") %>% arrange(-distDeg)
  all.data.pairwise.far  
  
  ##really want a third species of a different sort of distribution
  
  all.data.triplets <-  inner_join(all.data.pairwise, all.data.find.diff, by="park") %>% 
     mutate(distDeg.xz=((avgLat.x-avgLat)^2+(avgLong.x-avgLong)^2)^0.5,distDeg.yz=((avgLat.y-avgLat)^2+(avgLong.y-avgLong)^2)^0.5 )

  all.data.triplets.totDist <-   all.data.triplets %>% mutate(totDistDeg = distDeg + distDeg.xz + distDeg.yz)

  all.data.triplets.totDist %>% filter(park=="NISI") %>% arrange(desc(totDistDeg))

  one.park.summ.forshp <-  all.data.triplets.totDist %>% filter(park=="NISI") %>% arrange(desc(totDistDeg)) %>% select (taxon.name, taxon.name.x, taxon.name.y, distDeg, distDeg.xz, distDeg.yz, totDistDeg)
  spp.get.shp <-   one.park.summ.forshp[1,]

  some.parks <- c("BOHA","TAPR","FOVA", "CABR", "AZRU")
  for (pk in some.parks) {
    one.park.summ.forshp <-  all.data.triplets.totDist %>% filter(park==pk) %>% arrange(desc(totDistDeg)) %>% select (taxon.name, taxon.name.x, taxon.name.y, distDeg, distDeg.xz, distDeg.yz, totDistDeg)
    spp.get.shp <- bind_rows(spp.get.shp,  one.park.summ.forshp[1,])
  }

  spp.get.shp
  ## could be done with pivot_longer more elegantly, but this is quick enough
  spp.get.shp.longer <- bind_rows(   spp.get.shp %>% select (park, taxon.name) ,
                                     spp.get.shp %>% select (park, taxon.name.x) %>% rename(taxon.name = taxon.name.x) ,
                                     spp.get.shp %>% select (park, taxon.name.y) %>% rename(taxon.name = taxon.name.y) )

  write.csv(spp.get.shp.longer,file='spp.4shp.csv')
}


##compile lists, two per park
## first list, those spp on plots
all.radii.std.init <- all.radii %>% mutate(spp=ifelse(is.na(scientific.name),inat.taxon.name.first, scientific.name)) %>% select(park,spp,pts_park,radius_miles,pts_buffer_exclpark)

##nrow(all.radii.std.init)

##deduplicate in case downloaded more than once
all.radii.std.need.irank <- all.radii.std.init %>% group_by(park,spp,radius_miles)  %>% 
  summarise(max_pts_park = max(pts_park), max_pts_buffer_exclpark = max(pts_buffer_exclpark), .groups="keep") %>% rename(pts_park=max_pts_park,pts_buffer_exclpark=max_pts_buffer_exclpark)


################### LIMIT TO IRANK DATA, filtered by L48(I)
all.radii.std <- inner_join(all.radii.std.need.irank,spp.iRank.L48I,by="spp")
##nrow(all.radii.std)


##create wider view and summary of parks
one.park.name <- "ALL_PARKS"
write.csv(all.radii.std,file=paste(one.park.name,"_radii_all_data.csv",sep=''),row.names = FALSE)

##reformat to this:
##				                                    observations within X mile buffer around park			
##park    species scientific name	downloaded iNat observations	observations in park	10	25	50	100

data.wider.pre <- all.radii.std %>% 
    group_by(park,spp,irank,pts_park,radius_miles ) %>% 
    summarise(buff=max(pts_buffer_exclpark), .groups='keep')

data.wider <- data.wider.pre %>% pivot_wider(names_from=radius_miles,values_from=buff)


write.csv(data.wider,file=paste(one.park.name,"_radii_all_data_wider.csv",sep=""),row.names = FALSE)

#and summarise that by park
##names(data.wider)

park.summ <- data.wider %>% group_by(park) %>% summarise(spp_park=sum(pts_park>0), pts_park_allspp=sum(pts_park), spp_buffer100=sum(`100`>0), pts_buffer100_allspp=sum(`100`), .groups='keep')
write.csv(park.summ,file=paste(one.park.name,"_park_summary.csv",sep=""),row.names = FALSE)


park.exotics <- all.radii.std  %>% filter(pts_park>0) %>% group_by(park) %>% arrange(park, desc(pts_park)) %>% select(park,spp,irank,pts_park) %>% unique() %>%
  rename(`scientific name`=spp, `iNaturalist occurrences within park`=pts_park,iRank=irank)
write.csv(park.exotics,file="exotics_on_parks.csv", row.names=FALSE)

#watch list:
watch.list<-  all.radii.std  %>% filter(pts_park==0, radius_miles==100) %>% group_by(park) %>% arrange(park,desc(pts_buffer_exclpark))  %>% 
  mutate(rowN=row_number(), rankN=min_rank(desc(pts_buffer_exclpark))) %>% filter(rankN<=100 & pts_buffer_exclpark>0 ) %>% arrange(park,desc(pts_buffer_exclpark)) %>% select(park,spp,irank,pts_park,radius_miles,pts_buffer_exclpark) 

##consider that a long tail of 1 occurrences well over 100 is not so useful
check.long.tail <- watch.list %>% group_by(park) %>% summarise(nTot=n(),.groups='keep') %>% filter(nTot > 110)
##how long is the min of 1
one.tail <- watch.list %>% filter(pts_buffer_exclpark==1) %>% group_by(park) %>% summarise(nOne=n(),.groups='keep') %>% filter(nOne > 30)

tails.to.trim <- inner_join(check.long.tail,one.tail,by="park")

watch.list.2 <- left_join(watch.list,tails.to.trim,by="park") %>% filter(is.na(nOne) | pts_buffer_exclpark >1)
watch.list.write <- watch.list.2 %>% select(-nOne,-nTot) %>% rename(`scientific name`=spp, 
       `iNaturalist occurrences within park`=pts_park, `buffer radius (miles)`=radius_miles, `iNaturalist occurrences within buffer outside park`=pts_buffer_exclpark, iRank=irank)




write.csv(watch.list.write,file="park_watch_list.csv", row.names=FALSE)


## as.data.frame(watch.list %>% summarise(nOcc=n())) %>% arrange(nOcc)
## watch.list %>% filter(park=='CEBR' | park=='PIPE' | park=='AZRU') %>% filter(`pts_buffer_exclpark` ==1) %>% summarise(nOcc=n())

##export raw data, not really helpful as it's too large to load back:

##write.csv(all.data, file='all.data.exotics.csv')


cat('step 5 script done with ' , nrow(watch.list.write), ' data rows see file \n park_watch_list.csv \n')
