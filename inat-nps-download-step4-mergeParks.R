## set WD if not already set
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

## collect data:
summ.f <- list.files(path = "data", pattern = paste("[A-Z][A-Z][A-Z][A-Z]","_[0-9]+_summ.csv",sep=""))

cat('looking to merge data in ',length(summ.f),'files\n')

summ.ALL<- NULL

for (i in 1:length(summ.f)) {
  tryCatch({

  one.summ <- read.csv(paste('data/',summ.f[i],sep='')) 
  ##pull in data if there are any
  if (nrow(one.summ %>% filter(result.type==1))>0 ) {
    if (length(summ.ALL)==0) {
      summ.ALL <- one.summ
    } else {
      summ.ALL <- bind_rows(summ.ALL,one.summ)
    } 
  }
  } , error=function(e){
                      cat("missing data",conditionMessage(e), "\n")

                })

}



summ.ALL %>% group_by(result.type) %>% summarise(nOcc = n(), .groups='keep')

parks <- summ.ALL %>% group_by(park) %>% summarise(nOcc = n(), .groups='keep')
##dummy var to join so we get all possible park x spp combo

parks$x <- "x"
spp.all$x <- "x"

target.park.spp <- full_join(parks,spp.all,by="x")

target.park.spp$spp <- target.park.spp$scientific.name

missing.park.spp <- left_join(target.park.spp,summ.ALL, by=c("park","spp")) %>% filter(is.na(result.type),taxon.id!=-2) %>% select(-X.x, -spp, -x , -nOcc, -X.y, -result.type)

completed.park.spp <- inner_join(target.park.spp,summ.ALL, by=c("park","spp")) %>% select(-X.x, -spp, -x , -nOcc, -X.y, -result.type)


cat('',nrow(missing.park.spp),' spp-parks missing','\n',nrow(completed.park.spp),' spp-parks completed','\n')
cat(nrow(completed.park.spp) *100 / (nrow(missing.park.spp) + nrow(completed.park.spp)), '% done')


# as.data.frame(missing.park.spp %>% group_by(park) %>% summarise(nOcc=n()))
# remnants <- missing.park.spp %>% group_by(park) %>% summarise(nOcc=n()) %>% filter(nOcc<=20) %>% inner_join(missing.park.spp)
## %>% filter(nOcc<=20) %>% inner_join(missing.park.spp))
# as.data.frame(missing.park.spp %>% inner_join(remnants) )


# write.csv(missing.park.spp,file="missing.park.spp.csv")
# names( target.park.spp)

if (nrow(missing.park.spp) ==0) {

####### if we have everything, compile radii
one.rad <- NULL
rad.ALL <- NULL

ok.count <- 0
err.count <- 0

all.summ.data <- NULL

spp.need.check <- NULL

for (i in 1:length(summ.f)) {
  tryCatch({
  if (file.size(paste('data/',summ.f[i],sep='')) > 35) {
  one.summ <- read.csv(paste('data/',summ.f[i],sep='')) 
  one.summ$file.name <- summ.f[i]
  if (length(all.summ.data)==0) {
       all.summ.data <-one.summ
  } else {
       all.summ.data <-bind_rows(all.summ.data ,one.summ)
  }  

  ##pull in data if there are any
  one.summ.spp.with.data <- one.summ %>% filter(result.type==1)
  spp.with.data <- nrow(one.summ.spp.with.data)
  if (spp.with.data>0 ) {
       rad.file <- paste('data/',gsub("summ","radii",summ.f[i]),sep='')
       if (!file.exists(rad.file)) {
          if (length(spp.need.check)==0) {
            spp.need.check <- one.summ
          } else {
            spp.need.check <- bind_rows(spp.need.check , one.summ)
          }
        } else {
          cat(' exists: ' , rad.file, '\n')  
          one.rad <- read.csv(rad.file)
          ok.count <- ok.count + 1
          if (length(rad.ALL)==0) {
            rad.ALL <- one.rad
          } else {
            rad.ALL <- bind_rows(rad.ALL, one.rad)
          }}## have data
        } ## have rad file
  } ##file big enough to work with   
  }   #end try 
 , error=function(e){
         cat(i,' could not be found and so its # of species with data:',spp.with.data,one.summ.spp.with.data$spp[1],summ.f[i],'\n')
     }) 
 
}
cat(ok.count, ' files ok\n')

## cf rad and summ:
rad.cf <- rad.ALL %>% select(park,scientific.name,pts_download)
##summ.cf <- all.summ.data %>% select(-X)  %>% group_by(park, spp) %>% summarise(min.result.type=min(result.type), .groups="keep") %>% rename(scientific.name=spp)

#compare.summ.rad <- summ.cf %>% left_join(rad.cf, by=c("park","scientific.name"))

## illogical combos: 0 result.type and has downloaded pt
## 1 result.type and no downloaded pts

#weird.1 <- compare.summ.rad %>% filter(min.result.type==0 & !is.na(pts_download))  #none!
## but these are species that are mixed in with others that got a result...
##weird.2 <- compare.summ.rad %>% filter(min.result.type==1 & is.na(pts_download))


## but some of these could have been had subsequently
##need.check.cf <- spp.need.check %>% select(-X)  %>% group_by(park, spp) %>% summarise(min.result.type=min(result.type), .groups="keep") %>% rename(scientific.name=spp)
##compare.summ.rad <- need.check.cf %>% left_join(rad.cf, by=c("park","scientific.name"))



##dbl.check <- compare.summ.rad %>% filter(min.result.type != 0 & is.na(pts_download))
##cat(nrow(dbl.check),'need double check \n')
##dbl.check



##this could be done in previous script

##names(rad.ALL)

rad.ALL$scientific.name <- ifelse(is.na(rad.ALL$scientific.name),rad.ALL$inat.taxon.name.first,rad.ALL$scientific.name)

rad.ALL.exp <- rad.ALL %>% left_join(irank.unique,by="scientific.name") %>% select(-X) ## , -result.type)

if (length(unique(rad.ALL.exp$park))==1) {
   one.park.name <- unique(rad.ALL.exp$park)[1]
} else {
  one.park.name <- "_many"
}

write.csv(rad.ALL.exp,file=paste(one.park.name,"_radii_all_data.csv",sep=''),row.names = FALSE)




##reformat to this:
##				                                    observations within X mile buffer around park			
##park    species scientific name	downloaded iNat observations	observations in park	10	25	50	100

data.wider.pre <- rad.ALL.exp %>% 
    group_by(park,scientific.name,taxon.id,irank,pts_download,pts_park,radius_miles ) %>% 
    summarise(buff=max(pts_buffer_exclpark), .groups='keep')

data.wider <- data.wider.pre %>% pivot_wider(names_from=radius_miles,values_from=buff)


write.csv(data.wider,file=paste(one.park.name,"_radii_all_data_wider.csv",sep=""),row.names = FALSE)

cat('step 4 script finished successfully.  See files:  \n',
paste(one.park.name,"_radii_all_data_wider.csv",sep=""),'\n',
paste(one.park.name,"_radii_all_data.csv",sep=''),'\n')

}