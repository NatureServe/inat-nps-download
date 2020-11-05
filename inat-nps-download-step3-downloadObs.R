## assumes working directory is set to location of the files in the repository.  If not, set it here:
#setwd('C:\\Users\\michael_lee\\Documents\\NPS_Bioblitz\\testClean')

## downloads observations from iNaturalist using https://www.inaturalist.org/people/hanly

## a number of errors are possible in from this script, such as too many species or too much memory used
## the solution is to close and restart the script (with a shell command, ideally)

init.spp.chunk <- 50  ## number of species to get at once, more works well initially
  ### but smaller numbers can help fill in species that are not downloading well
  ## initial setting: 50, then can be decreased to 0.5 (half a species at a time, by location)

radii <- c(10,25,50,100) # mile buffers to see how many species are in each
memory.size.trigger.quit <- 5000 #more than this many MB, quit R and start again 

start.time<-date()

#load libs

library(iNatTools)
library(dplyr) #general R-ing
library(tidyr)
#spatial calculations/mapping:
library(httr)
library(maps)
library(rgdal)
library(rgeos) ## for buffer boundaries of parks
library(sf)  #spatial joining

park.df <- read.csv("target_NPS_parks.csv")
park.list <- (park.df$Park_Code)

which.park <- read.csv("park.to.run.csv")
one.park.name <- which.park$park[1]

## make sure data subfolder eixsts:
if (dir.exists("data")!=TRUE) {dir.create("data")}

##one.spp.quarter <- which.park$spp[1]  ##divide species into chunks so we don't go the whole list at once

## see if this park is done, if so, bump the counter up to next number for next park

   pCounter <- which(park.df$Park_Code==one.park.name)

if (file.exists(paste('data/',one.park.name,"_done.csv",sep=""))) {
  ## this park is done so go to next park 
  if (pCounter < length(park.df$Park_Code)) {

      cat(one.park.name,'is done and so setting up next park for next run')
      one.park.name <- park.df$Park_Code[pCounter + 1]
     ##save for next time:

       new.park.run <- as.data.frame(one.park.name)
       names(new.park.run) <- "park"
       write.csv(new.park.run, file="park.to.run.csv", row.names=FALSE)
    } else {
       ## all parks done
       cat("Script is all finished!  Press Enter to exit.")
        x <- readLines(con="stdin", 1)
    }
  
}


#custom functions:

#see what kind of result we got
evalResult <-function (one.data) {
if (length(one.data)>0) {
  return (1) } ### "have data") } 
  else {
    if (is.list(one.data)) { return (0) } ## "no data") }
    else {return (2)} #  "too much data")}
  }
}
## function to collect results
addToResults <- function(previousResults, newResults) {
    if (length(newResults) > 0 ) {
        if (length(previousResults) ==0) {
            ##these are the results
             return (newResults)
        } else {
            return(bind_rows(previousResults, newResults))
        }
    } else {
      return(previousResults) ##just old results
    }
  }
#nrow that returns zero for NULL
nrowZero<-function(df) {
  if (length(df)==0) {
    return (0) } 
  else {
    return (nrow(df))
  }
}

#there is probably a simpler way to do this, but this works to extract lat/long from list
getItem2 <- function(thelist) { return (as.numeric(unlist(thelist)[2]))}
getItem1 <- function(thelist) { return (as.numeric(unlist(thelist)[1]))}
naTo0 <- function(theval) { return(ifelse(is.na(theval),0,theval))}

  ## spp <- read.csv("usda_exotic_sn.csv")
  ## import spp directly from file:
  
  
  
  
   


  
  spp <- read.csv("spp.ids.csv")  
  
  
  ## species run thus far:
  summ.f <- list.files(path = "data", pattern = paste(one.park.name,"_[0-9]+_summ.csv",sep=""), full.names=TRUE, all.files = FALSE)
  
  if (length(summ.f) >0 ) {
   ## need to exclude any species already successfully retrieved
   

   for (i in 1:length(summ.f)) {
     if (file.size(summ.f[i])>35) {    
       one.summ <- read.csv(summ.f[i]) 

      if (i==1) {
        summ.ALL <- one.summ
      } else {
        summ.ALL <- bind_rows(summ.ALL,one.summ)
      } 
    } #size is OK, not empty file
   } #loop through files to get summaries
   ##exclude successfully gotten spp and those with no data
   pass.number <- length(summ.f) + 1 
   while( file.exists( paste('data/',one.park.name,"_",pass.number,"_summ.csv",sep=""))) {
      pass.number <- pass.number + 1 ## increase so we don't overwrite existing file, if there is a gap in the sequence
   }  ## loop through files to avoid overwrite
   
   spp.needed <- left_join(spp,summ.ALL, by=c("scientific.name"="spp")) %>% filter(is.na(result.type)) %>% select(scientific.name,taxon.id)
   spp <- spp.needed 

  } else { #no files,  this is the first pass
     pass.number <- 1 
  } #files or not 

## could filter spp here manually if some are problematic <-  spp %>% filter(scientific.name=="Emex spinosa")

if (pass.number > 12) { # time to give up and go on to next park
  ## bump to next one
  if (pCounter < length(park.df$Park_Code)) {
     one.park.name <- park.df$Park_Code[pCounter + 1]
     ##save for next time:

       new.park.run <- as.data.frame(one.park.name)
       names(new.park.run) <- "park"
       write.csv(new.park.run, file="park.to.run.csv", row.names=FALSE)
    } else {
       ## all parks done
       cat("Script is all finished!  Press Enter to exit.")
        x <- readLines(con="stdin", 1)
    }
  
} else {  ##run the script as pass number is OK



  spp <- spp %>% select(taxon.id,scientific.name) %>% filter(taxon.id != -2 )
  spp.list <- spp$scientific.name #just the list, not a data frame

  ## if we've been at this a long time, reduce the number of species retrieved at one time

   ## spp.chunk is how many species to retrieve with one request, based on pass number and initial number of species

   spp.chunk <- init.spp.chunk  
   ##reduce number of species after some passes
   if ( pass.number > 3 & init.spp.chunk == 50 ) {
     spp.chunk <- 20  
   }
  if ( pass.number > 5 & init.spp.chunk == 50 ) {
     spp.chunk <- 5
   }
     if ( pass.number > 7 & init.spp.chunk == 50) {
     spp.chunk <- 1
   }
   if ( pass.number > 9 & init.spp.chunk == 50) {
     #really having trouble getting species, so only get some of a species at a time
     spp.chunk <- 0.5
   }

if (length(spp.list)==0) {  ## have some species to get
   cat('no species remaining for ',one.park.name,'\n')
}

if (length(spp.list)>0) {  ## have some species to get

  ## get park GIS data so we can determine how close in species are to the boundary
  nps.parks <- readOGR("gis/nps_boundary.shp")

  start.loop <- date()
  #loop through parks:
  #set park.list at top

  #set collection objects to NULL
  collect.data<- NULL
  collect.species.summ <- NULL

  collect.park.radii <- NULL

  start.spp.number<- 1 #start at the beginning, or test: start.spp.number<- 890
  

 # if (NEED.RESUME == TRUE) {  ##this for debugging
 # if resuming work, need to set up park list and species list differently (only will finish current park)
 #  park.list.done <- unique(collect.park.radii$park)
 #  park.list <- park.list.done[length(park.list.done)]
 #  start.spp.number <- s  #start where we left off
 #   }




   #filter map to one park
   new.spatial.ref <- paste("+init=epsg:",park.df$epsg[pCounter ],sep="") # lookup in df
  
    one.park <- subset(nps.parks,UNIT_CODE==one.park.name)
    #reproject 
    one.park.reproj <- spTransform(one.park,CRS(new.spatial.ref ))

    #get park  bounds
   one.park.bounds.ll <- attributes(one.park)$bbox
   #buffer by 2 degrees lat/long for extent
   buffer.deg <- 2 ## 3 for higher lat .5 for test
   if (spp.chunk == 0.5) {
      geography.runs <- 20
      spp.chunk <- 1  #still one species at a time
   } else {
      geography.runs <- 1 ## run normally
   }
   one.park.bounds.super.extent <- c(one.park.bounds.ll[2,1]-buffer.deg,one.park.bounds.ll[1,1]-buffer.deg,one.park.bounds.ll[2,2]+buffer.deg,one.park.bounds.ll[1,2]+buffer.deg)
    swlat <- one.park.bounds.super.extent[1]
    nelng <- one.park.bounds.super.extent[4]
   nelat  <- one.park.bounds.super.extent[3]
    swlng <- one.park.bounds.super.extent[2]
     ##loop through species

      ##possibly more than one run per species
     for (L in start.spp.number:ceiling(length(spp.list)/spp.chunk)) {
         # L <-1 ##for testing #  for (L in 1:3) {

       s<- (L -1) * spp.chunk + 1 # s is the index of the first species to download
       one.spp <- spp.list[s]
       last.s <- min(s+spp.chunk-1,length(spp.list)) # last.s is the index of the last species to download
       some.w.taxon.id <- paste(unique(spp$taxon.id[s:last.s]),collapse=',') ## no need to get duplicates, so ask for unique only
       ##some.w.taxon.names <- as.data.frame(spp$scientific.name[s:last.s])
       some.w.taxon.names <- spp[s:last.s,] %>% select(scientific.name) ### ,taxon.id)

       names(some.w.taxon.names) <- c("spp")
       if (TRUE) {  ##could be overridden with species filter for testing
        # cat (paste(one.spp,'\n'))
         one.data <- NULL
         one.result.eval <- NULL
         cat (paste('\n','start',one.park.name,one.spp,":",spp$scientific.name[last.s],one.result.eval,'geog runs',geography.runs,'\n'))
        ## tryCatch({
            
           if (geography.runs>1) {
                            
              ##cent.lat <- (nelat + swlat ) /2              
              ##cent.lng <- (nelng + swlng ) /2              
 
              lat.span <- (nelat-swlat) / geography.runs             
              long.span <- (nelng-swlng) / geography.runs
              result.thus.far <- 0
              for (glat in 1:geography.runs) {
                for (glong in 1:geography.runs) {
                   quad.1 <- NULL
                 tryCatch({

                quad.1 <- iNat(captive="false", geo="true", quality_grade = "research", 
                   nelat = swlat + ((glat+1)*lat.span) , nelng = swlng + ((glong+1)*long.span),
                   swlat =  swlat + ((glat)*lat.span), swlng = swlng + ((glong)*long.span) , taxon_id=some.w.taxon.id)
            
           

                 quad.1.result.eval <- evalResult(quad.1)
                 result.thus.far <- max(result.thus.far , quad.1.result.eval)
             

      

                 if (  quad.1.result.eval ==1 ) {
                    ## no errors and data
                    one.data <- addToResults(one.data, quad.1)   
                 }  ##have data in one round

 } , error=function(e){
                      cat("missing one :",conditionMessage(e), "\n")
                       
                })



                } #loop long
               } #loop lat
               if ( result.thus.far  == 1) { 
                 if (nrow(one.data) != length(unique(one.data$id))) {
                    ## need deduplication
                    one.data.dedup <- one.data %>% select(id,taxon.id, taxon.name, quality_grade,observed_on, time_observed_at, user.id,geojson.coordinates)
                    one.data <- unique(one.data) 
                 } 
               }        
                one.result.eval <- result.thus.far

           } else {  ##run all at once
             one.data <- iNat(captive="false", geo="true", quality_grade = "research", 
               nelat = nelat , nelng = nelng , swlat = swlat , swlng = swlng , taxon_id=some.w.taxon.id)
             one.result.eval <- evalResult(one.data)
           }

        
         cat (paste('\n done with download of',one.park.name,one.spp,":",spp$scientific.name[last.s],one.result.eval,'\n'))
          
         ##fill in what we were looking for
          some.species.summ <- some.w.taxon.names
          some.species.summ$park <- one.park.name
          some.species.summ$result.type <- one.result.eval 
         ## collect these into collector object

         collect.species.summ <- addToResults(collect.species.summ, some.species.summ)         

         if (one.result.eval != 1 ) {  #this means it didn't get anything or there were too many request, either way wait a bit
            #give time to catch its breath, otherwise get XML returned instead of JSON which presumably is a "too many requests" situation
            Sys.sleep(3) ##was 10 

         } else {
            ##have data, limit fields and see which are close enough to the park
            one.data <- one.data %>% select(id,taxon.id, taxon.name, quality_grade, observed_on,time_observed_at, user.id,geojson.coordinates)

            inat.names <-one.data %>% select(taxon.id, taxon.name) %>% group_by(taxon.id) %>% 
                summarise(inat.taxon.name.first = first(taxon.name), .groups='keep')
            
            #parse out species name:
            one.data$taxon.species.level.name <- gsub("^([^ ]+ [^ ]+)( .*)?","\\1",one.data$taxon.name)
            
            ##extra lat and long to their own atts from geojson.coordinates
            one.data$longitude <- sapply(one.data$geojson.coordinates,getItem1)
            one.data$latitude <- sapply(one.data$geojson.coordinates,getItem2)
             
            one.data$geojson.coordinates <- NULL
            
            ## some species should also be run at species level, if there are some combination of var/ssp or species and at least one var/ssp:
            # unique list of species
            unique.full.names <- one.data %>% select(taxon.name,taxon.species.level.name, taxon.id) %>% group_by(taxon.name,taxon.species.level.name) %>% 
                summarise(min.taxon.id=min(taxon.id),.groups='keep')
                  
            # those that need running at species level too
            run.spp.level <- unique.full.names %>% ungroup()  %>% select(taxon.species.level.name,min.taxon.id) %>%
                group_by(taxon.species.level.name) %>% summarise(countNames=n(), min.taxon.id=  min(-1* min.taxon.id), .groups='keep') %>% filter(countNames>1)
            
            # add these to inat.names:
            inat.names.spp.level <-  run.spp.level %>% rename(taxon.id=     min.taxon.id, inat.taxon.name.first=taxon.species.level.name ) %>% select(-countNames)
                         
            if (nrow(inat.names.spp.level ) > 0 ) {
                 inat.names <- bind_rows( inat.names, inat.names.spp.level)
            }            

            ## adding variety/subspecies data at species level also
            one.data.spp.level <- one.data %>% select(-taxon.name,-taxon.id) %>% inner_join(run.spp.level,by="taxon.species.level.name") %>% 
                  mutate(taxon.name=taxon.species.level.name, taxon.id=min.taxon.id, analysis.level='species') %>% select(-min.taxon.id)

            one.data$analysis.level<-'raw'
            one.data$countNames<-1
            
            ##also then need to exclude these from one.data as they will be dealt with in spp level
            one.data.others <- left_join(one.data,run.spp.level,by=c("taxon.name"="taxon.species.level.name")) %>% filter(is.na(min.taxon.id))
          
            ## old approach: species.leveldata <- one.data %>% select(-taxon.name, -taxon.id) %>% rename(taxon.name = taxon.species.level.name)
            #lookup taxon.id for species level name:
            ## old approach:species.leveldata <-species.leveldata %>% left_join(inat.names,by=c("taxon.name"="inat.taxon.name.first"))

            #filter to just those records that don't match species level name
            ## old approach:var.ssp.data <- one.data %>% filter(taxon.name != taxon.species.level.name)       
            ## reset in testing collect.data <- NULL

            if (nrow( one.data.others)>0) {
              if (nrow(one.data.spp.level) >0) {
                 both.level.data <- addToResults(one.data.others,one.data.spp.level)
              } else {
                both.level.data <- one.data.others
              } 
            } else {
               both.level.data <-one.data.spp.level
            }

            collect.data <- addToResults(collect.data,both.level.data)
            species.spatial <-as.data.frame(both.level.data )
         
            ##make it spatial
            coordinates(species.spatial) <- ~longitude + latitude
            ##set projection
            proj4string(species.spatial) <- CRS("+init=epsg:4326") # WGS 84
            #plot(species.spatial)
            #plot(one.park, add=TRUE)

            species.spatial.reproj <- spTransform(species.spatial, CRS(new.spatial.ref))
 

            points_in_park <- st_join(st_as_sf(species.spatial.reproj),st_as_sf(one.park.reproj), left=FALSE)
            park.points.temp <- as.data.frame(points_in_park ) %>% select(id, taxon.id, taxon.name) 
            park.points.summ <- park.points.temp %>% group_by(taxon.id) %>% summarise(pts_park=n(),.groups="keep")
            
            park.points.temp <-park.points.temp %>% select(id)%>% mutate(reallyinpark=TRUE)
        
        for (r in radii) {  ## see what's in this radius outside of the park bounds:
            cat('start radius ',r)
            #buffer 160km
            ##one.park.reproj.buffer <- gBuffer(one.park.reproj,width=160000,quadsegs = 10)
            one.park.reproj.buffer <- gBuffer(one.park.reproj,width=1600*r,quadsegs = 10)
            

            ############ SPATIAL JOIN
             points_in_buffer <- st_join(st_as_sf(species.spatial.reproj),st_as_sf(one.park.reproj.buffer), left=FALSE)
        
      	#exclude points that are in from points that are not in
      	buffer.points <- as.data.frame(points_in_buffer)
	      points_in_buffer_exclpark <- left_join(buffer.points, park.points.temp, by="id") %>% filter(is.na(reallyinpark))
            

            
            buffer.points.summ <- buffer.points %>% group_by(taxon.id) %>% summarise(pts_buffer=n(),.groups="keep")
            buffer.points.exclpark.summ <- points_in_buffer_exclpark %>% group_by(taxon.id) %>% summarise(pts_buffer_exclpark=n(),.groups="keep")
            points.dl.summ <- both.level.data %>% group_by(taxon.id) %>% summarise(pts_download=n(),.groups="keep")
            
           

             ##count each species to count separately:
            ## this fails when getting multiple species at once                    
            #   one.park.radius.data <- as.data.frame(cbind(one.park.name,one.spp,one.result.eval,r,
            #   nrowZero(both.level.data),   nrowZero(points_in_buffer), nrowZero(points_in_park), nrowZero(points_in_buffer_exclpark)))
            
            #   names(one.park.radius.data) <- c("park","spp","result.type","radius_miles","dl_pts","pts_buffer","pts_park","pts_buffer_outside_park")
           
           ## collect data and use custom function naTo0 to convert na to 0
           one.park.radius.data <-  points.dl.summ %>% left_join( buffer.points.summ , by="taxon.id") %>% 
                                                       left_join( buffer.points.exclpark.summ, by="taxon.id")  %>%  
                                                       left_join( park.points.summ, by="taxon.id") %>%
                                                       mutate(across(.fns=naTo0)) %>%
                                                       mutate(park = one.park.name, result.type=one.result.eval, radius_miles = r)
                                         

           ## add scientific names back in
                       
           one.park.radius.data <-   one.park.radius.data %>% left_join(spp,by="taxon.id") %>% left_join(inat.names,by="taxon.id")

          collect.park.radii <- addToResults(collect.park.radii, one.park.radius.data)
            
             cat(' ... done with radius',r,'\n')
            ## cleanup
             rm(one.park.reproj.buffer)
             rm(points_in_buffer)
              rm(buffer.points)
             rm(points_in_buffer_exclpark)
             rm(one.park.radius.data)
             ## collect garbage
             gc()
             
           } ## next radius
             cat(' ... done with all radii \n')
         } #end of result type
                  
        
       ## end of error trapping
##      } , error=function(e){
  ##                    cat("SLEEP 60 AFTER ERROR :",conditionMessage(e), "\n")
    ##                  Sys.sleep(60 )
      ##          })

         

       }  ##species filter for testing
     ##fails to override  s <- (last.s + 1) ##override s to new
    
       if (memory.size() >= memory.size.trigger.quit ) {
            ##exit loop so we can write results and quit
            break
       }

    } #loop through each species set

   if (length(collect.data) > 0 ) {  #only write data portion if there is data
     #done earlier
     #collect.data$latitude <- sapply(collect.data$geojson.coordinates,getItem2)
     #collect.data$longitude <- sapply(collect.data$geojson.coordinates,getItem1)
     #collect.data$geojson.coordinates <- NULL
     
     collect.data.towrite <- as.data.frame(collect.data %>% select(latitude,longitude,quality_grade,id,taxon.id, taxon.name, observed_on))
     write.csv(collect.data.towrite,file=paste('data/',one.park.name,'_',pass.number,'_raw_data.csv',sep=''))
     write.csv(collect.park.radii,file=paste('data/',one.park.name,'_',pass.number,'_radii.csv',sep=''))
   }
     write.csv(collect.species.summ,file=paste('data/',one.park.name,'_',pass.number,'_summ.csv',sep=''))

     stats.to.write <- cbind("number observations",nrowZero(collect.data))
     stats.to.write <- rbind(stats.to.write, cbind("start time",start.time))
     stats.to.write <- rbind(stats.to.write, cbind("number species",nrowZero(collect.species.summ)))
     stats.to.write <- rbind(stats.to.write, cbind("number data points radii",nrowZero(collect.park.radii)))
     stats.to.write <- rbind(stats.to.write, cbind("end memory usage",memory.size()))
     stats.to.write <- as.data.frame(rbind(stats.to.write, cbind("end time",date())))
     names(stats.to.write) <- c("stat","value")
     write.csv(stats.to.write,file=paste('data/',one.park.name,'_',pass.number,'_stats.csv',sep=''))

    cat('step 3 script done successfully. additional species observations may need downloading by rerunning this script. \n ')   

} else {  ##there are no more species to get
    cat('all species downloaded',file=paste('data/',one.park.name,"_done.csv",sep=""))
    cat('step 3 script done successfully. all species observations downloaded \n ')   
}

   if ( !file.exists( paste(one.park.name,"_",pass.number,"_summ.csv",sep=""))) {
      ## create this file anyway, there's nothing here though, just headers
	  ## write.csv(collect.species.summ,file=paste(one.park.name,'_',pass.number,'_summ.csv',sep=''))
	  cat( '"","spp","park","result.type"\n',file=paste(one.park.name,'_',pass.number,'_summ.csv',sep=''))
   }

} ##really run, pass number OK 
