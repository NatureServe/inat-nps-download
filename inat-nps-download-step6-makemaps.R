## set working directory if not already done:
#setwd('C:\\Users\\michael_lee\\Documents\\NPS_Bioblitz\\testClean')

### get observations for each species observed in the bioblitz
## sources: https://stanford.edu/~vbauer/teaching/hillshade.html
## http://viewfinderpanoramas.org/Coverage%20map%20viewfinderpanoramas_org15.htm
# https://www.earthdatascience.org/courses/earth-analytics/lidar-raster-data-r/crop-raster-data-in-r/
# https://stackoverflow.com/questions/33227182/how-to-set-use-ggplot2-to-map-a-raster
# https://geodata.lib.berkeley.edu/catalog/stanford-gk628xr0233
# https://nceas.github.io/oss-lessons/spatial-data-gis-law/3-mon-intro-gis-in-r.html
# https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html

draw.elev <- FALSE
png.wid <- 1080*2
png.ht <- 800*2
png.res <- 144


if (FALSE) {
install.packages(c("httr","maps","ggspatial"))
install.packages("RColorBrewer")
install.packages(c("dplyr","rgdal","sf"))

install.packages("ggspatial")
}
if (draw.elev  == TRUE) {
  install.packages(c("rasterVis","viridis","raster"))   
  library(raster)
  library(rasterVis)
  library(viridis)
}

library(ggspatial)
library(httr)
library(maps)
library(RColorBrewer)
# not used : install.packages(c("USAboundaries"))
##"rinat" is not available from CRAN as of 2/25/2020 and must be installed locally using https://cran.r-project.org/src/contrib/Archive/rinat/
## use Packages | Install package(s) from local files and browse for the .tar.gz from the archive link above

##library(rinat)
library(dplyr)
library(rgdal)
library(ggplot2)
##

 library(rgeos) ## for buffer boundaries of parks


library(sf)  #spatial joining
## modified from https://ryanpeek.github.io/2019-04-29-spatial-joins-in-R/
## not used: library(USAboundaries)

## get park boundaries:
nps.parks <- readOGR( "gis/nps_boundary.shp")

##proj4string(nps.parks) <- CRS("+init=epsg:4326") # WGS 84

## transform to meters grid to buffer


  us.county <- readOGR("gis/cb_2016_us_county_500k.shp")

  us <- readOGR("gis/cb_2016_us_state_5m.shp")  ##states

 
  us@data$STFIPNum <- as.numeric(us@data$STATEFP)
  us.48 <- subset(subset(us,STFIPNum<=56), STUSPS !="HI")
 ## plot(us.48)


#expand limits functions
expandLims <- function(obj, incr.factor) {
  bb <- bbox(obj)

  bigscale <- max(bb[1,2] - bb[1,1],bb[2,2] - bb[2,1])

  bb.min.x <- bb[1,1] -  (bigscale ) * (incr.factor)
  bb.max.x <- bb[1,2] +   (bigscale ) * (incr.factor)


  bb.min.y <- bb[2,1]  -  (bigscale ) * (incr.factor)

  bb.max.y <- bb[2,2] +  (bigscale ) * (incr.factor)


  return(as.matrix(rbind(c(bb.min.x,bb.max.x),c(bb.min.y,bb.max.y))))

}


## convert points to this too:
## 
## too slow, too much memory species_collect <- read.csv("all.data.exotics.csv")

park.df <- read.csv("target_NPS_parks.csv")
park.list <- unique(park.df$Park_Code)

##make sure directory exists for maps:
if (dir.exists("maps")!=TRUE) {dir.create("maps")}

##functions to draw and save maps
createGGMapSpp <- function(thelimits, viewName, therelief, s, sppAll, sppDonut, sppPark ) {
 gg2 <- ggplot() + coord_fixed(xlim=thelimits[1,], ylim = thelimits[2,]) 

   if  (draw.elev == TRUE) {
        gg2 <- gg2 + geom_raster(data=therelief , mapping=aes(x=x,y=y,fill=value), show.legend=FALSE ) + theme_dark() 
    }
    gg2 <- gg2 + geom_path(us.county.reproj, mapping=aes(x=long,y=lat, group=group ) , color="#999999") 
    gg2 <- gg2 + geom_path(us.48.reproj, mapping=aes(x=long,y=lat, group=group))   +
    geom_point(sppAll@data, alpha=the.alpha, mapping=aes(x=coordinates(sppAll)[,1], 
         y=coordinates(sppAll)[,2], col="occurrences outside buffer"))  +
  geom_path(one.park.reproj.buffer, mapping=aes(x=long,y=lat, group=group, col= "160km buffer boundary"))   +
 geom_path(one.park.reproj, mapping=aes(x=long,y=lat, group=group, col="park boundary"))   +
       labs(title = paste(s , 'in and near' , one.park.name),
       subtitle = "Data from iNaturalist, Analysis by NatureServe",
       x = " ", y = " ") +   theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), panel.grid.major =element_blank(), panel.grid.minor =element_blank()) + 
####     scale_colour_manual(name="occurrences and boundaries") + 
     annotation_scale(location = "bl", width_hint = 0.5 , bar_cols = c("#000000","#FFFFFF"), text_col="#333333") +
     annotation_north_arrow(location = "bl", which_north = "grid", ### line_col = "#CCCCCC",  fill = c("#AAAAAA","#CCCCCC"), text_col = "#CCCCCC" ,
        pad_x = unit(0.75, "in"), pad_y = unit(0.2, "in"),  
        style = north_arrow_fancy_orienteering) +  scale_color_manual(values=color.five ) + theme(legend.title = element_blank())
  if (nrow(sppDonut)>0 ) {
    #convert geometry into coordinates
    sppDonut.coord <- st_coordinates(sppDonut$geometry)
    gg2 <- gg2 + geom_point(sppDonut, alpha=the.alpha, mapping=aes(x=sppDonut.coord[,1], y=sppDonut.coord[,2], col=paste("occurrences within buffer:",nrow(sppDonut))))    
  } else { ##transparent points so the legend stays the same
     gg2 <- gg2 + geom_point(sppAll@data, alpha=0, mapping=aes(x=coordinates(sppAll)[,1], y=coordinates(sppAll)[,2], col="occurrences within buffer: 0"))  
  }
  if (nrow(sppPark) >0 ) {
    #convert geometry into coordinates
    sppPark.coord <- st_coordinates(sppPark$geometry)
    gg2 <- gg2 +  geom_point(sppPark , alpha=the.alpha, mapping=aes(x=sppPark.coord[,1], y=sppPark.coord[,2], col=paste("occurrences within park: ",nrow(sppPark ))))   
  } else {
    gg2 <- gg2 + geom_point(sppAll@data, alpha=0, mapping=aes(x=coordinates(sppAll)[,1], y=coordinates(sppAll)[,2], col="occurrences within park: 0")) 
  }
 


  gg2
  ggsave(file=paste('maps/',one.park.name,'_',s,'_',viewName,'_v5.png',sep=''))
}

color.five <- c( brewer.pal(n=4, name = 'YlOrRd'), brewer.pal(n=5, name = 'PRGn')[2])

##display.brewer.pal(n=5, name='YlOrRd')
the.alpha <- 0.5 #transparency to use for dots

createGGMap <- function(thelimits, viewName, therelief, s ) {
 gg2 <- ggplot() + coord_fixed(xlim=thelimits[1,], ylim = thelimits[2,])
    if  (draw.elev == TRUE) {
        gg2 <- gg2  + geom_raster(data=therelief , mapping=aes(x=x,y=y,fill=value), show.legend=FALSE ) + theme_dark() 
    }
    gg2 <- gg2 + 
    geom_path(us.county.reproj, mapping=aes(x=long,y=lat, group=group ), color="#999999")   +
    geom_path(us.48.reproj, mapping=aes(x=long,y=lat, group=group))   +
    geom_point(species.spatial.reproj@data, alpha=the.alpha, mapping=aes(x=coordinates(species.spatial.reproj)[,1], 
         y=coordinates(species.spatial.reproj)[,2], col="occurrences outside buffer"))  
  if (nrow(points_in_buffer_exclpark)>0) {
   gg2 <- gg2 + geom_point(points_in_buffer_exclpark, alpha=the.alpha, mapping=aes(x=points_in_buffer_exclpark.coord[,1], y=points_in_buffer_exclpark.coord[,2], col="occurrences within buffer"))    
    }   
    if (TRUE) { gg2 <- gg2 + geom_path(one.park.reproj.buffer, mapping=aes(x=long,y=lat, group=group, col= "160km buffer boundary"))   }

   if (nrow(points_in_park)>0) { gg2 <- gg2 + geom_point(points_in_park, alpha=the.alpha, mapping=aes(x=points_in_park.coord[,1], y=points_in_park.coord[,2], col="occurrences within park"))  }
  gg2 <- gg2 + geom_path(one.park.reproj, mapping=aes(x=long,y=lat, group=group, col="park boundary"))   +
       labs(title = paste(s , 'in and near' , one.park.name),
       subtitle = "Data from iNaturalist",
       x = " ", y = " ") +   theme(axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank(), panel.grid.major =element_blank(), panel.grid.minor =element_blank()) + 
   ##  scale_colour_manual(name="occurrences and boundaries") + 
     annotation_scale(location = "bl", width_hint = 0.5 , bar_cols = c("#000000","#FFFFFF"), text_col="#333333") +
     annotation_north_arrow(location = "bl", which_north = "grid", 
        pad_x = unit(0.75, "in"), pad_y = unit(0.2, "in"),  
        style = north_arrow_fancy_orienteering) +  scale_color_manual(values=color.five ) + theme(legend.title = element_blank()) 
   gg2
  ggsave(file=paste('maps/',one.park.name,'_',s,'_',viewName,'_v5.png',sep=''))
}
#re-test: createGGMap(maplims ,'buffer', relief ,s  )
 ##this adds rank to watch lists
 watch.list <- read.csv("park_watch_list.csv") %>% group_by(park) %>% mutate(rankN=min_rank(desc(iNaturalist.occurrences.within.buffer.outside.park)))
   write.csv(watch.list,file="park_watch_list_rank.csv")


  watch.list$format.rank <- gsub(" ","0",format(watch.list$rankN))


###LOOP THROUGH PARKS
for (pCounter in 1:length(park.list)) {
  ##test: pCounter <- 36
  one.park.name <- park.list[pCounter]

  ##get data, this park only
  summ.f <- list.files(path = "data", pattern = paste(one.park.name,"_[0-9]+_raw_data.csv",sep=""))

  all.data <- NULL
  if (length(summ.f) ==0 ) {
     cat('no data for ' , one.park.name , '\n')
  } else { 
  for (i in 1:length(summ.f)) {
       data.file <- paste("data/", summ.f[i],sep="")
       one.data <- read.csv(data.file)
        if (length(all.data)==0) {
            all.data <- one.data
          } else {
            all.data <- bind_rows(all.data, one.data)
          }## have data
  } # loop through data this park

   ## deduplicate
     

   park.data.dedup <- all.data %>% group_by(id) %>% slice_head(n=1) # ,.preserve=TRUE)
   
   if (length(unique(all.data$id)) != nrow(park.data.dedup)) {
       cat('dedup failed \n') 
       stop('dedup failed!')
    }

   if (nrow(all.data) != nrow(park.data.dedup)) {
         cat(nrow(all.data),'rows of raw data',one.park.name,'\n')       
   }
   cat(nrow(park.data.dedup),'rows of deduplicated raw data',one.park.name,'\n')

   species.spatial <-as.data.frame(park.data.dedup)
   ##make it spatial
   coordinates(species.spatial) <- ~longitude + latitude
   ##set projection
    proj4string(species.spatial) <- CRS("+init=epsg:4326") # WGS 84
  
  new.spatial.ref <- paste("+init=epsg:",park.df$epsg[pCounter ],sep="") # lookup in df

   us.48.reproj <-  spTransform(us.48,CRS(new.spatial.ref))
   us.county.reproj <- spTransform(us.county,CRS(new.spatial.ref))

   species.spatial.reproj <- spTransform(species.spatial, CRS(new.spatial.ref))



##for (pCounter in 1:length(levels(nps.parks$sitename))) {  ##repeat for each park

#filter to one park

##one.park.name <- levels(nps.parks$sitename)[pCounter] ## "MACA"
##one.park.name <- "MACA"
one.park <- subset(nps.parks,UNIT_CODE==one.park.name)

one.park.reproj <- spTransform(one.park,CRS(new.spatial.ref))


#buffer 160km
one.park.reproj.buffer <- gBuffer(one.park.reproj,width=160000,quadsegs = 10)

############ SPATIAL JOIN

points_in_buffer <- st_join(st_as_sf(species.spatial.reproj),st_as_sf(one.park.reproj.buffer), left=FALSE)
points_in_park <- st_join(st_as_sf(species.spatial.reproj),st_as_sf(one.park.reproj), left=FALSE)


#exclude points that are in from points that are not in
park.points.temp <- as.data.frame(points_in_park ) %>% select(id) %>% mutate(reallyinpark=TRUE)
buffer.points <- as.data.frame(points_in_buffer)
points_in_buffer_exclpark <- left_join(buffer.points, park.points.temp, by="id") %>% filter(is.na(reallyinpark))


newlims <- expandLims(one.park.reproj.buffer,0.2)
tightlims <- expandLims(one.park.reproj,2)
   maplims <- expandLims(one.park.reproj.buffer,0.2)
  points_in_buffer_exclpark.coord<- st_coordinates(points_in_buffer_exclpark$geometry) 
  points_in_park.coord<-  st_coordinates(points_in_park$geometry) 
  ## need to recrop this: 3/11/2020
  
 if (draw.elev  == TRUE) {
  rast.loc <- "F:\\Data\\Elevation\\NED_USGS\\NED90m\\ned_90m_alb83i.tif"
  landcov <- raster(rast.loc) ##"C:/Users/Michael_Lee/Documents/local/GIS/dem/ned90m/ned_90m_albi83i_clipSE3.tif")
      ## plot(landcov , col=gray.colors(20,start=0,end=1))
      ## mosaic <- (mosaic, crs=proj)

  one.park.super.buffer <- gBuffer(one.park,width=240000,quadsegs = 10)

   #crop the raster to around the park, in the raster CRS
  raster.crs <- CRS("+proj=aea +lat_0=23 +lon_0=-96 +lat_1=29.5 +lat_2=45.5 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs")
  one.park.super.buffer.reproj <- spTransform(one.park.super.buffer,raster.crs)
  landcov.crop.extent <- crop(landcov,one.park.super.buffer)
  landcov.reproj <- projectRaster(landcov.crop.extent,crs=CRS(new.spatial.ref))


   newrasterlims <- expandLims(one.park.reproj.buffer,1)


   newrasterlims.park <-  expandLims(one.park.reproj,2.5)

   crop_extent <- crop(landcov.reproj,newrasterlims ) # one.park.reproj.buffer)
   crop_extent.park <- crop(landcov.reproj,newrasterlims.park) 
   crop_extent.agg <- aggregate(crop_extent,fact=4)

   slope <- terrain(3 * crop_extent.agg, opt="slope", unit='radians')
   aspect <- terrain(crop_extent.agg, opt="aspect", unit='radians')
   hillshade <- hillShade(slope, aspect, angle=45, direction=315)

   slope.park  <- terrain(crop_extent.park, opt="slope", unit='radians')
   aspect.park <- terrain(crop_extent.park, opt="aspect", unit='radians')
   hillshade.park <- hillShade(slope.park, aspect.park, angle=45, direction=315)

   ## thelimits<-tightlims 

   ##relief_spdf <- as(crop_extent,"SpatialPixelsDataFrame")
   relief_spdf <- as(hillshade,"SpatialPixelsDataFrame")
   ##relief <- as.data.frame(relief_spdf) %>% rename(value='ned_90m_albi83i_clipSE2')
   relief <- as.data.frame(relief_spdf) %>% rename(value= 'layer')

   relief.park_spdf <- as(hillshade.park,"SpatialPixelsDataFrame")
   ##relief <- as.data.frame(relief_spdf) %>% rename(value='ned_90m_albi83i_clipSE2')
   relief.park <- as.data.frame(relief.park_spdf) %>% rename(value= 'layer')
} ## drawing rasters


s <- "All exotics"


createGGMap(maplims ,'buffer', relief ,s  )  #uses s
createGGMap(tightlims ,'park', relief.park, s)



spp.results <- tibble('spp') ##,"nDownloaded","nWithinAreaNotOnPark","nOnPark"))
 names(spp.results) <- c("spp")
spp.results$park <- "park" 
spp.results$nDownloaded <- 0
 spp.results$nWithinAreaNotOnPark<- 0
 spp.results$nOnPark<- 0

 #  park.watch.list <- read.csv("park_watch_list.csv") %>% filter(park==one.park.name)  %>% 
 #    mutate(rankN=min_rank(desc(iNaturalist.occurrences.within.buffer.outside.park)))
  park.watch.list <- watch.list %>% filter(park==one.park.name)  
 ##done already write.csv(park.watch.list,file="park_watch_list_rank.csv")
 ##done already park.watch.list$format.rank <- gsub(" ","0",format(park.watch.list$rankN))
  
 for (s in unique(park.watch.list$scientific.name)){
    cat(s, "starting map  ", one.park.name , "\n")
    s.num <- which(park.watch.list$scientific.name==s)
    spp.rank <- park.watch.list$format.rank[s.num]
    spp.obs<- (species.spatial.reproj@data) %>% filter(taxon.name == s ) # , park == one.park.name)
    spp.donut<- (points_in_buffer_exclpark)  %>% filter(taxon.name == s ) #, park == one.park.name)
    spp.park<- (points_in_park) %>% filter(taxon.name == s ) # , park == one.park.name)
    
   # spp.donut <- subset( points_in_buffer_exclpark, target.spp == s , park == one.park.name)
  #not needed:   r.temp<- rbind(spp.results,c(s,one.park.name,nrow(spp.obs) ,nrow(spp.donut),nrow(spp.park)))
    ##create png
    onespppark.spatial <-as.data.frame(park.data.dedup %>% filter(taxon.name == s)) # , park == one.park.name))
   onespppark.spatial.reproj <- NULL
     ##make it spatial, if data exists
    if (nrow(onespppark.spatial)>0) {
     coordinates(onespppark.spatial) <- ~longitude + latitude
    ##set projection
      proj4string(onespppark.spatial) <- CRS("+init=epsg:4326") # WGS 84
      onespppark.spatial.reproj <- spTransform(onespppark.spatial, CRS(new.spatial.ref))

      onespppark_points_in_buffer <- st_join(st_as_sf(onespppark.spatial.reproj),st_as_sf(one.park.reproj.buffer), left=FALSE)
      onespppark_points_in_park <- st_join(st_as_sf(onespppark.spatial.reproj),st_as_sf(one.park.reproj), left=FALSE)


      onespppark.points.temp <- as.data.frame(onespppark_points_in_park ) %>% select(id) %>% mutate(reallyinpark=TRUE)
      onespppark.buffer.points <- as.data.frame(onespppark_points_in_buffer)
      onespppark.points_in_buffer_exclpark <- left_join(onespppark.buffer.points, onespppark.points.temp, by="id") %>% filter(is.na(reallyinpark))

    
   

   createGGMapSpp(maplims ,'buffer', relief , paste('watch list rank ',spp.rank,s), onespppark.spatial.reproj, spp.donut,  spp.park)
   createGGMapSpp(tightlims ,'park', relief.park, paste('watch list rank ',spp.rank,s), onespppark.spatial.reproj, spp.donut,  spp.park)

   }  ## if no spp, no maps   




 }  ## loop SPP
########################################

  cat('step 6 done creating maps.  Check "maps" subfolder to see if they were created correctly. \n ')
 } ## data or not
 } ########### LOOP THROUGH PARKS




