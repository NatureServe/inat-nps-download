### instead of using rinat, using new package
## from https://www.inaturalist.org/people/hanly
## Installation of R libraries

## assumes working directory is set to location of the files in the repository.  If not, set it here:
#setwd('C:\\Users\\michael_lee\\Documents\\NPS_Bioblitz\\testClean')

 tryCatch({  
   repo <- 'http://cran.us.r-project.org'  ## fine to change to a different repository if you like 

   install.status <- cbind("initializing",date())
   write.csv(install.status,file="install.status.csv")  ##install status can be used for automation steps


   install.packages(c("remotes"),repos=repo)
   library(remotes)
   install_github("pjhanly/iNatTools")
   install.packages(c("httr","maps","dplyr","tidyr","rgdal","sf","rgeos"),repos=repo)
   
  install.status <- cbind("installed",date())
  write.csv(install.status,file="install.status.csv")


## load libraries
library(iNatTools)
library(dplyr) #general R-ing
library(tidyr)
#spatial calculations/mapping:
library(httr)
library(maps)
library(rgdal)
library(rgeos) ## for buffer boundaries of parks
library(sf)  #spatial joining

## unzip shp zip file:

if (file.exists('gis/nps_boundary.shp') == FALSE ) {
  if (file.exists('gis/nps_boundary.zip')) {
  cat('unzipping NPS boundaries shapefile ... ')
  ## this was zipped to get it small enough to post in github
  unzip(zipfile='gis/nps_boundary.zip',exdir='gis',unzip = "internal",
      setTimes = FALSE)
  cat('done \n ')
  }
}
  ## check needed files:
  req.files <- c(
"gis/nps_boundary.prj",
"gis/nps_boundary.sbn",
"gis/nps_boundary.sbx",
"gis/nps_boundary.shp",
"gis/nps_boundary.shx",
"gis/nps_boundary.xml",
"gis/nps_boundary.cpg",
"gis/nps_boundary.dbf",
"target_NPS_parks.csv",
"irank_data.csv",
"spp.csv")
##,"altRApproach_v18c_downloadObsSspVar.R",
##"altRApproach_v18b_getSppID.R")

  req.files.exist<-as.data.frame(sapply(req.files,file.exists))
  names(req.files.exist) <- c("file.status")
  req.files.missing <- req.files.exist %>% filter(file.status==FALSE)
    
  if (nrow(req.files.missing)>0) { 
    install.status <- cbind("missing required files",paste(row.names(req.files.missing),collapse="; "))
   } else {
    install.status <- cbind("loaded",date())
  }
  install.status
  write.csv(install.status,file="install.status.csv")  ## first field will be "loaded" if successful

       ## end of error trapping
      } , error=function(e){
                     
                       install.status <- cbind("error",conditionMessage(e))
                       write.csv(install.status,file="install.status.csv")
                })


if (install.status[1,1] == 'loaded') {
   cat('step 1 script finished and looks good\n')
} else {
   cat('step 1 ERROR of some kind, see above and/or install.status.csv\n')
}

