
## assumes working directory is set to location of the files in the repository.  If not, set it here:
#setwd('C:\\Users\\michael_lee\\Documents\\NPS_Bioblitz\\testClean')



### step 2 file picks up after step 1, and makes sure it ran successfully:

install.status <- read.csv(file="install.status.csv")$V1
if (install.status != "loaded") {

  stop("R libraries don't seem to be loaded properly")
} 
install.status


## load libraries
library(iNatTools)
library(dplyr) #general R-ing

#spatial calculations/mapping:
library(httr)
library(maps)
library(rgdal)
library(rgeos) ## for buffer boundaries of parks
library(sf)  #spatial joining

spp.status.file <- 'spp.taxon.id.status.csv'

if (file.exists(spp.status.file)) {
  spp.status <- read.csv(spp.status.file)
  pass.num <- spp.status$pass[nrow(spp.status)] + 1  #get last pass and add one to it
} else {
  pass.num <- 1 ##initial pass through species
  cat('pass,topic,info\n',file=spp.status.file)
}

cat('start species taxon ID pass #',pass.num,'\n')

## read in list of species
spp <- read.csv("spp.csv")  

##check to see if this is done already:
if (file.exists("spp.ids.csv")) {
  spp.with.id <- read.csv("spp.ids.csv")
  spp <- spp.with.id
    
}

##get names on df  
spp.names <- as.data.frame(names(spp))
names(spp.names) <- c("column")
## is scientific.name on the list of names
has.sn <- spp.names %>% filter(column=="scientific.name") %>% summarise(occ=n())
has.taxon.id <- spp.names %>% filter(column=="taxon.id") %>% summarise(occ=n())


if (has.sn$occ[1] == 0) {
    stop('species list does not have "scientific name" column')
}


if (has.taxon.id$occ[1] == 0) {
   ## doesn't have taxon.id, add as -1
   spp$taxon.id <- (-1) 
}

spp.already.id <- spp %>% filter(taxon.id != -1) #these are already done
spp.need.id <- spp %>% filter(taxon.id == -1) 

cat('need id count ', nrow(spp.need.id),'\n')
cat('already have id count ', nrow(spp.already.id),'\n')

if ( nrow(spp.need.id) > 0 ) {  ## get more IDs

  ## modify copy of main iNat function to get taxonIDs
  iNat2 <- function (per_page = 200, order = "desc", order_by = "created_at", 
    acc = NULL, captive = NULL, endemic = NULL, geo = NULL, identified = NULL, 
    introduced = NULL, mappable = NULL, native = NULL, only_id = NULL, 
    out_of_range = NULL, pcid = NULL, photos = NULL, popular = NULL, 
    taxon_is_active = NULL, threatened = NULL, verifiable = "true", 
    id = NULL, not_id = NULL, place_id = NULL, project_id = NULL, 
    rank = NULL, site_id = NULL, taxon_id = NULL, without_taxon_id = NULL, 
    taxon_name = NULL, user_id = NULL, user_login = NULL, day = NULL, 
    month = NULL, year = NULL, term_id = NULL, term_value_id = NULL, 
    without_term_value_id = NULL, acc_above = NULL, acc_below = NULL, 
    d1 = NULL, d2 = NULL, created_d1 = NULL, created_d2 = NULL, 
    created_on = NULL, observed_on = NULL, unobserved_by_user_id = NULL, 
    apply_project_rules_for = NULL, cs = NULL, csa = NULL, csi = NULL, 
    geoprivacy = NULL, taxon_geoprivacy = NULL, hrank = NULL, 
    lrank = NULL, id_above = NULL, id_below = NULL, identifications = NULL, 
    lat = NULL, lng = NULL, radius = NULL, nelat = NULL, nelng = NULL, 
    swlat = NULL, swlng = NULL, list_id = NULL, not_in_project = NULL, 
    not_matching_project_rules_for = NULL, q = NULL, search_on = NULL, 
    quality_grade = NULL, updated_since = NULL, viewer_id = NULL, 
    reviewed = NULL, locale = NULL, preferred_place_id = NULL, 
    ttl = NULL) 
  {
    options(stringsAsFactors = FALSE)
    api <- "https://api.inaturalist.org/v1/observations"
    fetch <- list(per_page, order, order_by, acc, captive, endemic, 
        geo, identified, introduced, mappable, native, only_id, 
        out_of_range, pcid, photos, popular, taxon_is_active, 
        threatened, verifiable, id, not_id, place_id, project_id, 
        rank, site_id, taxon_id, without_taxon_id, taxon_name, 
        user_id, user_login, day, month, year, term_id, term_value_id, 
        without_term_value_id, acc_above, acc_below, d1, d2, 
        created_d1, created_d2, created_on, observed_on, unobserved_by_user_id, 
        apply_project_rules_for, cs, csa, csi, geoprivacy, taxon_geoprivacy, 
        hrank, lrank, id_above, id_below, identifications, lat, 
        lng, radius, nelat, nelng, swlat, swlng, list_id, not_in_project, 
        not_matching_project_rules_for, q, search_on, quality_grade, 
        updated_since, viewer_id, reviewed, locale, preferred_place_id, 
        ttl)
    names(fetch) <- c("per_page", "order", "order_by", "acc", 
        "captive", "endemic", "geo", "identified", "introduced", 
        "mappable", "native", "only_id", "out_of_range", "pcid", 
        "photos", "popular", "taxon_is_active", "threatened", 
        "verifiable", "id", "not_id", "place_id", "project_id", 
        "rank", "site_id", "taxon_id", "without_taxon_id", "taxon_name", 
        "user_id", "user_login", "day", "month", "year", "term_id", 
        "term_value_id", "without_term_value_id", "acc_above", 
        "acc_below", "d1", "d2", "created_d1", "created_d2", 
        "created_on", "observed_on", "unobserved_by_user_id", 
        "apply_project_rules_for", "cs", "csa", "csi", "geoprivacy", 
        "taxon_geoprivacy", "hrank", "lrank", "id_above", "id_below", 
        "identifications", "lat", "lng", "radius", "nelat", "nelng", 
        "swlat", "swlng", "list_id", "not_in_project", "not_matching_project_rules_for", 
        "q", "search_on", "quality_grade", "updated_since", "viewer_id", 
        "reviewed", "locale", "preferred_place_id", "ttl")
    res <- GET(api, query = c(fetch, list(page = 1)))
    resDF <- fromJSON(httr::content(res, as = "text"), flatten = TRUE)
    return(resDF )  #just return initial DS

  
  }

  getTaxonId <- function (taxonName) {
      x <- iNat2( taxon_name=taxonName,per_page=1)
      return((x$results)$taxon.id)
  }


  #lookup iNat taxon IDs so we can lookup by more than one spp at a time


  for (s in 1:nrow(spp)) {

  
    if (spp$taxon.id[s] == -1) {
      one.sci <- gsub(" ssp\\. "," ",gsub(" var\\. "," ",spp$scientific.name[s]))
      cat('get taxon id for ',one.sci,'\n')
      one.id <- -1  
      tryCatch({

      one.id <- getTaxonId(one.sci)
      ## end of error trapping
      } , error=function(e){
                      cat("SLEEP 60 AFTER ERROR :",conditionMessage(e), "\n")
                      Sys.sleep(60 )
                })


     if (length(one.id)==0) {
      spp$taxon.id[s] <- -2  
      cat(' got ID ',-2,'\n')
     }  else {
       spp$taxon.id[s] <- one.id       
      cat(' got ID ',one.id,'\n')
     }

     if ((s %% 20) == 0) {
       cat('snoozing 20 ...\n')
       Sys.sleep(10)
     }  ##pause every 20 species
   } #this one needs looking up
 
}  ## loop through spp

} ## some are needed

##merge spp

spp <- spp %>% select(scientific.name,taxon.id)

write.csv(spp,file="spp.ids.csv")

spp.without.id <- nrow(spp %>% filter(taxon.id==-1))

cat('done with taxon id lookup.  Spp without taxon id: ', spp.without.id  ,'\n')

mem.end <- memory.size()

cat(pass.num,',spp with taxon ID,', nrow(spp %>% filter(taxon.id!=-1)),'\n',file=spp.status.file, append="TRUE", sep="")
cat(pass.num,',spp still NEED taxon ID,', nrow(spp %>% filter(taxon.id==-1)),'\n',file=spp.status.file, append="TRUE", sep="")
cat(pass.num,',memory.end,',mem.end,'\n',file=spp.status.file, append="TRUE", sep="")

if (spp.without.id ==0) {

  cat('step 2 script successful \n ') 
  }  else {
  cat('step 2 script error of some kind or needs repeating which is normal \n ') 
}
