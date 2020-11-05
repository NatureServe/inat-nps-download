# inat-nps-download
R scripts to download species observations in iNaturalist occuring on National Park Service lands

consists of 6 separate scripts with purposes listed below.
Also requires files to define scope of species to download and NPS target parks, also described below.
The presence of required files is checked in the initial script.

## important
## you must either place these files in the default working directory for R, or set the working directory when you load R, or edit the script files by changing these lines:
##setwd('c:\\users\\michael_lee\\NPS_Bioblitz\\testclean')
to
setwd('YOUR_PATH_HERE')
## note the ## at the start of the line are removed, as the initial line is commented out.


the "gis" subfolder contains shapefiles for determining which observations are on the parks or near the parks,
  and also for drawing parks and the observations on or nearby
  if making maps and you want county lines on the maps in script 6, this also needs to contain the county level maps shapefiles
    
spp.csv
  -- list of scientific names to lookup on NPS parks, user can edit

target_NPS_parks.csv
  -- list of NPS parks, with additional information like projection to use for maps

park.to.run.csv
  -- list of parks to download from iNaturalist, user can edit this to a single park in target_NPS_Parks.csv

## scripts 2 and 3 are meant to be run many times as some downloads can fail.  
## you can either quit and and restart for the next script, or continue with the next script in the same R session
## there is no need to save workspaces when quitting R!

example_shell_script.bat
  -- this is an example Windows Batch file that run scripts 1, 2, and 3 repeatedly (it makes no attempt to stop running the scripts)
  -- similar scripts could be written for other operating systems
  -- the example script would need editing to the correct path for the R script executable and the script files themselves

inat-nps-download-step1-installLibraries.R 
  -- installs R libraries and checks for required files
  -- updates first row and columns of install.status.csv to "loaded" if all is loaded OK.
  -- success indicated by message:
     step 1 script finished and looks good
     
inat-nps-download-step2-getSppID.R
  -- takes the scientific names add adds iNaturalist taxon.id values to lookup the taxa more efficiently in bulk
  -- updates spp.taxon.id.status.csv with information about this process and how much memory is used.  Memory may be limiting and R can be quit and restarted with scripts (not originally part of this repository)
  -- finished when species status file contains:
       (pass number),spp still NEED taxon ID,0
  -- success indicated by message:
     step 2 script successful
  
inat-nps-download-step3-downloadObs.R
  --downloads observations from the park in park.to.run.csv for species in spp.csv
  -- memory constraints will quit this and it can be rerun with a shell script
  -- multiple passes of this file progressively gets more full lists of species until they are all finished
  -- saves data files in the 'data' subfolder
  -- updates status as it runs 
     -- script message shows Park, range of species downloading, and how many geographic runs are being made for that set of species, for example:
     start FOVA Abelmoschus esculentus : Aponogeton distachyos  geog runs 1 
     --iNatTools function message shows number of records from iNaturalist:
     3922 records fetched
     -- script message also shows radii analyzed:
      done with download of FOVA Abelmoschus esculentus : Aponogeton distachyos 1 
      start radius  10 ... done with radius 10 
      start radius  25 ... done with radius 25 
      start radius  50 ... done with radius 50 
      start radius  100 ... done with radius 100 
       ... done with all radii 
  -- success of script indicated by message:
  step 3 script done successfully. additional species observations may need downloading by rerunning this script.
  --rerun the script until all species observations are downloaded, indicated by message:
  step 3 script done successfully. all species observations downloaded
  
inat-nps-download-step4-mergeParks.R
  --merges data from various initial download files and combines information on all of them, but not all the data, as it's often too big
  -- status of data files reported, though some species sets will not have any data and may not have a data file
    exists:  data/FOVA_1_radii.csv 
       1  files ok
  -- success indicated by message:
  step 4 script finished successfully.  See files:  
     FOVA_radii_all_data_wider.csv 
     FOVA_radii_all_data.csv 
  
inat-nps-download-step5-compileLists.R
  --compiles watch lists
  -- success indicated by message:
  step 5 script done with  14  data rows see file 
     park_watch_list.csv 
  
inat-nps-download-step6-makemaps.R
  --creates maps of the watch list species in the 'maps' subfolder
  --uses additional libraries in R that are NOT installed in step 1 as some users may not want to make maps, just lists
  -- success indicated by message:
    step 6 done creating maps.  Check "maps" subfolder to see if they were created correctly.  

usda_exotic_status_x_SN.csv
  -- file for defining plants as introduced/exotic from USDA Plants
