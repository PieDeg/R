
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                                             #
#                       INITIALIZE                         ####
#                                                             #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Clean Memory
rm(list = ls())
gc()
# Source functions and initialize
source("./src/model/initialization.R")
# installPackages() # For new users
loadPackages()
sourceDirectory(here("src","adhoc"))
sourceDirectory(here("src","auxfunctions"))
sourceDirectory(here("src","model"))
sourceDirectory(here("src","preprocessing"))

set_parameters()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                                             #
#                      DATA PROCESSING                     ####
#                                                             #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

# 
# setwd("C:\Users\stisut\Möbius Group\COLRUY2056 - Documents\2. Data collection & analysis\3. Data processing\3. R")
# PathOutbound <- "Mobius_verk_201906"
# 
# reformatDataInLeveringen()
# reformatDataMasterArtikels()
# reformatDataMasterKlanten()
# reformatDataMasterLeveranciers()
# reformatDataOutOrderlijnen()
# reformatDataOutRitten()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#                                                             #
#                      AS IS SITUATION                     ####
#                                                             #
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% 

Finance <- Readfinancial()
