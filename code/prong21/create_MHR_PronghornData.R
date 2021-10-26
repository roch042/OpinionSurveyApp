require(tidyverse)

# read in pronghorn data ####
DAT <- readr::read_csv("data/prong21/data_idfg_ph_pronghorn_WORK.csv")

# add MHR data ####

filepath <- "K:/Wildlife/Shiny_Apps/EAR/MasterButtonApp/robjects_MasterButtonApp/MHR_Formatted"

files <- list.files(filepath)
files_sel <- files[which(grepl("2015|2016|2017|2018|2019|2020",files))]

MDAT <- purrr::map(files_sel,function(x){
  load(paste0(filepath,"/",x))
  X <- MHR$Data %>%
    dplyr::ungroup() %>%
    dplyr::filter(hunt_type == "CONTROLLED PRONGHORN") %>%
    dplyr::filter(SPORTSMAN_ID %in% DAT$ID_IDFG) %>%
    dplyr::mutate_all(.,as.character)
  return(X)
})
MDAT <- dplyr::bind_rows(MDAT)

FLT <- MDAT %>%
  dplyr::select(DOCUMENT_YEAR, SPORTSMAN_ID, unique_tag, HUNT_METHOD, HUNT_METHOD_HUNTED, HUNT_METHOD_UNIT, HARVESTED, HARVEST_METHOD, UNIT_HARVEST) %>%
  dplyr::filter(HUNT_METHOD_HUNTED == "YES") %>%
  dplyr::select(-HUNT_METHOD_HUNTED) %>%
  dplyr::arrange(SPORTSMAN_ID, unique_tag, DOCUMENT_YEAR)

# select most recent year reported for each sportsman
index <- FLT %>%
  dplyr::group_by(SPORTSMAN_ID) %>%
  dplyr::summarise(DOCUMENT_YEAR = max(DOCUMENT_YEAR))

RECENT_MHR <- dplyr::inner_join(FLT, index) # most recent MHR report and units they claimed to hunt in

toSave <- c("MDAT","RECENT_MHR")
save(list=toSave,file="data/prong21/MHR_PronghornData.RData")
