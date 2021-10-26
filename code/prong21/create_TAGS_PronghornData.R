# Connect to the database ####

con1 <- DBI::dbConnect(
  odbc::odbc(),
  driver = "SQL Server",
  database = "IFWIS_Wildlife",
  uid = "ShinyUserInternal", # "ShinyUserInternal",
  pwd = "hurt seven sat pupil", # "hurt seven sat pupil",
  server = "164.165.105.241",
  port = "1433")

source(file.path("K:","Wildlife","Shiny_Apps","EAR","HarvestLookUpApp","rcode","create_HUNT_PLANNER.R"),local=TRUE)
source(file.path("K:","Wildlife","Shiny_Apps","EAR","HarvestLookUpApp","rcode","create_UNITS_BY_AREA.R"),local=TRUE)
HUNT_PLANNER <- create_HUNT_PLANNER(con=con1)
UNITS_BY_AREA <- create_UNITS_BY_AREA(con=con1)

prong_huntplanner <- HUNT_PLANNER %>%
  dplyr::filter(Game == "Pronghorn Antelope" & OpGroupYear %in% c(2015:2020)) %>%
  dplyr::select(OpGroupYear, CHunt, AreaID) %>%
  dplyr::distinct() %>%
  dplyr::left_join(UNITS_BY_AREA[,c("AreaID","Unit")])


# Most recent tag purchase, controlled hunt and intended use area ####
TAGS <- readr::read_csv("data/prong21/_Pronghorn Tag Holders ALL 2015-2020.txt")

index <- TAGS %>%
  dplyr::group_by(SPORTSMAN_ID) %>%
  dplyr::summarise(DOCUMENT_YEAR = max(DOCUMENT_YEAR,na.rm=T))

RECENT_TAGS <- dplyr::inner_join(TAGS,index)

RECENT_TAGS <- RECENT_TAGS %>%
  dplyr::left_join(prong_huntplanner, by = c("DOCUMENT_YEAR"="OpGroupYear", "CH_HUNT_CHOICE_1"="CHunt")) # most recently purchased tag plus possible units for controlled hunt number

toSave <- c("HUNT_PLANNER","RECENT_TAGS","prong_huntplanner","UNITS_BY_AREA")
save(list=toSave,file="data/prong21/TAGS_PronghornData.RData")
