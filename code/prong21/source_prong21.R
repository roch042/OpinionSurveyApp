require(tidyverse)

load("data/prong21/TAGS_PronghornData.RData")
load("data/prong21/MHR_PronghornData.RData")

# read in pronghorn data ####
DAT <- readr::read_csv("data/prong21/data_idfg_ph_pronghorn_WORK.csv",col_types=cols(.default="c"))

source("code/prong21/pronghorn_survey_functions.R")

# read in pronghorn question translation ####
dt_translation <- readr::read_csv("data/prong21/cdbk_idfg_ph_2021_EAR.csv") %>%
  dplyr::mutate(ValuePlot = dplyr::recode(Value,
    "No, I will hunt pronghorn with any weapon allowed by a tag"="No",
    "Habitat loss (e.g., conversion to annual grasses or noxious weeds)"="Habitat loss",
    "Water hole locations (e.g., distribution)"="Water hole locations",
    "Connectivity (e.g., migration routes)"="Connectivity",
    "Competition with grazing (e.g., livestock)"="Competition with grazing",
    "Water availability (e.g., drought)"="Water availability",
    "A mix of public and private land"="A mix",
    "Crowding is not an issue"="Not an issue",
    "Non-hunters that use the same areas"="Non-hunters",
    "Personal issues (e.g., physical or medical issues)"="Personal issues",
    "Competition for specific hunting locations (e.g., water holes)"="Competition for specific hunting locations",
    "A few (maybe 1 or 2)"="A few",
    "Several (more than 2)"="Several",
    "Black (or African American)"="Black",
    "Hispanic (or Latino)"="Hispanic",
    "Indigenous (Native American or Native Alaskan)"="Indigenous",
    "Native Hawiian (or other Pacific Islander)"="Native Hawiian",
    "White (or Caucasian)"="White"
  ))


# which year were you born is screwed up
# age by category is screwd up 
# idaho resident year
# residency in years

# create pronghorn spatial groups ####
dt_pronghorn_spatial_groups <- dplyr::tribble(
  ~SpatialGroup, ~Unit,
  "South Hills", "54",
  "Raft River", "55",
  "Raft River", "57",
  "Island Park", "60",
  "Island Park", "60A",
  "Island Park", "61",
  "Island Park", "63A",
  "Medicine Lodge", "59",
  "Medicine Lodge", "59A",
  "Birch Creek", "30A",
  "Birch Creek", "58",
  "Lost Pahsimeroi", "37",
  "Lost Pahsimeroi", "37A",
  "Lost Pahsimeroi", "51",
  "Lemhi", "21A",
  "Lemhi", "29",
  "Lemhi", "30",
  "Morgan / Moyer", "28",
  "Morgan / Moyer", "36B",
  "Sawtooth / Spar", "36",
  "Sawtooth / Spar", "36A",
  "Pioneer", "49",
  "Pioneer", "50",
  "Mud Lake", "63",
  "Big Desert", "68",
  "Big Desert", "68A",
  "Bear Lake", "76",
  "Owyhee", "40",
  "Owyhee", "41",
  "Owyhee", "42",
  "Jarbidge", "46",
  "Jarbidge", "47",
  "Camas", "44",
  "Camas", "45",
  "Camas", "48",
  "Camas", "52",
  "Craters", "52A",
  "Craters", "53"
)

# pull out the DAT_Unit
DAT_Unit <- DAT %>%
  dplyr::select(ID_IDFG, Q9_1_Unit, Q9_2_Unit, Q9_3_Unit) %>%
  tidyr::pivot_longer(cols = c(Q9_1_Unit, Q9_2_Unit, Q9_3_Unit), names_to = "Original_Q", values_to = "Q9_Unit") %>%
  dplyr::mutate(Q9_Unit = as.character(Q9_Unit)) %>%
  dplyr::filter(!is.na(Q9_Unit)) %>%
  dplyr::left_join(dt_translation[,c("Name","Code","Value")], by = c("Original_Q"="Name","Q9_Unit"="Code")) %>%
  dplyr::select(-Q9_Unit) %>%
  dplyr::rename(Q9_Unit = Value) %>%
  dplyr::left_join(dt_pronghorn_spatial_groups,by=c("Q9_Unit"="Unit"))

# create a dataset with out the unit questions
DAT_NoUnit <- DAT %>%
  dplyr::select(-c(Q9_1_Unit, Q9_2_Unit, Q9_3_Unit))

# create an MHR Unit
DAT_MHRUnit <- RECENT_MHR %>%
  dplyr::select(DOCUMENT_YEAR, SPORTSMAN_ID, HUNT_METHOD_UNIT) %>%
  dplyr::rename(MHR_YEAR = DOCUMENT_YEAR,
                ID_IDFG = SPORTSMAN_ID,
                MHR_UNIT = HUNT_METHOD_UNIT) %>%
  dplyr::left_join(dt_pronghorn_spatial_groups,by=c("MHR_UNIT"="Unit"))

dt_summary_group_choices <- dt_translation %>%
  dplyr::select(Name, Label, Question_Number) %>%
  dplyr::distinct() %>%
  dplyr::filter(!Name %in% c("ID_Qualtrics","ID_IDFG") & !grepl("qnr",Name) & !grepl("SD",Name)) %>%
  # dplyr::mutate(Question_Number = unlist(purrr::map(strsplit(Name,split="_"),1))) %>%
  # dplyr::mutate(Question_Number = ifelse(grepl("Q",Question_Number),Question_Number,NA)) %>%
  dplyr::mutate(NewLabel = ifelse(!is.na(Question_Number), paste0("(",Question_Number,") ",Label), Label))

Q_means_wt <- c("Q3","Q4","Q5","Q7","Q17","Q18","Q19","Q20","Q21","Q24","Q25","Q27")
Q_means <- c("Q11","Q14","Q15","Age_Years","Residency","Residency_Start")

# need to think about addin gjust one group
# source("code/prong21/pronghorn_survey_functions.R")

# dt_translation = dt_translation
# dat = DAT_NoUnit
# group1 = "Q1A_Years_Applied" # "Q4_Satf_1_Overall", "Q11A_1_Land_Public" # the thing to take the mean of has to be first, can't filter
# group1_filter = NA
# group2 = "Q11A_1_Land_Public"
# group2_filter = NA
# group3 = NA
# group3_filter = NA
# DAT_Unit = DAT_Unit
# DAT_MHRUnit = DAT_MHRUnit
# Q_means = Q_means
# calc_mean = FALSE


#"Q11A_1_Land_Public" not working as secondary group, "Q14","Q15","which year born","age in years"
# xstar <- fnc_dat_trans_prong21(
#   dt_translation = dt_translation,
#   dat = dat,
#   group1 = group1,
#   group1_filter = group1_filter,
#   group2 = group2,
#   group2_filter = group2_filter,
#   group3 = group3,
#   group3_filter = group3_filter,
#   DAT_Unit = DAT_Unit,
#   DAT_MHRUnit = DAT_MHRUnit,
#   Q_means = Q_means,
#   Q_means_wt = Q_means_wt,
#   calc_mean = TRUE
# )
# 
# bar_border_color <- "Black"
# flt_palette <- "Blues"
# axis_text_size <- 14
# axis_title_size <- 16
# legend_text_size <- 14
# legend_title_size <- 16
# label_text_size <- 4
# bar_fill_color <- RColorBrewer::brewer.pal(3, flt_palette)[2]
# error_bar <- TRUE
# spinner_color <- "black"
# stacked <- FALSE
# all_x <- TRUE
# response <- xstar$response
# response_sd <- xstar$response_sd
# dat <- xstar$dat_plot_trans
# 
# fnc_plot_prong21(
#   dat = xstar$dat_plot_trans,
#   response =xstar$response,
#   response_sd = xstar$response_sd,
#   stacked = stacked,
#   all_x = all_x,
#   bar_border_color = bar_border_color,
#   flt_palette = flt_palette,
#   axis_text_size = axis_text_size,
#   axis_title_size = axis_title_size,
#   legend_text_size = legend_text_size,
#   legend_title_size = legend_title_size,
#   label_text_size = label_text_size,
#   bar_fill_color = bar_fill_color
# )
