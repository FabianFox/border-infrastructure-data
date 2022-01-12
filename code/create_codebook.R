# Create a codebook for the 'Border Infrastructure Data'

# Load/install packages
### ------------------------------------------------------------------------ ###
if(!require("xfun")) install.packages("xfun")
pkg_attach2("tidyverse", "labelled", "rio", "janitor", "codebook")

# Load data
### ------------------------------------------------------------------------ ###
bid.df <- import("./data/border-infrastructure-data-raw.xlsx",
                       sheet = 1, na = "NA") %>%
  select(1:3, 5:7, 8, 10:12, 18:21, 23:24) %>%
  as_tibble() %>%
  filter(!is.na(typology),
         !(state1 == "ARE" & state2 == "QAT"),        # no shared border (since 1974) 
         !(state1 == "QAT" & state2 == "ARE")) %>%    # https://bit.ly/39EOy4Y
  clean_names() %>%
  distinct(state1, state2, .keep_all = TRUE)

# Recode
### ------------------------------------------------------------------------ ###
bid.df <- bid.df %>%
  mutate(typology = case_when(
    typology == "frontier border" ~ 1,
    typology == "landmark border" ~ 2,
    typology == "checkpoint border" ~ 3,
    typology == "barrier border" ~ 4,
    typology == "fortified border" ~ 5,
    TRUE ~ NA_real_),
    fence = case_when(
      fence == "no" ~ 0,
      fence == "yes" ~ 1,
      TRUE ~ NA_real_),
    wall = case_when(
      wall == "no" ~ 0,
      wall == "yes" ~ 1,
      TRUE ~ NA_real_),
    additional_fortification = case_when(
      additional_fortification == "no" ~ 0,
      additional_fortification %in% c("yes", "barbed wire", "ditches") ~ 1,
      TRUE ~ NA_real_),
    fortification_description = if_else(fortification_description == "no", NA_character_, 
                                        fortification_description),
    landmine = case_when(
      landmine == "no" ~ 0,
      landmine == "light" ~ 1,
      landmine == "medium" ~ 2,
      landmine == "heavy" ~ 3,
      landmine == "massive" ~ 4,
      TRUE ~ NA_real_),
    bcp_infrastructure = case_when(
      bcp_infrastructure == "none" ~ 0,
      bcp_infrastructure == "basic" ~ 1,
      bcp_infrastructure == "extended" ~ 2,
      TRUE ~ NA_real_),
    avdan_2019 = case_when(
      avdan_2019 == "no" ~ 0,
      avdan_2019 == "yes" ~ 1,
      TRUE ~ NA_real_),
    hassner_wittenberg_2015 = case_when(
      hassner_wittenberg_2015 == "no" ~ 0,
      hassner_wittenberg_2015 == "yes" ~ 1,
      TRUE ~ NA_real_),
    jellissen_gottheil_2013 = case_when(
      jellissen_gottheil_2013 == "no" ~ 0,
      jellissen_gottheil_2013 == "yes" ~ 1,
      TRUE ~ NA_real_),
    jones_2012 = case_when(
      jones_2012 == "no" ~ 0,
      jones_2012 == "yes" ~ 1,
      TRUE ~ NA_real_),
    wikipedia = case_when(
      wikipedia == "no" ~ 0,
      wikipedia == "yes" ~ 1,
      TRUE ~ NA_real_),
    linnell_et_al_2016 = case_when(
      linnell_et_al_2016 == "no" ~ 0,
      linnell_et_al_2016 == "yes" ~ 1,
      TRUE ~ NA_real_),
  )

# Add variable and value labels
### ------------------------------------------------------------------------ ###
# state1 and state2
var_label(bid.df$state1) <- "Builder of border infrastructure (iso3c)" 
var_label(bid.df$state2) <- "Affected state (iso3c)"

# typology
var_label(bid.df$typology) <- "Border typology"
val_labels(bid.df$typology) <- c("No-man's-land Border" = 1, "Landmark Border" = 2,
                                "Checkpoint Border" = 3, "Barrier Border" = 4, 
                                "Fortified Border" = 5)

# fence
var_label(bid.df$fence) <- "Has a border fence been erected?"
val_labels(bid.df$fence) <- c("No" = 0, "Yes" = 1)

# wall
var_label(bid.df$wall) <- "Has a border wall been erected?"
val_labels(bid.df$wall) <- c("No" = 0, "Yes" = 1)

# additional_fortification
var_label(bid.df$additional_fortification) <- "Are additional obstacles installed (e.g. ditches, berms, barbed-wire)?"
val_labels(bid.df$additional_fortification) <- c("No" = 0, "Yes" = 1)

# fortification_description
var_label(bid.df$fortification_description) <- "Description of border barrier (if available)"

# landmine
var_label(bid.df$landmine) <- "Antipersonel landmine contamination, Source: Landmine Monitor 2018 (http://www.the-monitor.org/en-gb/reports/2018/landmine-monitor-2018.aspx)"
val_labels(bid.df$landmine) <- c("no" = 0, "light" = 1, "medium" = 2, "heavy" = 3, 
                                 "massive" = 4)

# bcp
var_label(bid.df$bcp) <- "Geocode (Latitude, Longitude) of border crossing point"

# bcp_infrastructure
var_label(bid.df$bcp_infrastructure) <- "Description of infrastructure at the border (using satellite images)."
val_labels(bid.df$bcp_infrastructure) <- c("none" = 0, "basic" = 1, "extended" = 2)

# avdan_2019
var_label(bid.df$avdan_2019) <- "Coded as fenced in Avdan (2019) Visas and Walls. Border Security in the Age of Terrorism, Philadelphia: PENN."
val_labels(bid.df$avdan_2019) <- c("No" = 0, "Yes" = 1)

# hassner_wittenberg_2015
var_label(bid.df$hassner_wittenberg_2015) <- "Coded as fenced in Hassner & Wittenberg (2015), doi:10.1162/ISEC_a_00206"
val_labels(bid.df$hassner_wittenberg_2015) <- c("No" = 0, "Yes" = 1)

# jellissen_gottheil_2013
var_label(bid.df$jellissen_gottheil_2013) <- "Coded as fenced in Jellissen & Gottheil (2013), doi:10.1080/14751798.2013.842707"
val_labels(bid.df$jellissen_gottheil_2013) <- c("No" = 0, "Yes" = 1)

# jones_2012
var_label(bid.df$jones_2012) <- "Coded as fenced in Jones (2012) Border Walls. Security and the War on Terror in the United States, India, and Israel, London: Zed."
val_labels(bid.df$jones_2012) <- c("No" = 0, "Yes" = 1)

# wittenberg_2013 
var_label(bid.df$wikipedia) <- "Coded as fenced in the English Wikipedia entry on border barriers (https://en.wikipedia.org/wiki/Border_barrier)"
val_labels(bid.df$wikipedia) <- c("No" = 0, "Yes" = 1)

# linnell_et_al_2016
var_label(bid.df$linnell_et_al_2016) <- "Coded as fenced in Linnell et al. (2016), doi:10.1371/journal.pbio.1002483"
val_labels(bid.df$linnell_et_al_2016) <- c("No" = 0, "Yes" = 1)

# Add metadata
### ------------------------------------------------------------------------ ###
metadata(bid.df)$name <- "Border Infrastructure Data"
metadata(bid.df)$description <- "A typology of border infrastructure"
metadata(bid.df)$creator <- "Gülzau, Fabian & Steffen Mau (2021)"
metadata(bid.df)$citation <- "Gülzau, F., & Mau, S. (2021). Walls, Barriers, Checkpoints, Landmarks, and 'No-Man's-Land.' A Quantitative Typology of Border Control Infrastructure. Historical Social Research, 46(3), 23-48. doi:10.12759/hsr.46.2021.3.23-48"
metadata(bid.df)$url <- ""
metadata(bid.df)$datePublished <- "2022-01-01"
metadata(bid.df)$temporalCoverage <- "April 2018 and October 2019" 
metadata(bid.df)$spatialCoverage <- "Global" 

# Export
### ------------------------------------------------------------------------ ###
list(".rds", ".dta", ".sav", ".csv", ".xlsx") %>%
  walk(~export(bid.df, file = str_c("./data/border-infrastructure-data", .x)))
