# should output (1) a cleaned sectorization file that can easily be linked to fkt shapefiles 
# and (2) a healthshed file with merged healthsheds + clinic locations 

# set working directory to this file's location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("..")
options(scipen=999)
rm(list=ls())

sect_to_shed <- function(sect_df){
  sect_df %>% 
    filter(!(is.na(fkt_uid) | fkt_uid == "" | is.na(fkt_name) | fkt_name == "")) %>% #pull(fs_uid) %>% n_distinct()
    st_as_sf() %>% 
    group_by(fs_uid) %>% 
    # for each clinic, merge the shapefiles
    summarise(# since some fkt_uid get matched twice, have to take only unique ones
      # before summing the population
      fs_pop = across(c(fkt_uid, fkt_pop_2022)) %>% unique %>%
        pull(fkt_pop_2022) %>% sum(na.rm = T),
      n_fkt_uid = n_distinct(fkt_uid, na.rm = T), 
      n_fkt_instat = n_distinct(fkt_instat_clean, na.rm = T),
      across(c(region_uid:district_name,
               starts_with("fs"),
               n_fkt_instat, n_fkt_uid),
             function(x){unique(na.omit(x))}),
      # recalculate the number of components
      n_components = {if(n_distinct(fkt_instat_clean, na.rm = T) > 0){
        st_relate(geometry[!is.na(fkt_instat_clean)]) %>%
                  as.data.frame() %>% 
                  mutate(across(everything(), 
                                ~paste0(str_sub(.x, 1,2), 
                                        str_sub(.x, 4,5)) %>% 
                                  grepl(pattern = "1|2"))) %>% 
                  as.matrix() %>%
          graph_from_adjacency_matrix %>%
                  components %>% # groups
                  extract2("no")} else{0}},
              geometry = {if(n_distinct(fkt_instat_clean, na.rm = T) > 0){
                st_union(geometry[!is.na(fkt_instat_clean)]) %>% 
                  st_combine %>% 
                  st_make_valid} else {empty_geom}}) %>% 
    rowwise() %>% 
    mutate(n_shape = length(geometry[[1]])) %>%
    ungroup
}

library(tidyverse)
library(magrittr)
library(sf)
library(igraph)
library(tmap)
library(ggpubr)

source("./scripts/0_utility_functions.R")

fkt_shape <- read_sf("./data/raw/mdg_adm_bngrc_ocha_20181031_shp", 
                     layer = "mdg_admbnda_adm4_BNGRC_OCHA_20181031") %>% 
  st_make_valid

# # split Nosy Mitsio from the mainland, its currently part of fkt Antenina MG71717150005, instat 717150005
fkt_shape <- rbind(fkt_shape %>% 
                     filter(ADM4_PCODE != "MG71717150005"), 
                   fkt_shape %>% 
                     filter(ADM4_PCODE == "MG71717150005") %>% 
                     st_cast("POLYGON") %>% 
                     mutate(group = (1:n()) > 1) %>% 
                     summarise(across(!geometry, unique), 
                               geometry = st_union(geometry),
                               .by = group) %>% 
                     select(-group) %>% 
                     mutate(ADM4_EN = ifelse(1:n() == 1, 
                                             ADM4_EN, 
                                             "Nosy Mitsio"), 
                            ADM4_PCODE = paste0(ADM4_PCODE, LETTERS[1:n()])))
# load UN OCHA mapping from BNGRC codes to PCODES
ocha_map <- readxl::read_excel("./data/raw/bngrc_codes_lut_pcodes_2018.xlsx", 
                               sheet = 5) %>% 
  mutate(across(everything(), as.character)) %>% 
  # add Nosy Mitsio as a separate line with a mapping 
  {rbind(filter(., ADM4_PCODE != "MG71717150005"), 
         filter(., ADM4_PCODE == "MG71717150005") %>% 
           mutate(across(c(ADM4_PCODE, ADM4C_BNGRC), 
                         ~paste0(.x, "A"))), 
         filter(., ADM4_PCODE == "MG71717150005") %>% 
           mutate(across(c(ADM4_PCODE, ADM4C_BNGRC), 
                         ~paste0(.x, "B")), 
                  ADM4_EN = "Nosy Mitsio", 
                  ADM4E_BNGRC = "NOSY MITSIO"))}

# load the 2022 sectorization file that Symphonia has annotated with INSTAT codes for the fkt in addtition to the existing UID codes
sect <- read.csv("./data/raw/mapping GESIS DHIS questions - FKT code instat.csv") %>%
  set_colnames(c("region_uid", "region_name", 
                 "district_uid", "district_name", 
                 "commune_uid", "commune_name",
                 "fs_uid", "fs_type", "fs_name", 
                 "f_or_nf", "geometry", 
                 "fkt_uid", "fkt_instat", "fkt_name", 
                 "fkt_pop_2022", "distance_csb_fkt", "transit")) %>% 
  mutate(fkt_instat = ifelse(fkt_instat == "717150005", 
                             "717150005A", 
                             fkt_instat))
# 1) fkt with instat codes duplicated -----
dup_fkt_instat <- read.csv("./data/raw/symphonia_verified/VERIFIER duplicated_fkt_instat_in_sectorisation2022.csv")
# for rows where the NOTE is blank or starts with "2 physically distinct clinics...", we can leave those rows alone in the sectorization
# we'll just assign the fkt to both healthsheds and let them overlap

# for the remaining rows:
# the one with 0 population should just be deleted.
# the ones with with "delete" in one row and "keep that row" in another row, combine the populations and keep the row with "keep" in the note
# should result in loosing 6 rows (21927 --> 21921) but no population 29,113,715
sect_filled <- sect %>% 
  left_join(dup_fkt_instat %>% 
              mutate(fkt_instat = as.character(fkt_instat)) %>%
              select(fkt_uid, fkt_instat, NOTE)) %>% 
  group_by(fkt_instat) %>% 
  mutate(merge_fkt = any(grepl("keep that row", NOTE)) & any(NOTE == "delete"), 
         fkt_pop_2022 = ifelse(merge_fkt, sum(fkt_pop_2022), fkt_pop_2022)) %>%
  ungroup %>% 
  filter(!merge_fkt | (merge_fkt & NOTE != "delete")) %>% 
  select(-merge_fkt, -NOTE)
# drops 6 rows --> 21880 to 21874

# 2) additional fkt matching ----
# pulling the 4 iterations (v1 fuzzy match verified, symphonia's manual match, 
# and v2 fuzzy match verified, v3 fuzzy match verified) together get the full set of matches, accounting for the 19 
# "matches" in the original sect file that aren't actually instat codes
match_check <- read.csv("./data/raw/symphonia_verified/VERIFIER fkt_name_matching_for_sectorisation2022.csv") %>% 
  mutate(NOTE = case_when(fkt_code_instat == 114110001 ~ "no", 
                          fkt_code_instat == 513010005 ~ "no",
                          fkt_code_instat == 204230010 ~ "no",
                          fkt_code_instat == 410050001 ~ "no",
                          fkt_code_instat == 713232001 ~ "no",
                          T ~ NOTE))
manual_match <- readxl::read_excel("./data/raw/symphonia_verified/FKT SECTO UNMATCHED.xlsx") %>% select(-`...1`) %>% 
  filter(fkt_code_instat != "305170005")
manual2 <- read.csv("./data/raw/geolocation/manual_fkt_match.csv") %>% 
  mutate(across(starts_with("match"), trimws)) %>% 
  select(-note_reason, -X) %>% unique 
match_check_v2 <- read.csv("./data/raw/symphonia_verified/VERIFIER fkt_name_matching_for_sectorisation2022_v2.csv")
match_check_v3 <- read.csv("./data/raw/symphonia_verified/VERIFIER fkt_name_matching_for_sectorisation2022_v3.csv")   

sect_filled %<>% #pull(fkt_instat) %>% n_distinct(na.rm = T) # 13687
  # remove fkt codes that aren't in the instat/ocha shapefile
  mutate(fkt_instat_clean = ifelse(fkt_instat %in% ocha_map$ADM4C_BNGRC, 
                                   fkt_instat, 
                                   NA)) %>% 
  # pull(fkt_instat_clean) %>% n_distinct(na.rm = T) # 13687 - 19 = 13668 ocha instat codes
  # add the confirmed matches from the first fuzzy string matching
  left_join(match_check %>% 
              filter(NOTE == "yes") %>% 
              select(fkt_uid, match1 = fkt_code_instat),
            by = "fkt_uid",
            multiple = "all") %>% # nrow
  # this left join adds 30 nrow because some fkt_uid got matched to multiple instat codes 
  # 21874 --> 21904 
  mutate(fkt_instat_clean = ifelse(is.na(fkt_instat_clean), 
                                   match1, 
                                   fkt_instat_clean)) %>% 
  # pull(fkt_instat_clean) %>% n_distinct(na.rm = T) # + 2536 rows (4 instat fkt are dupliated) --> 162000 instat/ocha codes
  # matches from Symphonia's manual matcing 
  left_join(manual_match %>% 
              mutate(check = as.numeric(fkt_code_instat)) %>%  
              filter(!is.na(check)) %>% 
              select(fkt_uid, match2 = fkt_code_instat)) %>% 
  mutate(fkt_instat_clean = ifelse(is.na(fkt_instat_clean), 
                                   match2, 
                                   fkt_instat_clean)) %>% 
  # pull(fkt_instat_clean) %>% n_distinct(na.rm = T)  # + 159 rows  --> 16359 instat/ocha codes match
  # confirmed matches from fuzzy string matching v2 
  left_join(match_check_v2 %>% 
              filter(NOTE == "yes") %>% 
              select(fkt_uid, match3 = fkt_code_instat),
            multiple = "all") %>% # nrow
  # this left join adds 14 nrow because some fkt_uid got matched to multiple instat codes 
  # 21904 --> 21918 
  mutate(fkt_instat_clean = ifelse(is.na(fkt_instat_clean), 
                                   match3, 
                                   fkt_instat_clean)) %>% 
  # pull(fkt_instat_clean) %>% n_distinct(na.rm = T)  # + 722 rows (6 instat codes matched to 2 uid) --> 17075 instat codes matched
  # confirmed matches from fuzzy string matching v3
  left_join(match_check_v3 %>% 
              filter(note == "yes") %>% 
              select(fkt_uid, match4 = fkt_code_instat),
            multiple = "all") %>% # nrow
  # this left join adds 2 nrow because some fkt_uid got matched to multiple instat codes
  # 21918 --> 21920
  mutate(fkt_instat_clean = ifelse(is.na(fkt_instat_clean), 
                                   match4, 
                                   fkt_instat_clean)) %>% 
  # pull(fkt_instat_clean) %>% n_distinct(na.rm = T)  # + 134 rows (6 instat codes matched to 2 uid) --> 17203 (of 17465 instat fkt matched)
  left_join(manual2 %>%
              filter(match_type == "fkt") %>% 
              select(match5 = fkt_instat_code, 
                     fkt_uid = match_uid)) %>% 
  mutate(fkt_instat_clean = ifelse(is.na(fkt_instat_clean), 
                                   match5, 
                                   fkt_instat_clean)) %>% 
  # pull(fkt_instat_clean) %>% n_distinct(na.rm = T)  # --> 17264 (of 17465 instat fkt matched)
  rename(fkt_instat_orig = fkt_instat, 
         fs_ll = geometry) %>% 
  mutate(match_type = case_when(fkt_instat_clean == fkt_instat_orig ~ "original", 
                                fkt_instat_clean == match1 ~ "fuzzy1",
                                fkt_instat_clean == match2 ~ "manual",
                                fkt_instat_clean == match3 ~ "fuzzy2",
                                fkt_instat_clean == match4 ~ "fuzzy3",
                                fkt_instat_clean == match5 ~ "manual2")) %>% 
  select(-(match1:match5)) 
# now 21920 rows x 19 col

# add on the manual matches where we match to clinic not fkt
sect_filled %<>% full_join(manual2 %>%
                             filter(match_type != "fkt") %>% 
                             transmute(fkt_instat_clean = as.character(fkt_instat_code), 
                                       fkt_name = fkt_ocha_name,
                                       match_type = "manual2",
                                       fs_uid = match_uid, 
                                       fkt_uid = "none"), 
                           by = c("fs_uid", "fkt_uid", "fkt_instat_clean", 
                                  "fkt_name",
                                  "match_type"))
# 21933 rows

# 3) join in the shapefiles ----
sect_filled %<>%  
  left_join(ocha_map %>% select(ADM4C_BNGRC, ADM4_PCODE), 
            by = c("fkt_instat_clean" = "ADM4C_BNGRC")) %>%
  left_join(fkt_shape %>% select(ADM4_PCODE))


# 4) check the geographic contiguity ----
# if there is one fkt thats not adjacent to the others, but the others are all 
# contiguous, drop that fkt (for now)
sect_filled %<>%
  group_by(fs_uid) %>% 
  mutate(n_fkt_instat = n_distinct(fkt_instat_clean, na.rm = T), 
         n_adj_fkt = {if(first(n_fkt_instat) == 0) {NA} else{
           st_relate(geometry) %>%
             as.data.frame() %>% 
             mutate(across(everything(), 
                           ~paste0(str_sub(.x, 1,2), 
                                   str_sub(.x, 4,5)) %>% 
                             grepl(pattern = "1|2"))) %>% 
             as.matrix() %>%
             rowSums() %>% 
             subtract(1)}}, 
         n_components = {if(first(n_fkt_instat) == 0) {NA} else{
           st_relate(geometry[!is.na(ADM4_PCODE)]) %>%
             as.data.frame() %>% 
             mutate(across(everything(), 
                           ~paste0(str_sub(.x, 1,2), 
                                   str_sub(.x, 4,5)) %>% 
                             grepl(pattern = "1|2"))) %>% 
             as.matrix() %>%
             graph_from_adjacency_matrix %>%
             components %>% # groups
             extract2("no")}}, 
         drop_solo = (sum(n_adj_fkt == 0) == 1) & 
           (sum(n_adj_fkt > 0) > 1) & 
           n_components == 2)
empty_geom <- sect_filled %>% filter(is.na(fkt_instat_clean)) %>% pull(geometry) %>% first

# drop the fkt match if removing it would leave the rest of the fkt in the 
# healthshed contiguous 
sect_filled %<>%
  ungroup %>% 
  mutate(fkt_instat_matched = fkt_instat_clean,
         fkt_instat_clean = case_when(drop_solo & n_adj_fkt == 0 & match_type != "manual2" ~ NA, 
                                        T ~ fkt_instat_clean),
         ADM4_PCODE = case_when(drop_solo & n_adj_fkt == 0 & match_type != "manual2" ~ NA, 
                                T ~ ADM4_PCODE)) %>% 
  # drop, then rejoin the geometry to remove the geometries for the ones you want to unmatch
  st_drop_geometry() %>% 
  left_join(fkt_shape %>% select(ADM4_PCODE)) 

# old code that doesn't work anymore
# geometry = case_when(drop_solo & n_adj_fkt == 0 & match_type != "manual2" ~ c(empty_geom), 
#                      T ~ c(geometry))) 

sect_filled %>% pull(fkt_instat_clean) %>% n_distinct
# currently 17184 assigned fkt instat codes 
ocha_map %>% filter(ADM4C_BNGRC %in% sect_filled$fkt_instat_clean == FALSE) %>% n_distinct()
# 283 unassigned 

# 5) merge into healthsheds ----
sheds <- sect_filled %>% sect_to_shed

# v1 state ----
sheds %>% 
  st_drop_geometry %>% 
  transmute(n_comp = case_when(n_components > 1 ~ ">1", 
                               n_components == 1 ~ "1", 
                               n_components < 1 ~ "<1"),
            mult_shape = n_shape > 1) %>% 
  table
# 2773 healthsheds (7 clinics didn't have any matched fkt)
# 66 have multiple components 
# 50 more have multiple shapes in the geometry but 1 component 

unmatched_fkt <- fkt_shape %>% 
  filter(ADM4_PCODE %in% sect_filled$ADM4_PCODE == FALSE) 
unmatched_fkt %>% nrow
# 283

# 6) 2018 sect matches ---- 
unmatched_fkt <- fkt_shape %>% 
  filter(ADM4_PCODE %in% sect_filled$ADM4_PCODE == FALSE) %>% 
  left_join(ocha_map %>% select(ADM4_PCODE, ADM4C_BNGRC))

sect2018 <- readxl::read_excel("./data/raw/sectorisation 2018.xlsx", 
                               skip = 2) %>% 
  rename_with(function(x){gsub(" ", "_", x)}, everything()) %>% 
  mutate(across(everything(), as.character)) %>% 
  mutate(row_id = 1:n())  %>% 
  select(-(`Population_totale_année_2018/FKT`:`0.076`))

mapping <- read.csv("./data/intermediate/mapping_gesis_dhis2_clean.csv")

matches2018 <- sect2018 %>% 
  # add clinic UID to the 2018 sect
  left_join(mapping %>% 
              filter(cAnnee == "2018") %>% 
              transmute(code_fs = as.character(code_fs),
                        fs_uid),
            by = c("Code_GESIS" = "code_fs")) %>%
  # join in the unmatched fkt so we can limit to those 
  left_join(unmatched_fkt %>% 
              left_join(ocha_map %>% 
                          select(ADM4_PCODE, ADM4C_BNGRC)) %>% 
              select(ADM4C_BNGRC, ADM4_EN, ADM2_EN, ADM1_EN), 
            by = c("Code_Fkt_BNGRC" = "ADM4C_BNGRC")) %>% 
  # limit to unmatched BNGRC codes
  filter(!is.na(ADM4_EN)) %>% 
  select(-(`Statut_(Pub_ou_Privé)`:`Accessibilté\n(nb_mois_par_an)`)) %>% 
  rename(fkt_geometry = geometry) %>% 
  # add on the fs geometry so we can plot them together
  left_join(sheds %>% rename(shed_geometry = geometry)) 

matches2018 %<>% 
  rowwise %>% 
  mutate(relate_str = st_relate(fkt_geometry, shed_geometry), 
         fkt_adj = relate_str %>% 
           {paste0(str_sub(., 1,2), 
                   str_sub(., 4,5))} %>% 
           grepl(pattern = "1|2")) %>% 
  ungroup %>% 
  # also add on information about which of the fs aren't in the healsheds geom 
  mutate(no_shed = fs_uid %in% sheds$fs_uid == FALSE) 

matches2018_in2022 <- matches2018 %>% 
  filter(fkt_adj | no_shed) %>% 
  filter(!is.na(fs_uid)) %>% #pull(fs_uid) %>% n_distinct 56 potential matches
  select(fs_uid, Code_GESIS, ends_with("geometry"), no_shed, n_components,
         Nom_Fokontany_par_CSB, Code_Fkt_BNGRC) %>% 
  distinct %>% 
  # join information about which have a close-ish string match in that fs_uid in 2022 sect
  {left_join(., 
             grouped_stringdist_fulljoin(select(., fs_uid, Nom_Fokontany_par_CSB),
                                         sect_filled %>% 
                                           filter(is.na(fkt_instat_clean)) %>%
                                           select(fkt_instat_clean, fs_uid, fkt_uid, 
                                                  fkt_name2022 = fkt_name,
                                                  commune_name, district_name) %>% 
                                           mutate(sect2022 = T), 
                                         "fs_uid", "fs_uid", 
                                         "Nom_Fokontany_par_CSB", "fkt_name2022", 
                                         select_match = (function(df) {group_by(df, df1_id) %>% 
                                             filter(dist == min(dist))})) %>% 
               filter(dist <= 0.11) %>% 
               mutate(good_match = T))} 

matches2018_in2022 %>% 
  pivot_longer(ends_with("geometry"), 
               names_to = "type", 
               values_to = "geometry") %>% 
  mutate(type = gsub("_geometry", "", type), 
         type = ifelse(type == "fkt" & !is.na(good_match), "good", type)) %>% 
  st_as_sf() %>%   
  filter(!st_is_empty(geometry)) %>% 
  {tmap::tm_shape(.) +
  tmap::tm_polygons('type', legend.show = FALSE,
                    palette = c("tomato2", "palegreen4", "grey")) +
  tmap::tm_facets('fs_uid', nrow = 7, ncol = 8)} %>% 
  tmap::tmap_save("./scratch/match2018_fkt_fs.png", 
            height = 21, width = 16, units = "in")

# matches2018_in2022 %>% pull(good_match) %>% is.na() %>% not %>% table
# we got 54 matches from this, 37 seemed reasonable except for 
# having a counterpart in the 2022 sect (or geographic contiguity)

# 7) update sect filled with new matches ----
sect_filled_v2 <- sect_filled %>% 
  left_join(matches2018_in2022 %>% 
              # filter(fs_uid %in% c("MrABDSyBJVd", "MXZMrSow5Oo", 
              #                      "Z0ywXaDzPRm", "JSWV4zCHVlg") == FALSE) %>% 
              filter(!is.na(good_match)) %>% 
              select(fkt_uid, 
                     match_sect2018 = Code_Fkt_BNGRC), 
            by = "fkt_uid") %>% 
  mutate(fkt_instat_clean = ifelse(is.na(fkt_instat_clean), 
                                   match_sect2018, 
                                   fkt_instat_clean), 
         match_type = ifelse(!is.na(match_sect2018), 
                             "sect2018", 
                             match_type))  

# 8) manual v2 and v3 ---- 
manual3 <- read.csv("./data/raw/geolocation/manual_fkt_match_v2.csv") %>% 
  unique
manual4 <- read.csv("./data/raw/geolocation/manual_fkt_match_v3.csv")
unmatch <- read.csv("./data/raw/geolocation/unmatch_fkt.csv")

sect_filled_v2 <- sect_filled_v2 %>% 
  left_join(unmatch %>% 
              transmute(fkt_instat_clean = as.character(fkt_instat_code), 
                        fkt_uid,
                        unmatch = T)) %>% 
  mutate(fkt_instat_clean = case_when(unmatch == T ~ NA,
                                      T ~ fkt_instat_clean)) %>% 
  left_join(manual3 %>% filter(match_type == "fkt") %>% 
              select(match6 = fkt_instat_code, 
                     fkt_uid = match_uid)) %>% 
  mutate(fkt_instat_clean = ifelse(is.na(fkt_instat_clean), 
                                   match6, 
                                   fkt_instat_clean)) %>% 
  left_join(manual4 %>% filter(match_type == "fkt") %>% 
              select(match7 = fkt_instat_code, 
                     fkt_uid = match_uid)) %>% 
  mutate(fkt_instat_clean = ifelse(is.na(fkt_instat_clean), 
                                   match7, 
                                   fkt_instat_clean),
         match_type = case_when(fkt_instat_clean == match6 ~ "manual3",
                                fkt_instat_clean == match7 ~ "manual4",
                                T ~ match_type))  %>% 
  full_join(manual3 %>%
              filter(match_type != "fkt") %>% 
              transmute(fkt_instat_clean = as.character(fkt_instat_code), 
                        fkt_name = "-",
                        match_type = "manual3",
                        fs_uid = match_uid, 
                        fkt_uid = "none"), 
            by = c("fs_uid", "fkt_uid", "fkt_instat_clean", 
                   "fkt_name",
                   "match_type")) %>% 
  full_join(manual4 %>%
              filter(match_type != "fkt") %>% 
              transmute(fkt_instat_clean = as.character(fkt_instat_code), 
                        fkt_name = "-",
                        match_type = "manual4",
                        fs_uid = match_uid, 
                        fkt_uid = "none"), 
            by = c("fs_uid", "fkt_uid", "fkt_instat_clean", 
                   "fkt_name",
                   "match_type"))

sect_filled_v2 %<>% 
  select(-c(ADM4_PCODE, geometry)) %>% 
  left_join(ocha_map %>% select(ADM4C_BNGRC, ADM4_PCODE), 
            by = c("fkt_instat_clean" = "ADM4C_BNGRC")) %>%
  left_join(fkt_shape %>% select(ADM4_PCODE))

sheds_v2 <- sect_filled_v2 %>% sect_to_shed

# v2 state ----
sheds_v2 %>% 
  st_drop_geometry %>% 
  transmute(n_comp = case_when(n_components > 1 ~ ">1", 
                               n_components == 1 ~ "1", 
                               n_components < 1 ~ "<1"),
            mult_shape = n_shape > 1) %>% 
  table
# 2773 healthsheds (7 clinics didn't have any matched fkt)
# 53 have multiple components 
# 40 more have multiple shapes in the geometry but 1 component 


unmatched_fkt2 <- fkt_shape %>% 
  filter(ADM4_PCODE %in% sect_filled_v2$ADM4_PCODE == FALSE) 
unmatched_fkt2 %>% nrow
# 131

# 10) assign remainder by commune ----
# if there's only 1 fs in that commune, assign it there even if its not in the sect?
district_map <- readRDS("./data/raw/geolocation/sect_ocha_district_match.rds")
fkt_to_fs_by_commune <- unmatched_fkt2 %>% 
  left_join(district_map %>% select(ADM2_PCODE, district_uid)) %>% 
  grouped_stringdist_fulljoin(sect_filled_v2 %>% 
                                filter(!is.na(fkt_name) & fkt_name != "") %>% 
                                group_by(commune_uid) %>% 
                                summarise(n_fs = n_distinct(fs_uid, na.rm = T), 
                                          across(starts_with("fs"), 
                                                 first),
                                          across(c(region_uid:district_name, 
                                                   commune_name), 
                                                 unique)), 
                              df1_group = "district_uid", 
                              df2_group = "district_uid",
                              df1_stringmatch = "ADM3_EN", 
                              df2_stringmatch = "commune_name") %>% 
  group_by(df1_id) %>% 
  filter(dist == min(dist)) %>% 
  filter(n_fs == 1) %>% 
  filter(dist < 0.075) 

# lets look at what this would assign
# panel for each fs, plot the original shed + new fkt to be added
by_commune_adj <- fkt_to_fs_by_commune %>% 
  ungroup %>% 
  select(fkt_code = ADM4_PCODE,
         fkt_geometry = geometry, fs_uid) %>% 
  left_join(sheds_v2 %>% select(fs_uid, geometry, n_components)) %>% 
  rename(fs_geometry = geometry) %>% 
  pivot_longer(ends_with("geometry"), 
               names_to = "type",
               values_to = "geometry") %>% 
  mutate(type = gsub("_geometry", "", type)) %>% 
  select(-fkt_code) %>% 
  unique %>% 
  group_by(fs_uid) %>% 
  mutate(n_comp_new = st_relate(geometry) %>%
           as.data.frame() %>% 
           mutate(across(everything(), 
                         ~paste0(str_sub(.x, 1,2), 
                                 str_sub(.x, 4,5)) %>% 
                           grepl(pattern = "1|2"))) %>% 
           as.matrix() %>%
           graph_from_adjacency_matrix %>%
           components %>% # groups
           extract2("no"), 
         type = ifelse(n_comp_new > n_components & type == "fkt", "bad", type)) 

by_commune_adj %>% 
  st_as_sf() %>% 
  {tmap::tm_shape(.) +
      tmap::tm_polygons('type', legend.show = FALSE,
                        palette = c("deepskyblue3", "grey")) +
      tmap::tm_facets('fs_uid', nrow = 16, ncol = 5)} %>% 
  tmap::tmap_save("./scratch/geolocation/match_fkt_fs_commune.png", 
                  height = 48, width = 10, units = "in")

sect_filled_v3 <- sect_filled_v2 %>% 
  full_join(fkt_to_fs_by_commune %>% 
              ungroup %>% 
              left_join(by_commune_adj %>% 
                          transmute(fs_uid, 
                                    keep = (n_comp_new <= n_components)) %>% 
                          unique) %>% 
              filter(keep) %>% 
              mutate(fkt_uid = "none",
                     match_type = "commune") %>% 
              left_join(ocha_map %>% select(ADM4_PCODE, ADM4C_BNGRC)) %>% 
              select(starts_with(c("region", "district", "commune", "fs")),
                     match_type, 
                     fkt_uid, fkt_name = ADM4_EN, 
                     fkt_instat_clean = ADM4C_BNGRC)) 
# should just add rows because there's nothing to join to --> 22027 + 117 = 22144

# also re-fill in the ADM4_PCODE and fkt geometry for everything
sect_filled_v3 %<>% 
  select(-c(ADM4_PCODE, geometry, n_components)) %>% 
  left_join(ocha_map %>% select(ADM4C_BNGRC, ADM4_PCODE), 
            by = c("fkt_instat_clean" = "ADM4C_BNGRC")) %>%
  left_join(fkt_shape %>% select(ADM4_PCODE))

# update sheds
sheds_v3 <- sect_filled_v3 %>% sect_to_shed()

# v3 state ----
sheds_v3 %>% 
  st_drop_geometry %>% 
  transmute(n_comp = case_when(n_components > 1 ~ ">1", 
                               n_components == 1 ~ "1", 
                               n_components < 1 ~ "<1"),
            mult_shape = n_shape > 1) %>% 
  table
# 2773 healthsheds (7 clinics didn't have any matched fkt)
# 48 have multiple components 
# 33 more have multiple shapes in the geometry but 1 component 


unmatched_fkt3 <- fkt_shape %>% 
  filter(ADM4_PCODE %in% sect_filled_v3$ADM4_PCODE == FALSE) 
unmatched_fkt3 %>% nrow
# 14 

# 11) updates from symphonia ---- 
noncontig_fixes <- readxl::read_excel("./data/raw/from Symphonia/Verification secto vs ocha.xlsx", 
                                      sheet = "SECTO 2022", col_types = "text") %>% 
# for the fixes, grab the fkt_uid and fs_uid, then merge in the relevant info for the fkt and fs from the sect filled,
# and finally, remove the lines with those fkt from the sect_filled_v3 and add these in
  filter(note...21 == "noncontig_healthsheds") %>% 
  # one fkt gets incorrectly moved to a different fs, just drop that row 
  filter(fkt_uid != "kJgR66aF9g6") %>% 
  # one fix we discussed didn't get added to the xlsx file, so lets fix it manually here 
  mutate(fs_uid = case_when(fkt_uid == "NWtrDDpi5n0" ~ "rmlBT17TlZ3", 
                            T ~ fs_uid)) %>% 
  select(fs_uid, fkt_uid, fkt_name = fkt_nm, fkt_instat = fkt_instat...15) %>% 
  # lets also manually move Marovaka to Marovovonana bc its two neighbors moved to that fs 
  rbind(data.frame(fs_uid = "xt1C9F1Flqu", 
                   fkt_uid = "XwLQg8znk5g", 
                   fkt_name = "Marovaka", 
                   fkt_instat = "303090008")) %>% 
  # accented e in fkt names got converted to ? somewhere in the line, fix it 
  mutate(fkt_name = gsub("\\?", "é", fkt_name)) %>% 
  # add on the fs information 
  left_join(sect_filled_v3 %>% 
              filter(!is.na(f_or_nf)) %>% 
              select(region_uid:fs_ll) %>% 
              distinct) %>% 
  # add on the fkt information 
  left_join(sect_filled %>% 
              select(fkt_uid, fkt_instat_orig:transit)) %>% 
  rename(fkt_instat_clean = fkt_instat) %>% 
  mutate(match_type = "noncontig_fix") 

# drop old columns from sect_filled_v3, and remove rows with fkt_uid from above
sect_filled_v4 <- sect_filled_v3 %>% 
  select(-c(n_fkt_instat, n_adj_fkt, drop_solo, unmatch, match6, match7, 
            fkt_instat_matched, match_sect2018, ADM4_PCODE, geometry)) %>% 
  # join in the noncontig fix fkt to know which rows to drop
  left_join(noncontig_fixes %>% 
              select(fkt_uid, fkt_name) %>% 
              mutate(drop = T)) %>% 
  filter(is.na(drop)) %>%  select(-drop) %>% 
  rbind(., noncontig_fixes %>% select(all_of(colnames(.)))) 

# also match the unmatch fkt from symphonia,
unmatched_fkt_fixes <- readxl::read_excel("./data/raw/from Symphonia/Verification secto vs ocha.xlsx", 
                                      sheet = "SECTO 2022", col_types = "text") %>%  
  filter(note...20 == "unmatch FKT") %>% 
  select(fs_uid, fkt_uid, fkt_name = fkt_nm, 
         fkt_ocha_code = fkt_instat...18, 
         fkt_ocha_name = instat_nm...19) %>% 
  left_join(ocha_map %>% select(fkt_ocha_code = ADM4_PCODE, fkt_instat_match = ADM4C_BNGRC)) %>% 
  mutate(row_id = 1:n())

sect_filled_v4 %<>% 
  full_join(unmatched_fkt_fixes) %>% 
  mutate(match_type = ifelse(!is.na(row_id), "unmatched_fkt_fix", match_type), 
         fkt_instat_clean = ifelse(!is.na(row_id),  fkt_instat_match, fkt_instat_clean)) %>% 
  select(-row_id)

# handle last few duplicate/instat code fixes from symphonia 
misc_fixes <- readxl::read_excel("./data/raw/from Symphonia/Verification secto vs ocha.xlsx", 
                   sheet = "SECTO 2022", col_types = "text") %>%  
  filter(note...20 %in% c("duplicate", "DUPLICATE", "New code instat", "INSTAT CODE FKT UPDATE") | 
           grepl("^\\d", fkt_instat...18)) 

sect_filled_v4 %<>% 
  left_join(misc_fixes %>% 
              select(fs_uid, fkt_uid, fkt_instat_clean = fkt_instat...15, 
                     fkt_instat_fix = fkt_instat...18, note = note...20) %>% 
              mutate(misc_id = 1:n())) %>% 
  filter(!grepl("duplicate", note, ignore.case = T)) %>% 
  mutate(match_type = ifelse(!is.na(misc_id), "misc_fix", match_type), 
         fkt_instat_clean = ifelse(!is.na(misc_id), fkt_instat_fix, fkt_instat_clean)) %>% 
  select(-misc_id)

# and finally, manual fixes of the coastal islands that are grouped with the wrong villages and therefore the wrong clinics
# not obvious how big an issue these are... depends on population size on the islands
# fkt 713151001 (islands should go with CSB2 Ankarongana)
# fkt 713151004 (islands should go with CSB2 Ankarongana)
# fkt 713232005 (island should go with CSB2 Mangaoka)
# fkt 713254003 (island should go with CSB2 Mahalina)
# fkt 713253004 (island should go with CSB2 Mahalina)
# fkt 713010004 (island should go with CSB2 Mahavanona)
island_fixes <- data.frame(fkt_instat = c("713151001", "713151004", 
                                          "713232005", "713254003", 
                                          "713253004", "713010004"),
                           fs_uid = c("Z0mzcQCqySd", "Z0mzcQCqySd", 
                                      "Zmgnljl5frE", "yPSYALQxVv2",
                                      "yPSYALQxVv2", "wnWoQcrvINJ")) %>% 
  left_join(ocha_map %>% select(fkt_instat = ADM4C_BNGRC, ADM4_PCODE))
# first update the fkt shapes to split the islands off
fkt_shape <- rbind(fkt_shape %>% 
                     filter(ADM4_PCODE %in% c(island_fixes$ADM4_PCODE) == FALSE),
                   fkt_shape %>% 
                     filter(ADM4_PCODE %in% c(island_fixes$ADM4_PCODE)) %>% 
                     mutate(row_id = 1:n()) %>% 
                     st_cast("POLYGON") %>% 
                     mutate(area = st_area(geometry)) %>%
                     mutate(area_rank = rank(-area), # biggest is rank 1, smallest is max rank
                            .by = row_id) %>% 
                     # for groups 1-3, 5 the largest shape is mainland and other islands can go together 
                     # for group 4, smallest island goes separate, larger island can stay with mainland
                     # for 5, i think the land is bigger, its the second element when its cast 
                     mutate(mainland = ifelse(ADM4_PCODE != "MG71713253004", area_rank == 1, area_rank != max(area_rank))) %>% 
                     select(-starts_with("area")) %>% 
                     # merge the mainland and non-mainland pieces 
                     summarise(across(!geometry, unique), 
                               geometry = st_union(geometry),
                               .by = c(row_id, mainland)) %>% 
                     select(-row_id) %>% 
                     # if its the mainland, leave the name and pcode, if its non-mainlan, add a letter to the name and pcode
                     mutate(ADM4_EN = ifelse(mainland,
                                             ADM4_EN, 
                                             paste0(ADM4_EN, "_A")), 
                            ADM4_PCODE = ifelse(mainland,
                                                ADM4_PCODE, 
                                                paste0(ADM4_PCODE, "_A"))) %>% 
                     select(-mainland))
# then add the new PCODEs and INSTAT codes to the OCHA mapping
ocha_map %<>% rbind(ocha_map %>%
                      filter(ADM4C_BNGRC %in% island_fixes$fkt_instat) %>% 
                      mutate(across(starts_with("ADM4"), ~paste0(.x, "_A"))))

# finally add new rows to the sect file for the islands
sect_filled_v4 %<>% 
  select(-(fkt_ocha_code:note)) %>% 
  full_join(island_fixes %>% select(-ADM4_PCODE) %>% 
              mutate(fkt_instat_clean = paste0(fkt_instat, "_A"),
                     fkt_uid = "none", 
                     fkt_name = "-",
                     match_type = "island_fix") %>% 
              select(-fkt_instat) %>% 
              # add the fs info from the sect file
              left_join(sect_filled_v4 %>% 
                          filter(!is.na(f_or_nf)) %>% 
                          select(region_uid:fs_ll) %>% 
                          distinct))
# should result in 22147 rows   

# 12) more non-contiguous fixes (hopefully last set) ----
# based on symphonia's annotations on the figures in noncontig_healthsheds_for_symphonia_v2.pdf and beamalo.jpg (the clarification for page 2 of previous pdf)
# ADM4_PCODE -- new FS UID assignment -- new FS name -- old FS assignment 
# MG31306210010 -- i1qayRnMeoB -- CSB1 Lanivolo -- Jyc7ZUS9YLv
# MG31306210011 -- i1qayRnMeoB -- CSB1 Lanivolo -- Jyc7ZUS9YLv
# MG31308050004 -- dGKlubfQOgz -- CSB2 Ambodiharina -- jcj1zEAX35z
# MG31308012007 -- dGKlubfQOgz -- CSB2 Ambodiharina -- jcj1zEAX35z
# MG22204192001 -- VHdiLXK2qng -- CSB1 Besofina -- ut1rxyiWBHV 
# MG22204191032 -- H939lgPHl3E -- CSB1 Ambinanindrano -- ut1rxyiWBHV
# MG22204191039 -- H939lgPHl3E -- CSB1 Ambinanindrano -- ut1rxyiWBHV
# MG22204191035 -- H939lgPHl3E -- CSB1 Ambinanindrano -- ut1rxyiWBHV
# MG51506330033 -- ZmPivRqpI9X -- CSB1 Beamalo -- PYhwogb2xPy
# MG51506330034 -- ZmPivRqpI9X -- CSB1 Beamalo -- PYhwogb2xPy
# MG51506330035 -- ZmPivRqpI9X -- CSB1 Beamalo -- PYhwogb2xPy
# MG51506330041 -- ZmPivRqpI9X -- CSB1 Beamalo -- PYhwogb2xPy
# MG52518151001 -- oJjyr3yQNqt -- CSB2 Ambahita -- pCCiyuAJuvF
# above was converted to csv 
noncontig_fixes2 <- read.csv("./data/raw/symphonia_verified/noncontig_v2_fix.csv") %>% 
  mutate(across(everything(), str_trim))

# for these fixes, we don't actually know which fkt uid in DHS2 we want to match to, 
# we just know that these previous matches should get unlinked, and instead link these spatial village locations from OCHA to specific clinics
# start by unlinking the previous entries
sect_filled_v4 %<>% 
  left_join(ocha_map %>% select(ADM4C_BNGRC, ADM4_PCODE), 
            by = c("fkt_instat_clean" = "ADM4C_BNGRC")) %>%
  left_join(noncontig_fixes2 %>% transmute(ADM4_PCODE, old_fs_uid, unmatch = T), 
            by = c("ADM4_PCODE", "fs_uid" = "old_fs_uid")) %>% 
  mutate(across(c(fkt_instat_orig, fkt_instat_clean, match_type, ADM4_PCODE), 
                ~ifelse(is.na(unmatch), .x, NA))) %>% 
  select(-unmatch) 

# now make a df with the correct information for the fixes and add it on 
noncontig_fixes2 %<>% 
  rename_with(~gsub("^new_", "", .x)) %>%
  select(-old_fs_uid, -fs_name) %>% 
  left_join(sect_filled_v4 %>% 
              select(region_uid:fs_ll) %>% 
              filter(!is.na(region_uid)) %>% 
              distinct, 
            by = c("fs_uid")) %>% 
  left_join(ocha_map %>% select(ADM4_PCODE, ADM4_EN, 
                                fkt_instat_clean = ADM4C_BNGRC)) %>% 
  rename(fkt_name = ADM4_EN) %>% 
  mutate(fkt_uid = "none", fkt_instat_orig = NA, 
         fkt_pop_2022 = NA, distance_csb_fkt = NA, transit = NA, 
         match_type = "noncontig_fix2")  

sect_filled_v4 %<>% 
  rbind(noncontig_fixes2) 
# 22160 rows now

# re-fill in the ADM4_PCODE and fkt geometry for everything
sect_filled_v4 %<>% 
  # save the old ADM4_PCODE info, for the places without instat codes
  rename(ADM4_PCODE_OLD = ADM4_PCODE) %>% 
  left_join(ocha_map %>% select(ADM4C_BNGRC, ADM4_PCODE), 
            by = c("fkt_instat_clean" = "ADM4C_BNGRC")) %>%
  # fill with old ADM4_PCODE if needed
  mutate(ADM4_PCODE = ifelse(is.na(ADM4_PCODE), ADM4_PCODE_OLD, ADM4_PCODE)) %>% 
  select(-ADM4_PCODE_OLD) %>% # now drop it 
  left_join(fkt_shape %>% select(ADM4_PCODE))  # add geometries

# update sheds
sheds_v4 <- sect_filled_v4 %>% sect_to_shed()

# v4 state ----
sheds_v4 %>% 
  st_drop_geometry %>% 
  transmute(n_comp = case_when(n_components > 1 ~ ">1", 
                               n_components == 1 ~ "1", 
                               n_components < 1 ~ "<1"),
            mult_shape = n_shape > 1) %>% 
  table
# 2773 healthsheds (7 clinics didn't have any matched fkt)
# 26 have multiple components 
# 25 more have multiple shapes in the geometry but 1 component (maybe due to donuts? or islands off the coast)

fkt_shape %>% 
  filter(ADM4_PCODE %in% sect_filled_v4$ADM4_PCODE == FALSE)  %>% 
  nrow
# 0 unmatched remaining 

# output on remaining state of things ----
# non-contig fig ----
test <- sheds_v4 %>%
  filter(n_components > 1) %>% 
  select(region_name, district_name, fs_name, uid = fs_uid, geom = geometry) %>% 
  # extract(1:2,) %>% 
  purrr::pmap(function(region_name, district_name, fs_name, uid, geom){
    x <- st_sf(fs_name = fs_name, 
               geometry = st_sfc(geom), 
               crs = 4326)
    near_sheds <- sheds_v4 %>% 
      st_filter(st_bbox(x) %>% st_as_sfc) %>% 
      # add column for which one is the fs of interest
      mutate(main = fs_uid == uid) %>% 
      # make the fs of interest the first one (for coloring purposes)
      arrange(desc(main)) %>% 
      mutate(fs_name = factor(fs_name, levels = unique(fs_name), ordered = T))
    
    {ggplot() +
        ggtitle(paste0(fs_name, " in ", district_name, ", ", region_name)) +
        # add the healtsheds
        geom_sf(data = near_sheds, 
                aes(fill = fs_name, lwd = I(ifelse(main, 1.1, 0.4))),
                color = "black") +
        # add unmatched non-contig shed of interest 
        # geom_sf(data = x,
        #         fill = "grey60",
        #         color = "black",
        #         lwd = 0.9) +
        # add fkt boundaries in a healthshed
        geom_sf(data = sect_filled_v4 %>% 
                  filter(fs_uid %in% near_sheds$fs_uid) %>% 
                  st_as_sf(),
                fill = NA,
                color = "grey35",
                linetype = "dotted") +
        # clinic location
        geom_sf(data = near_sheds %>%
                  st_drop_geometry() %>%
                  mutate(fs_ll = gsub("POINT \\(|\\)", "", fs_ll)) %>%
                  separate(fs_ll, into = c("x", "y"), sep = " ") %>%
                  filter(!is.na(y)) %>%
                  st_as_sf(coords = c("x", "y"), crs = 4326),
                inherit.aes = TRUE,
                color = "grey15", size = 2) +
        # ggsflabel::geom_sf_text(data = near_sheds %>%
        #                           st_drop_geometry() %>%
        #                           mutate(fs_ll = gsub("POINT \\(|\\)", "", fs_ll)) %>%
        #                           separate(fs_ll, into = c("x", "y"), sep = " ") %>%
        #                           filter(!is.na(y)) %>%
        #                           st_as_sf(coords = c("x", "y"), crs = 4326),
        #                         aes(label = fs_name),
        #                         colour = "black") +
        scale_fill_manual(name = "clinic healthsheds", 
                          values = c("grey60",
                                     MetBrewer::met.brewer("Renoir", 
                                                           nrow(near_sheds)) %>% 
                                       extract(-1) %>%
                                       rev)) +
        xlim(st_bbox(x)[c(1,3)] %>% 
               extendrange(f = 0.5)) +
        ylim(st_bbox(x)[c(2,4)] %>% 
               extendrange(f = 0.5)) +
        theme_classic() + 
        theme(legend.text = element_text(size = 7),
              legend.key.size = unit(unit(1,"line"))) + 
        xlab("x") + ylab("y")} %>% 
      return
  })

ggexport(ggarrange(plotlist = test, 
                   nrow = 1, ncol = 1), 
         filename = "./scratch/geolocation/noncontig_healthsheds_v3.pdf", 
         width = 7, height = 5)



# multiple shape fig ----
test2 <- sheds_v4 %>%
  filter(n_components == 1 & n_shape > 1) %>% 
  select(region_name, district_name, fs_name, uid = fs_uid, geom = geometry) %>% 
  # extract(-22,) %>%
  purrr::pmap(function(region_name, district_name, fs_name, uid, geom){
    x <- st_sf(fs_name = fs_name, 
               geometry = st_sfc(geom), 
               crs = 4326)
    # adjacent/nearby healthsheds
    near_sheds <- sheds_v4 %>% 
      st_filter(st_bbox(x) %>% st_as_sfc) %>% 
      # add column for which one is the fs of interest
      mutate(main = fs_uid == uid) %>% 
      # make the fs of interest the first one (for coloring purposes)
      arrange(desc(main)) %>% 
      mutate(fs_name = factor(fs_name, levels = fs_name, ordered = T))
    
    {ggplot() +
        ggtitle(paste0(fs_name, " in ", district_name, ", ", region_name)) +
        # add the healtsheds
        geom_sf(data = near_sheds, 
                aes(fill = fs_name, lwd = I(ifelse(main, 1.1, 0.4))),
                color = "black") +
        # add unmatched non-contig shed of interest 
        # geom_sf(data = x,
        #         fill = "grey60",
        #         color = "black",
        #         lwd = 0.9) +
        # add fkt boundaries in a healthshed
        geom_sf(data = sect_filled_v4 %>% 
                  filter(fs_uid %in% near_sheds$fs_uid) %>% 
                  st_as_sf(),
                fill = NA,
                color = "grey35",
                linetype = "dotted") +
        # clinic location
        geom_sf(data = near_sheds %>%
                  st_drop_geometry() %>%
                  mutate(fs_ll = gsub("POINT \\(|\\)", "", fs_ll)) %>%
                  separate(fs_ll, into = c("x", "y"), sep = " ") %>%
                  filter(!is.na(y)) %>%
                  st_as_sf(coords = c("x", "y"), crs = 4326),
                inherit.aes = TRUE,
                color = "grey15", size = 2) +
        # ggsflabel::geom_sf_text(data = near_sheds %>%
        #                           st_drop_geometry() %>%
        #                           mutate(fs_ll = gsub("POINT \\(|\\)", "", fs_ll)) %>%
        #                           separate(fs_ll, into = c("x", "y"), sep = " ") %>%
        #                           filter(!is.na(y)) %>%
        #                           st_as_sf(coords = c("x", "y"), crs = 4326),
        #                         aes(label = fs_name),
        #                         colour = "black") +
        scale_fill_manual(name = "clinic healthsheds", 
                          values = c("grey60",
                                     MetBrewer::met.brewer("Renoir", 
                                                           nrow(near_sheds)) %>% 
                                       extract(-1) %>%
                                       rev)) +
        xlim(st_bbox(x)[c(1,3)] %>% 
               extendrange(f = 0.5)) +
        ylim(st_bbox(x)[c(2,4)] %>% 
               extendrange(f = 0.5)) +
        theme_classic() + 
        theme(legend.text = element_text(size = 7),
              legend.key.size = unit(unit(1,"line"))) + 
        xlab("x") + ylab("y")} %>% 
      return
  })

ggexport(ggarrange(plotlist = test2, 
                   nrow = 1, ncol = 1), 
         filename = "./scratch/geolocation/multshape_healthsheds_v3.pdf", 
         width = 7, height = 5)

# unassigned clinics ----
sect_filled_v4 %>% 
  filter(fs_uid %in% (sheds_v4 %>% 
                        filter(n_components == 0) %>% 
                        pull(fs_uid))) %>%
  st_drop_geometry() %>% 
  select(-(fkt_instat_clean:geometry), 
         -fkt_instat_orig) %>% 
  write.csv("./scratch/geolocation/remaining_unmatched_fs_v3.csv", 
           row.names = FALSE)


# save sect file ---- 
sect_filled_v4 %>% 
  left_join(ocha_map %>% select(ADM4_PCODE, fkt_instat_name = ADM4_EN)) %>% 
  select(-geometry) %>%
  feather::write_feather("./data/intermediate/sectorization2022_cleaned.feather")

sect_filled_v4 %>%
  left_join(ocha_map %>% select(ADM4_PCODE, fkt_instat_name = ADM4_EN)) %>%
  rename(reg_uid = region_uid,
         reg_nm = region_name,
         dist_uid = district_uid,
         dist_nm = district_name,
         com_uid = commune_uid,
         com_nm = commune_name,
         fs_nm = fs_name,
         fkt_nm = fkt_name,
         km_fkt = distance_csb_fkt,
         fkt_instat = fkt_instat_clean,
         instat_nm = fkt_instat_name) %>%
  st_write("./data/intermediate/sectorization2022_cleaned_shapefile",
           driver = "ESRI Shapefile", append = FALSE)

# save healtsheds ----
sheds_v4 %>% 
  rename_with(function(x) gsub("region", "reg", x) %>%
                gsub("district", "dist", .) %>%
                gsub("_fkt_", "_", .) %>%
                gsub("components", "comp", .)) %>%
  st_write("./data/clean/healthsheds2022",
           driver = "ESRI Shapefile", append = FALSE)
sheds_v4 %>% 
  saveRDS("./data/clean/healthsheds2022.rds")

# SCRATCH  -----
# unassigned fkt ---- 
# test <- sheds_v3 %>% 
#   st_filter(st_bbox(unmatched_fkt3) %>% st_as_sfc)
# 
# {ggplot() +
#     ggtitle(paste0(nrow(unmatched_fkt3), " unmatched fokontany")) +
#     # add the healtsheds
#     geom_sf(data = test, 
#             aes(fill = fs_name),
#             color = "black") +
#     # add unmatched fkt 
#     geom_sf(data = unmatched_fkt3,
#             fill = "grey60",
#             color = "black",
#             lwd = 0.7) +
#     # add fkt boundaries in a healthshed
#     geom_sf(data = sect_filled_v3 %>% 
#               filter(fs_uid %in% test$fs_uid) %>% 
#               st_as_sf(),
#             fill = NA,
#             color = "grey35",
#             linetype = "dotted") +
#     # clinic location
#     geom_sf(data = test %>%
#               st_drop_geometry() %>%
#               mutate(fs_ll = gsub("POINT \\(|\\)", "", fs_ll)) %>%
#               separate(fs_ll, into = c("x", "y"), sep = " ") %>%
#               filter(!is.na(y)) %>%
#               st_as_sf(coords = c("x", "y"), crs = 4326),
#             inherit.aes = TRUE,
#             color = "grey15", size = 2) +
#     ggsflabel::geom_sf_text(data = test %>%
#                               st_drop_geometry() %>%
#                               mutate(fs_ll = gsub("POINT \\(|\\)", "", fs_ll)) %>%
#                               separate(fs_ll, into = c("x", "y"), sep = " ") %>%
#                               filter(!is.na(y)) %>%
#                               st_as_sf(coords = c("x", "y"), crs = 4326),
#                             aes(label = fs_name),
#                             colour = "black") +
#     scale_fill_manual(name = "clinic healthsheds", 
#                       values = MetBrewer::met.brewer("Renoir", 
#                                                      nrow(test) + 1) %>% 
#                         extract(-1) %>%
#                         rev) +
#     xlim(st_bbox(unmatched_fkt3)[c(1,3)] %>% 
#            extendrange(f = 0.5)) +
#     ylim(st_bbox(unmatched_fkt3)[c(2,4)] %>% 
#            extendrange(f = 0.5)) +
#     theme_classic() + 
#     theme(legend.text = element_text(size = 7),
#           legend.key.size = unit(unit(1,"line"))) + 
#     xlab("x") + ylab("y")} %>% 
#   ggsave(., filename = "./projects/madagascar_clinic_data_cleaning/scratch/geolocation/remaining_unmatched_fkt.png", 
#          width = 8, height = 6)
# 
# unmatched_fkt3 %>% 
#   select(ends_with("EN"), ADM4_PCODE) %>% 
#   st_drop_geometry() %>% 
#   write.csv("./projects/madagascar_clinic_data_cleaning/scratch/geolocation/remaining_unmatched_fkt.csv", 
#             row.names = FALSE)

# for meeting with symphonia -----
# sect_filled_v3 %>% 
#   filter(fs_uid %in% test$fs_uid) %>% 
#   left_join(ocha_map %>% select(ADM4_EN, ADM3_EN, ADM4_PCODE)) %>% 
#   select(ends_with("name"), fkt_uid, fkt_instat_clean, 
#          distance_csb_fkt, ends_with("EN")) %>% 
#   View
# 
# View(unmatch_fkt3 %>% select(ends_with("EN")))


# check the 2 with location information before saving
# CSB1 Analasoa POINT (46.117269 -23.252324)
# 
# temp <- data.frame(x = c(46.117269, 48.594096, 45.22634, 47.819243,
#                          46.6536, 48.07973), 
#                    y = c(-23.252324, -12.911521, -18.17919, -13.598709,
#                          -15.8393, -18.0819), 
#                    fs_name = c("CSB1 Analasoa", 
#                                "CSB1 Nosy Mitsio",
#                                "CSB1 Orimbato", 
#                                "CSB1 Nosy Iranja",
#                                "CSB1 Manarenja Doany", 
#                                "CSB1 Andilamenakely")) %>% 
#   st_as_sf(coords = c("x", "y"), crs = 4326)  %>% 
#   extract(-2,)
# 
# temp2 <- sheds_v3 %>% st_filter(temp[4,])
# temp2 <- sheds_v2 %>% st_filter(st_bbox(temp2) %>% st_as_sfc)
# ggplot() + 
#   geom_sf(data = temp2,
#           aes(fill = fs_name)) + 
#   geom_sf(data = sect_filled_v3 %>% 
#             filter(fs_uid %in% temp2$fs_uid) %>% 
#             st_as_sf(), 
#           fill = NA, color = "black") + 
#   geom_sf(data = temp2 %>%
#             st_drop_geometry() %>%
#             mutate(fs_ll = gsub("POINT \\(|\\)", "", fs_ll)) %>%
#             separate(fs_ll, into = c("x", "y"), sep = " ") %>%
#             filter(!is.na(y)) %>%
#             st_as_sf(coords = c("x", "y"), crs = 4326),
#           inherit.aes = TRUE,
#           color = "black", size = 1.2) +
#   geom_sf(data = temp[4,],
#           color = "white", cex = 1.5, ) +
#   geom_point(aes(x = 46.6552, y = -15.8375), 
#              color = "black", shape = 4) + 
#   theme_void()
# 
# testing <- fkt_shape %>% 
#   st_filter(temp[4,]) 
# 
# testing %>% 
#   st_cast("POLYGON") 
# mutate(group = (1:n()) > 1) %>% 
#   summarise(across(!geometry, unique), 
#             geometry = st_union(geometry),
#             .by = group) %>% 
#   mutate(id = 1:n()) %>%
#   ggplot(aes(fill = as.factor(id))) + 
#   geom_sf() + 
#   theme_void()
# 


# %>% 
#   st_set_crs(4326) %>% 
#   st_join(sheds_v3) %>% 
#   rename(unmatch_clinic_ll = geometry) %>% 
#   left_join(sheds_v3 %>% 
#               mutate(shed_geom = geometry) %>% 
#               st_drop_geometry %>% 
#               select(fs_uid, shed_geom)) 
# 
# temp2 <- temp %>% 
#   select(unmatch_clinic_name = fs_name.x, 
#          fs_uid, fs_ll,
#          overlap_shed_name = fs_name.y, 
#          unmatch_geometry = unmatch_clinic_ll, 
#          overlap_geometry = shed_geom) %>% 
#   mutate(fs_ll = gsub("POINT \\(|\\)", "", fs_ll)) %>%
#   separate(fs_ll, into = c("x", "y"), sep = " ", 
#            convert = T)  %>% 
#   {cbind(select(., -c(x, y)), 
#          st_drop_geometry(.) %>% 
#            select(c(x,y)) %>% 
#            st_as_sf(coords = c("x", "y"), crs = 4326) %>% 
#            rename(overlap_ll_geometry = geometry))}  %>% 
#   mutate(overlap_geometry = drop_attr(overlap_geometry, "class")) %>%
#   pivot_longer(ends_with("geometry"), 
#                names_to = "type", 
#                values_to = "geometry") %>% 
#   mutate(type = gsub("_geometry", "", type)) %>% 
#   st_as_sf() %>% 
#   {tmap::tm_shape(.) +
#       tmap::tm_polygons('type', legend.show = FALSE) +
#       tmap::tm_facets('fs_uid', nrow = 7, ncol = 8)} %>% 
#   tmap::tmap_save("./scratch/geolocation/unmatched_clinics.png", 
#                   height = 21, width = 16, units = "in")
# 
#   
# drop_attr <- function(x, attr_drop){
#   attr(x, attr_drop) <- NULL
#   return(x)
# }  