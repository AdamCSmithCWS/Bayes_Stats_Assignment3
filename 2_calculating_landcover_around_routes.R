# calculating proportion of landcover classes within buffer of BBS route

library(sf)
#library(sp)
#library(raster)
library(tidyverse)
library(terra)
library(bbsBayes)
library(stars)
library(exactextractr)



bird_data <- read.csv("Bayesian_Stats_Bird_data_01.csv")

#load the province and state map and filter down to just Ontario
region <- load_map("bbs_cws") %>% 
  filter(ST_12 == "CA-ON-13") %>% 
  select(ST_12) %>% 
  st_convex_hull() %>% #simplify with a convex hull
  st_buffer(dist = 100000) #buffer by 100km to ensure all routes are included
AEA_crs <- st_crs(region)

# load canada landcover raster and clip to region -------------------------
### Note: these data are not in the GitHub repo
### landcover data downloadable from here: https://open.canada.ca/data/en/dataset/ee1580ab-a23d-4f86-a09b-79763677eb47 
landcover_large = terra::rast("data/landcover-2020-classification.tif")

region = st_transform(region, st_crs(landcover_large))
landcover = terra::crop(landcover_large, region)
#remove large landcover object
rm(landcover_large)


# landcover categories ----------------------------------------------------
### xlsx file is information contained in pdf form at https://open.canada.ca/data/en/dataset/ee1580ab-a23d-4f86-a09b-79763677eb47
lc_class <- readxl::read_excel("data/habitat_classes.xlsx",
                               trim_ws = TRUE)

lc_j <- lc_class %>% 
  select(value,general_lc) %>% 
  mutate(value = as.integer(str_trim(value)))


# load bbs routes ---------------------------------------------------------
## this is a copy of the spatial information for all Canadian BBS routes, updated through 2019
## contact bbs@ec.gc.ca for more information and an updated copy
# unzip("data/Copy of All Routes 2019.kmz",
#       exdir = "data")
bbs <- st_read("data/Copy_of_All_Routes_2019.kml") %>% 
  st_transform(.,st_crs(landcover)) %>% 
  st_join(.,region,
          join = st_intersects) %>% 
  filter(!is.na(ST_12)) %>%  #clip out only routes that are within region
  group_by(Name) %>% 
  summarise() %>% 
  mutate(statenumber = as.integer(str_sub(Name,1,2)),
         routenumber = as.integer(str_sub(Name,4,6)),
         rt.uni = paste(statenumber,routenumber,sep = "-"),
         RouteName = str_sub(Name,7),
         id = row_number()) #add id column for linking back to extracted raster data

#buffer BBS routes by 1km
bbs_buffer = st_buffer(bbs,dist = 1000)

#use exact_extract to calculate area of each class in the raster
bbs_extract <- exact_extract(landcover,bbs_buffer,
                         include_area = TRUE)

bbs_cover <- bbs_extract %>% 
  bind_rows(.id = "id") %>% 
  left_join(.,lc_j,by = "value") %>% 
  group_by(id,general_lc) %>% 
  summarise(sum_area = sum(area*coverage_fraction,na.rm = T)) %>% #area * cov = area of each habitat (m^2)
  mutate(p_area = sum_area/sum(sum_area,na.rm = T)) %>% #proportional coverage
  pivot_wider(.,id_cols = id,names_from = general_lc,
              #names_prefix = "lc_",
              values_from = p_area,
              values_fill = 0) %>% #pivot to column per raster category
  mutate(id = as.integer(id)) %>% 
  arrange(id)

bbs_habitat <- bbs_buffer %>% 
  left_join(bbs_cover,by = "id") %>% 
  st_set_geometry(NULL) %>% 
  mutate(RouteName = str_trim(RouteName)) %>% 
  select(RouteName,rt.uni,crop,forest,shrub,urban,water,wetland,grassland) %>%
  as.data.frame()


bird_data <- bird_data %>% 
  inner_join(.,bbs_habitat,
            by = c("rt.uni","RouteName")) %>% 
  select(RouteName,Year,english,french,group,count,
         cumulative,change_human_footprint,
         forest) %>% 
  rename(human_footprint = cumulative,
         bird_guild = group,
         year = Year,
         proportion_forest = forest)


write.csv(bird_data,file = "bird_data_Bayes_Stats.csv",
          row.names = FALSE)


