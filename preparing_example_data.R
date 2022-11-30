### building a spatial route-level trend model with footprint covariate on abundance and slope


library(bbsBayes)
library(tidyverse)

firstYear = 2000 # first year to consider

# 

strat_data <- bbsBayes::stratify(by = "bbs_usgs")


region <- c("CA-ON-13")

on_routes <- strat_data$route_strat %>% 
  filter(strat_name == region,
         Year > firstYear) %>% 
  select(RouteName,rt.uni,Year,strat_name,RouteDataID)


sp_sell <- data.frame(species = c("Hermit Thrush",
                                 "Black-throated Green Warbler",
                                 "American Redstart",
                                 "Black-and-white Warbler",
                                 "Chestnut-sided Warbler",
                                 "Least Flycatcher",
                                 "Nashville Warbler",
                                 "Ovenbird",
                                 "Red-breasted Nuthatch",
                                 "Scarlet Tanager",
                                 "Veery",
                                 "White-throated Sparrow",
                                 "Wood Thrush",
                                 "Yellow-bellied Sapsucker",
                                 
                                 "Yellow Warbler",
                                 "Song Sparrow",
                                 "Northern Cardinal",
                                 "House Wren",
                                 "House Finch",
                                 "Common Grackle",
                                 "American Robin",
                                 "Blue Jay",
                                 "Baltimore Oriole",
                                 "Warbling Vireo",
                                 "Downy Woodpecker",
                                 "Black-capped Chickadee",
                                 "Chipping Sparrow",
                                 "Mourning Dove"),
                     group = c(rep("Forest",14),
                               rep("Urban",14))
                     )


# sp_sell <- sp_sell %>% 
#   filter(group == "Forest")

sp <- strat_data$species_strat %>% 
  select(sp.bbs,english,french) %>% 
  inner_join(.,sp_sell,by = c("english" = "species"))


sps <- sp$sp.bbs
#zero-fill
surveys <- on_routes %>% 
  distinct() %>% 
  expand_grid(.,sp.bbs = sps) %>% 
  left_join(.,sp,by = "sp.bbs")


bird <- strat_data$bird_strat %>% 
  select(RouteDataID,AOU,SpeciesTotal) %>% 
  right_join(.,surveys,by = c("AOU" = "sp.bbs",
                              "RouteDataID")) %>% 
  mutate(count = ifelse(is.na(SpeciesTotal),0,SpeciesTotal)) %>% 
  select(-SpeciesTotal,-RouteDataID,-AOU,-strat_name)

mnc = bird %>% 
  group_by(english) %>% 
  summarise(mc = mean(count)) %>% 
  filter(mc > 0.03)

bird <- bird %>% 
  filter(english %in% mnc$english)


# bird data load ----------------------------------------------------------


# Canadian current footprint data at route-level --------------------------

###---------
# NOTE - these data are not public and so not available in the repo
###--------
load("data/compiled_footprint_data.RData")

ch_fp <- fp_global_by_route %>% 
  filter(year %in% c(2000,2013)) %>% 
  select(rt.uni,mean_1km,year) %>% 
  pivot_wider(names_from = year,values_from = mean_1km,
              names_prefix = "y") %>% 
  group_by(rt.uni) %>% 
  summarise(change_human_footprint = y2000-y2013) %>% 
  filter(!is.na(change_human_footprint))


fp_4k <- fp_can_by_route %>% 
  select(rt.uni,contains("_1km_mean")) %>% 
  rename_with(~gsub(x = .x,pattern = "_1km_mean",replacement = "")) %>% 
  select(rt.uni,built,night_lights,population_density) %>% 
  mutate(cumulative = built+night_lights+population_density) %>% 
  inner_join(.,ch_fp,by = "rt.uni")


all <- bird %>% 
  inner_join(.,fp_4k,by = "rt.uni") %>% 
  group_by(english,group) %>% 
  mutate(cat_human = cut_number(cumulative, n = 3))

sum_t <- all %>% 
  group_by(english,group,cat_human) %>% 
  summarise(mc = mean(log(count+1)),
            mdc = median(log(count+1))) 



mnplot <- ggplot(data = sum_t,aes(x = english,y = mc))+
  geom_point(aes(group = cat_human, colour = cat_human),position = position_dodge(width = 0.3))+
  facet_wrap(vars(group),scales = "free_x")+
  coord_flip()
print(mnplot)




sp_sums <- ggplot(data = all,
                  aes(x = cumulative,y = log(count+1,10)))+
  geom_point(aes(colour = group),alpha = 0.3)+
  geom_smooth(method = "lm")+
  #scale_y_continuous(trans = "log10")+
  facet_wrap(vars(english),nrow = 3,scales = "free_y")

print(sp_sums)


write.csv(all,"Bayesian_Stats_Bird_data_01.csv",row.names = FALSE)









