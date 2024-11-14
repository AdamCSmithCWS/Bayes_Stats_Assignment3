
library(tidyverse)
library(rethinking)
#devtools::install_github("mjskay/tidybayes.rethinking")
# library(tidybayes)
# library(tidybayes.rethinking)


birds = read.csv("bird_data_Bayes_Stats.csv")

sp_indicators <- birds %>% #creates a data frame that list each species indicator number
  filter(bird_guild == "Forest") %>% 
  select(english) %>% 
  distinct() %>% # selects just the unique rows
  mutate(species_ind = as.integer(factor(english))) %>% 
  arrange(species_ind)


# forest cover compare partial pool with no pooling ----------------------------------------------


birds_sub <- birds %>% 
  filter(bird_guild == "Forest") %>% 
  mutate(route_ind = as.integer(factor(RouteName)),
         #species_ind = as.integer(factor(english)),
         guild_ind = as.integer(factor(bird_guild)),
         foot_scale = scale(human_footprint),
         change_foot_scale = scale(change_human_footprint),
         pforest_scale = scale(proportion_forest)) %>% 
  inner_join(sp_indicators,
            by = c("english")) %>% 
  select(count,species_ind,pforest_scale) # this removes all unecessary columns to keep ulam() happy

#partial pool with NB distribution
m1<- ulam(
  alist(
    count ~ dgampois( lambda , exp(log_scale) ),
    log(lambda) <- sp[species_ind] + b_for[species_ind]*pforest_scale,
    #sp[species_ind] ~ dnorm( sp_bar , sigma_sp ),
    sp[species_ind] ~ dnorm( 0 , 1.5 ),
    b_for[species_ind] ~ dnorm( b_for_bar , sigma_b_for ),
    #sp_bar ~ dnorm( 0 , 1.5 ),
    b_for_bar ~ dnorm( 0 , 1 ),
    #sigma_sp ~ dexp( 1 ),
    sigma_b_for ~ dexp(1),
    log_scale ~ dnorm(0,10)
  ), data=birds_sub , chains=4, cores = 4,
  cmdstan = TRUE)

#prior simulation
prior_m1 <- extract.prior(m1,pars = "b_for")



#posterior
b1 <- precis(m1,depth = 2,pars = c("b_for")) 

b1s <- data.frame(mean = b1$mean,
                  lci = b1$`5.5%`,
                  uci = b1$`94.5%`) %>% 
  mutate(model = "partial pool",
         species_ind = 1:14,
         nyears = 20) %>% 
  left_join(.,sp_indicators)


plot(m1,depth = 2)


#No pooling version of above
m2<- ulam(
  alist(
    count ~ dgampois( lambda , exp(log_scale) ),
    log(lambda) <- sp[species_ind] + b_for[species_ind]*pforest_scale,
    #sp[species_ind] ~ dnorm( sp_bar , sigma_sp ),
    sp[species_ind] ~ dnorm( 0 , 1.5 ),
    b_for[species_ind] ~ dnorm( 0 , 1.5 ),
    #sp_bar ~ dnorm( 0 , 1.5 ),
    log_scale ~ dnorm(0,10)
  ), data=birds_sub , chains=4, cores = 4,
  cmdstan = TRUE)


#posterior
b2 <- precis(m2,depth = 2,pars = c("b_for")) 

b2s <- data.frame(mean = b2$mean,
                  lci = b2$`5.5%`,
                  uci = b2$`94.5%`) %>% 
  mutate(model = "no pool",
         species_ind = 1:14,
         nyears = 20) %>% 
  left_join(.,sp_indicators)

bs <- bind_rows(b1s,b2s)

shrink <- ggplot(data = bs,
                 aes(x = english,y = mean,group = model,colour = model))+
  geom_pointrange(aes(ymin = lci,ymax = uci),
                  position = position_dodge(width = 0.5))+
  coord_flip()

print(shrink)



# same as above with fewer data (years) -----------------------------------

yr_sel <- c(2015)
nrs_sel <- length(yr_sel)
birds_sub <- birds %>% 
  filter(bird_guild == "Forest",
         year %in% yr_sel) %>% 
  mutate(route_ind = as.integer(factor(RouteName)),
         #species_ind = as.integer(factor(english)),
         guild_ind = as.integer(factor(bird_guild)),
         foot_scale = scale(human_footprint),
         change_foot_scale = scale(change_human_footprint),
         pforest_scale = scale(proportion_forest)) %>% 
  inner_join(sp_indicators,
             by = c("english")) %>% 
  select(count,species_ind,pforest_scale) # this removes all unecessary columns to keep ulam() happy

#partial pool with NB distribution
m1a<- ulam(
  alist(
    count ~ dgampois( lambda , exp(log_scale) ),
    log(lambda) <- sp[species_ind] + b_for[species_ind]*pforest_scale,
    #sp[species_ind] ~ dnorm( sp_bar , sigma_sp ),
    sp[species_ind] ~ dnorm( 0 , 1.5 ),
    b_for[species_ind] ~ dnorm( b_for_bar , sigma_b_for ),
    #sp_bar ~ dnorm( 0 , 1.5 ),
    b_for_bar ~ dnorm( 0 , 1 ),
    #sigma_sp ~ dexp( 1 ),
    sigma_b_for ~ dexp(1),
    log_scale ~ dnorm(0,10)
  ), data=birds_sub , chains=4, cores = 4,
  cmdstan = TRUE)

#prior simulation
# prior_m1 <- extract.prior(m1,pars = "b_for")



#posterior
b1a <- precis(m1a,depth = 2,pars = c("b_for")) 

b1as <- data.frame(mean = b1a$mean,
                  lci = b1a$`5.5%`,
                  uci = b1a$`94.5%`) %>% 
  mutate(model = "partial pool",
         species_ind = 1:14,
         nyears = nrs_sel) %>% 
  left_join(.,sp_indicators)


plot(m1a,depth = 2)


#No pooling version of above
m2a<- ulam(
  alist(
    count ~ dgampois( lambda , exp(log_scale) ),
    log(lambda) <- sp[species_ind] + b_for[species_ind]*pforest_scale,
    #sp[species_ind] ~ dnorm( sp_bar , sigma_sp ),
    sp[species_ind] ~ dnorm( 0 , 1.5 ),
    b_for[species_ind] ~ dnorm( 0 , 1.5 ),
    #sp_bar ~ dnorm( 0 , 1.5 ),
    log_scale ~ dnorm(0,10)
  ), data=birds_sub , chains=4, cores = 4,
  cmdstan = TRUE)


#posterior
b2a <- precis(m2a,depth = 2,pars = c("b_for")) 

b2as <- data.frame(mean = b2a$mean,
                  lci = b2a$`5.5%`,
                  uci = b2a$`94.5%`) %>% 
  mutate(model = "no pool",
         species_ind = 1:14,
         nyears = nrs_sel) %>% 
  left_join(.,sp_indicators)

bsa <- bind_rows(b1as,b2as)

shrinka <- ggplot(data = bsa,
                 aes(x = english,y = mean,group = model,colour = model))+
  geom_pointrange(aes(ymin = lci,ymax = uci),
                  position = position_dodge(width = 0.5))+
  coord_flip()

print(shrinka)


bs2 <- bind_rows(bs,bsa)


shrink2 <- ggplot(data = bs2,
                  aes(x = english,y = mean,group = interaction(model,nyears),
                      colour = interaction(model,nyears)))+
  geom_pointrange(aes(ymin = lci,ymax = uci),
                  position = position_dodge(width = 0.5))+
  coord_flip()

print(shrink2)













# forest and footprint poisson vs negative binomial -------------------------------------------------



birds_sub1 <- birds %>% 
  mutate(route_ind = as.integer(factor(RouteName)),
         guild_ind = as.integer(factor(bird_guild)),
         foot_scale = scale(human_footprint),
         change_foot_scale = scale(change_human_footprint),
         pforest_scale = scale(proportion_forest),
         year_scale = scale(year)) %>% 
  inner_join(sp_indicators,
             by = c("english")) %>% 
  select(count,species_ind,pforest_scale,foot_scale) # this removes all unecessary columns to keep ulam() happy

m1<- ulam(
  alist(
    count ~ dgampois( lambda , exp(log_scale) ),
    log(lambda) <- sp[species_ind] + b_for[species_ind]*pforest_scale + b_foot[species_ind]*foot_scale,
    #sp[species_ind] ~ dnorm( sp_bar , sigma_sp ),
    sp[species_ind] ~ dnorm( 0 , 1.5 ),
    b_for[species_ind] ~ dnorm( b_for_bar , sigma_b_for ),
    b_foot[species_ind] ~ dnorm( b_foot_bar , sigma_b_foot ),
    #sp_bar ~ dnorm( 0 , 1.5 ),
    b_for_bar ~ dnorm( 0 , 1 ),
    b_foot_bar ~ dnorm( 0 , 1 ),
    #sigma_sp ~ dexp( 1 ),
    sigma_b_for ~ dexp(1),
    sigma_b_foot ~ dexp(1),
    log_scale ~ dnorm(0,10)
  ), data=birds_sub , chains=4, cores = 4,
  log_lik = TRUE,
  cmdstan = TRUE)

m2<- ulam(
  alist(
    count ~ dpois( lambda),
    log(lambda) <- sp[species_ind] + b_for[species_ind]*pforest_scale + b_foot[species_ind]*foot_scale,
    #sp[species_ind] ~ dnorm( sp_bar , sigma_sp ),
    sp[species_ind] ~ dnorm( 0 , 1.5 ),
    b_for[species_ind] ~ dnorm( b_for_bar , sigma_b_for ),
    b_foot[species_ind] ~ dnorm( b_foot_bar , sigma_b_foot ),
    #sp_bar ~ dnorm( 0 , 1.5 ),
    b_for_bar ~ dnorm( 0 , 1 ),
    b_foot_bar ~ dnorm( 0 , 1 ),
    #sigma_sp ~ dexp( 1 ),
    sigma_b_for ~ dexp(1),
    sigma_b_foot ~ dexp(1)
  ), data=birds_sub , chains=4, cores = 4,
  log_lik = TRUE,
  cmdstan = TRUE)

precis(m1,depth = 2)
precis(m2,depth = 2)

PSIS(m1)
PSIS(m2)




