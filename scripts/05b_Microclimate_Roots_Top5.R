### 5. Get july average temp data, match microclimate to phenocams/root clusters, plot microclimate vs growth rate  ###
### Elise Gallois, elise.gallois94@gmail.com ###
### Date: 30th Oct 2023 ###

#### 1 - LOAD PACKAGES  ##### 

library(lubridate)
library(readr)
library(tidyverse) 
library(esquisse)
library(viridis)
library(gridExtra)
library(brms)
library(ggeffects)
library(sjPlot)

#### 2 - LOAD DATA ####

# load microclimate data
tomst <- read_csv("users/egallois/soilcores/data/all_sites_mean_daily_temp.csv")
soil <- read_csv("users/egallois/soilcores/data/all_sites_mean_daily_soil.csv")

# load root data
roots <- read_csv("users/egallois/soilcores/data/rootweights_phenocams.csv")  

# load site key
site_key <- read_csv("users/egallois/soilcores/data/tomst_key.csv") 


# rename niwot serialID
site_key$SerialID <- recode(site_key$SerialID, 
                            'sn_12' = 'NW12',
                            'sn_11' = 'NW11',
                            'sn_13' = 'NW13',
                            'sn_08' = 'NW8')

tomst$SerialID <- recode(tomst$SerialID, 
                         'sn_12' = 'NW12',
                         'sn_11' = 'NW11',
                         'sn_13' = 'NW13',
                         'sn_08' = 'NW8')
soil$SerialID <- recode(soil$SerialID, 
                        'sn_12' = 'NW12',
                        'sn_11' = 'NW11',
                        'sn_13' = 'NW13',
                        'sn_08' = 'NW8')


#### 2 - July-Aug AVERAGE ####
# group by logger ID and calculate july means
summer_surface_temp <-  tomst %>%
  subset(Date >= "2021-07-30" & Date <= "2021-08-31" |
           Date >= "2022-07-01" & Date <= "2022-08-31") %>% 
  group_by(SerialID) %>% 
  filter(mean_temp >= -90) %>%  # to exclude non-activated BC loggers
  mutate(summer_temp =  mean(mean_temp))

summer_soil_temp <-  soil %>%
  subset(Date >= "2021-07-30" & Date <= "2021-08-31" |
           Date >= "2022-07-01" & Date <= "2022-08-31") %>% 
  group_by(SerialID) %>% 
  filter(mean_temp >= -20) %>%  # to exclude non-activated BC loggers
  mutate(summer_temp =  mean(mean_temp))

# get quantiles of july temp grouped by site
summer_surface_temp <- summer_surface_temp %>% 
  group_by(site) %>% 
  mutate(quantilegroup = ntile(summer_temp, 4)) %>% 
  ungroup()

summer_soil_temp <- summer_soil_temp %>% 
  group_by(site) %>% 
  mutate(quantilegroup = ntile(summer_temp, 4)) %>% 
  ungroup()


# make logger ID a class 
summer_surface_temp$SerialID <- as.factor(summer_surface_temp$SerialID)
summer_soil_temp$SerialID <- as.factor(summer_soil_temp$SerialID)

tomst_table <- summer_surface_temp %>% 
  group_by(site) %>% 
  summarise(mean_prc = mean(summer_temp,na.rm = TRUE)) 


# boxplot of summer averages across sites
ggplot(summer_surface_temp) +
  aes(x = site, y = summer_temp, fill = site) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()


ggplot(summer_soil_temp) +
  aes(x = site, y = summer_temp, fill = site) +
  geom_boxplot() +
  scale_fill_hue(direction = 1) +
  theme_minimal()

#### 3 -  MATCH TOMST TO CAMERAS ####
# rename Mt Brew to BC Coastal
roots$Site <- recode(roots$Site, 'Mt_Brew' = 'BC_coastal')

# make sure serial ID is a factor
site_key$SerialID <- as.factor(site_key$SerialID)

# merge summer temp data to key
merge_clim <- left_join(summer_surface_temp, site_key)

# make temp-site column
merge_clim$plot_temp <- paste(merge_clim$summer_temp, ' ',merge_clim$Subplot)
unique(merge_clim$plot_temp)

# remove duplicate summer temps
merge_clim <- merge_clim[!duplicated(merge_clim$plot_temp), ]

# now we can get rid of it
merge_clim <- merge_clim %>% select (- plot_temp)



all_ingrowth <- left_join(roots, merge_clim)
str(all_ingrowth)


# plot summer temperatures
tomst %>%
  filter(Date >= "2022-06-01" & Date <= "2022-09-30") %>%
  filter(mean_temp >= 1L & mean_temp <= 27L) %>%
  ggplot() + aes( x = Date,y = mean_temp, colour = site,group = SerialID) +
  geom_line() +
  scale_color_hue(direction = 1) +
  labs(y = "Daily Surface Temperature degrees C") +
  theme_classic()



tomst_table <- tomst %>% 
  group_by(site) %>% 
  summarise(mean_prc = mean(summer_temp,na.rm = TRUE)) 


#### 5 - CALCULATE ROOT GROWTH RATES ####
# filter in all_growth only 
all_ingrowth <- all_ingrowth %>% 
  filter(Depth_Type %in% "Top" | is.na(Depth_Type))

# calculate belowground growing season length for each subplot
all_ingrowth <- all_ingrowth %>% 
  group_by(Subplot) %>% 
  mutate(growing_ssn = diff(range(DOY)))

# only keep relevant columns
root_only <- all_ingrowth %>%
  dplyr::select(Site, Subplot, Core_ID,Phenocam, Community,
                Snowmelt, rootmass_bulkdensity,growing_ssn, mean_temp, summer_temp, quantilegroup, async)

# rename segment root mass to av_roots
root_only$av_roots <- root_only$rootmass_bulkdensity
  
# remove duplicate averaged root values
root_only <- root_only[!duplicated(root_only$rootmass_bulkdensity), ]

# filter with only cairngorms
cairn_only <- root_only %>% filter(Site %in% "Cairngorms" | is.na(Site))

# all without cairngorms
no_cairn <- root_only %>% subset(Site != 'Cairngorms')

# make av_root columns wider by phenology type
wide_cairn <- cairn_only %>% 
  pivot_wider(names_from = Core_ID, values_from = rootmass_bulkdensity)

wide_three <- no_cairn %>% 
  pivot_wider(names_from = Core_ID, values_from = rootmass_bulkdensity)

# rate calculations
wide_three <- wide_three %>% 
  mutate(root_rate = ((P3-P1)/growing_ssn))

wide_cairn <- wide_cairn %>% 
  mutate(root_rate = ((P2-P1)/growing_ssn))

# add empty P3 column to cairngorms data
wide_cairn <- wide_cairn %>% mutate(P3 = "0")

# because the P1 was 0 for INT2 cairngorms, root_rate needs to match P2
wide_cairn <- wide_cairn[-7,]
wide_cairn[wide_cairn$Subplot == 'INT2', 'P1'] <- 0

# reorder columns prior to bind
wide_cairn <- wide_cairn %>%
  dplyr::select(Site, Subplot,Phenocam, Community,
                Snowmelt, growing_ssn, mean_temp, summer_temp, quantilegroup,P1, P2, P3, root_rate, async)

wide_cairn$P3 <- as.numeric(wide_cairn$P3)
wide_three$P3 <- as.numeric(wide_three$P3)

#rbind to get merged
all_rates <- rbind(wide_cairn, wide_three)



#### 6 - MICROCLIMATE VS GROWTH RATES and BIOMASS ####

# quantilegroup as a factor
all_rates$quantilegroup <- as.factor(all_rates$quantilegroup)
all_ingrowth$quantilegroup <- as.factor(all_ingrowth$quantilegroup)

# remove duplicate summer temps
short_ingrowth <- all_ingrowth[!duplicated(all_ingrowth$unique_ID), ]


# do rates vary by microclimate?
(micro_rate <- ggplot(data=subset(all_rates, !is.na(quantilegroup))) +
    aes(x = quantilegroup, y = root_rate, colour = quantilegroup) +
    geom_point(size = 2) +
    scale_color_viridis_d(option = "viridis", direction = 1) +
    labs(y = "Daily Root Biomass Growth (per g/cm3)",  x = "Climate Quantile (1 = cold, 4 = warm)",
         title ="Do growth rates vary across microclimate?") +
    facet_wrap(~Site) +
    theme_classic())
(micro_rate <- micro_rate  + theme(legend.position = "none"))

# does biomass vary by microclimate?
(micro_mass <-ggplot(data=subset(all_ingrowth, !is.na(quantilegroup))) +
    aes(x = quantilegroup, y = av_roots,  fill = quantilegroup) +
    geom_boxplot() +
    scale_fill_viridis_d(option = "viridis", direction = 1) +
    labs(y = "Dry Root Biomass (per g/cm3) ", x = "Climate Quantile (1 = cold, 4 = warm)",   
         title ="Does biomass vary across microclimate?") +
    facet_wrap(~Site) +
    theme_classic())
(micro_mass <- micro_mass  + theme(legend.position = "none"))

# does asynchrony vary by microclimate?
(micro_sync <- ggplot(data=subset(all_ingrowth, !is.na(quantilegroup))) +
    aes(x = quantilegroup, y = async, colour = quantilegroup) +
    geom_point(size = 2) +
    scale_color_viridis_d(option = "viridis", direction = 1) +
    labs(y = "% Root Growth at date of 100% Greening",  x = "Climate Quantile (1 = cold, 4 = warm)",
         title ="Does above- v below-ground synchrony vary across microclimate?") +
    facet_wrap(~Site) +
    ylim(0,70) +
    theme_classic())
(micro_sync <- micro_sync  + theme(legend.position = "bottom"))


micro_compare <- grid.arrange(micro_mass,
                              micro_rate,nrow=1)


#### 7 - COMMUNITY VS GROWTH RATES ####
# do rates vary by community?
(comm_rates <- ggplot(all_rates) +
   aes(x = Community, y = root_rate, color = Community) +
   geom_point(size = 4) +
   scale_color_viridis_d(option = "plasma", direction = 1) +
   facet_wrap(~Site) +
   labs(y = "Daily Root Biomass Growth (per g/cm3)", , title ="Do growth rates vary across community type?") +
   theme_classic())

# does biomass vary by community?
(comm_mass <- ggplot(data=subset(all_ingrowth, !is.na(Community), !is.na(Site))) +
    aes(x = Community, y = av_roots, fill = Community) +
    geom_boxplot() +
    scale_fill_viridis_d(option = "plasma", direction = 1) +
    facet_wrap(~Site) +
    labs(y = "Dry Root Biomass(per g/cm3)", title ="Does biomass vary across community type?") +
    theme_classic())

(comm_sync <- ggplot(data=subset(all_ingrowth, !is.na(Community))) +
    aes(x = Community, y = async, colour = Community) +
    geom_point(size = 4) +
    scale_color_viridis_d(option = "plasma", direction = 1) +
    labs(y = "% Root Growth at date of 100% Greening", 
         title ="Does above- v below-ground synchrony vary across community type?") +
    facet_wrap(~Site) +
    ylim(0,70) +
    theme_classic())


comm_compare <- grid.arrange(comm_mass,
                             comm_rates,nrow=1)

# make biomass barcharts of phenology stages vs community type
(biomass_barplot <- ggplot(short_ingrowth) +
    aes(x = Community, y = av_roots, fill = Core_ID, group = Core_ID,weight = rootmass_bulkdensity ) +
    geom_bar(position = "dodge",stat="identity") +
    scale_fill_manual(values = c(P1 = "#B7DD85",P2 = "#32C02E", P3 = "#077204")) +
    labs( x = "Community Type",  y = "Root Biomass / Bulk Density (g/cm3)") +
    #geom_errorbar(stat = 'summary', position = 'dodge', width = 0.9) +
    geom_point(aes(x = Community), shape = 21, position = 
                 position_jitterdodge(jitter.width = 0.1,  
                                      dodge.width=0.9)) +
    # facet_wrap(~Site) +
    
    theme_classic()) 

# We will use a function by Ben Marwick
# This code loads the function in the working environment
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

(biomass_rainfall <- 
    ggplot(data = short_ingrowth, 
           aes(x = Community, y = av_roots, fill = Core_ID)) +
    geom_flat_violin(position = position_nudge(x = 0.3, y = 0), 
                     alpha = 0.6, 
                     aes(y = av_roots, color = Core_ID, stat="identity")) +
    geom_point(aes(y = av_roots, color = Core_ID, shape = Site), 
               
               position=position_jitterdodge(), size = 2, alpha = 0.9) +
    geom_boxplot(width = 0.4, outlier.shape = TRUE, alpha = 0.6) +
    labs(y =bquote("Root Biomass (g"~cm^-3*")"), x = "\nCommunity Type") +
    guides(color = FALSE) +
    scale_y_continuous(limits = c(0, 2.5)) +
    scale_fill_manual(values = c(P1 = "#EAF59F",P2 = "#E6CA2E", P3 = "#DE8A0D")) +
    scale_colour_manual(values = c(P1 = "#EAF59F",P2 = "#E6CA2E", P3 = "#DE8A0D")) +
    # facet_wrap(~Site) +
    theme_classic())

(alt_removal <- ggplot(short_ingrowth) +
    aes( x = Core_ID, y = rootmass_bulkdensity, fill = Community, colour = Community ) +
    geom_boxplot(alpha = 0.6) +
    geom_point(aes(y = av_roots, color = Community, shape = Site), 
               position=position_jitterdodge(0.1), size = 3, alpha = 0.9) +
    scale_fill_manual( values = c(Graminoid = "#DBBA03", Mix = "#6A3EC4",Shrub = "#48BD84")) +
    scale_color_manual( values = c(Graminoid = "#DBBA03", Mix = "#6A3EC4", Shrub = "#48BD84") ) +
    labs(x="Core Removal ID", y=bquote("Root Biomass (g"~cm^-3*")")) +
    # facet_wrap(~Site, scales = "free") +
    theme_classic()   )

ggsave(alt_removal, filename = "users/egallois/soilcores/figures/top5_alt_community.png",
       height = 4, width = 9)

(alt_removal <- ggplot(all_rates) +
    aes(x = Community, y = root_rate, fill = Community, colour = Community ) +
    geom_boxplot(alpha = 0.6) +
    geom_point(aes(y = root_rate, color = Community), 
               position=position_jitterdodge(0.1), size = 3, alpha = 0.9) +
    scale_fill_manual( values = c(Graminoid = "#DBBA03", Mix = "#6A3EC4",Shrub = "#48BD84")) +
    scale_color_manual( values = c(Graminoid = "#DBBA03", Mix = "#6A3EC4", Shrub = "#48BD84") ) +
    labs(x="Community", y=bquote("Daily Root Biomass Accumulation (g"~cm^-3*")")) +
    facet_wrap(~Site, scales = "free") +
    theme_classic())


#### 8 - BRMS MODELS ####
## RQ1 model 1: root biomass ~ temp ##

#  scale  the data (square root)
all_ingrowth <- all_ingrowth %>%
  mutate(sqrt_avroot = sqrt(rootmass_bulkdensity))


rq1_biomass_temp <- brms::brm(sqrt_avroot ~ quantilegroup + (1|Site/Subplot),
                              data = all_ingrowth, family = gaussian, chains = 3,
                              iter = 3000, warmup = 1000)

# save model output
save(rq1_biomass_temp, file = "users/egallois/soilcores/models/top5_biomass_temp.RData")

summary(rq1_biomass_temp)
plot(rq1_biomass_temp)
pp_check(rq1_biomass_temp)  # posterior predictive checks
marginal_effects(rq1_biomass_temp)
tab_model(rq1_biomass_temp)

# generate predicted effects 
(ggpred <- ggpredict(rq1_biomass_temp,terms = c("quantilegroup", "Site"))) 

# convert to a dataframe for plotting
ggpred <- as.data.frame(ggpred)



## RQ1 model 2: root rate ~ temp ##
rq1_rate_temp <- brms::brm(root_rate ~ quantilegroup + (1|Site/Subplot),
                           data = all_rates,  family = gaussian, chains = 3,
                           iter = 3000, warmup = 1000)


# save model output
save(rq1_rate_temp, file = "users/egallois/soilcores/models/rate_temp.RData")


summary(rq1_rate_temp)
plot(rq1_rate_temp)
tab_model(rq1_rate_temp)
pp_check(rq1_rate_temp)  # posterior predictive checks
marginal_effects(rq1_rate_temp)

## RQ2 model 1: root biomass ~ comm ##
rq2_biomass_comm <- brms::brm(sqrt_avroot ~ Community +  (1|Site/Subplot),
                              data = all_ingrowth, family = gaussian, chains = 3,
                              iter = 3000, warmup = 1000)


# save model output
save(rq2_biomass_comm, file = "users/egallois/soilcores/models/biomass_comm.RData")

summary(rq2_biomass_comm)
plot(rq2_biomass_comm)
pp_check(rq2_biomass_comm)  # posterior predictive checks
tab_model(rq2_biomass_comm)
marginal_effects(rq2_biomass_comm)


# generate predicted effects 
(ggpred <- ggpredict(rq2_biomass_comm,terms = c("Community", "Site", "Subplot"))) 

# convert to a dataframe for plotting
ggpred <- as.data.frame(ggpred)

# plot model results against raw data
ggpred %>% ggplot(aes(x = x, y = predicted, color = x)) +
  geom_point() + 
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high, color = x), width = 0.7) +
  geom_point(data = all_ingrowth, aes(x = Community,  y = av_roots, colour = Community), alpha = 0.7, size = 5) +
  theme_classic()



## RQ1 model 2: root rate ~ comm ##
rq2_rate_comm <- brms::brm(root_rate ~ Community  +  (1|Site),
                           data = all_rates, family = gaussian, chains = 3,
                           iter = 4000, warmup = 1000) 

# save model output
save(rq2_rate_comm, file = "users/egallois/soilcores/models/rate_comm.RData")

summary(rq2_rate_comm)
plot(rq2_rate_comm)
pp_check(rq2_rate_comm)  # posterior predictive checks
tab_model(rq2_rate_comm)
marginal_effects(rq2_rate_comm)


## ALTERNATIVE: All biomass ##
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

short_ingrowth <- short_ingrowth %>% 
  group_by(Site) %>% 
  mutate(scale_temp = center_scale(mean_temp)) %>%  # apply standardization
  ungroup()  

short_ingrowth <- short_ingrowth %>%
  mutate(sqrt_avroot = sqrt(rootmass_bulkdensity))



biomass_temp_comm <- brms::brm(sqrt_avroot ~ Community + Core_ID + quantilegroup  + (1|Site),
                               data = short_ingrowth, family = gaussian, chains = 3,
                               iter = 3000, warmup = 1000)


biomass_temp <- brms::brm(sqrt_avroot ~ Core_ID + quantilegroup  + (1|Site),
                          data = short_ingrowth, family = gaussian, chains = 3,
                          iter = 3000, warmup = 1000)


biomass_comm <- brms::brm(sqrt_avroot ~ quantilegroup + Community + Core_ID + (1|Site),
                          data = short_ingrowth, family = gaussian, chains = 3,
                          iter = 3000, warmup = 1000)

(loo1 <- loo(biomass_temp_comm))
(loo2 <- loo(biomass_temp))
(loo3 <- loo(biomass_comm))

# compare both models
loo_compare(loo1, loo2, loo3)


# save model output
save(biomass_temp_comm, file = "users/egallois/soilcores/models/top5_biomass_temp_comm_quantile.RData")

print(biomass_temp_comm, digits = 4)
plot(biomass_temp_comm)
pp_check(biomass_temp_comm)  # posterior predictive checks
tab_model(biomass_temp_comm, digits = 4)
marginal_effects(biomass_temp_comm)


# generate predicted effects 
(ggpred <- ggpredict(biomass_temp_comm,terms = c("center_temp", "Community", "Core_ID", "Site"))) 

# convert to a dataframe for plotting
ggpred <- as.data.frame(ggpred)




## ALTERNATIVE: All rate ## 
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

all_rates <- all_rates %>% 
  group_by(Site) %>% 
  mutate(center_temp = center_scale(mean_temp)) %>% 
  ungroup() 



rate_temp_comm <-brms::brm(root_rate ~ Community  + quantilegroup + (1|Site),
                           data = all_rates, family = gaussian, chains = 3,
                           iter = 4000, warmup = 1000) 

# save model output
save(rate_temp_comm, file = "users/egallois/soilcores/models/top5_rate_temp_comm_quantile.RData")

print(rate_temp_comm, digits = 5)
plot(rate_temp_comm)
pp_check(rate_temp_comm)  # posterior predictive checks
tab_model(rate_temp_comm, digits = 4)
marginal_effects(rate_temp_comm)


### Syncrhony models ###
## centering function adapted from Erica Zaja's and Madi Anderson's 
#  centering with 'scale()'
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

all_ingrowth$async[all_ingrowth$async < 0] <- NA       # Replace negative values by NA


all_rates <- all_rates %>% 
  group_by(Site) %>% 
  mutate(center_async = center_scale(async)) %>% 
  ungroup() 

#all_rates$center_async <- center_scale(all_rates$async)
#all_rates$sqrt_async <- sqrt(all_rates$async)

# remove duplicate summer temps

async_temp_comm <- brms::brm(center_async ~ Community + quantilegroup + (1|Site),
                             data = all_rates, family = gaussian, chains = 3,
                             iter = 4000, warmup = 1000)




# save model output
save(async_temp_comm, file = "users/egallois/soilcores/models/top5_async_temp_comm_quantile.RData")

summary(async_temp_comm)
plot(async_temp_comm)
pp_check(async_temp_comm)  # posterior predictive checks
tab_model(async_temp_comm, digits = 4)
marginal_effects(async_temp_comm)

all_rates %>%
  ggplot() +
  aes(x = Site, y = center_async, fill = Site) +
  geom_boxplot() +
  labs(x = "Site", y = "% Root Growth at date of 100% Greening") +
  scale_fill_hue(direction = 1) +
  theme_classic()

#### 9 - LOAD MODELS ####
load("~/TeamShrubHub/users/egallois/soilcores/models/async_temp_comm_quantile.RData")
load("~/TeamShrubHub/users/egallois/soilcores/models/biomass_temp_comm_quantile.RData")
load("~/TeamShrubHub/users/egallois/soilcores/models/rate_temp_comm_quantile.RData")

# get tables
tab_model(async_temp_comm, digits = 4)
tab_model(biomass_temp_comm, digits = 4)
tab_model(rate_temp_comm, digits = 4)


#### 10 - Make final biomass plot ####
# get predicted dataframes
# generate predicted effects 
(biomass_pred <- ggpredict(biomass_temp_comm,terms = c("Community", "Core_ID", "Site"))) 

# convert to a dataframe for plotting
biomass_pred <- as.data.frame(biomass_pred) %>%
  rename(Core_ID = group) %>% 
  rename(Community = x) %>% 
  rename(av_roots = predicted)



# try to recreate rainplot
# We will use a function by Ben Marwick to get flat violin
# This code loads the function in the working environment
source("https://gist.githubusercontent.com/benmarwick/2a1bb0133ff568cbe28d/raw/fb53bd97121f7f9ce947837ef1a4c65a73bffb3f/geom_flat_violin.R")

(biomass_rainfall <- 
    ggplot(data = short_ingrowth, 
           aes(x = Community, y = av_roots, fill = Core_ID)) +
    geom_point(aes(y = av_roots, color = Core_ID), 
               position = position_jitter(width = 0.2), size = 1, alpha = 0.8) +
    geom_boxplot(aes(x = Community, y = av_roots, 
                     fill = Core_ID), 
                 width = 0.4, size = 0.3, position=position_dodge(width=0.5), alpha = 0.7) +
    geom_flat_violin(position = position_nudge(x = 0.3, y = 0), 
                     alpha = 0.6, 
                     aes(y = av_roots, color = Core_ID, stat="identity")) +
    labs(y =bquote("Root Biomass (g"~cm^-3*")"), x = "\nCommunity Type") +
    guides(color = FALSE) +
    scale_y_continuous(limits = c(0, 2.5)) +
    scale_fill_manual(values = c(P1 = "#EAF59F",P2 = "#E6CA2E", P3 = "#DE8A0D")) +
    scale_colour_manual(values = c(P1 = "#EAF59F",P2 = "#E6CA2E", P3 = "#DE8A0D")) +
    theme_classic()) 

ggsave(biomass_rainfall, filename = "users/egallois/soilcores/figures/biomass_community.png",
       height = 4, width = 9)



#### 11 - Make final microclimate panel ####
# generate predicted effects 
(temp_pred <- ggpredict(biomass_temp_comm,terms = c("quantilegroup", "Site", "Core_ID"))) 

# convert to a dataframe for plotting
temp_pred <- as.data.frame(temp_pred) %>%
  filter(facet %in% "P3") %>%
  rename(Site = group) %>% 
  rename(quantilegroup = x) %>% 
  rename(av_roots = predicted)



# does biomass vary by microclimate?
(micro_mass <-ggplot(data=subset(short_ingrowth, !is.na(quantilegroup))) +
    aes(x = quantilegroup, y = av_roots, fill = quantilegroup, color = quantilegroup) +
    #geom_boxplot(alpha = 0.3) +
    geom_point(aes(y = av_roots, color = quantilegroup), 
               position = position_jitter(width = 0.2), size = 1, alpha = 0.3) +
    geom_crossbar(data = temp_pred, aes(x = quantilegroup, ymin = conf.low^2, ymax = conf.high^2, 
                                        color = quantilegroup), 
                  width = 0.7, alpha = 0.2, size = 1,  position=position_dodge(width=0.5),
                  fatten = 1, linetype = 2) +
    scale_fill_viridis_d(option = "viridis", direction = 1) +
    scale_color_viridis_d(option = "viridis", direction = 1) +
    labs(y =bquote("Root Biomass (g"~cm^3*")"),  x = "\nClimate Quantile (1 = coldest, 4 = warmest)", 
         title = "(a)") +
    #facet_wrap(~Site) +
    theme_classic())
(micro_mass <- micro_mass  + theme(legend.position = "none"))


# generate predicted effects 
(rate_pred <- ggpredict(rate_temp_comm,terms = c("quantilegroup", "Site"))) 

# convert to a dataframe for plotting
rate_pred <- as.data.frame(rate_pred) %>%
  rename(Site = group) %>% 
  rename(quantilegroup = x) %>% 
  rename(root_rate = predicted)

# does asynchrony vary by microclimate?
# do rates vary by microclimate?
(micro_rate <- ggplot(data=subset(all_rates, !is.na(root_rate))) +
    aes(x = quantilegroup, y = root_rate, colour = quantilegroup, fill = quantilegroup) +
    geom_point(position = position_jitter(width = 0.2), size = 1, alpha = 0.3)+
    scale_color_viridis_d(option = "viridis", direction = 1) +
    scale_fill_viridis_d(option = "viridis", direction = 1) +
    geom_crossbar(data = rate_pred, aes(x = quantilegroup, ymin = conf.low, ymax = conf.high, 
                                        color = quantilegroup), 
                  width = 0.7, alpha = 0.2, size = 1, linetype = 2, position=position_dodge(width=0.5),
                  fatten = 1) +
    labs(y =bquote("Daily Root Biomass Accumulation (g"~cm^3*")"),  x = "\nClimate Quantile (1 = coldest, 4 = warmest)", 
         title = "(b)") +
    #facet_wrap(~Site) +
    theme_classic())

(micro_rate <- micro_rate  + theme(legend.position = "none")) 



micro_compare <- grid.arrange(micro_mass,
                              micro_rate,nrow=1)



ggsave(micro_compare, filename = "users/egallois/soilcores/figures/quantile_brms.png",
       height = 4, width = 7)

#### 12 - Quick figure on potential microclimate vs community co-variation ####
library(ggridges)

all_ingrowth %>%
  filter(!(NOTES %in% "slightly switches viewshed on 31st july\xca") | is.na(NOTES)) %>%
  ggplot() +
  aes(x = summer_temp, fill = Community) +
  #geom_density(adjust = 1L, alpha = 0.4) +
  geom_density_ridges(adjust = 1L,alpha = 0.4,
                      jittered_points = TRUE, point_alpha=0.2) +
  scale_fill_manual(
    values = c(Graminoid = "#EFCE16",
               Mix = "#7A60C0",
               Shrub = "#53E0B1")
  ) +
  labs(x = "Average July-Aug Surface Temperature (\u00B0C)", y = "Density") +
  theme_classic() 


(p2 <-ggplot(all_ingrowth, mapping = aes(y = Site, x = summer_temp, fill = Community, col = Community)) +
    geom_density_ridges(alpha = 0.4, panel_scaling = TRUE,
                        jittered_points = TRUE, point_alpha=0.8) +
    scale_fill_manual(
      values = c(Graminoid = "#EFCE16",
                 Mix = "#7A60C0",
                 Shrub = "#53E0B1")
    ) +
    scale_color_manual(
      values = c(Graminoid = "#EFCE16",
                 Mix = "#7A60C0",
                 Shrub = "#53E0B1")
    ) +
    labs(x = "Average July-Aug Surface Temperature (\u00B0C)", y = "Density") +
    theme_ridges())



cor(all_ingrowth$Community,all_ingrowth$summer_temp,  method = c("spearman"), use = "complete.obs")


comm_anova <- aov(Community ~ quantilegroup, data = all_rates)
summary(comm_anova)

ggplot(all_rates) + geom_boxplot(aes(Community, quantilegroup)) 



str(all_ingrowth)
