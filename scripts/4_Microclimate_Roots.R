### 5. Get july average temp data, match microclimate to phenocams/root clusters, plot microclimate vs growth rate  ###
### Date: 10th May 2023 ###

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
tomst <- read_csv("data/all_sites_mean_daily_temp.csv")

# load root data
roots <- read_csv("data/rootweights_phenocams.csv")  

# load site key
site_key <- read_csv("data/tomst_key.csv") 

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
# group by logger ID and calculate summer means
summer_surface_temp <- tomst %>%
  subset(Date >= "2022-06-01" & Date <= "2022-08-31") %>%
  filter(mean_temp >= -90) %>%  
  group_by(SerialID) %>%
  mutate(summer_temp = mean(mean_temp, na.rm = TRUE)) %>%  
  ungroup() %>% 
  group_by(site) %>%
  mutate(mean_prc = mean(mean_temp, na.rm = TRUE)) %>%  
  ungroup()

# assign quantiles for surface temperature grouped by site
summer_surface_temp <- summer_surface_temp %>%
  group_by(site) %>% 
  mutate(quantilegroup = ntile(summer_temp, 4)) %>% 
  ungroup()

# remove SerialID 20907917 which had logging issues in cairn
summer_surface_temp <- summer_surface_temp %>% 
  filter(SerialID != "20907917")

# make logger ID a class 
summer_surface_temp$SerialID <- as.factor(summer_surface_temp$SerialID)
summer_soil_temp$SerialID <- as.factor(summer_soil_temp$SerialID)

tomst_table <- summer_surface_temp %>% 
  group_by(site) %>% 
  summarise(mean_prc = mean(summer_temp,na.rm = TRUE)) 



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
  summarise(mean_prc = mean(mean_temp,na.rm = TRUE)) 


#### 5 - CALCULATE ROOT GROWTH RATES ####
# calculate belowground growing season length for each subplot
all_ingrowth <- all_ingrowth %>% 
                group_by(Subplot) %>% 
                mutate(growing_ssn = diff(range(DOY)))

only_32 <- all_ingrowth %>% 
            filter(Core_ID %in% c("P2", "P3")) %>% 
            group_by(Subplot) %>% 
             mutate(growing_ssn = diff(range(DOY)))

only_21 <- all_ingrowth %>% 
  filter(Core_ID %in% c("P1", "P2")) %>% 
  group_by(Subplot) %>% 
  mutate(growing_ssn = diff(range(DOY)))


all_ingrowth <- all_ingrowth %>% 
  mutate(unique_ID = paste(Site, ' ', Subplot, ' ', Core_ID)) %>% 
  mutate(plot_ID = paste(Site, ' ', Subplot)) %>% 
  group_by(unique_ID) %>%
  mutate(av_roots = mean(rootmass_bulkdensity)) %>% 
  ungroup()

only_32 <- only_32 %>% 
  mutate(unique_ID = paste(Site, ' ', Subplot, ' ', Core_ID)) %>% 
  mutate(plot_ID = paste(Site, ' ', Subplot)) %>% 
  group_by(unique_ID) %>%
  mutate(av_roots = mean(rootmass_bulkdensity)) %>% 
  ungroup()


only_21 <- only_21 %>% 
  mutate(unique_ID = paste(Site, ' ', Subplot, ' ', Core_ID)) %>% 
  mutate(plot_ID = paste(Site, ' ', Subplot)) %>% 
  group_by(unique_ID) %>%
  mutate(av_roots = mean(rootmass_bulkdensity)) %>% 
  ungroup()



# only keep relevant columns
root_only <- all_ingrowth %>%
  dplyr::select(Site, Subplot, Core_ID,Phenocam, Community,
         Snowmelt, av_roots,growing_ssn, mean_temp,summer_temp, async,quantilegroup)

only_21 <- only_21 %>%
  dplyr::select(Site, Subplot, Core_ID,Phenocam, Community,
                Snowmelt, av_roots,growing_ssn, mean_temp, summer_temp, quantilegroup)

only_32 <- only_32 %>%
  dplyr::select(Site, Subplot, Core_ID,Phenocam, Community,
                Snowmelt, av_roots,growing_ssn, mean_temp, summer_temp, quantilegroup)

# remove duplicate averaged root values
root_only <- root_only[!duplicated(root_only$av_roots), ]
only_21 <- only_21[!duplicated(only_21$av_roots), ]
only_32 <- only_32[!duplicated(only_32$av_roots), ]


# filter with only cairngorms
cairn_only <- root_only %>% filter(Site %in% "Cairngorms" | is.na(Site))

# all without cairngorms
no_cairn <- root_only %>% subset(Site != 'Cairngorms')

# make av_root columns wider by phenology type
wide_cairn <- cairn_only %>% 
  pivot_wider(names_from = Core_ID, values_from = av_roots)

wide_three <- no_cairn %>% 
  pivot_wider(names_from = Core_ID, values_from = av_roots)

wide_32 <- only_32 %>% 
  pivot_wider(names_from = Core_ID, values_from = av_roots)

wide_21 <- only_21 %>% 
  pivot_wider(names_from = Core_ID, values_from = av_roots)

# rate calculations
wide_three <- wide_three %>% 
              mutate(root_rate = ((P3-P1)/growing_ssn))




wide_32 <- wide_32 %>% 
  mutate(root_rate = ((P3-P2)/growing_ssn))

wide_21 <- wide_21 %>% 
  mutate(root_rate = ((P2-P1)/growing_ssn))

wide_cairn <- wide_cairn %>% 
  mutate(root_rate = ((P2-P1)/growing_ssn))

# add empty P3 column to cairngorms data
wide_cairn <- wide_cairn %>% mutate(P3 = "0")

# because the P1 was 0 for INT2 cairngorms, root_rate needs to match P2
wide_cairn <- wide_cairn[-7,]
wide_cairn[wide_cairn$Subplot == 'INT2', 'P1'] <- 0.2457131

# reorder columns prior to bind
wide_cairn <- wide_cairn %>%
  dplyr::select(Site, Subplot,Phenocam, Community,
         Snowmelt, growing_ssn, mean_temp, summer_temp, quantilegroup,P1, P2, P3, root_rate, async)

wide_three$P3 <- as.numeric(wide_three$P3)
wide_32$P3 <- as.numeric(wide_32$P3)
wide_21$P3 <- as.numeric(wide_21$P3)

wide_32$segment <- "P2-P3"
wide_21$segment <- "P1-P2"


colnames(wide_32)[colnames(wide_32) == "P2"] <- "PA"
colnames(wide_32)[colnames(wide_32) == "P3"] <- "PB"
colnames(wide_21)[colnames(wide_21) == "P1"] <- "PA"
colnames(wide_21)[colnames(wide_21) == "P2"] <- "PB"



#rbind to get merged
all_rates <- rbind(wide_cairn, wide_three)
sub_rates <- rbind(wide_21, wide_32)


# plot subrates
ggplot(sub_rates) +
  aes(x = segment, y = root_rate, fill = Community, color = Community) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(
    values = c(Graminoid = "#EFCE16",
               Mix = "#7A60C0",
               Shrub = "#53E0B1")) +
  scale_color_manual(
    values = c(Graminoid = "#EFCE16",
               Mix = "#7A60C0",
               Shrub = "#53E0B1")) +
  labs(y = "Daily Root Biomass Growth (per g/cm3)",  x = "Community") +
  theme_minimal()



#### 6 - MICROCLIMATE VS GROWTH RATES and BIOMASS ####

# quantilegroup as a factor
all_rates$quantilegroup <- as.factor(all_rates$quantilegroup)
all_ingrowth$quantilegroup <- as.factor(all_ingrowth$quantilegroup)

# remove duplicate summer temps
short_ingrowth <- all_ingrowth[!duplicated(all_ingrowth$unique_ID), ]



#### 7 - COMMUNITY VS GROWTH RATES ####

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


(alt_removal <- ggplot(short_ingrowth) +
  aes( x = Core_ID, y = av_roots, fill = Community, colour = Community ) +
  geom_boxplot(alpha = 0.6) +
  geom_point(aes(y = av_roots, color = Community, shape = Site), 
             position=position_jitterdodge(0.1), size = 3, alpha = 0.9) +
  scale_fill_manual( values = c(Graminoid = "#DBBA03", Mix = "#6A3EC4",Shrub = "#48BD84")) +
  scale_color_manual( values = c(Graminoid = "#DBBA03", Mix = "#6A3EC4", Shrub = "#48BD84") ) +
  labs(x="Core Removal ID", y=bquote("Root Biomass (g"~cm^-3*")")) +
 # facet_wrap(~Site, scales = "free") +
  theme_classic() +
    theme(
    axis.text = element_text(size = 14),       
    axis.title = element_text(size = 16),     
    legend.text = element_text(size = 14),    
    legend.title = element_text(size = 16)     
  )  )

ggsave(alt_removal, filename = "figures/alt_community.png",
       height = 4, width = 9)

(alt_rates <- ggplot(all_rates) +
    aes(x = Community, y = root_rate, fill = Community, colour = Community ) +
    geom_boxplot(alpha = 0.6) +
    geom_point(aes(y = root_rate, color = Community), 
               position=position_jitterdodge(0.1), size = 3, alpha = 0.9) +
    scale_fill_manual( values = c(Graminoid = "#DBBA03", Mix = "#6A3EC4",Shrub = "#48BD84")) +
    scale_color_manual( values = c(Graminoid = "#DBBA03", Mix = "#6A3EC4", Shrub = "#48BD84") ) +
    labs(x="Community", y=bquote("Daily Root Biomass Accumulation (g"~cm^-3*")")) +
    facet_wrap(~Site, scales = "free") +
    theme_classic()+
    theme(
      axis.text = element_text(size = 14),      # Font size for axis text (tick labels)
      axis.title = element_text(size = 16),     # Font size for axis titles
      legend.text = element_text(size = 14),    # Font size for legend text
      legend.title = element_text(size = 16),
      strip.text = element_text(size = 14) ,# Font size for legend title
      legend.position = "none"                  # Remove the legend
    )# Font size for legend title
    )
  
ggsave(alt_rates, filename = "figures/alt_community_rate.png",
       height = 6, width = 10)

#### 8 - BRMS MODELS ####

## All biomass ----
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

short_ingrowth <- short_ingrowth %>% 
                group_by(Site) %>% 
                mutate(scale_temp = center_scale(summer_temp)) %>%  # apply standardization
                ungroup()  

short_ingrowth <- short_ingrowth %>%
  mutate(sqrt_avroot = sqrt(av_roots))

short_ingrowth_ord <- short_ingrowth %>% mutate(sqrt_avroot = sqrt(av_roots),
       quantilegroup = factor(quantilegroup, ordered = TRUE),
       Core_ID = factor(Core_ID, ordered = TRUE))


#biomass_temp_comm <- brms::brm(sqrt_avroot ~ Community + Core_ID + quantilegroup  + (1|Site),
                             # data = short_ingrowth, family = gaussian, chains = 3,
                             # iter = 3000, warmup = 1000)

# Final model with right-skew normal and interaction term
biomass_temp_comm_skew <- brms::brm(av_roots ~ Community*Core_ID + quantilegroup + (1|Site), 
                                    data = short_ingrowth, 
                                    family = skew_normal(), 
                                    chains = 3, iter = 3000, warmup = 1000)


tab_model(biomass_temp_comm)
tab_model(biomass_temp_comm_skew)

biomass_temp_continuous <- brms::brm(av_roots ~ Community*Core_ID + scale_temp + (1|Site),
                               data = short_ingrowth, family = skew_normal(), 
                               chains = 3,
                               iter = 3000, warmup = 1000)

# save model output
save(biomass_temp_comm, file = "models/biomass_temp_comm_quantile.RData")
save(biomass_temp_comm_skew, file = "models/biomass_temp_comm_skew_quantile.RData")
save(biomass_temp_continuous, file = "models/biomass_temp_continuous.RData")

print(biomass_temp_comm_skew, digits = 4)
plot(biomass_temp_comm_skew)
pp_check(biomass_temp_comm_skew)  # posterior predictive checks
tab_model(biomass_temp_comm_skew, digits = 4)
marginal_effects(biomass_temp_comm_skew)
pp_check(biomass_temp_continuous)  # posterior predictive checks
tab_model(biomass_temp_continuous)

##  All rate ----
center_scale <- function(x) {
  scale(x, scale = FALSE)
}

all_rates <- all_rates %>% 
  group_by(Site) %>% 
  mutate(scale_temp = center_scale(summer_temp)) %>% 
  ungroup() 

rate_temp_comm <-brms::brm(root_rate ~ Community  + quantilegroup + (1|Site),
                           data = all_rates, family = gaussian, chains = 3,
                           iter = 4000, warmup = 1000) 

rate_temp_continuous  <-brms::brm(root_rate ~ Community  + scale_temp + (1|Site),
                           data = all_rates, family = gaussian, chains = 3,
                           iter = 4000, warmup = 1000) 
# save model output
save(rate_temp_comm, file = "models/rate_temp_comm_quantile.RData")
save(rate_temp_continuous, file = "models/rate_temp_continuous_quantile.RData")

print(rate_temp_comm, digits = 5)
plot(rate_temp_comm)
pp_check(rate_temp_comm)  # posterior predictive checks
tab_model(rate_temp_comm, digits = 4)
marginal_effects(rate_temp_comm)
pp_check(rate_temp_continuous)  # posterior predictive checks
tab_model(rate_temp_continuous, digits = 4)
marginal_effects(rate_temp_continuous)

# All synchrony ----

center_scale <- function(x) {
  scale(x, scale = FALSE)
}

all_ingrowth$async[all_ingrowth$async < 0] <- NA       # Replace negative values by NA


all_rates <- all_rates %>% 
  group_by(Site) %>% 
  mutate(center_async = center_scale(async)) %>% 
  mutate(scale_temp = center_scale(summer_temp)) %>% 
  ungroup() 


async_temp_comm <- brms::brm(center_async ~ Community + quantilegroup + (1|Site),
                               data = all_rates, family = gaussian, chains = 3,
                             iter = 4000, warmup = 1000)


async_temp_continuous <- brms::brm(center_async ~ Community + scale_temp + (1|Site),
                             data = all_rates, family = gaussian, chains = 3,
                             iter = 4000, warmup = 1000)


# save model output
save(async_temp_comm, file = "models/async_temp_comm_quantile.RData")
save(async_temp_continuous, file = "models/async_temp_cont_quantile.RData")

summary(async_temp_comm)
plot(async_temp_comm)
pp_check(async_temp_comm)  # posterior predictive checks
tab_model(async_temp_comm, digits = 4)
marginal_effects(async_temp_comm)
pp_check(async_temp_continuous)  # posterior predictive checks
tab_model(async_temp_continuous, digits = 4)
marginal_effects(async_temp_continuous)

all_rates %>%
  ggplot() +
  aes(x = Site, y = center_async, fill = Site) +
  geom_boxplot() +
  labs(x = "Site", y = "% Root Growth at date of 100% Greening") +
  scale_fill_hue(direction = 1) +
  theme_classic()

#### 9 - LOAD MODELS ####
load("models/async_temp_comm_quantile.RData")
load("models/biomass_temp_comm_skew_quantile.RData")
load("models/rate_temp_comm_quantile.RData")

# get tables
tab_model(async_temp_comm, digits = 4)
tab_model(biomass_temp_comm_skew, digits = 4)
tab_model(rate_temp_comm, digits = 4)


#### 10 - Make final biomass plot ####
# get predicted dataframes
# generate predicted effects 
(biomass_pred <- ggpredict(biomass_temp_comm_skew,terms = c("Community", "Core_ID", "Site"))) 

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

ggsave(biomass_rainfall, filename = "figures/biomass_community.png",
       height = 4, width = 9)



#### 11 - Make final microclimate panel ####
# generate predicted effects 
(temp_pred <- ggpredict(biomass_temp_comm_skew,terms = c("quantilegroup", "Site", "Core_ID"))) 

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
    theme_classic() +
    theme(
      axis.text = element_text(size = 14),    
      axis.title = element_text(size = 16),    
      legend.text = element_text(size = 14),    
      legend.title = element_text(size = 16)))
(micro_mass <- micro_mass  + theme(legend.position = "none"))


# generate predicted effects 
(rate_pred <- ggpredict(rate_temp_comm,terms = c("quantilegroup", "Site"))) 

# convert to a dataframe for plotting
rate_pred <- as.data.frame(rate_pred) %>%
  rename(Site = group) %>% 
  rename(quantilegroup = x) %>% 
  rename(root_rate = predicted) 
 
all_rates_clean <- subset(all_rates,!is.na(quantilegroup))

(micro_rate <- ggplot(data=subset(all_rates_clean, !is.na(root_rate), !is.na(quantilegroup))) +
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
    theme_classic() +
    theme(
      axis.text = element_text(size = 14),      
      axis.title = element_text(size = 16),     
      legend.text = element_text(size = 14),    
      legend.title = element_text(size = 16)))

(micro_rate <- micro_rate  + theme(legend.position = "none")) 



micro_compare <- grid.arrange(micro_mass,
                              micro_rate,nrow=1)



ggsave(micro_compare, filename = "figures/quantile_brms.png",
       height = 5, width = 11)

#### 12 - Quick figure on potential microclimate vs community co-variation ####
library(ggridges)

all_ingrowth %>%
  #filter(!(NOTES %in% "slightly switches viewshed on 31st july\xca") | is.na(NOTES)) %>%
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
                        jittered_points = FALSE, point_alpha=0.8) +
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
    labs(x = "Average June-Aug Surface Temperature (\u00B0C)", y = "Density") +
    theme_ridges())

all_ingrowth <- subset(all_ingrowth, !is.na(quantilegroup))

(p2 <-ggplot(all_ingrowth, mapping = aes(y = Site, x = quantilegroup, fill = Community, col = Community)) +
    geom_density_ridges(alpha = 0.4, panel_scaling = TRUE,
                        jittered_points = FALSE, point_alpha=0.8) +
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
    labs(x = "Climate Quantiles (1 = coldest, 4 = warmest)", y = "Density") +
    theme_ridges())



