### 4. Pull in root weight data whenever it comes in and analyse depth values ###
### Elise Gallois, elise.gallois94@gmail.com ###
### Date: 18th Nov 2022 ###

#### 1 - LOAD PACKAGES  ##### 

library(dplyr)
library(readr)
library(tidyverse) 
library(esquisse)
library(viridis)
library(ggpubr)
library(forcats)
library(FactoMineR)
library(factoextra)
library(corrplot)
library(patchwork)
library(gridExtra)


#### 2 - LOAD  ROOT PHENOLOGY  DATA ####
roots <- read_csv("data/190623_rootmass.csv")

# rename columns
roots$increment <- as.factor(roots$`Depth Increment`)
roots$Depth_Type <- as.factor(roots$Depth_Type)

# rename
roots$increment  <- recode(roots$increment, '5-12.5' = '5-15')
roots$increment  <- recode(roots$increment, '5-9.7' = '5-10')
roots$increment  <- recode(roots$increment, '05-10' = '5-10')
roots$increment  <- recode(roots$increment, '05-15' = '5-15')

# reorder factors
levels(roots$increment)
roots$increment <- factor(roots$increment, levels = c("25-30","15-25", "15-20", "5-15", "5-10", "0-5"))  
roots$Depth_Type <- factor(roots$Depth_Type, levels = c("Bottom", "Middle2", "Middle", "Top"))  

roots$scaling <- (roots$`Subsection Dried Weight (g)`/roots$`Subsection Wet Weight (g)`)*roots$`Depth Wet Weight (g)`
roots$drybiomass <- roots$`Root Dry Biomass Total (g)`/roots$scaling

# stacked bar of root weight by increment
P3_inc_plot <- roots %>%
  filter(Core_ID %in% c("P3")) %>%
  ggplot() +
  aes(x = Subplot, fill = increment, weight = rootmass_bulkdensity) +
  labs(x = "Kluane Plateau Phenocam #", y = "Root biomass g/cm3 of bulk density ") +
  geom_bar() +
  scale_fill_hue() +
  theme_classic() +
  facet_wrap(vars(Site), scales = "free_x")


P3_inc_plot + scale_y_reverse()


P2_inc_plot <- roots %>%
  #filter(Site %in% "Cairngorms") %>%
  filter(Core_ID %in% c("P2")) %>%
  ggplot() +
  aes(x = Subplot, fill = increment, weight = rootmass_bulkdensity) +
  labs(x = "Plot ID", y = "Root biomass g/cm3 of bulk density ") +
  geom_bar() +
  scale_fill_hue() +
  theme_classic() +
  facet_wrap(vars(Site), scales = "free_x")



P2_inc_plot + scale_y_reverse()


P1_inc_plot <- roots %>%
  filter(Site %in% "Cairngorms") %>%
  filter(Core_ID %in% c("P1")) %>%
  ggplot() +
  aes(x = Subplot, fill = increment, weight = rootmass_bulkdensity) +
  labs(x = "Plot ID", y = "Root biomass g/cm2 of bulk density ") +
  geom_bar() +
  scale_fill_hue() +
  theme_classic() +
  facet_wrap(vars(Core_ID), scales = "free_x")


P1_inc_plot + scale_y_reverse()

#### 3 - PCA  ####
# choose active -aka numeric- columns
#reorder data frame so ID comes first & remove redundant columns
colnames(roots)

# summarise root morphology types
roots_morpho <- roots %>% 
  mutate(shrub = Black_branch_biomass +
           Brown_branch_biomass +
           Branched_Biomass + 
           White_branched_biomass +
           Fine_branched_biomass +
           Yellow_Branched +
           thicker_branched +
           `light branched` +
           woody_branched +
           Big_Brown +
           Big_Branched +
           Sticky_biomass) %>% 
  mutate(graminoid =   Thick_biomass +
           Skinny_white_biomass +
           White_chunky +
           flat +
           Chunky_white_Branched +
           `Thicker_orangey branched` +
           `Chunky orange branched` +
           Flat_branched + 
           Black_Chunky)  %>% 
  mutate(sprouts_seedlings = Sprout + 
           Seed_biomass)


colnames(roots_morpho)
roots_long <- roots_morpho[,c(1,2,3,4,5,8,9,16,21,23,53,54,55)]
colnames(roots_long)

# create dataframes with and without relevant variables - and with no NAs
root.active <- roots_long[9:13]
root.active <- na.omit(root.active, na.action = "omit")
root.inactive <- na.omit(roots_long, na.action = "omit")


# get variables factor map
soil.pca <- PCA(root.active, graph = TRUE)

# generate scree plot - what is relative importance of each dimension?
fviz_eig(soil.pca, addlabels = TRUE, ylim = c(0, 50)) + 
  theme_classic(base_size = 16) 

var <- get_pca_var(soil.pca)

# Coordinates
head(var$coord)

# Cos2: quality on the factor map
head(var$cos2)

# Contributions to the principal components
head(var$contrib)

# Coordinates of variables
head(var$coord, 4)

# Examine dimensions 1 and 2
fviz_pca_var(soil.pca, col.var = "black") 

#correlation plot - weighting of variable by dimension
(correlation_subplot <- corrplot(var$cos2,tl.col = 'black',is.corr=TRUE)) + ggtitle("d)")  


# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(soil.pca, choice = "var", axes = 1:2)

# Color by cos2 values: quality on the factor map
fviz_pca_var(soil.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)

# Change the transparency by cos2 values
fviz_pca_var(soil.pca, alpha.var = "cos2")
head(var$contrib, 4)

# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(281)
soil.km <- kmeans(var$coord, centers = 3, nstart = 27)
grp <- as.factor(soil.km$cluster)

# Color variables by groups
fviz_pca_var(soil.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster",
             repel = TRUE)

#look at individual contributions
ind <- get_pca_ind(soil.pca)
ind

# Coordinates of individuals
head(ind$coord)

# Quality of individuals
head(ind$cos2)

# Contributions of individuals
head(ind$contrib)
fviz_pca_ind(soil.pca)
fviz_pca_ind(soil.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)

fviz_pca_ind(soil.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)


#contribution of individuals
fviz_cos2(soil.pca, choice = "ind")

# Total contribution on PC1 and PC2
fviz_contrib(soil.pca, choice = "ind", axes = 1:2)

# Create a random continuous variable of length 281,
# Same length as the number of active individuals in the PCA
set.seed(281)
my.cont.var <- rnorm(281)

# Color individuals by the continuous variable
fviz_pca_ind(soil.pca, col.ind = my.cont.var,
             gradient.cols = NULL,
             legend.title = "Cont.Var")

#colour by groups
head(roots.active, 8)
colnames(root.inactive)
# PCA - cluster by Plot?
root.inactive$Site <- as.factor(root.inactive$Site)
root.inactive$Depth_Increment <- as.factor(root.inactive$`Depth Increment`)
root.inactive$Core_ID <- as.factor(root.inactive$Core_ID)
root.inactive$Community <- as.factor(root.inactive$Community)


# PCA - cluster by depth?
fviz_pca_ind(soil.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = root.inactive$Depth_Increment , # color by groups
             palette = "pal",
             addEllipses = TRUE, ellipse.type = "confidence",# Concentration ellipses,
             legend.title = "Groups")

# PCA - cluster by timestamp?
head(roots.active, 3)
fviz_pca_ind(soil.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = root.inactive$Core_ID , # color by groups
             palette = "pal",
             ellipse.type = "confidence",# Concentration ellipses,
             legend.title = "Groups")

# PCA - cluster by community?
head(roots.active, 3)
fviz_pca_ind(soil.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = root.inactive$Community , # color by groups
             palette = "pal",
             ellipse.type = "confidence",# Concentration ellipses,
             legend.title = "Groups")


# PCA - cluster by site?
head(roots.active, 3)
fviz_pca_ind(soil.pca,
             habillage=root.inactive$Site,
             #geom.ind = "point", # show points only (nbut not "text")
             #col.ind = root.inactive$Site , # color by groups
             addEllipses=TRUE, ellipse.type = "confidence",# Concentration ellipses,
             palette = "Dark2",
             legend.title = "Groups")

fviz_pca_ind(soil.pca, axes = c(2,3),label="none", habillage=root.inactive$Core_ID,
             addEllipses=TRUE, ellipse.level=0.5, palette = "Dark2")

fviz_pca_ind(soil.pca,axes = c(2,3), label="none", habillage=root.inactive$Community,
             addEllipses=TRUE, ellipse.level=0.5, palette = "Dark2")

fviz_pca_ind(soil.pca,axes = c(1,2), label="none", habillage=root.inactive$Site,
             addEllipses=TRUE, ellipse.level=0.5, palette = "Dark2")

(biplotdim12 <- fviz_pca_biplot(soil.pca, axes = c(1,2),
                                # Individuals
                                geom.ind = "point",
                                fill.ind  = root.inactive$Core_ID, col.var="black",
                                col.ind  = root.inactive$Core_ID, 
                                pointshape = 21, pointsize = 2,alpha = 0.9,
                                #ellipse.type = "confidence",
                                #ellipse.alpha = 0.6,
                                palette = "viridis",
                                repel = TRUE) +
    theme_classic(base_size = 16) +
    scale_fill_viridis(discrete=FALSE, guide=FALSE,option = "plasma",root.inactive$Core_ID) +
    scale_color_viridis(discrete=FALSE, guide=FALSE,option = "plasma") +
    guides(fill=guide_legend(title="Core Type"))+
    guides(color = FALSE))

(biplotdim23 <- fviz_pca_biplot(soil.pca, axes = c(2,3),
                                # Individuals
                                geom.ind = "point",
                                # fill.ind  = root.inactive$Core_ID, col.var="black",
                                # col.ind  = root.inactive$Core_ID, 
                                pointshape = 21, pointsize = 2,alpha = 0.9,
                                #addEllipses = TRUE, ellipse.type = "confidence",
                                ellipse.alpha = 0.6,
                                palettepalette = "viridis",
                                repel = TRUE) +
    theme_classic(base_size = 16) +
    scale_fill_viridis(discrete=TRUE, guide=FALSE,option = "plasma",root.inactive$Core_ID) +
    scale_color_viridis(discrete=TRUE, guide=FALSE,option = "plasma") +
    guides(fill=guide_legend(title="Core Type"))+
    guides(color = FALSE))




(simple_biplot <- fviz_pca_biplot(soil.pca, 
                                  # Individuals
                                  geom.ind = "point",
                                  fill.ind = root.inactive$Community, col.ind = "black",
                                  pointshape = 21, pointsize = 2,
                                  #addEllipses = TRUE, ellipse.type = "confidence",
                                  ellipse.alpha = 0.5,
                                  palettepalette = "viridis",
                                  # Variables  
                                  repel = TRUE))

ind.p <- fviz_pca_biplot(soil.pca, geom = "point", col.ind = root.inactive$Core_ID)
ggpubr::ggpar(ind.p,
              title = "Principal Component Analysis",
              fill.ind = root.inactive$Core_ID, col.ind = "black",
              subtitle = "Root Biomass Characteristics",
              addEllipses = TRUE, ellipse.type = "confidence",
              xlab = "PC1", ylab = "PC2", 
              legend.title = "Timestamp", legend.position = "top",
              ggtheme = theme_linedraw())



#### 4 - Growth rate metrics ####

# remove after check - assign KLUANE P1 to a site
roots$Subplot <- recode(roots$Subplot, A = '7', 
                        B = '9',
                        C = '8',
                        D = '4',
                        E = '5')

# concat ID for "site_subplot_phase" and concat_ID for "site_subplot"
# average rootmass_bulkdenisty for each concat 

growth_rates <- roots %>%
  mutate(unique_ID = paste(Site, ' ', Subplot, ' ', Core_ID)) %>% 
  mutate(plot_ID = paste(Site, ' ', Subplot)) %>% 
  group_by(unique_ID) %>%
  mutate(av_roots = mean(rootmass_bulkdensity)) %>% 
  ungroup()

# 'time series' plot for each "site_subplot" - colour points and lines by site
ggplot(growth_rates) +
  aes(x = Core_ID, y = av_roots, fill = Core_ID) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(trans = "reverse") +
  labs(x = "Core timestamp", y = "Average Root Biomass / Bulk Density (g/cm3)") +
  theme_classic() +
  facet_wrap(vars(Site), scales = "free")

# deeper diver into the individual plots
growth_rates %>%
  filter(Site %in% "Toolik") %>%
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, weight = av_roots) +
  geom_bar() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(trans = "reverse") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)") +
  theme_classic() +
  facet_wrap(vars(plot_ID))

growth_rates %>%
  filter(Site %in% "Cairngorms") %>%
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, weight = av_roots) +
  geom_bar() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(trans = "reverse") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)") +
  theme_classic() +
  facet_wrap(vars(plot_ID))


growth_rates %>%
  filter(Site %in% "Kluane") %>%
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, weight = av_roots) +
  geom_bar() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(trans = "reverse") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)") +
  theme_classic() +
  facet_wrap(vars(plot_ID))

growth_rates %>%
  filter(Site %in% "Mt_Brew") %>%
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, weight = av_roots) +
  geom_bar() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(trans = "reverse") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)") +
  theme_classic() +
  facet_wrap(vars(plot_ID))

growth_rates %>%
  filter(Site %in% "Niwot") %>%
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, weight = av_roots) +
  geom_bar() +
  scale_fill_viridis_d(option = "plasma") +
  scale_y_continuous(trans = "reverse") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)") +
  theme_classic() +
  facet_wrap(vars(plot_ID))


growth_rates %>%
  filter(!is.na(Site)) %>%
  filter(!is.na(Core_ID)) %>%
  ggplot() +
  aes(x = DOY, y = av_roots, colour = Core_ID) +
  geom_point(aes(col = plot_ID),size = 3L) +
  geom_smooth(aes(col = plot_ID),  size = 2, method = "lm") +
  #scale_color_brewer(palette = "Dark2") +
  # scale_y_continuous(trans = "reverse") +
  labs(x = "Day of Year", y = "Root biomass / bulk density (g/cm3)") +
  theme_linedraw() +
  facet_wrap(vars(Site),scales = "free")

# toolik only
growth_rates %>%
  filter(Site %in% "Toolik") %>%
  filter(!is.na(Site)) %>%
  filter(!is.na(Core_ID)) %>%
  ggplot() +
  aes(x = DOY, y = av_roots, colour = Core_ID) +
  geom_point(aes(col = plot_ID),size = 3L) +
  geom_smooth(aes(col = plot_ID),  size = 2, alpha = 0.2, method = "lm") +
  #scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(trans = "reverse") +
  labs(x = "Day of Year", y = "Root biomass / bulk density (g/cm3)") +
  theme_linedraw() +
  facet_wrap(vars(Site))

# niwot only
growth_rates %>%
  filter(Site %in% "Niwot") %>%
  filter(!is.na(Site)) %>%
  filter(!is.na(Core_ID)) %>%
  ggplot() +
  aes(x = DOY, y = av_roots, colour = Core_ID) +
  geom_point(aes(col = plot_ID),size = 3L) +
  geom_smooth(aes(col = plot_ID),  size = 2, alpha = 0.2, method = "lm") +
  #scale_color_brewer(palette = "Dark2") +
  scale_y_continuous(trans = "reverse") +
  labs(x = "Day of Year", y = "Root biomass / bulk density (g/cm3)") +
  theme_linedraw() +
  facet_wrap(vars(Site))



# rename and reorder
growth_rates$Site <- recode(growth_rates$Site, 'Mt_Brew' = 'BC_coastal')
growth_rates$Site <- factor(growth_rates$Site, levels = c("BC_coastal", "Cairngorms",
                                                          "Kluane", "Niwot", "Toolik"))  

# by community
(full_core <- ggplot(growth_rates) +
  aes(x = DOY, y = rootmass_bulkdensity, fill = Community, colour = Community) +
  geom_point(size = 3L) +
  geom_smooth(method = 'lm') +
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
  labs(y =bquote("Full Core Root Biomass (g"~cm^-3*")"), x = "\nDay of Year") +
  facet_wrap(~Site, scales = "free") +
  theme_classic())
(full_core <- full_core  + theme(legend.position = "none")) 


#top only 
top_rates <- growth_rates %>% 
              filter(Depth_Type %in% "Top") 
  
(top_core <- ggplot(top_rates) +
    aes(x = DOY, y = rootmass_bulkdensity, fill = Community, colour = Community) +
    geom_point(size = 3L) +
    geom_smooth(method = 'lm') +
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
    labs(y =bquote("Top 5cm Root Biomass (g"~cm^-3*")"), x = "\nDay of Year") +
    facet_wrap(~Site, scales = "free") +
    theme_classic())
(top_core <- top_core  + theme(legend.position = "none")) 


# P3 "depth curve"
growth_rates %>%
  filter(Core_ID %in% "P3") %>%
  ggplot() +
  aes(
    x = rootmass_bulkdensity,
    y = Depth_Type,
    fill = Community
  ) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "viridis") +
  theme_classic()

#### 5 - Bring in phenocam data ####
phenocam <- read_csv("data/phenocam_2023.csv")

phenocam$PLOT <- as.factor(phenocam$PLOT)

# rename niwot cams
phenocam$PLOT <- recode(phenocam$PLOT, 'NIW_08' = 'sn_08', 
                        'NIW_11' = 'sn_11',
                        'NIW_12' = 'sn_12',
                        'NIW_13' = 'sn_13')

# rename site name 
phenocam$Site <- recode(phenocam$Site, 'Toolik Lake' = 'Toolik')
phenocam$Site <- recode(phenocam$Site, 'Mt Brew' = 'Mt_Brew')
phenocam$Site <- recode(phenocam$Site, 'Niwot Ridge' = 'Niwot')


# group into ndvi proxies
phenocam <- phenocam %>% 
  mutate(
    greenness = case_when(
      phenophase == "bud_burst" ~ 0.01,
      phenophase == "green50" ~ 0.5,
      phenophase ==  "green100" ~ 1,
      phenophase == "first_yellow" ~ 0.99,
      phenophase == "yellow50" ~ 0.5,
      phenophase == "yellow100" ~ 0.00))

# calculate day after snowmelt
growth_rates$doy <- growth_rates$DOY - growth_rates$Snowmelt

# rename doy variable
phenocam$cam_doy <- phenocam$doy

# get averages of green100 in 2022
agg_tbl <- phenocam %>% group_by(Site) %>% 
  filter(Year == 2022) %>%
  filter(phenophase %in% "green100") %>%
  summarise(mean_peak = mean(doy,na.rm = TRUE),
            .groups = 'drop')

# plot greening curves
(too_greening_curves <- phenocam %>%
    filter(Site %in% "Toolik") %>%
    filter(Year == 2022) %>%
    ggplot() +
    aes(x = doy, y = greenness*100) +
    geom_smooth(span = 1, ymax = 1, ymin = 0, alpha = 0.1, se = FALSE, col = "darkgreen") +
    geom_point(size = 3,  col = "#138513") +
    geom_vline(xintercept=22, alpha = 0.6, 
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y = "% Leaves Green per Plot",  title = "Toolik") +
    theme_classic() +
    xlim(-5, 110) +
    ylim(0, 100)) 



(too_root_rate <- growth_rates %>%
    filter(!is.na(Site)) %>%
    filter(Site %in% "Toolik") %>%
    ggplot() +
    aes(x = doy, y = av_roots) +
    geom_point(shape = "circle", size = 3, colour = "#5C4523") +
    geom_smooth(method = 'lm', fill = "#5C4523", colour = "#5C4523") +
    theme_classic() +
    geom_vline(xintercept=22, alpha = 0.6, 
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y =bquote("Root Biomass (g"~cm^3*")")) +
    xlim(-5, 110) +
    ylim(0, 2.3))
(too_root_rate <- too_root_rate  + theme(legend.position = "none"))


toolik_compare <- grid.arrange(too_greening_curves,
                               too_root_rate,ncol=1)



(cair_greening_curves <- phenocam %>%
    filter(Site %in% "Cairngorms") %>%
    filter(Year == 2022) %>% 
    ggplot() +
    aes(x = doy, y = greenness*100) +
    geom_smooth(span = 1, ymax = 1, ymin = 0, alpha = 0.1, se = FALSE, col = "darkgreen") +
    geom_point(size = 3,  col = "#138513") +
    geom_vline(xintercept=106, alpha = 0.6, 
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y = "% Leaves Green per Plot",  title = "Cairngorms") +
    theme_classic()  +
    xlim(-5, 300) +
    ylim(0, 100))


(cair_root_rate <- growth_rates %>%
    filter(!is.na(Site)) %>%
    filter(Site %in% "Cairngorms") %>%
    ggplot() +
    aes(x = doy, y = av_roots) +
    geom_point(shape = "circle", size = 3, colour = "#5C4523") +
    geom_smooth(method = 'lm', fill = "#5C4523", colour = "#5C4523") +
    geom_vline(xintercept=106, alpha = 0.6, 
               color = "#14CCA1", size=3) +
    theme_classic() +
    labs(x = "Day after Snowmelt", y =bquote("Root Biomass (g"~cm^3*")")) +
    xlim(-5, 300) +
    ylim(0, 2.3))
(cair_root_rate <- cair_root_rate  + theme(legend.position = "none"))


cairngorms_compare <- grid.arrange(cair_greening_curves,
                                   cair_root_rate,ncol=1)



(klu_greening_curves <- phenocam %>%
    filter(Site %in% "Kluane") %>%
    filter(Year == 2022) %>% 
    ggplot() +
    aes(x = doy, y = greenness*100) +
    geom_smooth(span = 1, ymax = 1, ymin = 0, alpha = 0.1, se = FALSE, col = "darkgreen") +
    geom_point(size = 3,  col = "#138513") +
    geom_vline(xintercept=16, alpha = 0.6,
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y = "% Leaves Green per Plot", title = "Kluane") +
    theme_classic()  +
    xlim(-5, 100) +
    ylim(0, 100))


(klu_root_rate <- growth_rates %>%
    filter(!is.na(Site)) %>%
    filter(Site %in% "Kluane") %>%
    ggplot() +
    aes(x = doy, y = av_roots) +
    geom_point(shape = "circle", size = 3, colour = "#5C4523") +
    geom_smooth(method = 'lm', fill = "#5C4523", colour = "#5C4523") +
    theme_classic() +
    geom_vline(xintercept=16,alpha = 0.6, 
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y =bquote("Root Biomass (g"~cm^3*")")) +
    xlim(-5, 100) +
    ylim(0, 2.3))

(klu_root_rate <- klu_root_rate  + theme(legend.position = "none"))


kluane_compare <- grid.arrange(klu_greening_curves,
                               klu_root_rate,ncol=1)


(brew_greening_curves <- phenocam %>%
    filter(Site %in% "Mt_Brew") %>%
    filter(Year == 2022) %>% 
    ggplot() +
    aes(x = doy, y = greenness*100) +
    geom_smooth(span = 1, ymax = 1, ymin = 0, alpha = 0.1, se = FALSE, col = "darkgreen") +
    geom_point(size = 3,  col = "#138513") +
    geom_vline(xintercept=18, alpha = 0.6, 
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y = "% Leaves Green per Plot",  title = "BC Coastal Mountains") +
    theme_classic() +
    xlim(-5, 60) +
    ylim(0, 100))  


(brew_root_rate <- growth_rates %>%
    filter(!is.na(Site)) %>%
    filter(Site %in% "BC_coastal") %>%
    ggplot() +
    aes(x = doy, y = av_roots) +
    geom_point(shape = "circle", size = 3, colour = "#5C4523") +
    geom_smooth(method = 'lm', fill = "#5C4523", colour = "#5C4523") +
    theme_classic() +
    geom_vline(xintercept=18, alpha = 0.6, 
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y =bquote("Root Biomass (g"~cm^3*")")) +
    xlim(-5, 60) +
    ylim(0, 2.3))
(brew_root_rate <- brew_root_rate  + theme(legend.position = "none"))


brew_compare <- grid.arrange(brew_greening_curves,
                             brew_root_rate,ncol=1)



(cair_greening_curves <- phenocam %>%
    filter(Site %in% "Cairngorms") %>%
    filter(Year == 2022) %>% 
    ggplot() +
    aes(x = doy, y = greenness*100) +
    geom_smooth(span = 1, ymax = 1, ymin = 0, alpha = 0.1, se = FALSE, col = "darkgreen") +
    geom_point(size = 3,  col = "#138513") +
    geom_vline(xintercept=106, alpha = 0.6,
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y = "% Leaves Green per Plot", , title = "Cairngorms") +
    theme_classic()  +
    xlim(-5, 300) +
    ylim(0, 100))


(cair_root_rate <- growth_rates %>%
    filter(!is.na(Site)) %>%
    filter(Site %in% "Cairngorms") %>%
    ggplot() +
    aes(x = doy, y = av_roots) +
    geom_point(shape = "circle", size = 3, colour = "#5C4523") +
    geom_smooth(method = 'lm', fill = "#5C4523", colour = "#5C4523") +
    theme_classic() +
    geom_vline(xintercept=106, alpha = 0.6,
               color = "#14CCA1", size=3) +
    labs(x = "Day after Snowmelt", y =bquote("Root Biomass (g"~cm^3*")")) +
    xlim(-5, 300) +
    ylim(0, 2.3))


cair_compare <- grid.arrange(cair_greening_curves,
                             cair_root_rate,ncol=1)


(nw_greening_curves <- phenocam %>%
    filter(Site %in% "Niwot") %>%
    filter(Year == 2022) %>% 
    ggplot() +
    aes(x = doy, y = greenness*100) +
    geom_vline(xintercept=32, alpha = 0.6,
               color = "#14CCA1", size=3) +
    geom_smooth(span = 1, ymax = 1, ymin = 0, alpha = 0.1, se = FALSE, col = "darkgreen") +
    geom_point(size = 3, col = "#138513") +
    labs(x = "Day after Snowmelt", y = "% Leaves Green per Plot",  title = "Niwot Ridge") +
    theme_classic() +
    xlim(-5, 150) +
    ylim(0, 100)) 



(nw_root_rate <- growth_rates %>%
    filter(!is.na(Site)) %>%
    filter(Site %in% "Niwot") %>%
    ggplot() +
    aes(x = doy, y = av_roots) +
    geom_vline(xintercept=32, alpha = 0.6, 
               color = "#14CCA1", size=3) +
    geom_point(shape = "circle", size = 3, colour = "#5C4523") +
    geom_smooth(method = 'lm', fill = "#5C4523", colour = "#5C4523") +
    theme_classic() +
    labs(x = "Day after Snowmelt", y =bquote("Root Biomass (g"~cm^3*")")) +
    xlim(-5, 150) +
    ylim(0, 2.3)) 
(nw_root_rate <- nw_root_rate  + theme(legend.position = "none"))


nw_compare <- grid.arrange(nw_greening_curves,
                           nw_root_rate,ncol=1)



sites_compare <- grid.arrange(kluane_compare,
                               brew_compare, 
                               toolik_compare,
                              nw_compare,
                              cairngorms_compare,
                              nrow=1)

ggsave(sites_compare, filename = "figures/phenocam_vs_rootgrowth.png",
       height = 4, width = 12)

#### 6 - Shrub vs Graminoid comparisons ####
#define function to scale values between 0 and 1
scale_values <- function(x){(x-min(x))/(max(x)-min(x))}

#scale values in 'sales' column to be between 0 and 1
community <- growth_rates %>% 
  mutate(unique_id  = paste(Site,Subplot)) %>% 
  dplyr::group_by(unique_id) %>% 
  mutate(percent = scale_values(av_roots)) %>% 
  ungroup 

# attempt 1 - raw root biomass
comm_too <- growth_rates %>%
  filter(Site %in% "Toolik") %>%
  filter(`Depth Increment` %in% '0-5') %>% 
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, y = av_roots) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)", title = "Toolik") +
  theme_classic() +
  facet_wrap(vars(Community))

comm_cairn <- growth_rates %>%
  filter(Site %in% "Cairngorms") %>%
  filter(`Depth Increment` %in% '0-5') %>% 
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, y = av_roots) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Core Timestamp",y =bquote("Root Biomass (g"~cm^3*")"), title = "Cairngorms") +
  theme_classic() +
  facet_wrap(vars(Community))

comm_klu <- growth_rates %>%
  filter(Site %in% "Kluane") %>%
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, y = av_roots) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)", title = "Kluane") +
  theme_classic() +
  facet_wrap(vars(Community))

cairngorms_compare <- grid.arrange(cair_greening_curves,
                                   cair_root_rate,ncol=1)


comm_brew <- growth_rates %>%
  filter(Site %in% "Mt_Brew") %>%
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, y = av_roots) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)", title = "Mt_Brew") +
  theme_classic() +
  facet_wrap(vars(Community))




comm_nw <- growth_rates %>%
  filter(Site %in% "Niwot") %>%
  ggplot() +
  aes(x = Core_ID, fill = Core_ID, y = av_roots) +
  geom_boxplot() +
  scale_fill_viridis_d(option = "plasma") +
  labs(x = "Core Timestamp", y = "Root biomass / bulk density (g/cm3)", title = "Niwot Ridge") +
  theme_classic() +
  facet_wrap(vars(Community))

niw_compare <- grid.arrange(nw_greening_curves,nw_root_rate,ncol=1)


comm_compare <- grid.arrange(comm_too,
                             comm_cairn,
                             comm_klu,
                             comm_brew,
                             comm_nw, ncol=1)



#### 7 - Merge phenocam and growth rate dataframes ####
phenocam_22 <- phenocam %>%
  dplyr::select(-doy) %>% 
  filter(Year == 2022)

growth_rates$Phenocam <- as.factor(growth_rates$Phenocam)        

full_growth_22 <- left_join(growth_rates, phenocam_22)


#### 8 - Calculate asynchrony metric ####
# extract peak season data only 
full_growth_green <- full_growth_22 %>%
  filter(phenophase %in% 'green100')

# remove duplicate rows
full_growth_green = full_growth_green[!duplicated(full_growth_green$av_roots),]

# make site and phenophase factors instead of characters
full_growth_green$Site <- as.factor(full_growth_green$Site)
full_growth_green$phenophase <- as.factor(full_growth_green$phenophase)

# calculate metric of asynchrony
asynchrony <- full_growth_green %>%
  group_by(plot_ID) %>% # for every core cluster ....
  mutate(slope_dif = ((max(av_roots) - min(av_roots))/(max(doy)-min(doy)))) %>% # get the growth rate
  mutate(offset = cam_doy - min(doy)) %>% # peak green day minus date of P1 extraction
  mutate(peak_est = offset * slope_dif) %>% # days elapsed multiplied by rate
  mutate(async = (peak_est/max(av_roots))*100) %>% # est root biomass at peak divided by total final biomass, expressed as a percent
  filter(async >= 0) %>% # no minus values
  ungroup()



# only keep relevant columns
str(asynchrony)
asynchrony <- asynchrony %>%
  dplyr::select(Site, Subplot, Core_ID,Phenocam, Community,
                Snowmelt, plot_ID, slope_dif, offset, peak_est,async)

asynchrony %>%
  ggplot() +
  aes(x = Site, y = async, fill = Site) +
  geom_boxplot() +
  labs(x = "Site", y = "% Root Growth at date of 100% Greening") +
  scale_fill_hue(direction = 1) +
  theme_classic()


# merge once again baby
full_growth2 <- left_join(full_growth, asynchrony)

# add a new column with 5cm roots only 
top_only <- full_growth %>% 
  filter(`Depth Increment` %in% '0-5')



# save final tommie file as a master csv
write.csv(full_growth, file = "data/rootweights_phenocams.csv", row.names = FALSE)
write.csv(top_only, file = "data/toponly_phenocams.csv", row.names = FALSE)

full_growth <- read_csv("data/rootweights_phenocams.csv")


full_sen <- full_growth2 %>%
  filter(phenophase %in% 'first_yellow')


# remove duplicate rows
full_sen = full_sen[!duplicated(full_sen$av_roots),]


#
yellow_percent <- full_sen %>%
  group_by(plot_ID) %>% 
  mutate(offset =  max(doy) - cam_doy) %>% 
  mutate(percent_diff = (offset/max(doy))*100) 


yellow_table <- yellow_percent %>% 
  group_by(Site) %>% 
  summarise(mean_prc = mean(percent_diff,na.rm = TRUE))



# calculate % root growth after sen onset 
full_sen <- full_growth %>%
  filter(phenophase %in% 'first_yellow')


# remove duplicate rows
full_sen = full_sen[!duplicated(full_sen$av_roots),]


#
yellow_percent <- full_sen %>%
  group_by(plot_ID) %>% 
  mutate(offset =  max(doy) - cam_doy) %>% 
  mutate(percent_diff = (offset/max(doy))*100) 


yellow_table <- yellow_percent %>% 
  group_by(Site) %>% 
  summarise(mean_prc = mean(percent_diff,na.rm = TRUE))


yellow_offset <- yellow_percent %>% 
  group_by(Site) %>% 
  summarise(mean_offset = mean(offset,na.rm = TRUE))
  

