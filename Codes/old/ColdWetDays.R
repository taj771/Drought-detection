#Templates
#Cold Wet Days
#Last Updated February 20, 2025

#This script plots annual normals for ERA5 data and daily NEX-GDDP CMIP6 future and historic average temperature and precipitation data. 

#Inputs: 
# ERA5 monthly normal number of days with concurrent cold and wet days in tabular format 
# NEX_GDDP CMIP6 monthly normal number of days with concurrent cold and wet days in tabular format
# both from notebook XX_Concurrent_ColdWet
# insert your project information where you see [FILL THIS IN]

#Outputs: 
# rasters and graphs of total snow days and the ratio of snow to rain days for the winter months 
# for the historical and future time periods

#Project Specific Notes:
#

# File Structure -----------------------------
#Create folders for figure outputs 
dir.create(here("Figures/ColdWet/ModelCheck"),recursive = TRUE)
dir.create(here("Figures/ColdWet/PDF"),recursive = TRUE)
dir.create(here("Figures/ColdWet/Rasters"),recursive = TRUE)
dir.create(here("Figures/ColdWet/Tables"),recursive = TRUE)

# Data Acquisition ---------------------------------------
# Pull in data from Databricks
cmip_snow <- tbl(sc, in_catalog("projects", project.folder, "snowdays_cmip"))
era5_snow <- tbl(sc, in_catalog("projects", project.folder, "snowdays_era5"))

# Collect data into R Session
cmip_snow <- collect(cmip_snow)
era5_snow <- collect(era5_snow)

## Join to Categorical Variable ------------------------
era5_snow <- era5_snow %>%
  left_join(cat.var, by=c("x","y"))

cmip_snow <- cmip_snow %>%
  left_join(cat.var, by=c("x","y"))


# Raster Visualizations: Projected Future Normals and Differences  ---------------------------------

#### Data Aggregation -----

months_tab <- data.frame(month = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), 
                         month_nam = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
                                       "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
era5_winter <- era5_snow %>%
  filter(month %in% winter_months)

cmip_winter <- cmip_snow %>%
  filter(month %in% winter_months)

# Look at distribution of differences
ggsave(here('Figures/ColdWet/ModelCheck/ColdWetDiffDistribution.png'), height = 9, width =7, units = 'in',
       ggplot() + 
         geom_boxplot(data = cmip_winter, aes(x = model, y = snowdays_diff)) + 
         facet_wrap(ssp~future) +   
         theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
#INSERT NOTES ON WHAT YOU SEE HERE
#EXAMPLE: models median largely agree on a decrease but the ranges are variable

era5_winter_bind <- era5_winter %>%
  rename(mean_snowdays_proj = mean_snowdays_era5,
         mean_wetdays_proj = mean_wetdays_era5)


era5_winter_2010 <- era5_winter %>%
  filter(future == 2010) %>%
  select(-future) 


proj <- cmip_winter %>%
  left_join(era5_winter_2010, by = c('x', 'y', 'month')) %>%
  mutate(snowdays_proj = snowdays_diff + mean_snowdays_era5,
         wetdays_proj = wetdays_diff + mean_wetdays_era5) %>%
  group_by(x, y, month, future, ssp) %>%
  summarize(mean_snowdays_proj = mean(snowdays_proj), 
            mean_snowdays_diff = mean(snowdays_diff),
            mean_wetdays_proj = mean(wetdays_proj), 
            mean_wetdays_diff = mean(wetdays_diff))

bind <- proj %>%
  select(x, y, month, future, ssp, mean_snowdays_proj,mean_wetdays_proj) %>%
  bind_rows(era5_winter_bind) %>%
  mutate(future = factor(future, levels = c(1985, 2010, 2035, 2060))) %>%
  mutate(snowrain_ratio=(mean_snowdays_proj/mean_wetdays_proj)*100)


#### Snow Metric Rasters ---------
#Cold Wet Day Projections
ColdWetProj <- ggplot() + 
  geom_raster(data = bind, aes(x = x, y = y, fill = mean_snowdays_proj)) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'gray20') +
  facet_wrap(month~future+ssp) +
  scale_fill_continuous_sequential(pal = 'blues') + 
  labs(fill = 'Mean Monthly \nSnowdays', x = "", y = "") +
  theme_classic()

ggsave(here('Figures/ColdWet/PDF/ColdWet_ProjMap.pdf'), ColdWetProj, height = 9, width =7, units = 'in', bg = "transparent")

# export map figure as rasters
for(i in 1:6){
  for(j in 1:length(winter_months)){
    Raster <- bind %>%
      mutate(ssp = replace_na(ssp, 0)) %>%
      filter(future== futures$Time[i] & ssp == futures$ssp[i] & month == winter_months[j]) %>%
      ungroup() %>%
      select(x, y, mean_snowdays_proj)
    
    Raster_export <- rast(Raster, crs = "EPSG:4326")
    writeRaster(Raster_export, here(paste('Figures/ColdWet/Rasters/','ColdWet_Proj_',futures$Time[i],'_',futures$ssp[i],'_', winter_months[j],'.tif',sep="")), overwrite=TRUE)
  }}

#Cold Wet Day Differences
ColdWetDiff <- ggplot() + 
  geom_raster(data = proj, aes(x = x, y = y, fill = mean_snowdays_diff)) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'gray20') +
  scale_fill_continuous_diverging(pal = 'Blue-Red3', rev = TRUE) + 
  facet_wrap(month~future+ssp) +
  labs(fill = 'Monthly \nSnowday \nDifference', x = "", y = "") +
  theme_classic()

ggsave(here('Figures/ColdWet/PDF/ColdWet_DiffMap.pdf'), ColdWetDiff, height = 9, width =7, units = 'in', bg = "transparent")

# export map figure as rasters
for(i in 3:6){
  for(j in 1:length(winter_months)){
    Raster <- proj %>%
      mutate(ssp = replace_na(ssp, 0)) %>%
      filter(future== futures$Time[i] & ssp == futures$ssp[i] & month == winter_months[j]) %>%
      ungroup() %>%
      select(x, y, mean_snowdays_diff)
    
    Raster_export <- rast(Raster, crs = "EPSG:4326")
    writeRaster(Raster_export, here(paste('Figures/ColdWet/Rasters/','ColdWet_Diff_',futures$Time[i],'_',futures$ssp[i],'_', winter_months[j], '.tif',sep="")), overwrite=TRUE)
  }}

ColdWetRatio <-ggplot() + 
  geom_raster(data = bind, aes(x = x, y = y, fill = snowrain_ratio)) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'gray20') +
  scale_fill_continuous_sequential(pal = 'blues') + 
  facet_wrap(month~future+ssp) +
  labs(fill = 'Percent of Precip \nFalling as Snow', x = "", y = "") +
  theme_classic()


ggsave(here('Figures/ColdWet/PDF/ColdWet_SnowRainRatioMap.pdf'), ColdWetRatio, height = 9, width =7, units = 'in', bg = "transparent")

# export map figure as rasters
for(i in 1:6){
  for(j in 1:length(winter_months)){
    Raster <- bind %>%
      mutate(ssp = replace_na(ssp, 0)) %>%
      filter(future== futures$Time[i] & ssp == futures$ssp[i], month == winter_months[j]) %>%
      ungroup() %>%
      select(x, y, snowrain_ratio)
    
    Raster_export <- rast(Raster, crs = "EPSG:4326")
    writeRaster(Raster_export, here(paste('Figures/ColdWet/Rasters/','ColdWet_SnowRainRatio_',futures$Time[i],'_',futures$ssp[i],'_', winter_months[j],'.tif',sep="")), overwrite=TRUE)
    
  }
}
# Graph Visualizations: Projected Future Normals   ---------------------------------

# Determine if one or two categorical variables for faceting of graphs
facet1 <- sym(category_name)
facet2 <- if (!is.na(category_name2)) sym(category_name2) else NULL
#### Data Aggregation  -----
# Calculate ERA5 values by class
era5_proj_1 <- era5_winter %>%
  mutate(snowpercent=(mean_snowdays_era5/mean_wetdays_era5)*100) %>%
  group_by(across(all_of(category_names)),future, month) %>% #example: group_by(Country, climate_zo)
  drop_na() %>%
  summarize(med_snowdays = mean(mean_snowdays_era5), # finding median value over the class
            med_wetdays = mean(mean_wetdays_era5),
            med_snowpercent = mean(snowpercent)) %>% # finding median value over class
  mutate(ssp = 0) # creating a future and ssp column


# Aggregate CMIP Projections
cmip_projs <- cmip_winter %>%
  left_join(era5_winter_2010, by = c('x', 'y', 'month', category_name)) %>% # adding era5 normals to create projections
  mutate(cmip_snowday_projs = snowdays_diff + mean_snowdays_era5, # calculate projection
         cmip_wetday_projs = wetdays_diff + mean_wetdays_era5) %>% # calculate projection
  mutate(cmip_snowpercent_projs = (cmip_snowday_projs/cmip_wetday_projs)*100) %>%
  group_by( month, model, ssp, future, across(all_of(category_names))) %>% #example: group_by(Country, model, ssp, future)
  summarize(mean_cmip_snowday_projs = mean(cmip_snowday_projs), # find mean class projection by model, ssp, and future
            mean_cmip_wetday_projs = mean(cmip_wetday_projs),
            mean_cmip_snowpercent_projs = mean(cmip_snowpercent_projs)) %>%
  group_by(month, future, ssp, across(all_of(category_names))) %>% #example:  group_by(Country, future, ssp)
  drop_na() %>%
  summarize(med_snowdays = median(mean_cmip_snowday_projs), # calculate median, 2.5% and 97.5% values over 17 models
            lwr_snowdays = quantile(mean_cmip_snowday_projs, 0.025), 
            upr_snowdays = quantile(mean_cmip_snowday_projs, 0.975), 
            med_wetdays = median(mean_cmip_wetday_projs), 
            lwr_wetdays = quantile(mean_cmip_wetday_projs, 0.025), 
            upr_wetdays = quantile(mean_cmip_wetday_projs, 0.975),
            med_snowpercent = median(mean_cmip_snowpercent_projs), 
            lwr_snowpercent = quantile(mean_cmip_snowpercent_projs, 0.025), 
            upr_snowpercent = quantile(mean_cmip_snowpercent_projs, 0.975))

# Join Precip and Temp Projection data, add era5 data , and add class labels
comb_proj <- cmip_projs %>%
  bind_rows(era5_proj_1) %>%
  left_join(months_tab, by = 'month') %>%
  mutate(upr_snowdays = ifelse(is.na(upr_snowdays), med_snowdays, upr_snowdays), 
         upr_snowpercent = ifelse(is.na(upr_snowpercent), med_snowpercent, upr_snowpercent)) %>%
  mutate(future_ssp = paste(future, ssp, sep = "|")) %>% #create column for grouping errorbars correctly
  mutate(future_ssp = factor(future_ssp, levels = c("1985|0","2010|0", "2035|245", "2035|585", #creating levels for grouping categories
                                                    "2060|245", "2060|585"))) %>%
  mutate(ssp = factor(ssp, levels = c(0, 245, 585)), #make ssps in right order
         future = factor(future, levels = c(1985, 2010, 2035, 2060)), # make time periods in right order
         month_nam = factor(month_nam, levels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))) # make time periods in right order
Snowdays_max <- max(comb_proj[, c("med_snowdays", "upr_snowdays")], na.rm = TRUE)
Snowdays_min <- min(comb_proj[, c("med_snowdays", "lwr_snowdays")], na.rm = TRUE)

#### Projected Snow Days -----
MeanMonthlySnowDays_hold <- ggplot() + 
         geom_crossbar(data = comb_proj, aes(x = month_nam, y= med_snowdays, ymin = lwr_snowdays,
                                             ymax = upr_snowdays, color= future_ssp, fill=future_ssp, group = future_ssp),
                       width = 0.5,alpha = 0.5,
                       position = position_dodge(0.9)) +
         geom_text(data = comb_proj, 
                   aes(x = month_nam, y = upr_snowdays, label = future, color = future_ssp), 
                   position = position_dodge(0.9), vjust = -1.5, family = "mono") +
         geom_text(data = comb_proj, 
                   aes(x = month_nam, y = upr_snowdays, label = paste0("SSP",ssp), color = future_ssp), 
                   position = position_dodge(0.9), vjust = -0.2, family = "serif") +
         scale_color_manual(values = precip_colors_discrete) + 
         scale_fill_manual(values = precip_colors_discrete) + 
         labs(x = 'Months', y = 'Mean Monthly Snow Days', color = 'Time Period') + 
        scale_y_continuous(limits = c(Snowdays_min, Snowdays_max+1))+
         theme_classic() + 
         theme(text = element_text(family = "sans", size =10),
               panel.grid.major.y = element_line(color =alpha("#8e908f", 0.7) , linetype = '15', size = 1),
               axis.title = element_text(family ="mono", size=11,  color ="#000000", face = 'bold'),
               axis.text = element_text(size=10, color = "#636363"),
               legend.position="none",
               axis.line.y = element_blank(),
               axis.line.x = element_line(color="#636363"),
               strip.background = element_blank(),
               axis.ticks = element_blank())+
         theme(panel.background = element_rect(fill='transparent'),
               plot.background = element_rect(fill='transparent', color=NA),
               legend.background = element_rect(fill='transparent'))

if (is.null(facet2)) {
  MeanMonthlySnowDays<- MeanMonthlySnowDays_hold +
    facet_wrap(vars(!!facet1), ncol = ceiling(cat.no/2))
} else {
  MeanMonthlySnowDays <- MeanMonthlySnowDays_hold + 
    facet_grid(rows = vars(!!facet1),
               cols = vars(!!facet2)) 
}

snowdays_tab <- comb_proj %>%
  select(month_nam, med_snowdays, lwr_snowdays, upr_snowdays, future, ssp, all_of(category_name))
ggsave(here('Figures/ColdWet/PDF/Snowdays_ProjGraph.pdf'), MeanMonthlySnowDays, height = 7.5, width =10.5, units = 'in', bg = "transparent")
write.csv(snowdays_tab, here('Figures/ColdWet/Tables/Snowdays_ProjTable.csv'), row.names = FALSE)

#### Projected Snow Percent -----
Snowpercent_max <- max(comb_proj[, c("med_snowpercent", "upr_snowpercent")], na.rm = TRUE)
Snowpercent_min <- min(comb_proj[, c("med_snowpercent", "lwr_snowpercent")], na.rm = TRUE)

SnowPercent_Graph_hold <- ggplot() + 
  geom_crossbar(data = comb_proj, aes(x = month_nam, y= med_snowpercent, ymin = lwr_snowpercent,
                                             ymax = upr_snowpercent, color= future_ssp, fill=future_ssp, group = future_ssp),
                       width = 0.5,alpha = 0.5,
                       position = position_dodge(0.9)) +
  geom_text(data = comb_proj, 
                   aes(x = month_nam, y = upr_snowpercent, label = future, color = future_ssp), 
                   position = position_dodge(0.9), vjust = -1.5, family = "mono") +
  geom_text(data = comb_proj, 
                   aes(x = month_nam, y = upr_snowpercent, label = paste0("SSP",ssp), color = future_ssp), 
                   position = position_dodge(0.9), vjust = -0.2, family = "serif") +
  scale_color_manual(values = precip_colors_discrete) + 
  scale_fill_manual(values = precip_colors_discrete) + 
  labs(x = 'Months', y = 'Percent of Precip Falling as Snow', color = 'Time Period') +
  scale_y_continuous(labels = ~ paste0(.x, "%"), limits = c(Snowpercent_min, Snowpercent_max+1))+
  theme_classic() + 
  theme(text = element_text(family = "sans", size =10),
               panel.grid.major.y = element_line(color =alpha("#8e908f", 0.7) , linetype = '15', size = 1),
               axis.title = element_text(family ="mono", size=11,  color ="#000000", face = 'bold'),
               axis.text = element_text(size=10, color = "#636363"),
               legend.position="none",
               axis.line.y = element_blank(),
               axis.line.x = element_line(color="#636363"),
               strip.background = element_blank(),
               axis.ticks = element_blank())+
  theme(panel.background = element_rect(fill='transparent'),
               plot.background = element_rect(fill='transparent', color=NA),
               legend.background = element_rect(fill='transparent'))

if (is.null(facet2)) {
  SnowPercent_Graph<- SnowPercent_Graph_hold +
    facet_wrap(vars(!!facet1), ncol = ceiling(cat.no/2))
} else {
  SnowPercent_Graph <- SnowPercent_Graph_hold + 
    facet_grid(rows = vars(!!facet1),
               cols = vars(!!facet2)) 
}
snowpercent_tab <- comb_proj %>%
  select(month_nam, med_snowpercent, lwr_snowpercent, upr_snowpercent, future, ssp, all_of(category_name))

ggsave(here('Figures/ColdWet/PDF/Snowpercent_ProjGraph.pdf'), SnowPercent_Graph, height = 7.5, width =10.5, units = 'in', bg = "transparent")
write.csv(snowpercent_tab, here('Figures/ColdWet/Tables/Snowpercent_ProjTable.csv'), row.names = FALSE)

# Graph Visualizations: Differences   ---------------------------------
cmip_winter_diffs <- cmip_winter %>%
  mutate(snowpercent = (mean_snowdays/mean_wetdays)*100,
         snowpercent_hist = (mean_snowdays_hist/mean_wetdays_hist)*100) %>%
  mutate(snowpercent_hist = ifelse(mean_snowdays_hist == 0 & mean_wetdays_hist == 0, 
                                   0, snowpercent_hist)) %>%
  mutate(snowpercent = ifelse(mean_snowdays == 0 & mean_wetdays ==0, 0, snowpercent)) %>%
  mutate(snowpercent_diff = snowpercent-snowpercent_hist)


#### Data Aggregation  -----
# Aggregate CMIP Projections
cmip_diffs <- cmip_winter_diffs %>%
  group_by(month, model, ssp, future, across(all_of(category_names))) %>% # example: group_by(Country, climate_zo, model, ssp, future)
  summarize(mean_cmip_snowdays_diff = mean(snowdays_diff), # find mean class projection by model, ssp, and future
            mean_cmip_wetdays_diff = mean(wetdays_diff),
            mean_cmip_snowpercent_diff = mean(snowpercent_diff)) %>%
  group_by(month, future, ssp, across(all_of(category_names))) %>% #example: group_by(Country, future, ssp) %>%
  drop_na() %>%
  summarize(med_snowdays = median(mean_cmip_snowdays_diff), # calculate median, 2.5% and 97.5% values over 17 models
            lwr_snowdays = quantile(mean_cmip_snowdays_diff, 0.025), 
            upr_snowdays = quantile(mean_cmip_snowdays_diff, 0.975), 
            med_wetdays = median(mean_cmip_wetdays_diff), 
            lwr_wetdays = quantile(mean_cmip_wetdays_diff, 0.025), 
            upr_wetdays = quantile(mean_cmip_wetdays_diff, 0.975),
            med_snowpercent = median(mean_cmip_snowpercent_diff), 
            lwr_snowpercent = quantile(mean_cmip_snowpercent_diff, 0.025), 
            upr_snowpercent = quantile(mean_cmip_snowpercent_diff, 0.975))

# add class labels
comb_diff <- cmip_diffs %>%
  left_join(months_tab, by = 'month') %>%
  mutate(future_ssp = paste(future, ssp, sep = "|")) %>% #create column for grouping errorbars correctly
  mutate(future_ssp = factor(future_ssp, levels = c("2035|245", "2035|585", #creating levels for grouping categories
                                                    "2060|245", "2060|585"))) %>%
  mutate(ssp = factor(ssp, levels = c( 245, 585)), #make ssps in right order
         future = factor(future, levels = c( 2035, 2060)), # make time periods in right order
         month_nam = factor(month_nam, levels = c('Oct', 'Nov', 'Dec', 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep'))) # make time periods in right order

#### Difference in Snowdays -----
SnowdaysDiff_max <- max(comb_diff[, c("med_snowdays", "upr_snowdays")], na.rm = TRUE)
SnowdaysDiff_min <- if_else(min(comb_diff[, c("med_snowdays", "lwr_snowdays")], na.rm = TRUE)>0,0,min(comb_diff[, c("med_snowdays", "lwr_snowdays")]))

Snowday_Diff_hold <- ggplot() + 
  geom_hline(yintercept=0,color="#636363")+
  geom_crossbar(data = comb_diff, aes(x = month_nam, y= med_snowdays, ymin = lwr_snowdays,
                                      ymax = upr_snowdays, color= future_ssp, fill=future_ssp, group = future_ssp),
                width = 0.5,alpha = 0.5, position = position_dodge(0.9)) +
  geom_text(data = comb_diff, 
            aes(x = month_nam, y = upr_snowdays, label = future, color = future_ssp), 
            position = position_dodge(0.9), vjust = -1.5, family = "mono") +
  geom_text(data = comb_diff, 
            aes(x = month_nam, y = upr_snowdays, label = paste0("SSP",ssp), color = future_ssp), 
            position = position_dodge(0.9), vjust = -0.2, family = "serif") +
  scale_color_manual(values = precip_colors_discrete[3:6]) + 
  scale_fill_manual(values = precip_colors_discrete[3:6]) +  
  labs(x = 'Month', y = 'Difference in Monthly Snowdays') + 
  scale_y_continuous( limits=c(SnowdaysDiff_min, SnowdaysDiff_max*1.1))+
  theme_classic() + 
  theme(text = element_text(family = "sans"),
               panel.grid.major.y = element_line(color =alpha("#8e908f", 0.7) , linetype = '15', size = 1),
               axis.title = element_text(family="mono" , color ="#000000", face = 'bold'),
               axis.text = element_text( color = "#636363"),
               legend.position="none",
               axis.line.y = element_blank(),
               axis.line.x = element_line(color="#636363"),
               strip.background = element_blank(),
               axis.ticks = element_blank())+
  theme(panel.background = element_rect(fill='transparent'),
               plot.background = element_rect(fill='transparent', color=NA),
               legend.background = element_rect(fill='transparent'))

if (is.null(facet2)) {
  Snowday_Diff <- Snowday_Diff_hold +
    facet_wrap(vars(!!facet1), ncol = ceiling(cat.no/2))
} else {
  Snowday_Diff <- Snowday_Diff_hold + 
    facet_grid(rows = vars(!!facet1),
               cols = vars(!!facet2)) 
}
ggsave(here('Figures/ColdWet/PDF/Snowday_DiffGraph.pdf'), Snowday_Diff, height = 7.5, width =10.5, units = 'in', bg = "transparent")

snowdays_diff <- comb_diff %>%
  select(month_nam, med_snowdays, lwr_snowdays, upr_snowdays, future, ssp, all_of(category_name))
write.csv(snowdays_diff, here('Figures/ColdWet/Tables/Snowday_DiffTable.csv'), row.names = FALSE)


#### Difference in Percent of Days with Snow-----
SnowpercentDiff_max <- max(comb_diff[, c("med_snowpercent", "upr_snowpercent")], na.rm = TRUE)
SnowpercentDiff_min <- if_else(min(comb_diff[, c("med_snowpercent", "lwr_snowpercent")], na.rm = TRUE)>0,0,min(comb_diff[, c("med_snowpercent", "lwr_snowpercent")]))

Snowpercent_Diff_hold <- ggplot() + 
  geom_hline(yintercept=0,color="#636363")+
  geom_crossbar(data = comb_diff, aes(x = month_nam, y= med_snowpercent, ymin = lwr_snowpercent,
                                      ymax = upr_snowpercent, color= future_ssp, fill=future_ssp, group = future_ssp),
                width = 0.5,alpha = 0.5, position = position_dodge(0.9)) +
  geom_text(data = comb_diff, 
            aes(x = month_nam, y = upr_snowpercent, label = future, color = future_ssp), 
            position = position_dodge(0.9), vjust = -1.5, family = "mono") +
  geom_text(data = comb_diff, 
            aes(x = month_nam, y = upr_snowpercent, label = paste0("SSP",ssp), color = future_ssp), 
            position = position_dodge(0.9), vjust = -0.2, family = "serif") +
  scale_color_manual(values = precip_colors_discrete[3:6]) + 
  scale_fill_manual(values = precip_colors_discrete[3:6]) +  
  labs(x = 'Month', y = 'Difference in Percent of Days with Precipitation as Snow') + 
  scale_y_continuous(labels = ~ paste0(.x, "%"),limits=c(SnowpercentDiff_min, SnowpercentDiff_max*1.1))+
  theme_classic() + 
  theme(text = element_text(family = "sans"),
        panel.grid.major.y = element_line(color =alpha("#8e908f", 0.7) , linetype = '15', size = 1),
        axis.title = element_text(family="mono" , color ="#000000", face = 'bold'),
        axis.text = element_text( color = "#636363"),
        legend.position="none",
        axis.line.y = element_blank(),
        axis.line.x = element_line(color="#636363"),
        strip.background = element_blank(),
        axis.ticks = element_blank())+
  theme(panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        legend.background = element_rect(fill='transparent'))

Snowpercent_Diff
if (is.null(facet2)) {
  Snowpercent_Diff <- Snowpercent_Diff_hold +
    facet_wrap(vars(!!facet1), ncol = ceiling(cat.no/2))
} else {
  Snowpercent_Diff <- Snowpercent_Diff_hold + 
    facet_grid(rows = vars(!!facet1),
               cols = vars(!!facet2)) 
}
ggsave(here('Figures/ColdWet/PDF/Snowpercent_DiffGraph.pdf'), Snowday_Diff, height = 7.5, width =10.5, units = 'in', bg = "transparent")

snowpercent_diff <- comb_diff %>%
  select(month_nam, med_snowpercent, lwr_snowpercent, upr_snowpercent, future, ssp, all_of(category_name))
write.csv(snowpercent_diff, here('Figures/ColdWet/Tables/Snowpercent_DiffTable.csv'), row.names = FALSE)

##### MODEL AGREEMENT MAPS -----
# Direction
snowdays_mod_agr_direction <- cmip_winter %>%
  mutate(increase = ifelse(snowdays_diff > 0, 1, 0)) %>%
  group_by(x, y, ssp, month, future, !!facet1, !!!facet2) %>%
  summarize(model_pos = sum(increase)) %>%
  mutate(bins = ifelse(model_pos %in% c(0,1), "All Agree - Decrease",
                       ifelse(model_pos %in% c(2:4), "Most Agree - Decrease",
                              ifelse(model_pos %in% c(5:7), "Slightly more indicate decrease",
                                     ifelse(model_pos %in% c(8,9), "50-50",
                                            ifelse(model_pos %in% c(10:12), "Slightly more indicate increase",
                                                   ifelse(model_pos %in% c(13:15), "Most Agree - Increase", "All Agree - Increase"))))))) %>%
  mutate(bins = factor(bins, levels = c("All Agree - Increase", "Most Agree - Increase", "Slightly more indicate increase", "50-50",
                                        "Slightly more indicate decrease","Most Agree - Decrease","All Agree - Decrease")))

snowdays_graph <- ggplot(snowdays_mod_agr_direction)+
  geom_raster(aes(x = x , y = y, fill = bins), show.legend = TRUE) +
  scale_fill_manual(values = c("All Agree - Decrease" = "#BB0103","Most Agree - Decrease" = "#ff5e3f","Slightly more indicate decrease" = "#ffa47d",
                               "50-50" = "#FFFFFF","Slightly more indicate increase" = "#E7F9FF","Most Agree - Increase" = "#8F88D2","All Agree - Increase" = "#1207A3"), 
                    limits = c( "All Agree - Decrease", "Most Agree - Decrease", "Slightly more indicate decrease", 
                                "50-50", "Slightly more indicate increase", "Most Agree - Increase", "All Agree - Increase"), 
                    drop=FALSE) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'black') +
  labs(fill = 'Model \nAgreement', title = 'Snowdays difference')+
  facet_grid(future+ssp ~ month) + 
  theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(here('Figures/ColdWet/PDF/SnowdaysDiff_ModelAgreementMap.pdf'), snowdays_graph, height = 5, width =7, units = 'in', bg = "transparent")

wetdays_mod_agr_direction <- cmip_winter %>%
  mutate(increase = ifelse(wetdays_diff > 0, 1, 0)) %>%
  group_by(x, y, ssp, month, future, !!facet1, !!!facet2) %>%
  summarize(model_pos = sum(increase)) %>%
  mutate(bins = ifelse(model_pos %in% c(0,1), "All Agree - Decrease",
                       ifelse(model_pos %in% c(2:4), "Most Agree - Decrease",
                              ifelse(model_pos %in% c(5:7), "Slightly more indicate decrease",
                                     ifelse(model_pos %in% c(8,9), "50-50",
                                            ifelse(model_pos %in% c(10:12), "Slightly more indicate increase",
                                                   ifelse(model_pos %in% c(13:15), "Most Agree - Increase", "All Agree - Increase"))))))) %>%
  mutate(bins = factor(bins, levels = c("All Agree - Increase", "Most Agree - Increase", "Slightly more indicate increase", "50-50",
                                        "Slightly more indicate decrease","Most Agree - Decrease","All Agree - Decrease")))

wetdays_graph <- ggplot(wetdays_mod_agr_direction)+
  geom_raster(aes(x = x , y = y, fill = bins), show.legend = TRUE) +
  scale_fill_manual(values = c("All Agree - Decrease" = "#BB0103","Most Agree - Decrease" = "#ff5e3f","Slightly more indicate decrease" = "#ffa47d",
                               "50-50" = "#FFFFFF","Slightly more indicate increase" = "#E7F9FF","Most Agree - Increase" = "#8F88D2","All Agree - Increase" = "#1207A3"), 
                    limits = c( "All Agree - Decrease", "Most Agree - Decrease", "Slightly more indicate decrease", 
                                "50-50", "Slightly more indicate increase", "Most Agree - Increase", "All Agree - Increase"), 
                    drop=FALSE) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'black') +
  labs(fill = 'Model \nAgreement', title = 'Wetdays difference')+
  facet_grid(future+ssp ~ month) + 
  theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

ggsave(here('Figures/ColdWet/PDF/WetdaysDiff_ModelAgreementMap.pdf'), wetdays_graph, height = 5, width =7, units = 'in', bg = "transparent")

# Range
snowdays_mod_agr_mag <- cmip_winter %>%
  group_by(x,y,ssp,month, future,!!facet1, !!!facet2) %>%
  summarize(range = max(snowdays_diff)- abs(min(snowdays_diff)))

snowdays_graph <- ggplot(snowdays_mod_agr_mag)+
  geom_raster(aes(x = x , y = y, fill = range)) +
  scale_fill_gradient(low = "white", high = 'darkred')+ 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'black') +
  labs(fill = 'Range of Model \nPrediction', title = 'Wetdays difference range')+
  facet_grid(future+ssp ~ month) + 
  theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave(here('Figures/ColdWet/PDF/SnowdaysDiff_ModelRangeMap.pdf'), snowdays_graph, height = 5, width =7, units = 'in', bg = "transparent")

wetdays_mod_agr_mag <- cmip_winter %>%
  group_by(x,y,ssp,month, future,!!facet1, !!!facet2) %>%
  summarize(range = max(wetdays_diff)- abs(min(wetdays_diff)))

wetdays_graph <- ggplot(wetdays_mod_agr_mag)+
  geom_raster(aes(x = x , y = y, fill = range)) +
  scale_fill_gradient(low = "white", high = 'darkred')+ 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'black') +
  labs(fill = 'Range of Model \nPrediction', title = 'Wetdays difference range')+
  facet_grid(future+ssp ~ month) + 
  theme_classic()+
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())
ggsave(here('Figures/ColdWet/PDF/WetdaysDiff_ModelRangeMap.pdf'), wetdays_graph, height = 5, width =7, units = 'in', bg = "transparent")


##### DISTRIBUTION PLOTS ----

#aggregate to be able to display by model
count <- cmip_winter %>%
  mutate(bin_snowdays = round(snowdays_diff, 2),
         bin_wetdays = round(wetdays_diff, 2)) 
#Snowdays
count_snowdays <- count %>%
  group_by(ssp, future, model, month, !!facet1, !!facet2) %>%
  count(bin_snowdays)

count_snowdays_summary <- count_snowdays %>%
  group_by(ssp, future, model, month, !!facet1, !!facet2) %>%
  summarize(min = min(bin_snowdays),max = max(bin_snowdays))

snowdays_probs <- cmip_winter %>%
  group_by(ssp, future, month, !!facet1, !!facet2) %>%
  summarize(
    q90 = quantile(snowdays_diff, 0.90, na.rm = TRUE),
    q80 = quantile(snowdays_diff, 0.80, na.rm = TRUE),
    q70 = quantile(snowdays_diff, 0.70, na.rm = TRUE),
    q60 = quantile(snowdays_diff, 0.60, na.rm = TRUE),
    q50 = quantile(snowdays_diff, 0.50, na.rm = TRUE),
    q40 = quantile(snowdays_diff, 0.40, na.rm = TRUE),
    q30 = quantile(snowdays_diff, 0.30, na.rm = TRUE),
    q20 = quantile(snowdays_diff, 0.20, na.rm = TRUE),
    q10 = quantile(snowdays_diff, 0.10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("q"),
    names_to = "quantile",
    values_to = "value"
  ) %>%
  mutate(
    quantile = as.numeric(sub("q", "0.", quantile))  # convert q90 → 0.90, etc.
  )

snowdays_graph_hold <- ggplot() +
  geom_bar(data = count_snowdays, aes(x = bin_snowdays, y =n, fill = model),stat = "identity")+
  geom_vline(data = snowdays_probs, aes(xintercept = value),
             color = "#8e908f", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "#636363") +
  labs(x = 'Snowdays Difference', y = "Occurrence") +
  scale_x_continuous(labels = ~ paste0(.x, ""))+
  theme(text = element_text(family = "sans"),
        panel.grid.major.y = element_line(color = alpha("#8e908f", 0.7), linetype = '15', size = 1),
        axis.title = element_text(family = "mono", color = "#000000", face = 'bold'),
        axis.text = element_text(color = "#636363"),
        legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "#636363"),
        strip.background = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent'))

if (is.null(facet2)) {
  snowdays_graph <- snowdays_graph_hold + 
    facet_grid(
      cols = vars(month, !!facet1),
      rows = vars(ssp, future),
      scales = 'free_y'
    )
} else {
  snowdays_graph <- snowdays_graph_hold + 
    facet_grid(
      cols = vars(month, !!facet1, !!facet2),
      rows = vars(ssp, future),
      scales = 'free_y'
    )
}

ggsave(here('Figures/ColdWet/PDF/Snowdays_DistributionGraph.pdf'), snowdays_graph, height = 10, width = 15, units = 'in', bg = "transparent")
write.csv(count_snowdays_summary, here('Figures/ColdWet/Tables/Snowdays_ModelDistribution_summary.csv'), row.names = FALSE)
write.csv(snowdays_probs, here('Figures/ColdWet/Tables/Snowdays_Quantiles_summary.csv'), row.names = FALSE)

#Wetdays
count_wetdays <- count %>%
  group_by(ssp, future, model, month, !!facet1, !!facet2) %>%
  count(bin_wetdays)

count_wetdays_summary <- count_wetdays %>%
  group_by(ssp, future, model, month, !!facet1, !!facet2) %>%
  summarize(min = min(bin_wetdays),max = max(bin_wetdays))

wetdays_probs <- cmip_winter %>%
  group_by(ssp, future, month, !!facet1, !!facet2) %>%
  summarize(
    q90 = quantile(wetdays_diff, 0.90, na.rm = TRUE),
    q80 = quantile(wetdays_diff, 0.80, na.rm = TRUE),
    q70 = quantile(wetdays_diff, 0.70, na.rm = TRUE),
    q60 = quantile(wetdays_diff, 0.60, na.rm = TRUE),
    q50 = quantile(wetdays_diff, 0.50, na.rm = TRUE),
    q40 = quantile(wetdays_diff, 0.40, na.rm = TRUE),
    q30 = quantile(wetdays_diff, 0.30, na.rm = TRUE),
    q20 = quantile(wetdays_diff, 0.20, na.rm = TRUE),
    q10 = quantile(wetdays_diff, 0.10, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_longer(
    cols = starts_with("q"),
    names_to = "quantile",
    values_to = "value"
  ) %>%
  mutate(
    quantile = as.numeric(sub("q", "0.", quantile))  # convert q90 → 0.90, etc.
  )

wetdays_graph_hold <- ggplot() +
  geom_bar(data = count_wetdays, aes(x = bin_wetdays, y =n, fill = model),stat = "identity")+
  geom_vline(data = wetdays_probs, aes(xintercept = value),
             color = "#8e908f", linetype = "dashed") +
  geom_vline(xintercept = 0, color = "#636363") +
  labs(x = 'Wetdays Difference', y = "Occurrence") +
  scale_x_continuous(labels = ~ paste0(.x, ""))+
  theme(text = element_text(family = "sans"),
        panel.grid.major.y = element_line(color = alpha("#8e908f", 0.7), linetype = '15', size = 1),
        axis.title = element_text(family = "mono", color = "#000000", face = 'bold'),
        axis.text = element_text(color = "#636363"),
        legend.position = "none",
        axis.line.y = element_blank(),
        axis.line.x = element_line(color = "#636363"),
        strip.background = element_blank(),
        axis.ticks = element_blank()) +
  theme(panel.background = element_rect(fill = 'transparent'),
        plot.background = element_rect(fill = 'transparent', color = NA),
        legend.background = element_rect(fill = 'transparent'))

if (is.null(facet2)) {
  wetdays_graph <- wetdays_graph_hold + 
    facet_grid(
      cols = vars(month, !!facet1),
      rows = vars(ssp, future),
      scales = 'free_y'
    )
} else {
  wetdays_graph <- wetdays_graph_hold + 
    facet_grid(
      cols = vars(month, !!facet1, !!facet2),
      rows = vars(ssp, future),
      scales = 'free_y'
    )
}

ggsave(here('Figures/ColdWet/PDF/Wetdays_DistributionGraph.pdf'), wetdays_graph, height = 10, width = 15, units = 'in', bg = "transparent")
write.csv(count_wetdays_summary, here('Figures/ColdWet/Tables/Wetdays_ModelDistribution_summary.csv'), row.names = FALSE)
write.csv(wetdays_probs, here('Figures/ColdWet/Tables/Wetdays_Quantiles_summary.csv'), row.names = FALSE)

