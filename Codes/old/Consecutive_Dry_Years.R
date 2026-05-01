#Templates
#Consecutive Dry Days
#Template Last Updated March 27, 2025

#This script generates rasters and graphs of consecutive dry days
#It compares the historical reference period to the CMIP6 projections

#Inputs: 
# ERA5 count of consecutive dry days in tabular format 
# NEX_GDDP CMIP6 count of consecutive dry days historic
# NEX_GDDP CMIP6 count of consecutive dry days future
# all from notebook XX_Consecutive_Dry_Days
# insert your project information where you see [FILL THIS IN]

#Outputs: 
# rasters and graphs of consecutive dry days comparing 
# the historical reference period to the future projections

#Project Specific Notes:
# CDD defined as XX or more days with 1mm or less precip. 

# File Structure -----------------------------
#Create folders for figure outputs 
dir.create(here("Figures/ConsecutiveDryYears/ModelCheck"),recursive = TRUE)
dir.create(here("Figures/ConsecutiveDryYears/PDF"),recursive = TRUE)
dir.create(here("Figures/ConsecutiveDryYears/Rasters"),recursive = TRUE)
dir.create(here("Figures/ConsecutiveDryYears/Tables"),recursive = TRUE)

# Data Acquisition ---------------------------------------
# Pull in data from Databricks
cmip_hist_cdy <- tbl(sc, in_catalog("projects", project.folder, "cdyear_cmip_hist"))
cmip_hist_cdy <- collect(cmip_hist_cdy)
cmip_cdy <- tbl(sc, in_catalog("projects", project.folder, "cdyear_cmip_future"))
cmip_cdy <- collect(cmip_cdy)
era5_cdy <- tbl(sc, in_catalog("projects", project.folder, "cdyear_era5"))
era5_cdy <- collect(era5_cdy)
#cat.var <- tbl(sc, in_catalog('projects', project.folder, cat.var.table.name))

cmip_hist_cdy <- cmip_hist_cdy %>%
  inner_join(cat.var, by=c("x","y"))
cmip_cdy <- cmip_cdy %>%
  inner_join(cat.var, by=c("x","y"))
era5_cdy <- era5_cdy %>%
  inner_join(cat.var, by=c("x","y"))


# Raster Visualizations: Projected Future Normals and Differences  ---------------------------------
## Data Aggregation -----
### CMIP Historical Data Aggregation ----
cmip_hist_cdy_out <- cmip_hist_cdy %>%
  #filter(year %in% c(2000:2020)) %>%
  filter(count >= 3) %>%
  mutate(mult = floor(count/3)) %>% # finding out how many multiples of the target run lenght each count is
  mutate(freq = mult * freq_hist) %>% #multiplying to get the frequency of the target run length per year
  group_by(x, y, model) %>%
  summarize(freq_sum = sum(freq)) #%>% #adding up the frequencies within the year to get a annual total
#group_by(x, y, model) %>%
#summarize(frequency = sum(freq_sum)/21) %>% #taking the average over the time period
#collect()

### CMIP Future Data Aggregation ----
cmip_cdy_2035 <- cmip_cdy %>%
  filter(future == 2035 ) %>%
  filter(count >= 3) %>%
  mutate(mult = floor(count/3)) %>% # finding out how many multiples of the target run lenght each count is
  mutate(freq = mult * freq_fut) %>% #multiplying to get the frequency of the target run length per year
  group_by(x, y, model,future, ssp) %>%
  summarize(future_freq_sum = sum(freq)) #%>% #adding up the frequencies within the year to get a annual total
#group_by(x, y, model, ssp) %>%
#summarize(future_frequency = sum(freq_sum)/21) %>% #taking the average over the time period
#mutate(future = 2035) %>%
#collect()

cmip_cdy_2060 <- cmip_cdy %>%
  filter(future == 2060) %>%
  filter(count >= 3) %>%
  mutate(mult = floor(count/3)) %>% # finding out how many multiples of the target run lenght each count is
  mutate(freq = mult * freq_fut) %>% #multiplying to get the frequency of the target run length per year
  group_by(x, y, model, future, ssp) %>%
  summarize(future_freq_sum = sum(freq)) #%>% #adding up the frequencies within the year to get a annual total
#group_by(x, y, model, ssp) %>%
#summarize(future_frequency = sum(freq_sum)/21) %>% #taking the average over the time period
#mutate(future = 2060) %>%
#collect()

cmip_fut_cdy_out <- cmip_cdy_2035 %>%
  bind_rows(cmip_cdy_2060)

### Combining CMIP Historical and Future Data ----
cmip_cdy_out <- cmip_fut_cdy_out %>%
  full_join(cmip_hist_cdy_out, by = c('x', 'y', 'model')) %>%
  mutate(freq_sum = ifelse(is.na(freq_sum), 0, freq_sum), # add 0 where there is no historic value
         future_freq_sum = ifelse(is.na(future_freq_sum), 0, future_freq_sum)) %>% #add 0 where there is no future value
  mutate(freq_diff = future_freq_sum/freq_sum)%>% #calculate future:historic ratio
  mutate(freq_diff = ifelse(is.infinite(freq_diff) & future_freq_sum != 0, 2, 
                            ifelse(is.nan(freq_diff), 1, freq_diff))) # If divided by 0, set to 2

### ERA5 Data Aggregation ----
era5_cdy_2010 <- era5_cdy %>%
  #filter(year %in% c(2000:2020)) %>%
  filter(count >= 3) %>%
  mutate(mult = floor(count/3)) %>%
  mutate(freq = mult * freq_era5) %>%
  group_by(x, y) %>%
  summarize(era5_freq_sum = sum(freq)) #%>%
#group_by (x, y) %>%
#summarize(era5_frequency = sum(freq_sum)/21) %>%
#collect() #not adding future column to this one so we can easily join with cmip data

# era5_cdd_1985 <- era5_cdd %>%
#   filter(year %in% c(1975:1985)) %>%
#   filter(count >= run.length.cdd) %>%
#   mutate(mult = floor(count/run.length.cdd)) %>%
#   mutate(freq = mult * freq_era5) %>%
#   group_by(x, y, year) %>%
#   summarize(freq_sum = sum(freq)) %>%
#   group_by (x, y) %>%
#   summarize(era5_frequency = sum(freq_sum)/21) %>%
#   mutate(future = 1985) %>%
#   collect()

# Creating df with both futures to bind to cmip data for mapping
era5_cdy_out <- era5_cdy_2010 %>%
  mutate(future = 2010) #%>%
#bind_rows(era5_cdd_1985)

### Projections ----
cdy_join <- cmip_cdy_out %>%
  full_join(era5_cdy_2010, by = c('x', 'y')) %>%
  mutate(frequency_projection = era5_freq_sum * freq_diff) %>%
  mutate(freq_sum = ifelse(is.na(freq_sum), 0, freq_sum), 
         era5_freq_sum = ifelse(is.na(era5_freq_sum), 0, era5_freq_sum), 
         frequency_projection = ifelse(is.na(frequency_projection) & future_freq_sum != 0, future_freq_sum, 
                                       ifelse(frequency_projection == 0 & future_freq_sum != 0, future_freq_sum, frequency_projection))) %>%
  group_by(x, y, future, ssp) %>%
  summarize(frequency_mean = sum(frequency_projection)/17, 
            diff_mean = sum(freq_diff)/17) %>%
  filter(!is.na(ssp))

# Bind ERA5 for all time periods
era5_bind <- era5_cdy_out %>%
  rename(frequency_mean = era5_freq_sum) # rename columns to match with cmip data

cdy_bind <- cdy_join %>%
  bind_rows(era5_bind) %>%
  filter(!is.na(future))

## Look at distribution of model differences ----
ggsave(here('Figures/ConsecutiveDryYears/ModelCheck/CDYDistribution.png'), height = 9, width =7, units = 'in',
       ggplot() + 
         geom_boxplot(data = cmip_cdy_out, aes(x = model, y = freq_diff)) + 
         facet_wrap(ssp~future) + 
         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
)
#INSERT NOTES ON WHAT YOU SEE HERE
#EXAMPLE: models median range centered around 1 CANESM5 is outlier in all time periods and ssps

## Maps ----
CDY_DiffMap <- ggplot() + 
  geom_raster(data = cdy_join, aes(x = x, y = y , fill = diff_mean)) + 
  facet_wrap(ssp~future) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'gray20') + 
  scale_fill_continuous_diverging(pal = 'green-brown', mid = 1) + 
  labs(x = "", y = "", 
       fill = paste("Future:Historic Ratio\n", "3 Year Dry Streaks", sep = "")) + 
  theme_classic()

ggsave(here('Figures/ConsecutiveDryYears/PDF/ConsecutiveDryYears_DiffMap.pdf'), CDY_DiffMap, height = 9, width =7, units = 'in', bg = "transparent")

# export map figure as rasters
for(i in 3:6){
  Raster <- cdy_join %>%
    filter(future== futures$Time[i] & ssp == futures$ssp[i])
  
  Raster_export <- rast(Raster %>% ungroup() %>%
                          select(x, y, diff_mean), crs = "EPSG:4326")
  writeRaster(Raster_export, here(paste('Figures/ConsecutiveDryYears/Rasters/ConsecutiveDryYears_Diff_',futures$Time[i],'_',futures$ssp[i],'.tif',sep="")), overwrite=TRUE)
}


CDYProj <-ggplot() + 
  geom_raster(data = cdy_bind, aes(x = x, y = y, fill = frequency_mean)) + 
  facet_wrap(ssp~future) + 
  scale_fill_continuous_sequential(pal = 'Lajolla', rev = FALSE) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'gray20') + 
  labs(x = "", y = "", 
       fill = paste("Frequency of \n", "3 Year Dry Streak", sep = "")) + 
  theme_classic()

ggsave(here('Figures/ConsecutiveDryYears/PDF/ConsecutiveDryYears_ProjMap.pdf'), CDYProj, height = 9, width =7, units = 'in', bg = "transparent")

# Export rasters
# for(i in 1:6){
#   Raster <- cdy_bind %>%
#     mutate(ssp = replace_na(ssp, 0)) %>%
#     filter(future== futures$Time[i] & ssp == futures$ssp[i])
#   
#   Raster_export <- rast(Raster %>%
#                           ungroup() %>%
#                           select(x, y, frequency_mean), crs = "EPSG:4326")
#   writeRaster(Raster_export, here(paste('Figures/ConsecutiveDryYears/Rasters/ConsecutiveDryYears_Proj_',futures$Time[i],'_',futures$ssp[i],'.tif',sep="")), overwrite=TRUE)
# }
# Graph Visualizations: Projected Future CDD   ---------------------------------
## Data Aggregation ----
#cat.var.join <- collect(cat.var)

area_count <- cat.var.join %>%
  group_by(across(all_of(category_names))) %>%
  summarize(area_count = n())
### Historical CMIP Data Aggregation -----
hist <- cmip_hist_cdy %>%
  #filter(year %in% c(2000:2020)) %>%
  filter(count >= 3) %>%
  mutate(mult = floor(count/3)) %>%
  mutate(freq = mult * freq_hist) %>%
  group_by(x, y, model) %>%
  summarize(freq_sum_hist = sum(freq))#%>%
#group_by(x, y, model) %>%
#summarize(frequency_hist = sum(freq_sum)/21) %>%
#collect()

### Future CMIP Data Aggregation -----
fut_2035 <- cmip_cdy %>%
  filter(future == 2035) %>%
  filter(count >= 3) %>%
  mutate(mult = floor(count/3)) %>%
  mutate(freq = mult * freq_fut) %>%
  group_by(x, y, model, ssp) %>%
  summarize(freq_sum_fut = sum(freq))%>%
  #group_by(x, y, model, ssp) %>%
  #summarize(frequency_fut = sum(freq_sum)/21) %>%
  mutate(future = 2035) #%>%
#collect()

fut_2060 <- cmip_cdy %>%
  filter(future == 2060) %>%
  filter(count >= 3) %>%
  mutate(mult = floor(count/3)) %>%
  mutate(freq = mult * freq_fut) %>%
  group_by(x, y, model, ssp) %>%
  summarize(freq_sum_fut = sum(freq))%>%
  #group_by(x, y, model, ssp) %>%
  #summarize(frequency_fut = sum(freq_sum)/21) %>%
  mutate(future = 2060) #%>%
#collect()

fut_out <- fut_2035 %>%
  bind_rows(fut_2060)

cmip_cdy_graph <- fut_out %>%
  full_join(hist, by = c('x', 'y', 'model')) %>%
  mutate(freq_ratio = freq_sum_fut/freq_sum_hist) %>%
  mutate(freq_ratio = ifelse(is.infinite(freq_ratio) & freq_sum_fut != 0, 2, 
                             ifelse(is.na(freq_ratio), 1, freq_ratio)))


### ERA5 Data Aggregation ----
era5_2010 <- era5_cdy %>%
  filter(count >= 3) %>%
  mutate(coord = paste(x, y, sep = "_")) %>%
  mutate(mult = floor(count/3)) %>%
  mutate(freq = mult * freq_era5) %>%
  group_by(x, y) %>%
  summarize(freq_sum_era5 = sum(freq))#%>%
#mutate(future = 2010)
#group_by(x, y) %>%
#summarize(frequency_era5 = sum(freq_sum)/21) %>%
#collect()

# era5_1985 <- era5_cdd %>%
#   filter(year %in% c(1975:1985)) %>%
#   filter(count >= run.length.cdd) %>%
#   mutate(mult = floor(count/run.length.cdd)) %>%
#   mutate(freq = mult * freq_era5) %>%
#   group_by(x, y, year) %>%
#   summarize(freq_sum = sum(freq))%>%
#   group_by(x, y) %>%
#   summarize(frequency_era5 = sum(freq_sum)/21) %>%
#   mutate(future = 1985) %>%
#   collect()

era5_proj_1 <- era5_2010 %>%
  #bind_rows(era5_1985) %>%
  mutate(freq_sum_era5 = ifelse(is.na(freq_sum_era5), 0, freq_sum_era5)) %>%
  right_join(cat.var.join, by = c('x', 'y')) %>%
  right_join(area_count, by = category_name) %>%
  filter(!is.na(freq_sum_era5))%>%
  group_by(across(all_of(category_names)),  area_count) %>%
  summarize(sum_hold = sum(freq_sum_era5))%>%
  mutate(med_proj = sum(sum_hold)/area_count) %>%
  mutate(ssp = 0) %>%
  mutate(future = 2010)

### Calculate projections from ERA5 ----
cmip_projs <- cmip_cdy_graph %>%
  right_join(cat.var.join, by = c('x', 'y')) %>%
  right_join(area_count, by = category_name) %>%
  right_join(era5_2010, by = c('x', 'y')) %>%
  #filter(!is.na(KZ))%>%
  filter(!is.na(future)) %>%
  mutate(proj = freq_sum_era5 * freq_ratio) %>%
  group_by(across(all_of(category_names)), future, ssp, model, area_count) %>%
  summarize(sum_hold = sum(proj)) %>%
  mutate(proj_mean = sum(sum_hold)/area_count) %>%
  #filter(!is.na(future)) %>%
  group_by(future, ssp, across(all_of(category_names))) %>%
  summarize(med_proj = median(proj_mean), 
            upr_proj = quantile(proj_mean, 0.975), 
            lwr_proj = quantile(proj_mean, 0.025))

# Join Precip and Temp Projection data, add era5 data , and add class labels
comb_proj <- cmip_projs %>%
  bind_rows(era5_proj_1) %>%
  mutate(future_ssp = paste(future, ssp, sep = "|")) %>% #create column for grouping errorbars correctly
  mutate(future_ssp = factor(future_ssp, levels = c("1985|0", "2010|0", "2035|245", "2035|585", #creating levels for grouping categories
                                                    "2060|245", "2060|585"))) %>%
  mutate(ssp = factor(ssp, levels = c(0, 245, 585)), #make ssps in right order
         future = factor(future, levels = c(1985, 2010, 2035, 2060))) # make time periods in right order

### Projections Graphs
#determine the limits of the y axis
Proj_max <- max(comb_proj[, c("med_proj", "upr_proj")], na.rm = TRUE)
Proj_min <- min(comb_proj[, c("med_proj", "lwr_proj")], na.rm = TRUE)
#y limits are set with these values plus 10% more on the max value
CDYProj_graph <- projection_graph(y_val = med_proj, y_min = lwr_proj,
                                  y_max = upr_proj, 
                                  y_lab = paste('3 Year Dry Streak Frequency', sep = ""), 
                                  y_lab_type = "", 
                                  lim_min = Proj_min, lim_max = Proj_max, 
                                  color_pal = precip_colors_discrete)

ggsave(here('Figures/ConsecutiveDryYears/PDF/ConsecutiveDryYears_ProjGraph.pdf'), CDYProj_graph, height = 5, width =7, units = 'in', bg = "transparent")
write.csv(comb_proj, here('Figures/ConsecutiveDryYears/Tables/ConsecutiveDryYears_ProjTable.csv'), row.names = FALSE)
# Graph Visualizations: Differences CDD   ---------------------------------
cmip_diffs <- cmip_cdy_graph %>%
  right_join(cat.var.join, by = c('x', 'y')) %>%
  right_join(area_count, by = category_name) %>%
  group_by(model, ssp, future, across(all_of(category_names)), area_count) %>%
  summarize(sum_hold = sum(freq_ratio)) %>%
  mutate(mean_freq_ratio = sum_hold/area_count)%>% # find mean class projection by model, ssp, and future
  drop_na()%>%
  group_by(future, ssp, across(all_of(category_names))) %>% 
  summarize(med_ratio = median(mean_freq_ratio), # calculate median, 2.5% and 97.5% values over 17 models
            lwr_ratio = quantile(mean_freq_ratio, 0.025), 
            upr_ratio = quantile(mean_freq_ratio, 0.975))%>%
  mutate(med_percent = (med_ratio - 1)*100, 
         lwr_percent = (lwr_ratio - 1)*100, 
         upr_percent = (upr_ratio - 1)*100)

comb_diff <- cmip_diffs %>%
  mutate(future_ssp = paste(future, ssp, sep = "|")) %>% #create column for grouping errorbars correctly
  mutate(future_ssp = factor(future_ssp, levels = c("2035|245", "2035|585", #creating levels for grouping categories
                                                    "2060|245", "2060|585"))) %>%
  mutate(ssp = factor(ssp, levels = c(245, 585)), #make ssps in right order
         future = factor(future, levels = c(2035, 2060))) # make time periods in right order

#determine the limits of the y axis
Diff_max <- max(comb_diff[, c("med_percent", "upr_percent")], na.rm = TRUE)
Diff_min <- if_else(min(comb_diff[, c("med_percent", "lwr_percent")], na.rm = TRUE)>0,0,min(comb_diff[, c("med_percent", "lwr_percent")]))
#y limits are set with these values plus 10% more on the max value

CDYDiff_graph <- difference_graph(yint = 0, y_val = med_percent, y_min = lwr_percent, 
                                  y_max = upr_percent, 
                                  y_lab = paste("Percent Difference in", "3 Year Dry Streaks", sep = ""), 
                                  y_lab_type = "%", lim_min = Diff_min, lim_max = Diff_max, 
                                  color_pal = precip_colors_discrete)

ggsave(here('Figures/ConsecutiveDryYears/PDF/ConsecutiveDryYears_DiffGraph.pdf'), CDYDiff_graph, height = 5, width =7, units = 'in', bg = "transparent")
write.csv(comb_diff, here('Figures/ConsecutiveDryYears/Tables/ConsecutiveDryYears_DiffTable.csv'), row.names = FALSE)
