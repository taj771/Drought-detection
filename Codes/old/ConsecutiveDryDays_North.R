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
dir.create(here("Figures/ConsecutiveDryDays_North/ModelCheck"),recursive = TRUE)
dir.create(here("Figures/ConsecutiveDryDays_North/PDF"),recursive = TRUE)
dir.create(here("Figures/ConsecutiveDryDays_North/Rasters"),recursive = TRUE)
dir.create(here("Figures/ConsecutiveDryDays_North/Tables"),recursive = TRUE)

# Data Acquisition ---------------------------------------
# Pull in data from Databricks
cmip_hist_cdd <- tbl(sc, in_catalog("projects", project.folder, "cdd_cmip_hist"))

cmip_cdd <- tbl(sc, in_catalog("projects", project.folder, "cdd_cmip_future"))

era5_cdd <- tbl(sc, in_catalog("projects", project.folder, "cdd_era5"))

#cat.var <- tbl(sc, in_catalog('projects', project.folder,cat.var.table))

cmip_hist_cdd <- collect(cmip_hist_cdd)
cmip_cdd <- collect(cmip_cdd)
era5_cdd <- collect(era5_cdd)

cmip_hist_cdd <- cmip_hist_cdd%>%
  inner_join(cat.var, by=c("x","y")) 

cmip_cdd <- cmip_cdd%>%
  inner_join(cat.var, by=c("x","y")) 

era5_cdd <- era5_cdd%>%
  inner_join(cat.var, by=c("x","y")) 

# Raster Visualizations: Projected Future Normals and Differences  ---------------------------------
## Data Aggregation -----
### CMIP Historical Data Aggregation ----
cmip_hist_cdd_out <- cmip_hist_cdd %>%
  filter(year %in% c(2000:2020)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>% # finding out how many multiples of the target run lenght each count is
  mutate(freq = mult * freq_hist) %>% #multiplying to get the frequency of the target run length per year
  group_by(x, y, model, year) %>%
  summarize(freq_sum = sum(freq)) %>% #adding up the frequencies within the year to get a annual total
  group_by(x, y, model) %>%
  summarize(frequency = sum(freq_sum)/21) %>% #taking the average over the time period
  collect()

### CMIP Future Data Aggregation ----
cmip_cdd_2035 <- cmip_cdd %>%
  filter(year %in% c(2025:2045)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>% # finding out how many multiples of the target run lenght each count is
  mutate(freq = mult * freq_fut) %>% #multiplying to get the frequency of the target run length per year
  group_by(x, y, model, year, ssp) %>%
  summarize(freq_sum = sum(freq)) %>% #adding up the frequencies within the year to get a annual total
  group_by(x, y, model, ssp) %>%
  summarize(future_frequency = sum(freq_sum)/21) %>% #taking the average over the time period
  mutate(future = 2035) %>%
  collect()

cmip_cdd_2060 <- cmip_cdd %>%
  filter(year %in% c(2050:2070)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>% # finding out how many multiples of the target run lenght each count is
  mutate(freq = mult * freq_fut) %>% #multiplying to get the frequency of the target run length per year
  group_by(x, y, model, year, ssp) %>%
  summarize(freq_sum = sum(freq)) %>% #adding up the frequencies within the year to get a annual total
  group_by(x, y, model, ssp) %>%
  summarize(future_frequency = sum(freq_sum)/21) %>% #taking the average over the time period
  mutate(future = 2060) %>%
  collect()

cmip_fut_cdd_out <- cmip_cdd_2035 %>%
  bind_rows(cmip_cdd_2060)

### Combining CMIP Historical and Future Data ----
cmip_cdd_out <- cmip_fut_cdd_out %>%
  full_join(cmip_hist_cdd_out, by = c('x', 'y', 'model')) %>%
  mutate(frequency = ifelse(is.na(frequency), 0, frequency), # add 0 where there is no historic value
         future_frequency = ifelse(is.na(future_frequency), 0, future_frequency)) %>% #add 0 where there is no future value
  mutate(freq_diff = future_frequency/frequency)%>% #calculate future:historic ratio
  mutate(freq_diff = ifelse(is.infinite(freq_diff) & future_frequency != 0, 2, 
                            ifelse(is.nan(freq_diff), 1, freq_diff))) # If divided by 0, set to 2

### ERA5 Data Aggregation ----
era5_cdd_2010 <- era5_cdd %>%
  filter(year %in% c(2000:2020)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>%
  mutate(freq = mult * freq_era5) %>%
  group_by(x, y, year) %>%
  summarize(freq_sum = sum(freq)) %>%
  group_by (x, y) %>%
  summarize(era5_frequency = sum(freq_sum)/21) %>%
  collect() #not adding future column to this one so we can easily join with cmip data

era5_cdd_1985 <- era5_cdd %>%
  filter(year %in% c(1975:1985)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>%
  mutate(freq = mult * freq_era5) %>%
  group_by(x, y, year) %>%
  summarize(freq_sum = sum(freq)) %>%
  group_by (x, y) %>%
  summarize(era5_frequency = sum(freq_sum)/21) %>%
  mutate(future = 1985) %>%
  collect()

# Creating df with both futures to bind to cmip data for mapping
era5_cdd_out <- era5_cdd_2010 %>%
  mutate(future = 2010) %>%
  bind_rows(era5_cdd_1985)

### Projections ----
cdd_join <- cmip_cdd_out %>%
  full_join(era5_cdd_2010, by = c('x', 'y')) %>%
  mutate(frequency_projection = era5_frequency * freq_diff) %>%
  mutate(frequency = ifelse(is.na(frequency), 0, frequency), 
         era5_frequency = ifelse(is.na(era5_frequency), 0, era5_frequency), 
         frequency_projection = ifelse(is.na(frequency_projection) & future_frequency != 0, future_frequency, 
                       ifelse(frequency_projection == 0 & future_frequency != 0, future_frequency, frequency_projection))) %>%
  group_by(x, y, future, ssp) %>%
  summarize(frequency_mean = sum(frequency_projection)/17, 
            diff_mean = sum(freq_diff)/17) %>%
  filter(!is.na(ssp))

# Bind ERA5 for all time periods
era5_bind <- era5_cdd_out %>%
  rename(frequency_mean = era5_frequency) # rename columns to match with cmip data

cdd_bind <- cdd_join %>%
  bind_rows(era5_bind) %>%
  filter(!is.na(future))

## Look at distribution of model differences ----
ggsave(here('Figures/ConsecutiveDryDays_North/ModelCheck/CDDDistribution.png'), height = 9, width =7, units = 'in',
       ggplot() + 
         geom_boxplot(data = cmip_cdd_out, aes(x = model, y = freq_diff)) + 
         facet_wrap(ssp~future) + 
         theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))
)
#INSERT NOTES ON WHAT YOU SEE HERE
#EXAMPLE: models median range centered around 1 CANESM5 is outlier in all time periods and ssps

## Maps ----
CDD_DiffMap <- ggplot() + 
  geom_raster(data = cdd_join, aes(x = x, y = y , fill = diff_mean)) + 
  facet_wrap(ssp~future) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'gray20') + 
  scale_fill_continuous_diverging(pal = 'green-brown', mid = 1) + 
  labs(x = "", y = "", 
       fill = paste("Future:Historic Ratio\n", run.length.cdd, "-day Dry Streaks", sep = "")) + 
  theme_classic()

ggsave(here('Figures/ConsecutiveDryDays_North/PDF/ConsecutiveDryDays_DiffMap.pdf'), CDD_DiffMap, height = 9, width =7, units = 'in', bg = "transparent")

# export map figure as rasters
for(i in 3:6){
  Raster <- cdd_join %>%
    filter(future== futures$Time[i] & ssp == futures$ssp[i])
  
  Raster_export <- rast(Raster %>% ungroup() %>%
                          select(x, y, diff_mean), crs = "EPSG:4326")
  writeRaster(Raster_export, here(paste('Figures/ConsecutiveDryDays_North/Rasters/ConsecutiveDryDays_Diff_',futures$Time[i],'_',futures$ssp[i],'.tif',sep="")), overwrite=TRUE)
}


CDDProj <-ggplot() + 
  geom_raster(data = cdd_bind, aes(x = x, y = y, fill = frequency_mean)) + 
  facet_wrap(ssp~future) + 
  scale_fill_continuous_sequential(pal = 'Lajolla', rev = FALSE) + 
  geom_spatvector(data = shapefile_proj, fill = 'transparent', color = 'gray20') + 
  labs(x = "", y = "", 
       fill = paste("Annual frequency of \n",run.length.cdd, "-day Dry Streak", sep = "")) + 
  theme_classic()

ggsave(here('Figures/ConsecutiveDryDays_North/PDF/ConsecutiveDryDays_ProjMap.pdf'), CDDProj, height = 9, width =7, units = 'in', bg = "transparent")

# Export rasters
for(i in 1:6){
  Raster <- cdd_bind %>%
    mutate(ssp = replace_na(ssp, 0)) %>%
    filter(future== futures$Time[i] & ssp == futures$ssp[i])
  
  Raster_export <- rast(Raster %>%
                          ungroup() %>%
                          select(x, y, frequency_mean), crs = "EPSG:4326")
  writeRaster(Raster_export, here(paste('Figures/ConsecutiveDryDays_North/Rasters/ConsecutiveDryDays_Proj_',futures$Time[i],'_',futures$ssp[i],'.tif',sep="")), overwrite=TRUE)
}
# Graph Visualizations: Projected Future CDD   ---------------------------------
## Data Aggregation ----
cat.var.join <- collect(cat.var)

area_count <- cat.var.join %>%
  group_by(across(all_of(category_names))) %>%
  summarize(area_count = n())
### Historical CMIP Data Aggregation -----
hist <- cmip_hist_cdd %>%
  filter(year %in% c(2000:2020)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>%
  mutate(freq = mult * freq_hist) %>%
  group_by(x, y, model, year) %>%
  summarize(freq_sum = sum(freq))%>%
  group_by(x, y, model) %>%
  summarize(frequency_hist = sum(freq_sum)/21) %>%
  collect()

### Future CMIP Data Aggregation -----
fut_2035 <- cmip_cdd %>%
  filter(year %in% c(2025:2045)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>%
  mutate(freq = mult * freq_fut) %>%
  group_by(x, y, model, ssp, year) %>%
  summarize(freq_sum = sum(freq))%>%
  group_by(x, y, model, ssp) %>%
  summarize(frequency_fut = sum(freq_sum)/21) %>%
  mutate(future = 2035) %>%
  collect()

fut_2060 <- cmip_cdd %>%
  filter(year %in% c(2050:2070)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>%
  mutate(freq = mult * freq_fut) %>%
  group_by(x, y, model, ssp, year) %>%
  summarize(freq_sum = sum(freq))%>%
  group_by(x, y, model, ssp) %>%
  summarize(frequency_fut = sum(freq_sum)/21) %>%
  mutate(future = 2060) %>%
  collect()

fut_out <- fut_2035 %>%
  bind_rows(fut_2060)

cmip_cdd_graph <- fut_out %>%
  full_join(hist, by = c('x', 'y', 'model')) %>%
  mutate(freq_ratio = frequency_fut/frequency_hist) %>%
  mutate(freq_ratio = ifelse(is.infinite(freq_ratio) & frequency_fut != 0, 2, 
                             ifelse(is.na(freq_ratio), 1, freq_ratio)))


### ERA5 Data Aggregation ----
era5_2010 <- era5_cdd %>%
  filter(year %in% c(2000:2020)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(coord = paste(x, y, sep = "_")) %>%
  mutate(mult = floor(count/run.length.cdd)) %>%
  mutate(freq = mult * freq_era5) %>%
  group_by(x, y, year) %>%
  summarize(freq_sum = sum(freq))%>%
  group_by(x, y) %>%
  summarize(frequency_era5 = sum(freq_sum)/21) %>%
  collect()

era5_1985 <- era5_cdd %>%
  filter(year %in% c(1975:1985)) %>%
  filter(count >= run.length.cdd) %>%
  mutate(mult = floor(count/run.length.cdd)) %>%
  mutate(freq = mult * freq_era5) %>%
  group_by(x, y, year) %>%
  summarize(freq_sum = sum(freq))%>%
  group_by(x, y) %>%
  summarize(frequency_era5 = sum(freq_sum)/21) %>%
  mutate(future = 1985) %>%
  collect()

era5_proj_1 <- era5_2010 %>%
  mutate(future = 2010) %>%
  bind_rows(era5_1985) %>%
  mutate(frequency_era5 = ifelse(is.na(frequency_era5), 0, frequency_era5)) %>%
  right_join(cat.var.join, by = c('x', 'y')) %>%
  right_join(area_count, by = category_name) %>%
  group_by(across(all_of(category_names)), future, area_count) %>%
  summarize(sum_hold = sum(frequency_era5))%>%
  mutate(med_proj = sum(sum_hold)/area_count) %>%
  mutate(ssp = 0)

### Calculate projections from ERA5 ----
cmip_projs <- cmip_cdd_graph %>%
  right_join(cat.var.join, by = c('x', 'y')) %>%
  right_join(area_count, by = category_name) %>%
  right_join(era5_2010, by = c('x', 'y')) %>%
  mutate(proj = frequency_era5 * freq_ratio) %>%
  group_by(across(all_of(category_names)), future, ssp, model, area_count) %>%
  summarize(sum_hold = sum(proj)) %>%
  mutate(proj_mean = sum(sum_hold)/area_count) %>%
  filter(!is.na(future)) %>%
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
CDDProj_graph <- projection_graph(y_val = med_proj, y_min = lwr_proj,
                                  y_max = upr_proj, 
                                  y_lab = paste(run.length.cdd, '-day Dry Streak Frequency', sep = ""), 
                                  y_lab_type = "", 
                                  lim_min = Proj_min, lim_max = Proj_max, 
                                  color_pal = precip_colors_discrete)

ggsave(here('Figures/ConsecutiveDryDays_North/PDF/ConsecutiveDryDays_ProjGraph.pdf'), CDDProj_graph, height = 5, width =7, units = 'in', bg = "transparent")
write.csv(comb_proj, here('Figures/ConsecutiveDryDays_North/Tables/ConsecutiveDryDays_ProjTable.csv'), row.names = FALSE)
# Graph Visualizations: Differences CDD   ---------------------------------
cmip_diffs <- cmip_cdd_graph %>%
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

CDDDiff_graph <- difference_graph(yint = 0, y_val = med_percent, y_min = lwr_percent, 
                                  y_max = upr_percent, 
                                  y_lab = paste("Percent Difference in", run.length.cdd, "-day Dry Streaks", sep = ""), 
                                  y_lab_type = "%", lim_min = Diff_min, lim_max = Diff_max, 
                                  color_pal = precip_colors_discrete)

ggsave(here('Figures/ConsecutiveDryDays_North/PDF/ConsecutiveDryDays_DiffGraph.pdf'), CDDDiff_graph, height = 5, width =7, units = 'in', bg = "transparent")
write.csv(comb_diff, here('Figures/ConsecutiveDryDays_North/Tables/ConsecutiveDryDays_DiffTable.csv'), row.names = FALSE)


##### MODEL AGREEMENT MAPS ------
cdd_agr <- model_agreement_direction(cmip_cdd_graph, freq_ratio, 1, paste("Future:Historic Ratio of", run.length.cdd, "-day Dry Streaks", sep = ""))
ggsave(here('Figures/ConsecutiveDryDays_North/PDF/ConsecutiveDryDays_ModelAgreementMap.pdf'), cdd_agr, height = 5, width =7, units = 'in', bg = "transparent")

cdd_range <- model_agreement_magnitude(dataframe = cmip_cdd_graph, metric = freq_ratio, 
                                          color = 'darkblue', title = paste("Range of Future:Historic Ratio of", run.length.cdd, "-day Dry Streaks", sep = "")) 
ggsave(here('Figures/ConsecutiveDryDays_North/PDf/ConsecutiveDryDays_ModelRangeMap.pdf'), cdd_range, height = 5, width =7, units = 'in', bg = "transparent")

##### DISTRIBUTION PLOTS ----
df_out <- cmip_cdd_graph %>% 
  right_join(cat.var.join, by = c('x', 'y'))

cdd_dist <- distribution_graph(xint = 1 , dataframe = df_out, metric = freq_ratio,
                               xlab = paste("Future:Historic Ratio of", run.length.cdd, "-day Dry Streaks", sep = ""), x_lab_type = "")

ggsave(here('Figures/ConsecutiveDryDays_North/PDF/ConsecutiveDryDays_DistributionGraph.pdf'), cdd_dist$graph, height = 10, width =15, units = 'in', bg = "transparent")
write.csv(cdd_dist$table, here('Figures/ConsecutiveDryDays_North/Tables/ModelDistribution_summary.csv'), row.names = FALSE)
write.csv(cdd_dist$table_qs, here('Figures/ConsecutiveDryDays_North/Tables/Quantiles_summary.csv'), row.names = FALSE)

