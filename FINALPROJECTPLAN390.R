library(tidyverse)
library(sf)
census = read_sf("orange_durham_wake_block_groups.shp")
View(census[1:20,])
ggplot() +
  geom_sf(data=census)
census_demographics = read_csv("triangle_census.csv")
any(duplicated(census$GEOID))
any(duplicated(census_demographics$GEOID))
census_demographics = read_csv(
  "triangle_census.csv",
  col_types=c(GEOID="character")
)
census = left_join(census, census_demographics, on="GEOID")
#Joined two datasets together so I can map demographics onto block groups and census tracts
View(census[1:20,])
potholes = read_csv("Cityworks_Potholes.csv")
View(potholes)
potholes = filter(potholes, !is.na(X))
# There were some missing data for the coordinates which I had to remove to convert to shape
potholes = st_as_sf(potholes, coords=c("X", "Y"), crs=4326)
# Converted to shapefile so I can map it.
ggplot() +
  geom_sf(data=census) +
  geom_sf(data=potholes, color="red")
wake_census = filter(census, COUNTYFP == "183")
# This is only data related to Raleigh so I only need wake county
ggplot() +
  geom_sf(data=wake_census) +
  geom_sf(data=potholes, color="red")
wake_census = st_transform(census, 32119)
potholes = st_transform(potholes, 32119)
intersects = st_intersects(wake_census, potholes)
wake_census$potholes_zone = apply(intersects, 1, any)
ggplot() +
  geom_sf(data=wake_census, aes(fill=potholes_zone)) +
  geom_sf(data=potholes, color="red")
wake_census$potholes_zone = apply(intersects, 1, sum)
#Now I know the number of potholes reported in each census tract
View(wake_census)
wake_census = filter(wake_census, potholes_zone > 3)
# I only needed data relevant to Raleigh so I filtered out areas without potholes
ggplot() +
  geom_sf(data=wake_census, aes(fill=potholes_zone)) +
  geom_sf(data=potholes, color="red")
wake_census2 = st_join(wake_census, potholes)
#Joined these together so the hour column would be with census data
view(wake_census2)
group_by(wake_census, TRACTCE) %>%
  summarize(pct_black=sum(race_black_african_american_alone) / sum(total_population) * 100)
# Finding demographics of each block group
group_by(wake_census, TRACTCE) %>%
  summarize(pct_poor=sum(households_income_less_than_35k) / sum(total_households) * 100)
group_by(wake_census, TRACTCE) %>%
  summarize(pct_potholes=sum(potholes_zone) / sum(total_households) * 100)
wake_census$pct_poc = wake_census$race_black_african_american_alone / wake_census$total_population * 100
wake_census$pct_poor = wake_census$households_income_less_than_35k / wake_census$total_households * 100
wake_census$pct_potholes = wake_census$potholes_zone / wake_census$total_households * 100
ggplot(data = wake_census, aes(x = pct_poor, y = pct_potholes)) +
  geom_point() +
  stat_smooth(method = 'lm')
#First scatterplot shows small correlation
ggplot(data = wake_census, aes(x = pct_poc, y = pct_potholes)) +
  geom_point() +
  stat_smooth(method = 'lm')
#Even smaller correlation
group_by(wake_census2, TRACTCE) %>%
  summarize(hours_avg = mean(hours))
wake_census3 = group_by(wake_census2, TRACTCE, BLKGRPCE) %>%
  summarize(hours_avg = mean(hours))
view(wake_census3)
wake_census = left_join(wake_census, as_tibble(wake_census3), by=c("TRACTCE", "BLKGRPCE"))
view(wake_census)
ggplot(data = wake_census, aes(x = pct_poor, y = hours_avg)) +
  geom_point() +
  stat_smooth(method = 'lm')
#Pothole repairs take shorter in low-income communities
ggplot(data = wake_census, aes(x = pct_poc, y = hours_avg)) +
  geom_point() +
  stat_smooth(method = 'lm')
crashes = read_sf("Reported_Crash_Locations.shp")
#Shapefile for locations of crashes in Raleigh
crashes = st_transform(crashes, 32119)
intersects = st_intersects(wake_census, crashes)
wake_census$crashes_zone = apply(intersects, 1, sum)
group_by(wake_census, TRACTCE) %>%
  summarize(pct_crashes=sum(crashes_zone) / sum(total_households) * 100)
wake_census$pct_crashes = wake_census$crashes_zone / wake_census$total_households * 100
ggplot(data = wake_census, aes(x = pct_crashes, y = pct_potholes)) +
  geom_point() +
  stat_smooth(method = 'lm')
#Strong correlation between potholes and crashes
ggplot(data = wake_census, aes(x = pct_poor, y = pct_crashes)) +
  geom_point() +
  stat_smooth(method = 'lm')



