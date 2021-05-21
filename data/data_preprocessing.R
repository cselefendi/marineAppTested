library(tidyverse)
library(geodist)
options(scipen = 999)

folder <- '/data'
setwd(folder)

csv_data_file <- 'ships.csv'

df <- read.csv(csv_data_file)

out <- df %>%
  mutate(obs_id = row_number(),
         ts = as.POSIXct(DATETIME, 
                         format="%Y-%m-%dT%H:%M:%OSZ", tz="GMT")) %>%
  select(obs_id, ts, SHIPNAME, ship_type, LAT, LON) %>%
  arrange(SHIPNAME, ts) %>%
  group_by(SHIPNAME) %>%
  mutate(prev_LAT = lag(LAT, 1L, default = NA),
         prev_LON = lag(LON, 1L, default = NA),
         diff_LAT = LAT - prev_LAT,
         diff_LON = LON - prev_LON,
         dist_m = geodist::geodist_vec(x1 = LON, y1 = LAT, 
                                       x2 = prev_LON, y2 = prev_LAT, 
                                       paired = T,
                                       sequential = F,
                                       measure = 'geodesic')) %>%
  ungroup() %>%
  rename(ship_name = SHIPNAME)

ships_data <- out %>%
  select(ship_name, ship_type, ts, dist_m, LAT, LON, prev_LAT, prev_LON) %>%
  arrange(ship_name, ship_type, desc(dist_m), desc(ts)) %>%
  group_by(ship_name) %>%
  filter(row_number() == 1) %>%
  ungroup()

ship_markers <- ships_data %>%
  select(ship_name, ship_type, dist_m, LAT, LON, prev_LAT, prev_LON) %>%
  rename(lat_from = prev_LAT,
         lat_to = LAT,
         lon_from = prev_LON,
         lon_to = LON) %>%
  pivot_longer(cols = -(1:3),
               names_to = 'var',
               values_to = "val") %>%
  mutate(latlon = str_match(var, "lat|lon")[,1],
         fromto = str_match(var, "from|to")[,1]) %>%
  select(-var) %>%
  pivot_wider(id_cols = c(1,2,3,6),
              names_from = latlon,
              values_from = val) %>%
  mutate(label_text = sprintf('%s position for vessel %s for largest distance route (%.0f meters).',
                              ifelse(fromto == 'from', 'Starting', 'End'),
                              ship_name,
                              dist_m),
         popup_text = label_text)

save(ship_markers,
     file = "ships.Rdata")
