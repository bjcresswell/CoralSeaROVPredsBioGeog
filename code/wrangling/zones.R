# Adding zonation

zones <- 
  fish_raw %>% 
  select(Reef_1, Site, Site_lat, Site_long) %>% 
  distinct() %>% 
  mutate(Zone = 
           case_when(
             Reef_1 == "Bougainville" & Site_long < 147.108883 & Site_lat > -15.495 | 
               Reef_1 == "Marion" & Site_lat < -19.118333 |
               Reef_1 == "Kenn" & Site_lat < -21.215283 |
               Reef_1 == "Ashmore" |
               Reef_1 == "Boot" |
               Reef_1 == "Diamond Islets" |
               Reef_1 == "Flinders" |
               Reef_1 == "Frederick" |
               Reef_1 == "Heralds Surprise" |
               Reef_1 == "Holmes" |
               Reef_1 == "Saumarez" |
               Reef_1 == "Willis" |
               Reef_1 == "Wreck" 
               ~ "HPZ", .default = "NTMR")) 