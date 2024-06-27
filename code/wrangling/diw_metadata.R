# Metadata wrangling

source('code/packages.R')

# Biogeographic parameters
reef_biogeo <- read_excel("data/reef_biogeo.xlsx", 2) %>% 
  mutate_if(is.character, as.factor)          # Fix ch vars


# Main metadata file (want a copy of the full metadata so will import that first)
rmetadata_raw <- data.io::read$csv("https://figshare.com/ndownloader/files/42424437?private_link=0a7edbfa91467638b998") 
#save(rmetadata_raw, file = "data/Rdata/rmetadata_raw.Rdata")    
#load("data/Rdata/rmetadata_raw.Rdata")    


rmetadata_raw %>% 
  select(Reef_1) %>% 
  distinct()


# Need to turn Reef_1_Area into categorical variable so need to know some ranges etc.
reef_biogeo %$% summary(Reef_1_Area)
# Will split into lower, middle and upper quartiles below


# Trim down to only transects on the outside of reefs and add other variables as required - leaves us with 496 transects
rmetadata <- rmetadata_raw %>% 
  filter(Habitat != "lagoon") %>%  # Get rid of lagoon transects early
  filter(Situation != "inner") %>%  # Get rid of any transect not on an outside reef
  droplevels() %>% 
  mutate(Aspect_descriptive = replace(Aspect_descriptive, Aspect_descriptive == "modertae_slope", "moderate_slope")) %>%  # Fix variable spelling
  mutate(T_ID = paste(Survey, Site, Survey_Day, Dive_No, Transect, sep = "-"),  # T_ID variable for wrangling
         Depth_bin = factor(case_when(Depth_m < 11 ~ '0-10',
                                      Depth_m >= 11  & Depth_m < 21 ~ '11-20', 
                                      Depth_m >= 21  & Depth_m < 31 ~ '21-30', 
                                      Depth_m >= 31  & Depth_m < 41 ~ '31-40', 
                                      Depth_m >= 41  & Depth_m < 51 ~ '41-50', 
                                      Depth_m >= 51  & Depth_m < 61 ~ '51-60', 
                                      Depth_m >= 61  & Depth_m < 71 ~ '61-70', 
                                      Depth_m >= 71  & Depth_m < 81 ~ '71-80', 
                                      Depth_m >= 81  & Depth_m < 91 ~ '81-90', 
                                      Depth_m >= 91  ~ '91-100'), ordered = TRUE),
         Depth_bin_meso = fct_relevel(factor(case_when(Depth_m < 31 ~ 'Altiphotic',
                                              Depth_m >= 31  & Depth_m < 61 ~ 'Upper_mesophotic', 
                                              Depth_m >= 61   ~ 'Lower_mesophotic')), "Altiphotic", "Upper_mesophotic")) %>% 
  mutate(Depth = factor(case_when(grepl("Altiphotic", Depth_bin_meso) ~ "Shallow",
                                  grepl("Upper_mesophotic", Depth_bin_meso) ~ "Upper",
                                  grepl("Lower_mesophotic", Depth_bin_meso) ~ "Lower"))) %>% 
  mutate(Depth = fct_relevel(Depth, c("Shallow", "Upper")))  |>
  mutate(Site_Depth = factor(paste(Site, Depth, sep = "_")))  |>  
  left_join(reef_biogeo) %>%  # Combine with biogeographic metadata
  mutate_if(is.character, as.factor) %>%             # Fix ch vars
  mutate(Region = fct_relevel(Region, "Far_North", "North")) %>% 
  mutate(Reef_size = factor(case_when(Reef_1_Area < 3.5 ~ 'LQ',
                                      Reef_1_Area >= 3.5 & Reef_1_Area < 10.3 ~ 'Mean',
                                      Reef_1_Area >= 10.3 ~ 'UQ'))) %>% 
  mutate(Isol = sqrt(dGBR) / sqrt(Reef_100),      
         #Isol = sqrt(dGBR+dNN) / sqrt(Reef_1_Area + Reef_100),
         IsolRank = dense_rank(desc(Isol))) %>% 
  mutate(Reef_1 = fct_reorder(Reef_1, desc(Isol))) %>% 
  mutate(Zone = factor(                # Ideally want to get this all into the metadata
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
      ~ "HPZ", .default = "NTMR"))) %>% 
    tibble()

rmetadata %>% str()
rmetadata %$% summary(Aspect_descriptive)
rmetadata %$% summary(Reef_1)                            
rmetadata %$% summary(Habitat)                           
rmetadata %$% summary(Depth_bin)
rmetadata %$% summary(Depth_bin_meso)
rmetadata %$% summary(Isol)

rmetadata %>% 
  select(Reef_1, Isol, IsolRank) %>% 
  distinct() %>% 
  arrange(Isol)

#glimpse(rmetadata)
#summary(rmetadata)
rmetadata %$% str(T_ID)
rmetadata %$% summary(Aspect_descriptive)

#modsave(rmetadata, file = "data/Rdata/rmetadata.Rdata")    
#load("data/Rdata/rmetadata.Rdata")    


