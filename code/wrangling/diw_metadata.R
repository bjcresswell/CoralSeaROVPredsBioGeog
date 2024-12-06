# Metadata wrangling

source('code/packages.R')

# Biogeographic parameters
reef_biogeo <- read_excel("data/reef_biogeo.xlsx", 2)  |> 
  mutate_if(is.character, as.factor)          # Fix ch vars


# Main metadata file (want a copy of the full metadata so will import that first)
rmetadata_raw <- data.io::read$csv("https://figshare.com/ndownloader/files/42424437?private_link=0a7edbfa91467638b998")  |> 
  as_tibble()
#save(rmetadata_raw, file = "data/Rdata/rmetadata_raw.Rdata")    
#load("data/Rdata/rmetadata_raw.Rdata")    


rmetadata_raw  |> 
  select(Reef_1) |> 
  distinct()


# Need to turn Reef_1_Area into categorical variable so need to know some ranges etc.
reef_biogeo %$% summary(Reef_1_Area)
# Will split into lower, middle and upper quartiles below


# Trim down to only transects on the outside of reefs and add other variables as required - leaves us with 496 transects
rmetadata <- rmetadata_raw  |> 
  #filter(Habitat != "lagoon")  |>   # Get rid of lagoon transects early
  filter(Situation != "inner")  |>   # Get rid of any transect not on an outside reef
  droplevels()  |> 
  mutate(T_ID = paste(Survey, Site, Survey_Day, Dive_No, Transect, sep = "-")) |>   # T_ID variable for wrangling
  left_join(reef_biogeo) %>%  # Combine with biogeographic metadata
  mutate_if(is.character, as.factor) |>             # Fix ch vars
  mutate(Region = fct_relevel(Region, "Far_North", "North"))  |> 
  # Might need to include protection level in models:
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
      ~ "HPZ", .default = "NTMR"))) 

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


