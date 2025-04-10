## Wrangle raw data


# Housekeeping + prelims
#rm(list=ls())
#dev.off()
source("code/packages.R")

# Load main fish data - 18487 rows (V1 was 15274 rows)
fish_raw <- data.io::read$csv("https://figshare.com/ndownloader/files/42424440?private_link=0a7edbfa91467638b998") |> 
  mutate_if(is.character, as.factor) |>                   # Fix ch vars
  as_tibble()

#save(fish_raw, file = "data/Rdata/fish_raw.Rdata")    
#load("data/Rdata/fish_raw.Rdata")    


#glimpse(fish)
#head(fish)

# Load metadata - 617 rows total corresponding to # Transects. 496 transects, with reef interiors excluded
source("code/wrangling/diw_metadata.R")
# OR
#load("data/Rdata/rmetadata.Rdata")    


# Combine main data with metadata - 14279 (V1 - 12146) now that transects from reef interiors are excluded
# We will include 0 obs transects in from the rmetadata
fish <- fish_raw |> 
  full_join(rmetadata) |> 
  filter(Situation != "inner") |>   # Get rid of any transect not on an outside reef
  droplevels() |> 
  mutate(Reef_1 = fct_reorder(Reef_1, desc(Site_lat))) |>  # Set Reef_1 to appear N-S in figures 
  mutate(Number = replace_na(Number, 0))  # Need to replace NAs in transects where there were no observations
  
# Check - 4217 records that are from the inside of reefs (lagoon etc)
insiders <- fish_raw |> 
  anti_join(fish)

18487 - 4217 # = 14270 which is..
14279 - 14270 # 9 short of the expected number. 

#This is due to the transects with 0 observations of any fishes on 9 transects - GOOD!
fish |> filter(is.na(OpCode))

# There are 538 transects listed in the metadata. Check they all made it into the final df:
fish |>  select(T_ID) |>  distinct() |> tally() # 538 

# There were 700 transects in the raw metadata so the rest must be in from reef interiors:
insiders |> select(T_ID) |>  distinct() |> tally() # 125

538 + 125 # = 663. Missing 37. Find out what these are:

# Diffs between rmeta raw and rmeta:
non_meta <- anti_join(rmetadata_raw, rmetadata) |> as_tibble()  # 162
non_fish <- insiders |> select(T_ID) |> distinct()            # 125
check <- anti_join(non_meta, non_fish)                          

# This contans our missing transects = they are all lagoon/inside reef and we don't need them:
check |> filter(Situation == "inner") |> tally()
  

#save(fish, file = "data/Rdata/fish.Rdata")    
#load(file = "data/Rdata/fish.Rdata")    


# Extract predator fishes, based on established criteria - 1312 total actual obs
preds <- fish |> 
  filter(str_detect(Functional.Group, "pisc") |
           str_detect(Functional.Group, "carn") |
           str_detect(Trophic.Group, "pisc") |
           #Trophic.Level >= 3.7 |
           Family =="Carangidae" | # Trevallies 
           Family =="Scombridae" | # Mackerels/tunas
           Family == "Lethrinidae" | # Emperors
           Family == "Lutjanidae" | # Snappers
           Family == "Haemulidae" | # Grunts/sweetlips
           Family == "Sphyraenidae" | # Barracudas
           Family == "Serranidae" | # Groupers (also contains pseudanthias etc - will get rid of these below)
           Family == "Synodontidae" | # lizardfishes - mostly ID'd to genus so get filtered out because trophic level/group etc. is NA
           Family == "Epinephelidae" | # Some of the groupers listed under this fam
           Family == "Carcharhinidae" | # Sharks
           Family == "Aulostomidae" | # Trumpet fish (all classified as piscivores, but gets kicked out where sp is not identified)
           Family == "Pinguipedidae" | # sandperches - again mostly ID'd to genus so get filtered out because trophic level/group etc. is NA
           Binomial == "Cheilinus undulatus") |> # Allowing this one large omnivore in - there are only a few so doesn't make a massive difference anyways  
  # Things to get rid of manually...
  filter(Family != "Echeneidae" & # Get rid of remoras
           #Family != "Apogonidae" & # Could get rid of cardinal fishes - but all 4 obs in the whole df are small piscivore (Cheilodipterus)
           Genus != "Pseudanthias" &  # Get rid of fairy basslets/anthias
           Genus != "Serranocirrhitus" &  # .. as above
           Genus != "Pyronotanthias" &  # .. as above
           Genus != "Liopropoma" &  # .. as above
           Genus != "Selar" & # Planktivorous carangid (only one observation but taking out anyways)
           Genus != "Chromis" & # couple observations snuck in (listed under Serrandidae erroneously = need a long term fix)
           !grepl("cleaner", Functional.Group)) |>  # and cleaner wrasse etc
          # Trophic.Level >= 3.3) |> 
  full_join(rmetadata) %>%                 # The full join with the rmetadata needs to happen before...
  mutate(Number = replace_na(Number, 0)) %>% 
  droplevels()

preds %$% summary(Zone)


# 538 transects, once filtered down
preds %>% select(T_ID) %>%  distinct() |> tally()

# Should have no NAs in the Number var
preds %$% summary(Number)
preds |>  filter(is.na(Number))

# Check what is excluded (13116 observations)
nonpreds <- fish |> 
  anti_join(preds) |> 
  droplevels()



# See if there are any "non-preds" with high trophic level. 
nonpreds %$% 
  summary(Trophic.Level) # Quite a few

# Investigating further...
top_nonpreds <- nonpreds |> 
  filter(Trophic.Level > 3.4) |> 
  select(Family, Genus, Species, Trophic.Group, Functional.Group, Trophic.Level, Habitat) |> 
  distinct()

# looks ok. The really high level ones are mostly the symbionts - cleaner wrasse, remora etc Lots of goat fishes and mid-level sp

# Overall check of non-preds:
nonpreds_check <- nonpreds |> 
  group_by(Family, Genus, Species, Trophic.Group, Functional.Group) |> 
  summarise()

# Overall check of preds: 103 different "taxa" 
preds_check <- preds |> 
  select(Family, Genus, Species, Trophic.Group, Functional.Group) |> 
  distinct()
# Note some of these are just genus level obs and also there is a line of just NAs so really, it's 102 taxa


# Overall check of fishes:
fish_check <- fish |> 
  select(Family, Genus, Species, Trophic.Group, Functional.Group) |> 
  distinct()

# Alt way of checking nonpreds - should have same # of taxa as the first one
nonpreds_check2 <- fish_check  |> 
  anti_join(preds_check)


# Checks to see if any non-predators have crept into final tbl

## By trophic level
preds %$% 
  summary(Trophic.Level) 

# Some NAs - check
trophic_NAs <- preds |> 
  filter(is.na(Trophic.Level))

# Not sure how these have crept in but they are all solid predator taxa

## Check spp under 3.5
preds |>  
  filter(Trophic.Level < 3.5) |> 
  select(Family, Genus, Species, Trophic.Group, Functional.Group, Trophic.Level) |> 
  distinct()
# 5 taxa - all look reasonable

## By body size
preds %$% summary(Length_mm) # Some crazy small measurements in here - pass on to GFG
small_preds <- preds |> filter(Length_mm <300) # 
#lg_preds <- preds %>% filter(Length_mm >300) # 

# Now can explore some summary stats
## No of sp
predtaxa <- preds |> group_by(Binomial) |> summarise() # 102 taxa total

## No of families
(fams <- preds |> group_by(Family) |> summarise()) # 22 fams total

## Sites
preds |> group_by(Site) |> summarise() |> tally() # 33 sites..

## Site - Depth combinations
preds |> group_by(Site, Depth) |> summarise() |> ungroup() |>  tally() # 79 site - depth combos


## Reefs
preds |> group_by(Reef_1) |> summarise() |> tally() # ..over 13 reefs

## Transects
preds |> group_by(T_ID) |> summarise() |> tally() # 538 transects


# Save if required
#save(preds, file = "data/Rdata/preds.Rdata")    
#load("data/Rdata/preds.Rdata")    


