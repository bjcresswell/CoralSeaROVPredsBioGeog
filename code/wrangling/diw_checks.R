

#source('code/diw_rawdata.R')


fish %$% summary(Functional.Group)
fish %$% summary(Trophic.Group)


# See numbers of piscivores by trophic vs functional group  
piscivores_tg <- fish %>% filter(str_detect(Trophic.Group, "pisc"))
piscivores_fg <- fish %>% filter(str_detect(Functional.Group, "pisc"))

# These are different sizes - see what excluded from the functional group tbl
missing_piscs1 <- anti_join(piscivores_tg, piscivores_fg) # The FG var has some of these species down as carnivores and also benth invertivores
missing_piscs2 <- anti_join(piscivores_fg, piscivores_tg) # 

# Cross-compare
piscivores_fg %$% summary(Trophic.Group)
piscivores_tg %$% summary(Functional.Group)


# We have some rows (transects) with 0 obs - currently 38:
zero_obs <- fish %>%  filter(Zero_obs == TRUE)
NAS <- fish %>% filter(is.na(Functional.Group))
NA_check <- NAS %>% anti_join(zero_obs) # 1 transect that should be changed to Zero_obs = TRUE

# Explore
fish %$% summary(Functional.Group)
fish %$% summary(Trophic.Group)
fish %$% summary(Family) 


# Checks by reef
fish %$% summary(Reef_1)
fish %$% summary(Reef_2) 

# Reef 2 has Lihou, Flinders and Holmes reefs broken down into sub-reefs:
fish %>% 
  filter(Reef_1 == "Lihou") %>% 
  droplevels() %$% 
  summary(Reef_2)

fish %>% 
  filter(Reef_1 == "Flinders") %>% 
  droplevels() %$% 
  summary(Reef_2)

fish %>% 
  filter(Reef_1 == "Holmes") %>% 
  droplevels() %$% 
  summary(Reef_2)

# So we'll use Reef_2 as our reef variable


# Check what reefs have only small number of transects - Boot has only 3 transects
check_reef1 <- fish %>% 
  dplyr::select(Reef_2, T_ID) %>% 
  distinct() %>% 
  group_by(Reef_2) %>% 
  summarise(Count = n()) %>% 
  arrange(Count)

# Can do the same with metadata to double check
check_reef2 <- rmetadata %>% 
  dplyr::select(Reef_2, T_ID) %>% 
  distinct() %>% 
  group_by(Reef_2) %>% 
  summarise(Count = n()) %>% 
  arrange(Count)

# Compare
check_reef2 %>% anti_join(check_reef1)

# Reefs with low numbers of transects:
boot <- fish %>% 
  filter(Reef_2 == 'Boot') %>% 
  droplevels()

chill <- fish %>% 
  filter(Reef_2 == 'Chilcott') %>% 
  droplevels()

wreck <- fish %>% 
  filter(Reef_2 == 'Wreck') %>% 
  droplevels()



# Checks by taxa
parrots <- fish %>% filter(Family == "Scaridae")
no_genus <- fish %>% filter(is.na(Genus))
serranids <- fish %>% filter(Family == "Serranidae")

# Investigate how many occurances of C undulatus there are
fish %>% filter(Genus == "Cheilinus")

hawks <- fish %>% 
  filter(Family =="Cirrhitidae") %>% 
  select(Family, Genus, Species, Trophic.Group, Functional.Group) %>% 
  distinct()

holocentrids <- fish %>% 
  filter(Family =="Holocentridae") #%>% 
  select(Family, Genus, Species, Trophic.Group, Functional.Group) %>% 
  distinct()

holocentrids %>% 
  filter(str_detect(Functional.Group, "pisc") |
           str_detect(Functional.Group, "carn") |
           str_detect(Trophic.Group, "pisc"))

serranids <- fish %>% 
  filter(Family =="Serranidae") %>% 
  select(Family, Genus, Species, Trophic.Group, Functional.Group) %>% 
  distinct()

clupeids <- fish %>% 
  filter(Family =="Clupeidae") #%>% 
#select(Family, Genus, Species, Trophic.Group, Functional.Group) %>% 
#distinct()


# Check genera assigned to correct families and trophic vs functional group
famcheck <- fish %>% 
  group_by(Family, Genus, Trophic.Group, Functional.Group) %>% 
  summarise()

# Checks by trophic group
omnis <- fish %>% filter(`Trophic Group` == "omnivore") # Def don't want these in preds tbl


# Places

chilcott <- preds %>% filter(Reef_1 == "Chillcot")
saumarez <- fish %>% filter(Reef == "Saumarez")
frederick <- fish %>% filter(Reef == "Frederick")
marion <- fish %>% filter(Reef == "Marion")
osprey <- fish %>% filter(Reef == "Osprey")
boug <- fish %>% filter(Reef == "Bougainville")
boot <- fish %>% filter(Reef_1 == "Boot")
herald_preds <- preds %>% filter(Reef_1 == "Herald")


# Check benthic complexity scores
preds  %$%  summary(Benthic_Complexity)
preds %>%  filter(Benthic_Complexity > 4)
  

