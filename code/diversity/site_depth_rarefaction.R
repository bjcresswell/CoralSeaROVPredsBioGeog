# EDA combining depth bin and site

glimpse(preds)



# 1. First we'll store a list of the environmental properties we'll be interested in at the end.
env_vars <- 
  preds |> 
  select(Region, Reef_1, Site,
         dCT, dGBR,
         Isol, Reef_100,
         Reef_1_Area, Reef_size,
         Depth_bin_meso, Depth, Site_Depth) |> 
  distinct()

# 2. Summarise species accounts by depth bin and site:
depth_site_speccount <- 
  preds |> 
  group_by(Site_Depth, Binomial) |> # Just the grouping variable (single) that we are interested in here
  dplyr::summarise(Count=sum(Number)) |> 
  ungroup()

# 3. Check we are capturing all our expected taxa (106 - but this includes "NA" as taxon - we'll get rid below)
depth_site_speccount |> 
  select(Binomial) |> 
  distinct()
# All good

# 4. Convert into "wide" diversity tbl - 65. We have lost 6 site-depth combos that only had 0 or 1 species
depth_site_matrix <- 
  depth_site_speccount |> 
  pivot_wider(names_from = Binomial, values_from=Count, values_fill = 0) |> 
  dplyr::select(!`NA`) |>  # Get rid of column where the sp name ('Binomial') had been NA
  ungroup() |> 
  mutate(Total = rowSums(across(where(is.numeric)))) |> 
  filter(Total > 4) |>                  # We are removing the 0 or 1 observation transects but might need to put them back in later for calculating means
  select(!Total)


# 4a. Check which sites had only one or less species
#zero_observations <- 
  depth_site_speccount |> 
  pivot_wider(names_from = Binomial, values_from=Count, values_fill = 0) |> 
  dplyr::select(!`NA`) |>  # Get rid of column where the sp name ('Binomial') had been NA
  ungroup() |> 
  mutate_if(is.numeric, ~1 * (. != 0)) |> 
  mutate(Total = rowSums(across(where(is.numeric)))) |> 
  filter(Total < 2) |>                             
  select(Site_Depth, Total)

# Six site-depths:
#  Site_Depth       Total
#  <fct>            <dbl>
# 1 ASHM-03_Lower     1
# 2 FRAN-01_Upper     1
# 3 LORN-01_Lower     0
# 4 OBSN-01_Lower     0
# 5 SHAR-01_Upper     0
# 6 TURT-01_Upper     0  
  
  

# 5. Make matrix for iNEXT input
inext_matrix <- 
  depth_site_matrix |> 
  column_to_rownames("Site_Depth") |> 
  t() #|> 
#  as.data.frame()  |> 
#  rownames_to_column() |> 
#  select(!rowname) |> 
#  as.matrix() |> 
#  t()

# OLD CODE - WHEN I THOUGHT YOU NEEDED TO PROVIDE A LIST OF LISTS
# 6. And list of lists for iNEXT input
#inext_list <- inext_matrix |> 
#  split(row(inext_matrix)) |> 
#  lapply(function(x) x[x!=0])    

# 7. Assign meaningful names back
#names(inext_list) <- depth_site_matrix$Site_Depth     


# 8. Run in iNEXT
# Abundance based
site_depth_rarefied <- 
  inext_matrix |> 
  iNEXT(q = c(0,1),  datatype = "abundance", endpoint = 1000, nboot = 100)



# The basic plots are dreadful (and ggiNEXT even worse)
#plot(site_depth_rarefied, type = 1, ask = FALSE)

site_depth_rarefied$iNextEst$size_based

# Explore
site_depth_rarefied$DataInfo # basic data information.

site_depth_rarefied$AsyEst # asymptotic diversity estimates.

site_depth_rarefied$iNextEst$size_based 
site_depth_rarefied$iNextEst$coverage_based 


# Store asymptotic species richness 
sp_rich_asymp <- env_vars |> 
  right_join(site_depth_rarefied$AsyEst  |> 
               rename(Site_Depth = Assemblage) |>
               filter(Diversity == "Species richness"))



Dest <- estimateD(inext_matrix, datatype = "abundance", base = "coverage", level = 0.99) %>% 
  rename(Site_Depth = Assemblage) %>% 
  full_join(env_vars)  %>% 
 # mutate_if(is.character, as.factor)  %>% 
  replace(is.na(.), 0)
  



Dest |> 
  filter(Order.q == 1) |>  # Shannon
  group_by(Reef_1_Area) |> 
  summarise(median_hdci(qD)) |> 
  ggplot() +
  geom_pointrange(aes(x = Reef_1_Area, y = y, ymin = ymin, ymax = ymax)) +
  geom_smooth(aes(x = Reef_1_Area, y = y), method = "glm")
  
  
Dest |> 
  group_by(Reef_100) |> 
  summarise(median_hdci(qD)) |> 
  ggplot() +
  geom_pointrange(aes(x = Reef_100, y = y, ymin = ymin, ymax = ymax))

Dest |> 
  group_by(Isol) |> 
  summarise(median_hdci(qD)) |> 
  ggplot() +
  geom_pointrange(aes(x = Isol, y = y, ymin = ymin, ymax = ymax))


Dest |> 
  group_by(dCT) |> 
  summarise(median_hdci(qD)) |> 
  ggplot() +
  geom_pointrange(aes(x = dCT, y = y, ymin = ymin, ymax = ymax))



## EDA - move to alt script

# Mean spp richness by depth
sp_rich_asymp |> 
  group_by(Depth) |> 
  summarise(Mean_sp_rich = mean(Estimator), SE = plotrix::std.error(Estimator)) |> 
  ggplot() +
  geom_pointrange(aes(x = Depth, y = Mean_sp_rich, ymin = Mean_sp_rich-SE, ymax = Mean_sp_rich+SE))
  
# Mean spp richness by reef size - categorical
sp_rich_asymp |> 
  group_by(Reef_size) |> 
  summarise(Mean_sp_rich = mean(Estimator), SE = plotrix::std.error(Estimator)) |> 
  ggplot() +
  geom_pointrange(aes(x = Reef_size, y = Mean_sp_rich, ymin = Mean_sp_rich-SE, ymax = Mean_sp_rich+SE))

# Mean spp richness by reef size - continuous
sp_rich_asymp |> 
  group_by(Reef_1, Reef_1_Area) |> 
  summarise(median_hdci(Estimator)) |> 
  ggplot() +
  geom_point(aes(x = Reef_1_Area, y = y))  +
  geom_smooth(aes(x = Reef_1_Area, y = y), method = "glm")

# Mean spp richness by isolation - continuous
sp_rich_asymp |> 
  group_by(Reef_1, Isol) |> 
  summarise(median_hdci(Estimator)) |> 
  ggplot() +
  geom_point(aes(x = Isol, y = y))  +
  geom_smooth(aes(x = Isol, y = y), method = "glm")


# Mean spp richness by isolation - categorical, min, mean and max
sp_rich_asymp |> 
  summarise(Low_isol = quantile(Isol, probs = 0.25), Mean_Isol = mean(Isol), High_Isol = quantile(Isol, probs = 0.75))
  
sp_rich_asymp |> 
    mutate(Isolation = case_when(Isol < 4 ~ "Min.",
                                 Isol > 4 & Isol < 7.07 ~ "Mean",
                               Isol > 7.07 ~ "Max.")) |> 
  group_by(Isolation) |> 
  summarise(Mean_sp_rich = mean(Estimator), SE = plotrix::std.error(Estimator)) |> 
  ggplot() +
  geom_pointrange(aes(x = Isolation, y = Mean_sp_rich, ymin = Mean_sp_rich-SE, ymax = Mean_sp_rich+SE))



# Mean spp richness by reef 
sp_rich_asymp |> 
  group_by(Reef_1) |> 
  summarise(Mean_sp_rich = mean(Estimator), SE = plotrix::std.error(Estimator)) |> 
  ggplot() +
  geom_pointrange(aes(x = Reef_1, y = Mean_sp_rich, ymin = Mean_sp_rich-SE, ymax = Mean_sp_rich+SE))


# Mean spp richness by region
sp_rich_asymp |> 
  group_by(Region) |> 
  summarise(Mean_sp_rich = mean(Estimator), SE = plotrix::std.error(Estimator)) |> 
  ggplot() +
  geom_pointrange(aes(x = Region, y = Mean_sp_rich, ymin = Mean_sp_rich-SE, ymax = Mean_sp_rich+SE))

