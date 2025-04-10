# Wrangling Notes


# Introduction
This repo works off the ROV fish dataset found at https://figshare.com/ndownloader/files/41975694?private_link=0a7edbfa91467638b998. This dataset contains observations from 617 transects across 17 reefs, as can be found by loading the metadata, found at https://figshare.com/ndownloader/files/42151230?private_link=0a7edbfa91467638b998.The main wrangling script brings these two files together and also combines with a spatial/biogeographic metadata df, currently saved locally but I will upload to a repo at some point.


## Data wrangling:

### Exclusion of lagoon transects
Early in the diw, lagoon transects are removed from the dataset, as these add only noise to the data. Prob need a reference for justifying this but definitely an established concept (conversations with JHC etc). With lagoon excluded from the dataset there are only 514 transects in the data. This also has the effect of removing the following:

- Marion Reef: 25 transects at Marion so far, from 2 trips: Feb 21 and Feb 22. All were in the lagoon so Marion is removed from further analysis
- Chilcott Reef: 4 transects at Chilcott on our first ever trip. Only one fish observed in total!! Chilcott also removed from further analysis

### Other problem reefs and wrangling issues
Sampling is unbalanced throughout, a result of the opportunistic and weather dependent nature of the project in general. There are a few reefs that warranted further investigation to see if they should be excluded from the analysis:

- Frederick: 2 transects, 9 obs of predators total
- Wreck: 3 transects, 5 obs of predators total
- Boot: 3 transects, 22 obs of predators

Will leave these sites in the analysis for now.

### Wrangling predator observations
From the whole fish assemblage dataset, observations of predators are extracted in the script diw_rawdata.R. The output of this process is a df called "preds". This df is then used to create two further dfs: predsum, which is used for modeling abundance and preddiv, used for modelling the diversity metrics and also for multivariate assemblage structure work.


# Variables

## Response variables
We are interested in the structure of predatory fish assemblages, specifically in terms of:
1. Distribution and abundance of numbers of predators
2. Trends in diversity (e.g. highs vs lows, and what drives these)
3. Patterns in community structure and where breaks are in terms of depth

# Predictor variables: fixed effects
The aim of this study is to dig deeper into predator assemblages on remote, oceanic seamounts and investigate whether assemblages vary with depth, so *depth* is our first fixed effect. There is growing evidence that depth mitigates the effects of other biogeographic drivers such as *latitude* and *longitude*

Further, previous work on seamounts and pinnacles suggest that neither seamount *size* nor situation (i.e. *isolation*) are limiting factors for predatory and general fish assemblages, so we also want to test the interacting effects of this.

Finally, in the context of marine spatial planning, it is possible that seamounts in protected areas may benefit from more diverse and abundant predatory fish assemblages as these organisms are not targeted by fishers. So we will also model assemblages against *MPA zonation*


## Latitude
Like many of the other variables incorporated here this is a continuous variable. It ranges from -9.985 in the North (Boot) through to -22.199 in the South and probably needs needs converting into a binned variable make sense of it: Far North, North, Central and South. This is captured in the variable "Region" in the metadata


## Depth
Again, a continuous variable that needs to be binned into a 10 bin ordered variable


## Random effects: reef and/or site
Neither reef nor site are interesting as a fixed effect (although individual reef will co-vary in line with reef area as a fixed effect in any model). As a random effect, there is not enough replication at the site level to use this as a R.E. so we'll use reef (specifically Reef_1) as a random effect, with 15 levels once Marion and Chilcott are removed.






