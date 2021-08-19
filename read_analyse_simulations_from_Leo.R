
### R script to:
#import, explore and analyse simulations
#compute structural properties in bipartite plant pollinator networks ###
#last edited 13/08/2021


#load packages
library("bipartite")
library("tidyverse")
library("lubridate")


#specify path
# wd.path<-"Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_14/Simulation_experiments/2ndweek_aug/ex1"
wd.path <- "/home/leonardo/Academicos/GitProjects/NL_proyect/ex1"

listfile<- list.files(wd.path, pattern = "^Visits.*csv",full.names = T, recursive = TRUE)

# testing with less files
listfile <-listfile[1:4] 

# Read the habitat parameters file
#
hab_parms <- read_delim(paste0(wd.path,"/Run_habitat_parameters.csv"),";",col_names = FALSE)
colnames(hab_parms) <- c("simul_fn","landscape_type","land_cover_classes", "seed_percent", "habitat_proportions","mean_free_path")

#From Leo
# Read all simulations in simul_fn and merge with the info in habitat parameters file

mdl <- lapply(listfile, function(x){ 
  ff <- read_delim(x,";") %>% mutate(simul_fn=paste0("Simulations/",basename(x))) %>% inner_join(hab_parms)
})
 
# Combine the list of all data.frames in one
#
mdl <- bind_rows(mdl)
# Correct names
names(mdl) <- str_trim(names(mdl))

#
# The result is one dataframe with all simulations
#

# To get visits of individual pollinators to the same plant you need to consider plant_patch 

res <- mdl %>% group_by(seed_percent, pollinator_species,plant_species,pollinator_agent,plant_patch) %>% summarize(n_visits=n()) %>% filter(n_visits>1)

# To get visits to the same plant species by individual pollinator 

res <- mdl %>% group_by(seed_percent, pollinator_species,plant_species,pollinator_agent) %>% summarize(n_visits=n()) %>% filter(n_visits>1)


# To get visits to the same plant species by pollinator species 

res <- mdl %>% group_by(seed_percent, pollinator_species,plant_species) %>% summarize(n_visits=n()) %>% filter(n_visits>1)


# where n_visits is the number of visits to the same plant


# Check foraging distance of one pollinator
#
res <- mdl %>% filter(pollinator_agent==0) %>%  select(run,plant_patch,foraging_distance) 


