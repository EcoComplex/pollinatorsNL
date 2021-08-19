
### R script to:
#import, explore and analyse simulations
#compute pollination service
#compute structural properties in bipartite plant pollinator networks ###
#last edited 19/08/2021


#load packages
library("bipartite") #Needed to calculate and analyse bipartite networks
library("tidyverse") #Needed to easily summarise and analyse data 
library("lubridate") #Getting R to agree that your data contains the dates and times

#set wd (Susanne's)
setwd("Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_15/R_scripts")

#specify data path file
#Susanne's
wd.path<-"Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_15/Simulation_experiments/2ndweek_aug/ex1"
#Leo's
wd.path <- "/home/leonardo/Academicos/GitProjects/NL_proyect/ex1"

#read list with simulations
listfile<- list.files(wd.path, pattern = "^Visits.*csv",full.names = T, recursive = TRUE)

# testing with less files
#listfile <-listfile[1:4] 

# Read the habitat parameters file
hab_parms <- read_delim(paste0(wd.path,"/Run_habitat_parameters.csv"),";",col_names = FALSE)
colnames(hab_parms) <- c("simul_fn","landscape_type","land_cover_classes", "seed_percent", "habitat_proportions","mean_free_path")


# Read all simulations in simul_fn and merge with the info in habitat parameters file
mdl <- lapply(listfile, function(x){ 
  ff <- read_delim(x,";") %>% mutate(simul_fn=paste0("Simulations/",basename(x))) %>% inner_join(hab_parms)
})
 
# Combine the list of all data.frames into one 
#
mdl <- bind_rows(mdl)
# Correct names
names(mdl) <- str_trim(names(mdl))

#Analyse data
#
# where n_visits is the number of visits to the same plant

# To get visits of individual pollinators to the same plant you need to consider plant_patch 
res <- mdl %>% group_by(seed_percent, pollinator_species,plant_species,pollinator_agent,plant_patch) %>% summarize(n_visits=n()) %>% filter(n_visits>1)

# To get visits to the same plant species by individual pollinator 
res <- mdl %>% group_by(seed_percent, pollinator_species,plant_species,pollinator_agent) %>% summarize(n_visits=n()) %>% filter(n_visits>1)

# To get visits to the same plant species by pollinator species 
res <- mdl %>% group_by(seed_percent, pollinator_species,plant_species) %>% summarize(n_visits=n()) %>% filter(n_visits>1)

# foraging distance of one pollinator
#
res <- mdl %>% filter(pollinator_agent==0) %>%  select(run,plant_patch,foraging_distance) 

###########################################################################################################

#To calculate consecutive visits 
#load function
#pl.no<-c(2,3,3,4,5,5,5,6,7) #example of an agent's plant visits 
source("consecutive_visits.R") 

# Read the file list again as we need a list structure, and not one data.frame, to run the loop
file.list <- lapply(listfile, function(x){ 
  ff <- read_delim(x,";") %>% mutate(simul_fn=paste0("Simulations/",basename(x))) %>% inner_join(hab_parms)
})
#trim the column names and make data.frames of tibbles
#convert to data.frame because of subsetting the way I did does not work with tibbles, can be adjusted
for (i in 1:length(file.list)){
  names(file.list[[i]]) <- str_trim(names(file.list[[i]])) #trim col names
  file.list[[i]] <- as.data.frame(file.list[[i]]) #make data.frames
}

#Loop on the simulation file list to summarise data and calculate consecutive visits to same plant (minimum twice), and which plant
#consecutive visits to same plant species (minimum twice), and which plant
agent.data<-list()
for (z in 1:length(file.list)){ 
  
  vis<-c() 
  sp.no<-c()
  mean.forag.dist<-c()
  seed<-unique(file.list[[z]]$seed_percent) 
  ex_run<-file.list[[z]] 
  #run.no<-unique(ex_run$run)
  poll.no<-length(unique(ex_run$pollinator_agent)) #pollinator number in file.list z=[1:50]
  #loop on pollinator agents to calculate agent-wise consecutive visits to same plant species
  for (i in 1:poll.no){
    
    ags.id<-sort(unique(ex_run$pollinator_agent))  #agents ids of the z file
    ag.rows<-ex_run[which(ex_run$pollinator_agent==ags.id[i]),] #rows of agent i
    
    pl.no<-ag.rows[, 6] #subset plant visits of agent i 
    sp.no[i]<-unique(ag.rows$pollinator_species) #which species does agent i belong to
    mean.forag.dist[i]<-mean(ag.rows[,9]) #subset foraging distance column
    
    freq.con.vis<-con.vis(pl.no) #calculates consecutive visits
    vis<-c(vis, freq.con.vis)    #stores the visits
  
  }
  agent.data[[z]]<-as.data.frame(cbind(ags.id, sp.no, vis, mean.forag.dist, seed))
  colnames(agent.data[[z]])<- c("agent.id", "species_no", "con_visits", "mean.foraging.dist", "seed.percent")
}

#check agent data file
head(agent.data[[1]])
#create one file
one.file_ex1 <- bind_rows(agent.data)
#names(one.file_ex1) <- str_trim(names(agent.data))

# some stats
#boxplot etc
par(mfrow=c(1,1)) 
res.con.vis<-one.file_ex1 %>% group_by(agent.id, species_no, seed.percent) %>% summarise(mean.con.vis=con_visits)
boxplot(mean.con.vis ~ seed.percent, data = res.con.vis, main="Ex1", xlab="seed.percent", ylab="con.visits") 
boxplot(mean.con.vis ~ species_no, data = res.con.vis, main="Ex1", xlab="pollinator species", ylab="con.visits") 

#loop to calculate percent.seed-wise simulations 
par(mfrow=c(2,3)) 
s<-unique(res.con.vis$seed.percent)
for (i in 1:length(s)){
  t<-filter(res.con.vis, seed.percent==s[i])
  boxplot(t$mean.con.vis ~ t$species_no, data = res.con.vis, main=paste("Ex5",s[i]), xlab="pollinator sp", ylab="con.visits") 
}







