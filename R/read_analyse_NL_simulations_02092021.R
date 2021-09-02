
### R script to:
#import, explore and analyse simulations
#compute pollination service
#compute structural properties in bipartite plant pollinator networks ###
#last edited 02/09/2021


#load packages
library("bipartite") #Needed to calculate and analyse bipartite networks
library("tidyverse") #Needed to easily summarise and analyse data 
library("lubridate") #Getting R to agree that your data contains the dates and times
#library("viridis")
library("RColorBrewer")
library("igraph")

#Figure dir and pixels
ppi<-300 #pixels per inches
dirF<-"../Figures/"

#set wd (Susanne's)
setwd("Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_15/R_scripts")

#specify data path file
#Susanne's
#wd.path<-"Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_16/sim_exp/ex1" #neutral model
wd.path<-"Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_15/Simulation_experiments/aug_2nd_run/ex1"
wd.path<-"Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_15/Simulation_experiments/aug_2nd_run/ex6" #effect of niche and preferences
#wd.path<-"Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_15/Simulation_experiments/2ndweek_aug/ex3"

#Leo's
#wd.path <- "/home/leonardo/Academicos/GitProjects/NL_proyect/ex1"

#read list with simulations
listfile<- list.files(wd.path, pattern = "^Visits.*csv",full.names = T, recursive = TRUE)
# testing with less files
#listfile <-listfile[1:5] 

# Read the habitat parameters file
hab_parms <- read_delim(paste0(wd.path,"/Run_habitat_parameters.csv"),";",col_names = FALSE)
colnames(hab_parms) <- c("simul_fn","landscape_type","land_cover_classes", "seed_percent", "habitat_proportions","mean_free_path")

############################################################################

# Read all simulations in simul_fn and merge with the info in habitat parameters file
mdl <- lapply(listfile, function(x){ 
  ff <- read_delim(x,";") %>% mutate(simul_fn=paste0("Simulations/",basename(x))) %>% inner_join(hab_parms)
})
# Combine the list of all data.frames into one 
mdl <- bind_rows(mdl)
# Correct names
names(mdl) <- str_trim(names(mdl))

#Pollinator parameters
#load pollinator parameters for analyses
poll.params<-read.csv(paste0(wd.path,"/pollinator_parameters_ex1.csv")) #ex6
poll.params<-read.csv(paste0(wd.path,"/pollinator_parameters_ex6.csv")) #ex6

#plant parameters
plant.params<-read.csv(paste0(wd.path,"/plant_parameters.csv")) #ex

##########################################################################################
#Analyse data
#
# where n_visits is the number of visits to the same plant

#### to get total number of visits per pollinator for each seed ####
# Total Number of visits 
no.visits<-mdl %>% group_by(run, seed_percent, pollinator_species) %>% summarize(number_visits = n() ) 
#png(paste0(dirF, "Boxplot_Poll.sp_tot_vis_per_seed_ex6",".png"),width=7*ppi, height = 4*ppi, res=ppi)
#png(paste0(dirF, "Boxplot_Poll.sp_tot_vis_per_seed",".png"), width=4.5*ppi, height = 4*ppi, res=ppi)
box.col.sp<-poll.params$eusocial+1
soc.col<-brewer.pal(n = 3, name = "Set2")
#no.of visits per poll species for each seed percent
par(mfrow=c(2,3))
par(mar=c(4,4,3,1), oma=c(2,2,2,2))
s<-c(1e-05, 1e-04, 1e-03, 1e-02, 1e+00) #seed.percents
for (i in 1:length(s)){
  t<-filter(no.visits, seed_percent==s[i])
  boxplot(t$number_visits ~ t$pollinator_species, data = no.visits, cex=1, main=paste("Ex6",s[i]), xlab="pollinator species", ylab="total no of plant visits", col=soc.col[box.col.sp]) 
}
plot.new()
legend("center", legend=c("sol.no_nest", "sol.nest", "eusocial"), col=soc.col, lwd=3, lty=1, cex=2,  bty = "n")
#dev.off()

#### to get total number of visits per plant for each seed ####
# Total Number of visits 
no.visits<-mdl %>% group_by(run, seed_percent, plant_species) %>% summarize(number_visits = n() ) 
#png(paste0(dirF, "Boxplot_Poll.sp_tot_vis_per_seed_ex6",".png"),width=7*ppi, height = 4*ppi, res=ppi)
#png(paste0(dirF, "Boxplot_Poll.sp_tot_vis_per_seed",".png"), width=4.5*ppi, height = 4*ppi, res=ppi)
#Color habitats
plant.col<-brewer.pal(n = 4, name = "Set3")
plant.habs<-c(1,2,3,4,1,2,3,4)

#no.of visits per poll species for each seed percent
par(mfrow=c(2,3))
par(mar=c(4,4,3,1), oma=c(2,2,2,2))
s<-c(1e-05, 1e-04, 1e-03, 1e-02, 1e+00) #seed.percent used in the loop
for (i in 1:length(s)){
  t<-filter(no.visits, seed_percent==s[i])
  boxplot(t$number_visits ~ t$plant_species, data = no.visits, cex=1, main=paste("Ex6",s[i]), xlab="plant species", ylab="total no of plant visits", col=plant.col[plant.habs]) 
}
plot.new()
legend("center", legend=c("hab1", "hab2", "hab3", "hab4"), col=plant.col, lwd=3, lty=1, cex=2,  bty = "n")
#dev.off()
#plots mean visits per plant for all seeds
mean.vis<- no.visits %>% group_by(plant_species) %>% summarise(mean_visits=mean(number_visits))
par(mfrow=c(1,1))
barplot(mean.vis$mean_visits,  col=plant.col,  xlab="plant sp", ylab="mean Tot.sum.con.visits")

###total number of visits to plants per seed percent
tot.plants.vis<-mdl %>% group_by(run, seed_percent) %>% summarize(number_visits = n() ) 
boxplot(number_visits ~ seed_percent, data = tot.plants.vis, main="tot plants vis", xlab="seed_percent", ylab="total no plants visited", cex=1) 
#summary statistics
tot.plants.vis.stats<-tot.plants.vis %>% group_by(seed_percent) %>% summarise_at(vars(number_visits), list(Q1=~quantile(., probs = 0.25), median=median, Q3=~quantile(., probs = 0.75), mean=mean, sd=sd))

##### testing difference between seed percentages
#http://www.sthda.com/english/wiki/one-way-anova-test-in-r
tot.plants.vis_aov <- aov(tot.plants.vis$number_visits ~ as.factor(tot.plants.vis$seed_percent))
summary(tot.plants.vis_aov) #we can conclude that there are significant differences between the seed percents
#check for normality
plot(tot.plants.vis_aov, 1) 
plot(tot.plants.vis_aov, 2) #OUTLIERS
#OUTLIERS!!!! check for normality with shapiro wilk test
# Extract the residuals
aov_residuals <- residuals(object = tot.plants.vis_aov )
# Run Shapiro-Wilk test
shapiro.test(x = aov_residuals ) #no indication of normailty violation
# Tukey test for pairwise comparisons
TukeyHSD(tot.plants.vis_aov) #groups are not really differemnt

### to get the mean run visits to same plant by pollinator species per seed percent ###
#no of unique pollinator ind and poll ids
#long and winding code, sorry LEO
res.ag <- mdl %>% group_by(seed_percent, pollinator_species, pollinator_agent)
poll.no<-length(unique(res.ag$pollinator_agent))
poll.id<-sort(unique(res.ag$pollinator_agent))
seed<-sort(unique(res.ag$seed_percent))
sp.poll.id<-rep(c(1:8),each=10)
soc.id<-rep(poll.params$eusocial, each=10)+1
soc.col<-brewer.pal(n = 3, name = "Set2")

#Plot figure total pollinator agent visits to plants as a function of seed percent
#png(paste0(dirF, "Poll_ind_tot_vis",".png"), width=4.5*ppi, height = 4*ppi, res=ppi)
par(mfrow=c(1,1))
plot(total.vis, seed, type="n", xlab="seed percent", ylab="total plant visits by poll. ind.", ylim=c(100, 700), xlim=c(1,5), labels=F, main="ex6")
axis(side=1,at=c(1,2,3,4,5),labels=c(seed))
axis(side=2,at=c(100, 200, 300, 400, 500, 600, 700))

for (i in 1:poll.no){
poll.id<-sort(unique(res.ag$pollinator_agent))  #agents ids 
res <- mdl %>% group_by(run, seed_percent, pollinator_species,plant_species) %>% filter(pollinator_agent==poll.id[i]) %>% summarize(n_visits=n()) %>% filter(n_visits>1)
res.mean<- res %>% group_by(seed_percent, plant_species) %>% summarise(mean_visits=mean(n_visits))
res.mean.tab<-res.mean%>%spread(plant_species, mean_visits)%>% head()
total.vis<-round(apply(res.mean.tab[,-1], 1, sum, na.rm=TRUE))
#plot( xlab="seed percent", ylab="total plant visits")
lines(total.vis, type="l", col=soc.col[soc.id][i], lwd=1) 
}
legend(4, 640, legend=c("sol.no_nest", "sol.nest", "eusocial"), col=soc.col, lwd=1, lty=1, cex=0.8,  bty = "n")
#dev.off()


#Some additional analyses, not finished
# To get visits of individual pollinators to the same plant you need to consider plant_patch 
res <- mdl %>% group_by(run, seed_percent, pollinator_species,plant_species,pollinator_agent,plant_patch) %>% summarize(n_visits=n()) %>% filter(n_visits>1)
table(res$seed_percent, res$n_visits)

# To get visits to the same plant species by individual pollinator 
res <- mdl %>% group_by(seed_percent, pollinator_species,plant_species,pollinator_agent) %>% summarize(n_visits=n()) %>% filter(n_visits>1)
#table(res$seed_percent, res$n_visits)

# To get visits to the same plant species by pollinator species 
res <- mdl %>% group_by(seed_percent, pollinator_species,plant_species) %>% summarize(n_visits=n()) %>% filter(n_visits>1)
#table(res$seed_percent, res$plant_species)

# foraging distance of one pollinator
#
res <- mdl %>% filter(pollinator_agent==0) %>%  select(run,plant_patch,foraging_distance) 


###########################################################################################################

#To calculate consecutive visits 
#load function
#pl.no<-c(2,3,3,4,5,5,5,6,7) #example of an agent's plant visits 
source("consecutive_visits_II.R") 
source("consecutive_visits_II_plants.R") 
#source("consecutive_visits_II_plants.R") 
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

#PLANTS PERSPECTIVE
#Loop on the simulation file list to summarise data and calculate consecutive visits to same plant (minimum twice), and which plant
#consecutive visits to same plant species (minimum twice), and which plant
plant.vis.data<-list()
poll.data<-list()
tot.vis<-matrix(0, length(file.list), 8)
seed<-c()
all.dat<-c()
for (z in 1:length(file.list)){ 
  #z<-16
  ex_run<-file.list[[z]]
  poll.no<-length(unique(ex_run$pollinator_agent)) #pollinator number in file.list z=[1:50]
  #loop on pollinator agents to calculate agent-wise consecutive visits to same plant species
  plants.convis<-matrix(0, poll.no, 8)
  sp.no<-c()
  ags.id<-c()
  s<-c()
  r<-c()
  #pl.no<-list()
  for (i in 1:poll.no){
    ags.id<-sort(unique(ex_run$pollinator_agent))  #agents ids of the z file
    ag.rows<-ex_run[which(ex_run$pollinator_agent==ags.id[i]),] #rows of agent i
    sp.no[i]<-unique(ag.rows$pollinator_species) #which species does agent i belong to
    pl.no<-ag.rows[, 6] 
    s<-unique(file.list[[z]]$seed_percent)
    r<-unique(file.list[[z]]$run)
    #}
    res<-con.vis.plants(pl.no) #calculates consecutive visits
    #res<-0
    if (is.null(res)) {res<-0}
    tab.id<-as.data.frame(table(res))
    id.plants<-as.numeric(as.vector(tab.id[,1]))
    plants.convis[i, id.plants]<- tab.id[,2]

  }
  plant.vis.data[[z]]<-plants.convis
  #test<-as.data.frame(cbind(ags.id, sp.no, plants.convis, s, r))
  poll.data[[z]]<-as.data.frame(cbind(ags.id, sp.no, s, r))
  all.dat[[z]]<-as.data.frame(cbind(ags.id, sp.no, s, r, plants.convis))
  tot.vis[z,]<-apply(plant.vis.data[[z]], 2, sum)
  seed<-c(seed, s)
}


#check number of pollinators per run
#table(poll.data[[9]][,c(2)])

tot.plant.vis.seed<-as.data.frame(cbind(tot.vis, seed))

#tot.plant.vis.seed %>% group_by(seed) %>% summarise(mean(V1), mean(V2), mean(V3))

plant.col<-brewer.pal(n = 8, name = "Set3")
#png(paste0(dirF, "Boxplot_plants_con.vis_per_seed",".png"),width=7*ppi, height = 4*ppi, res=ppi)
par(mfrow=c(2,3)) 
s<-unique(tot.plant.vis.seed$seed)
for (i in 1:length(s)){
  t<-tot.plant.vis.seed[which(tot.plant.vis.seed$seed==s[i]),]
  boxplot(t[,-9], data = tot.plant.vis.seed, main=paste("Ex6",s[i]), xlab="plant sp", ylab="Tot.con.visits",col=plant.col) 
}
#plot.new()
barplot(apply(tot.plant.vis.seed[,-9], 2, mean),  col=plant.col,  xlab="plant sp", ylab="mean Tot.sum.con.visits")
#legend("center", legend=c("sol.no_nest", "sol.nest", "eusocial"), col=soc.col, lwd=3, lty=1, cex=2,  bty = "n")
#dev.off()

#Total number of consective visits to a plant per seed percent
#boxplot etc
par(mfrow=c(1,1)) 
con.vis.sum<-cbind(tot.plant.vis.seed[, 9], apply(tot.plant.vis.seed[, -9], 1, sum))
colnames(con.vis.sum)<-c("seed_percent", "con.vis.sum")
boxplot(con.vis.sum[,2] ~ con.vis.sum[,1], xlab="seed_percent", ylab="total consecutive visits")
#summary stats
tot.con.vis.stats<-as.tibble(con.vis.sum)%>% group_by(seed_percent) %>% summarise_at(vars(con.vis.sum), list(Q1=~quantile(., probs = 0.25), median=median, Q3=~quantile(., probs = 0.75), mean=mean, sd=sd))

##### testing difference between seed percentages
#http://www.sthda.com/english/wiki/one-way-anova-test-in-r
tot.con.vis.stats_aov <- aov(con.vis.sum[,2] ~ as.factor(con.vis.sum[,1]))
summary(freq.con.vis_aov) #we can conclude that there are significant differences between the seed percents
#check for normality
plot(freq.con.vis_aov, 1)
# Tukey test for pairwise comparisons
TukeyHSD(freq.con.vis_aov)


#Agent perspective
#Loop on the simulation file list to summarise data and calculate consecutive visits to same plant (minimum twice), and which plant
#consecutive visits to same plant species (minimum twice), and which plant
plant.vis.data<-list()
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
    
    #plants.convis<-matrix(0, length(poll.no), 8)
    
    ags.id<-sort(unique(ex_run$pollinator_agent))  #agents ids of the z file
    ag.rows<-ex_run[which(ex_run$pollinator_agent==ags.id[i]),] #rows of agent i
    
    pl.no<-ag.rows[, 6] #subset plant visits of agent i 
    sp.no[i]<-unique(ag.rows$pollinator_species) #which species does agent i belong to
    mean.forag.dist[i]<-mean(ag.rows[,9]) #subset foraging distance column
    
    freq.con.vis<-con.vis(pl.no) #calculates consecutive visits
    vis<-c(vis, freq.con.vis)    #stores the visits in this vector

    #tab.id<-as.data.frame(table(res))
    #id.plants<-as.numeric(as.vector(tab.id[,1]))
    #plants.convis[i, id.plants]<- tab.id[,2]
  }
  #plant.vis.data[[z]]<-plants.convis
  agent.data[[z]]<-as.data.frame(cbind(ags.id, sp.no, vis, mean.forag.dist, seed))
  colnames(agent.data[[z]])<- c("agent.id", "species_no", "con_visits", "mean.foraging.dist", "seed.percent")
}

#check agent data file
head(agent.data[[1]])
#create one file
one.file_ex1 <- bind_rows(agent.data)
#names(one.file_ex1) <- str_trim(names(agent.data))
#one.file_ex1[which(one.file_ex1$con_visits==0),]

# some stats
#boxplot etc
par(mfrow=c(1,1)) 
res.con.vis<-one.file_ex1 %>% group_by(agent.id, species_no, seed.percent) %>% summarise(mean.con.vis=con_visits)
boxplot(mean.con.vis ~ species_no, data = res.con.vis, main="Ex1", xlab="pollinator species", ylab="con.visits") 

#loop to calculate percent.seed-wise simulations 
box.col.sp<-poll.params$eusocial+1
soc.col<-brewer.pal(n = 3, name = "Set2")

#png(paste0(dirF, "Boxplot_Poll.sp_tot_vis_per_seed",".png"),width=7*ppi, height = 4*ppi, res=ppi)
par(mfrow=c(2,3)) 
s<-unique(res.con.vis$seed.percent)
for (i in 1:length(s)){
  t<-filter(res.con.vis, seed.percent==s[i])
  boxplot(t$mean.con.vis ~ t$species_no, data = res.con.vis, main=paste("Ex6",s[i]), xlab="pollinator sp", ylab="con.visits",col=soc.col[box.col.sp]) 
}
plot.new()
legend("center", legend=c("sol.no_nest", "sol.nest", "eusocial"), col=soc.col, lwd=3, lty=1, cex=2,  bty = "n")
#dev.off()

#some more boxplot etc, not finished
par(mfrow=c(1,1)) 
freq.con.vis<-one.file_ex1 %>% group_by(species_no, seed.percent) %>% summarise(no.con.vis=con_visits)
boxplot(con_visits ~ seed.percent, data = one.file_ex1 , main="Ex6", xlab="seed.percent", ylab="con.visits") 

#summary statistics
freq.con.vis.stats<-freq.con.vis %>% group_by(seed.percent) %>% summarise_at(vars(no.con.vis), list(Q1=~quantile(., probs = 0.25), median=median, Q3=~quantile(., probs = 0.75), mean=mean, sd=sd))

##### testing difference between seed percentages
#http://www.sthda.com/english/wiki/one-way-anova-test-in-r
freq.con.vis_aov <- aov(freq.con.vis$no.con.vis ~ as.factor((freq.con.vis$seed.percent)))
summary(freq.con.vis_aov) #we can conclude that there are significant differences between the seed percents
#if we want to show this, it could go into suppl.mat
par(mfrow=c(1,3))
boxplot(con_visits ~ seed.percent, data = one.file_ex1 , main="Ex6", xlab="seed.percent", ylab="con.visits") 
plot(freq.con.vis_aov, 1)
plot(freq.con.vis_aov, 2)
# Tukey test for pairwise comparisons
TukeyHSD(freq.con.vis_aov)

                                                                                 
##### BOXPLOTS for visits per seed percent
#https://www.r-bloggers.com/2013/06/box-plot-with-r-tutorial/ #boxplot R tutorial
par(mfrow=c(1,3)) 
boxplot(number_visits ~ seed_percent, data = no.visits, main="tot plants vis", xlab="seed_percent", ylab="total no plants visited", cex=1) 
boxplot(con.vis.sum~tot.plant.vis.seed[, 9], xlab="seed_percent", ylab="total consecutive visits", main="tot con vis plants", cex=1)
boxplot(no.con.vis ~ seed.percent, data = freq.con.vis, main="freq con vis", xlab="seed.percent", ylab="con.visits", cex=1) 

####################################################################################################################################################

#Add network analyses here!!!!

#make weighted graphs
#plants in rows and pollinators in columns
w.mat<-list()
for (i in 1:length(file.list)){
  w.mat[[i]]<-t(as.matrix(ftable(file.list[[i]][,c(4,6)])))
}

#index, metric names, metric length
M1<-w.mat[[1]]wd.path<-"Z:/Documents/postdoc_project_Helsinki/myNetlogo_models/Netlogo_models/pollinatorsNL-main_15/Simulation_experiments/aug_2nd_run/ex6"
M1.ind<-networklevel(M1)
names(M1.ind)
length(w.mat)


net.ind<-matrix(0, length(w.mat), length(M1.ind))
colnames(net.ind)<-names(M1.ind)
for (i in 1:length(file.list)){
  #mat<-t(as.matrix(ftable(file.list[[i]][,c(4,6)])))
  net.ind[i,]<-networklevel(w.mat[[i]], index="ALLBUTDD", level="both", weighted=TRUE, 
                            ISAmethod="Bluethgen",  SAmethod = "Bluethgen", extinctmethod = "r", 
                            nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE, 
                            logbase="e", intereven="prod", H2_integer=TRUE, fcweighted=TRUE, 
                            fcdist="euclidean", legacy=FALSE)
}

#write.csv(net.ind, "net.ind.csv")
##https://jeffollerton.co.uk/2017/12/06/local-and-regional-specialization-in-plant-pollinator-networks-a-new-study-just-published/
  
#select metrics
#con<- which(colnames(net.ind)=="connectance") 
#NODF<-which(colnames(net.ind)=="NODF") 
w.nodf<-which(colnames(net.ind)=="weighted NODF")
spec.asym<-which(colnames(net.ind)=="specialisation asymmetry") 
link.den<-which(colnames(net.ind)=="linkage density") 
weigh.con<-which(colnames(net.ind)=="weighted connectance") 
#inter.even<-which(colnames(net.ind)=="interaction evenness") 
robust.HL<-which(colnames(net.ind)=="robustness.HL") 
robust.LL<-which(colnames(net.ind)=="robustness.LL") 
fun.compl.HL<-which(colnames(net.ind)=="functional.complementarity.HL") 
fun.compl.LL<-which(colnames(net.ind)=="functional.complementarity.LL") 
gen.HL<-which(colnames(net.ind)=="generality.HL") 
vul.LL<-which(colnames(net.ind)=="vulnerability.LL") 

#make dataframe of metrics #net.ind[, con], net.ind[, NODF],
sel.ind<- cbind(net.ind[, w.nodf], net.ind[,spec.asym], net.ind[,link.den], net.ind[,weigh.con], net.ind[,robust.HL],
                net.ind[,robust.LL], net.ind[,fun.compl.HL], net.ind[,fun.compl.LL], net.ind[,gen.HL] ,net.ind[,vul.LL])
#"con", "NODF",
colnames(sel.ind)<- c("w.nodf", "w.spec.asym", "w.link.den", "w.conn", "robust.HL", "robust.LL", "fun.compl.HL", "fun.compl.LL", "gen.HL", "vul.LL") #"w.inter.even"

#add seeds to the metic dataframe
seeds<-c(1e-05, 1e-04, 1e-03, 1e-02, 1e+00) #seed.percents
rep.seeds<-rep(seeds, each=10) #each=equal to number of repetitions
#net.ind<-as.data.frame(cbind(net.ind, rep.seeds))
sel.ind<-as.data.frame(cbind(sel.ind, rep.seeds))

#net.ind <- as.data.frame(net.ind)
#sel.ind.quantiles<-sel.ind %>% group_by(rep.seeds) %>% summarise(w.nodf = quantile(w.nodf, probs=c(0.25,0.5,0.75)), w.spec.asym = quantile(w.spec.asym, probs=c(0.25,0.5,0.75)), wlink.den=quantile(w.link.den, probs=c(0.25,0.5,0.75)), w.conn=quantile(w.conn, probs=c(0.25,0.5,0.75)), w.inter.even=quantile(w.inter.even, probs=c(0.25,0.5,0.75)))
#probs<-rep(c(0.25,0.5,0.75), 5)
#med<-which(sel.ind.quantiles[,1]==0.5)
#sel.med<-sel.ind.quantiles[med,]

#BOXPLOTS!!!!!
par(cex=0.5, mai=c(0.3,0.3,0.2,0.2)) 
par(mfrow=c(5,2))
for (i in 1:10){
  boxplot(sel.ind[,i] ~ sel.ind$rep.seeds, data = sel.ind, main=colnames(sel.ind)[i], xlab="", ylab="") 
}

#Plot one network example for each seed percent
par(cex=2, mai=c(0.1,0.1,0.1,0.1)) 
par(mfrow=c(1,1))
plotweb(w.mat[[1]],labsize=1.2, col.high="orange", col.low="darkgreen")
plotweb(w.mat[[11]])
plotweb(w.mat[[21]])
plotweb(w.mat[[31]])
plotweb(w.mat[[41]])


#make colored network file

one.file<-mdl
head(one.file)
#pdf(paste0(dirF, "ind.networks.pdf"),
#    width=6, height = 4)
#layout(matrix(c(1:3, 20, 4:19), byrow=TRUE, ncol=3))
par(mfrow=c(1,1), mar=c(0,2.5,1,0.5), oma=c(4,0,0,0), font.main=1, cex.main=1)
#par(cex=2, mai=c(0.1,0.1,0.1,0.1)) 
#par(mar=c(0.5, 0.5, 0.2, 0.2), mfrow=c(,2), oma = c(4, 4, 0.2, 0.2))
#par(mfrow=c(2,3))
for (i in 1:length(unique(one.file$seed_percent))){
  i<-3
  #ids<-which(one.file$pollinator_agent%in% c(1, 17, 23, 35, 47, 55)=="TRUE")
  ids<-which(one.file$pollinator_agent%in% c(6, 13, 27, 38, 43, 57)=="TRUE")
  one.file.ind<-one.file[ids,]
  sp11_seed<-subset(one.file.ind, one.file.ind$seed_percent==unique(one.file.ind$seed_percent)[i])
  sp11.interactions<-sp11_seed[, c(4,6)]
  sp11.w.mat<-t(as.matrix(ftable(sp11.interactions)))
  plotweb(sp11.w.mat, labsize=1.2, col.high="orange", col.low="darkgreen") #low.spacing=
} 

poll.counts.niche<-t(as.matrix(ftable(one.file[, c(4,6)])))


#MAKE colored NETWORK PLOT per pollinator species interactions
seed_0.001<-subset(one.file, seed_percent=="0.001")
table_0.001<-table(seed_0.001$pollinator_species, seed_0.001$plant_species) 
row_sum<-apply(table_0.001, 1, sum)
col_sum<-apply(table_0.001, 2, sum)

par(cex=2, mai=c(0.1,0.1,0.1,0.1)) 
par(mfrow=c(3,3))
for (i in 1:length(unique(one.file$pollinator_species))){
  sp_seed_0.001<-subset(one.file, pollinator_species==i&seed_percent=="0.001")
  sp.interactions<-sp_seed_0.001[, c(4,6)]
  sp.w.mat<-t(as.matrix(ftable(sp.interactions)))
  plotweb(sp.w.mat, labsize=1.2, col.high="orange", col.low="darkgreen")
}










