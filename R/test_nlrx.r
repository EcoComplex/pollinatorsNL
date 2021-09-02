# test nlrx 

require(nlrx)
require(readr)
require(dplyr)
require(ggplot2)

source("R/functions.r")

# Unix default NetLogo installation path (adjust to your needs!):
netlogopath <- file.path("/home/leonardo/NetLogo")
simfolder <- "/home/leonardo/Dropbox/Projects/DynamicForestExtinction"
modelpath <- file.path(simfolder, "ContactProcessForestAsTurtlesSelection.nlogo")
outpath <- file.path(simfolder,"Simulations")

# If not defined set the JAVA version of your local 
if(Sys.getenv("JAVA_HOME")==""){
  Sys.setenv(JAVA_HOME = "/usr/lib/jvm/java-11-openjdk-amd64")
  ## "/usr/lib/jvm/java-8-oracle"
}

nl <- nl(nlversion = "6.1",
         nlpath = netlogopath,
         modelpath = modelpath,
         jvmmem = 2048)

nl@experiment <- experiment(expname="CP_birds1-1.6_forest3-4.5_adSel",
                            outpath=outpath,
                            repetition=1,
                            tickmetrics="false",
                            idsetup="setup-center",
                            idgo="go",
                            runtime=1000,
                            metrics=c("habitat-proportion", "birds-proportion"),
                            variables = list("birth-rate-forest" = list(min=3, max=4.5, step=0.01 , qfun="qunif"),
                                             "birth-rate-birds" = list(min=1, max=1.6, step=0.01, qfun="qunif")), 
                            constants = list("world-width" = 101,
                                             "world-height" = 101,
                                             "death-rate-birds" = 1,
                                             "death-rate-forest" = 1,
                                             "birds-behavior" = "\"AdultSelection\""))


nl@simdesign <- simdesign_ff(nl=nl,nseeds=5)

#nl@simdesign <- simdesign_lhs(nl=nl,nseeds=1,samples=10000,precision=3)

require(future)
plan(multisession,workers=24)
require(tictoc)
tic()
results <- run_nl_all(nl)
toc()
plan(sequential)

#
# Using BehaviorSpace 80.5 sims/min

setsim(nl, "simoutput") <- results 
write_simoutput(nl)
