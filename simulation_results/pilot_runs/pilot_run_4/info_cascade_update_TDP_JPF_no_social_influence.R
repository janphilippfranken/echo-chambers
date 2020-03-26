## Agent Based Modelling in R using RNet ##
install.packages("rJava")
install.packages("RNetLogo")

install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")

library(rJava)
library(RNetLogo)

library(dplyr)
library(reshape2)
library(ggplot2)

#######################################################
#### Cascade Across Network (Varying Priors Model) ####
#######################################################

nl.path <- "C:/Users/Toby/Dropbox/R/Network ABMs/Cascades Modelling (Full Paper)/app" # The R file and workspace should be set to the same folder as your netlogo model
model.path <- "C:/Users/Toby/Dropbox/R/Network ABMs/Cascades Modelling (Full Paper)/info_cascade_update_TDP_JPF_22_10.nlogo"

# Startup:
nl.jarname <- "netlogo-6.0.4.jar" # this file needs to be in the app folder, which in turn should be in the working directory folder
NLStart(nl.path, nl.jarname=nl.jarname, gui = FALSE) # will run netlogo in headless mode
NLLoadModel(model.path)

# Exiting (and therefore closing a NetLogo instance) **ONLY DO THIS ONCE YOU HAVE FINISHED RUNNING SIMULATIONS
NLQuit()

# Simulation functions
sim1 <- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 1.00", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.00", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

sim2 <- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 1.00", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.10", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

sim3 <- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 1.00", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.20", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

sim4 <- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 0.50", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.00", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

sim5<- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 0.50", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.10", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

sim6 <- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 0.50", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.20", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

sim7 <- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 0.10", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.00", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

sim8 <- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 0.10", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.10", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

sim9 <- function(pval) {
  MSimList <- seq(1, 10, 1) # number of simulations
  retb <- 0 # Initialisations of recording variables
  retnb <- 0
  retut <- 0
  retpeakprate <- 0
  retsim <- 0
  rettick <- 0
  for(x in MSimList){
    NLCommand("set no-social-influence TRUE", "set n_agents 1000", "set max_links ",(pval), "set p-h-given-c 0.0", "set prop-likelihood 0.10", "set prior-variance TRUE", "set prior-mean 0.5", "set prior-sd 0.2", "set p-E-mean 0.5", "set p-E-sd 0.2", "set min_E 0.0", "set max_E 1.0", "set p-T-mean 0.5", "set p-T-sd 0.2", "set min_T 0.0", "set max_T 1.0", "set con-in-prior FALSE", "set con-threshold 0.0", "set p-con-sd 0.0",
              "set beta_T 0", "set alpha_E 0", "set evidence 0", "set alpha_T 0", "set beta_E 0", "set change_shape? FALSE", "set scale-free FALSE", "set learning-through-RL FALSE", "set learning-through-Bayes TRUE", "set ground-truth-impact? FALSE", "set ground-truth 0.0", "set n_init_believers 1", "set neut-event-YN TRUE", "set prox-YN TRUE", "set nb-prop-YN TRUE", "set con-in-learning FALSE",
              "set quadratic-con-prior? FALSE", "set min_con 0.02", "set max_con 1.00", "set alpha_con 2.58", "set beta_con 1.50", "set expertise_influence 0.20", "set sc-bel-prop 0.0", "set p-con-mean 0.0", "set modulate-weight-by-mean FALSE", "set learning_rate 0.0")
    NLCommand("setup")
    NLDoCommandWhile("match-counter <= 1", "go");
    retb <- retb + NLReport("(opinion-A-agents / number-agents) * 100")
    retnb <- retnb + NLReport("(opinion-B-agents / number-agents) * 100")
    retut <- retut + NLReport("((number-agents - (opinion-B-agents + opinion-A-agents)) / number-agents) * 100")
    retpeakprate <- retpeakprate + NLReport("peak-spread")
    retsim <- retsim + NLReport("cl-prop-same")
    rettick <- rettick + NLReport("ticks")}
  retlist <- c((retb/10),(retnb/10),(retut/10),(retpeakprate/10),(retsim/10),(rettick/10)) # recorded variables then need to be stored as means in this instance, so need to divide by the number of simulations (and also do this other ways)
  return(retlist)}

# Worth including maximum propogation rate in the above simulation as a stored varaible (see BelNet2 graphical interface for equation)
# may be pointless if this estimation is already subsumed by a normal distribution over the total-initial_agents / total ticks that is
# already implied by the data included in the current simulation iteration.

### creating simulation sequence
d1 <- seq(5,501,5)# setting up list

### RUN SIM SET(s) - The above 9 simulations then each run through the d1 sequence as the inserted variable (pval). In the present example this means the variable "maxLinks" runs 10 simulations each each step of 5, from 5 to 500.
ppb_1 <- sapply(d1, function(dens) sim1(dens))
ppb_2 <- sapply(d1, function(dens) sim2(dens))
ppb_3 <- sapply(d1, function(dens) sim3(dens))
ppb_4 <- sapply(d1, function(dens) sim4(dens))
ppb_5 <- sapply(d1, function(dens) sim5(dens))
ppb_6 <- sapply(d1, function(dens) sim6(dens))
ppb_7 <- sapply(d1, function(dens) sim7(dens))
ppb_8 <- sapply(d1, function(dens) sim8(dens))
ppb_9 <- sapply(d1, function(dens) sim9(dens))

ppb_1_2 <- t(ppb_1)#transposing simulation output 1
colnames(ppb_1_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_1_2 <- transform(ppb_1_2, Links = (d1/10))
ppb_1_2 <- transform(ppb_1_2, Expertise_Influence = "Neutral")
ppb_1_2 <- transform(ppb_1_2, P_Prop = "1.0 (100% propagation)")
ppb_1_2 <- transform(ppb_1_2, P_H_SD = "0.5")
#ppb_1_2 <- transform(ppb_1_2, Total_Agents = "1000")

ppb_2_2 <- t(ppb_2)#transposing simulation output 2
colnames(ppb_2_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_2_2 <- transform(ppb_2_2, Links = (d1/10))
ppb_2_2 <- transform(ppb_2_2, Expertise_Influence = "+/- 0.1")
ppb_2_2 <- transform(ppb_2_2, P_Prop = "1.0 (100% propagation)")
ppb_2_2 <- transform(ppb_2_2, P_H_SD = "0.5")
#ppb_2_2 <- transform(ppb_2_2, Total_Agents = "1000")

ppb_3_2 <- t(ppb_3)#transposing simulation output 3
colnames(ppb_3_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_3_2 <- transform(ppb_3_2, Links = (d1/10))
ppb_3_2 <- transform(ppb_3_2, Expertise_Influence = "+/- 0.2")
ppb_3_2 <- transform(ppb_3_2, P_Prop = "1.0 (100% propagation)")
ppb_3_2 <- transform(ppb_3_2, P_H_SD = "0.5")
#ppb_3_2 <- transform(ppb_3_2, Total_Agents = "1000")

ppb_4_2 <- t(ppb_4)#transposing simulation output 4
colnames(ppb_4_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_4_2 <- transform(ppb_4_2, Links = (d1/10))
ppb_4_2 <- transform(ppb_4_2, Expertise_Influence = "Neutral")
ppb_4_2 <- transform(ppb_4_2, P_Prop = "0.1 (10% propagation)")
ppb_4_2 <- transform(ppb_4_2, P_H_SD = "0.5")
#ppb_4_2 <- transform(ppb_4_2, Total_Agents = "2000")

ppb_5_2 <- t(ppb_5)#transposing simulation output 5
colnames(ppb_5_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_5_2 <- transform(ppb_5_2, Links = (d1/10))
ppb_5_2 <- transform(ppb_5_2, Expertise_Influence = "+/- 0.1")
ppb_5_2 <- transform(ppb_5_2, P_Prop = "0.1 (10% propagation)")
ppb_5_2 <- transform(ppb_5_2, P_H_SD = "0.5")
#ppb_5_2 <- transform(ppb_5_2, Total_Agents = "2000")

ppb_6_2 <- t(ppb_6)#transposing simulation output 6
colnames(ppb_6_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_6_2 <- transform(ppb_6_2, Links = (d1/10))
ppb_6_2 <- transform(ppb_6_2, Expertise_Influence = "+/- 0.2")
ppb_6_2 <- transform(ppb_6_2, P_Prop = "0.1 (10% propagation)")
ppb_6_2 <- transform(ppb_6_2, P_H_SD = "0.5")
#ppb_6_2 <- transform(ppb_6_2, Total_Agents = "2000")

ppb_7_2 <- t(ppb_7)#transposing simulation output 6
colnames(ppb_7_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_7_2 <- transform(ppb_7_2, Links = (d1/10))
ppb_7_2 <- transform(ppb_7_2, Expertise_Influence = "Neutral")
ppb_7_2 <- transform(ppb_7_2, P_Prop = "0.5 (50% propagation)")
ppb_7_2 <- transform(ppb_7_2, P_H_SD = "0.5")
#ppb_7_2 <- transform(ppb_7_2, Total_Agents = "3000")

ppb_8_2 <- t(ppb_8)#transposing simulation output 6
colnames(ppb_8_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_8_2 <- transform(ppb_8_2, Links = (d1/10))
ppb_8_2 <- transform(ppb_8_2, Expertise_Influence = "+/- 0.1")
ppb_8_2 <- transform(ppb_8_2, P_Prop = "0.5 (50% propagation)")
ppb_8_2 <- transform(ppb_8_2, P_H_SD = "0.5")
#ppb_8_2 <- transform(ppb_8_2, Total_Agents = "3000")

ppb_9_2 <- t(ppb_9)#transposing simulation output 6
colnames(ppb_9_2) <- c("Opinion_A","Opinion_B","No_Opinion","Peak_Rate","Prcnt_Same_Clust","Duration")
ppb_9_2 <- transform(ppb_9_2, Links = (d1/10))
ppb_9_2 <- transform(ppb_9_2, Expertise_Influence = "+/- 0.2")
ppb_9_2 <- transform(ppb_9_2, P_Prop = "0.5 (50% propagation)")
ppb_9_2 <- transform(ppb_9_2, P_H_SD = "0.5")
#ppb_9_2 <- transform(ppb_9_2, Total_Agents = "3000")

###

## W-S formating
ppb_1_2_a <- transform(ppb_1_2, Agent_type = "Opinion_A")
ppb_1_2_a <- transform(ppb_1_2_a, Percentage = Opinion_A)
ppb_1_2_b <- transform(ppb_1_2, Agent_type = "Opinion_B")
ppb_1_2_b <- transform(ppb_1_2_b, Percentage = Opinion_B)
ppb_1_2_c <- transform(ppb_1_2, Agent_type = "No_Opinion")
ppb_1_2_c <- transform(ppb_1_2_c, Percentage = No_Opinion)
ppb_1_3 <- rbind(ppb_1_2_a,ppb_1_2_b,ppb_1_2_c)

ppb_2_2_a <- transform(ppb_2_2, Agent_type = "Opinion_A")
ppb_2_2_a <- transform(ppb_2_2_a, Percentage = Opinion_A)
ppb_2_2_b <- transform(ppb_2_2, Agent_type = "Opinion_B")
ppb_2_2_b <- transform(ppb_2_2_b, Percentage = Opinion_B)
ppb_2_2_c <- transform(ppb_2_2, Agent_type = "No_Opinion")
ppb_2_2_c <- transform(ppb_2_2_c, Percentage = No_Opinion)
ppb_2_3 <- rbind(ppb_2_2_a,ppb_2_2_b,ppb_2_2_c)

ppb_3_2_a <- transform(ppb_3_2, Agent_type = "Opinion_A")
ppb_3_2_a <- transform(ppb_3_2_a, Percentage = Opinion_A)
ppb_3_2_b <- transform(ppb_3_2, Agent_type = "Opinion_B")
ppb_3_2_b <- transform(ppb_3_2_b, Percentage = Opinion_B)
ppb_3_2_c <- transform(ppb_3_2, Agent_type = "No_Opinion")
ppb_3_2_c <- transform(ppb_3_2_c, Percentage = No_Opinion)
ppb_3_3 <- rbind(ppb_3_2_a,ppb_3_2_b,ppb_3_2_c)

ppb_4_2_a <- transform(ppb_4_2, Agent_type = "Opinion_A")
ppb_4_2_a <- transform(ppb_4_2_a, Percentage = Opinion_A)
ppb_4_2_b <- transform(ppb_4_2, Agent_type = "Opinion_B")
ppb_4_2_b <- transform(ppb_4_2_b, Percentage = Opinion_B)
ppb_4_2_c <- transform(ppb_4_2, Agent_type = "No_Opinion")
ppb_4_2_c <- transform(ppb_4_2_c, Percentage = No_Opinion)
ppb_4_3 <- rbind(ppb_4_2_a,ppb_4_2_b,ppb_4_2_c)

ppb_5_2_a <- transform(ppb_5_2, Agent_type = "Opinion_A")
ppb_5_2_a <- transform(ppb_5_2_a, Percentage = Opinion_A)
ppb_5_2_b <- transform(ppb_5_2, Agent_type = "Opinion_B")
ppb_5_2_b <- transform(ppb_5_2_b, Percentage = Opinion_B)
ppb_5_2_c <- transform(ppb_5_2, Agent_type = "No_Opinion")
ppb_5_2_c <- transform(ppb_5_2_c, Percentage = No_Opinion)
ppb_5_3 <- rbind(ppb_5_2_a,ppb_5_2_b,ppb_5_2_c)

ppb_6_2_a <- transform(ppb_6_2, Agent_type = "Opinion_A")
ppb_6_2_a <- transform(ppb_6_2_a, Percentage = Opinion_A)
ppb_6_2_b <- transform(ppb_6_2, Agent_type = "Opinion_B")
ppb_6_2_b <- transform(ppb_6_2_b, Percentage = Opinion_B)
ppb_6_2_c <- transform(ppb_6_2, Agent_type = "No_Opinion")
ppb_6_2_c <- transform(ppb_6_2_c, Percentage = No_Opinion)
ppb_6_3 <- rbind(ppb_6_2_a,ppb_6_2_b,ppb_6_2_c)

ppb_7_2_a <- transform(ppb_7_2, Agent_type = "Opinion_A")
ppb_7_2_a <- transform(ppb_7_2_a, Percentage = Opinion_A)
ppb_7_2_b <- transform(ppb_7_2, Agent_type = "Opinion_B")
ppb_7_2_b <- transform(ppb_7_2_b, Percentage = Opinion_B)
ppb_7_2_c <- transform(ppb_7_2, Agent_type = "No_Opinion")
ppb_7_2_c <- transform(ppb_7_2_c, Percentage = No_Opinion)
ppb_7_3 <- rbind(ppb_7_2_a,ppb_7_2_b,ppb_7_2_c)

ppb_8_2_a <- transform(ppb_8_2, Agent_type = "Opinion_A")
ppb_8_2_a <- transform(ppb_8_2_a, Percentage = Opinion_A)
ppb_8_2_b <- transform(ppb_8_2, Agent_type = "Opinion_B")
ppb_8_2_b <- transform(ppb_8_2_b, Percentage = Opinion_B)
ppb_8_2_c <- transform(ppb_8_2, Agent_type = "No_Opinion")
ppb_8_2_c <- transform(ppb_8_2_c, Percentage = No_Opinion)
ppb_8_3 <- rbind(ppb_8_2_a,ppb_8_2_b,ppb_8_2_c)

ppb_9_2_a <- transform(ppb_9_2, Agent_type = "Opinion_A")
ppb_9_2_a <- transform(ppb_9_2_a, Percentage = Opinion_A)
ppb_9_2_b <- transform(ppb_9_2, Agent_type = "Opinion_B")
ppb_9_2_b <- transform(ppb_9_2_b, Percentage = Opinion_B)
ppb_9_2_c <- transform(ppb_9_2, Agent_type = "No_Opinion")
ppb_9_2_c <- transform(ppb_9_2_c, Percentage = No_Opinion)
ppb_9_3 <- rbind(ppb_9_2_a,ppb_9_2_b,ppb_9_2_c)

### Simulation storage
### Compiling full model for graphing / analysis ###

# Version for 0 Prior variance run
cascade_model_BSCM_pilot <- rbind(ppb_1_3, ppb_2_3, ppb_3_3, ppb_4_3, ppb_5_3, ppb_6_3, ppb_7_3, ppb_8_3, ppb_9_3)
write.csv(cascade_model_BSCM_pilot, file = "cascade_model_BSCM_pilot_4_no_social_influence.csv")
# 
# # loading model and adding opinion proportion to the model
# cascade_model_BSCM_pilot <- data.frame(read.csv('~/cascade_model_BSCM_pilot.csv'))
# cascade_model_BSCM_pilot$Opinion_Prop <- (cascade_model_BSCM_pilot$Opinion_A / (cascade_model_BSCM_pilot$Opinion_A + cascade_model_BSCM_pilot$Opinion_B))
# 
# # Version for .1 Prior variance run
# #model_C2Pvar.1_full <- rbind(ppb_1_3, ppb_2_3, ppb_3_3, ppb_4_3, ppb_5_3, ppb_6_3, ppb_7_3, ppb_8_3, ppb_9_3)
# #write.csv(model_C2Pvar.1_full, file = "Cascade_model_C2Pvar.1_full.csv")
# 
# # Version for .2 Prior variance run
# #model_C2Pvar.2_full <- rbind(ppb_1_3, ppb_2_3, ppb_3_3, ppb_4_3, ppb_5_3, ppb_6_3, ppb_7_3, ppb_8_3, ppb_9_3)
# #write.csv(model_C2Pvar.2_full, file = "Cascade_model_C2Pvar.2_full.csv")
# 
# #model_C2_PVar_ALL <- rbind(model_C2Pvar0_full,model_C2Pvar.1_full,model_C2Pvar.2_full)
# #write.csv(model_C2_PVar_ALL, file = "Cascade_model_C2_PVar_ALL.csv")
# 
# # Model version with Singular opinion proportion
# model_C2LV_full <- read.csv("Cascade_model_2LV_df3_full.csv")
# model_C2LV_full$P_H_Given_C <- factor(model_C2LV_full$P_H_Given_C, levels = c('+/- 0.2','+/- 0.1','Neutral'))
# #model_C6_full <- transform(model_C6_full, Opinion_Prop = (Opinion_A/(Opinion_A + Opinion_B))
# 
# ######################
# ## Graphing Model 1 ##
# ######################
# 
# ## proportion of believers figures
# ggplot(Cascade_model_Update1ConVar_full, aes(x = Links, linetype=P_H_Given_C)) +
#   stat_summary(fun.y=mean, aes(y = Opinion_Prop), geom="line", size = .8) +
#   scale_x_continuous(breaks = round(seq(min(Cascade_model_Update1ConVar_full$Links) - 0.5, max(Cascade_model_Update1ConVar_full$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
#   scale_y_continuous(breaks = round(seq(0, 1, by = .1),1), limits = c(0, 1)) +
#   theme_bw() +
#   #scale_color_manual(values=c("#fc0000", "#006dfc", "#000000"), labels=c('+/- 0.2','+/- 0.1','Neutral')) +
#   #geom_vline(xintercept = .5, size = .5, colour = "black", linetype = "dashed") +
#   #geom_hline(yintercept = .5, size = .5, colour = "black", linetype = "dashed") +
#   labs(x='Interconnectivity (%)', y='Global Proportion of Opinions', linetype='Opinion Strength') +
#   theme(text = element_text(size = 14),
#         panel.grid = element_blank(),
#         axis.title.y = element_text(hjust = 0.5, vjust = 1),
#         legend.position = 'right') +
#   facet_grid(. ~ P_Prop)
# 
# 
# ## peak rates of spread
# ggplot(Cascade_model_Update1ConVar_full, aes(x = Links, linetype=P_H_Given_C)) +
#   stat_summary(fun.y=mean, aes(y = Peak_Rate), geom="line", size = .8) +
#   scale_x_continuous(breaks = round(seq(min(Cascade_model_Update1ConVar_full$Links) - 0.5, max(Cascade_model_Update1ConVar_full$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
#   scale_y_continuous(breaks = round(seq(0, 100, by = 10),1), limits = c(0,100)) +
#   theme_bw() +
#   #scale_color_manual(values=c("#000000")) +
#   labs(x='Interconnectivity (%)', y='Peak Rate of Spread (%)', linetype='Opinion Strength') +
#   theme(text = element_text(size = 13),
#         #legend.title = element_blank(),
#         panel.grid = element_blank(),
#         axis.title.y = element_text(hjust = 0.5, vjust = 1),
#         legend.position = 'right') +
#   facet_grid(. ~ P_Prop)
# 
# 
# ## Degree of Clustering
# ggplot(Cascade_model_Update1ConVar_full, aes(x = Links, linetype=P_H_Given_C)) +
#   stat_summary(fun.y=mean, aes(y = Prcnt_Same_Clust), geom="line", size = .8) +
#   scale_x_continuous(breaks = round(seq(min(Cascade_model_Update1ConVar_full$Links) - 0.5, max(Cascade_model_Update1ConVar_full$Links), by = 5),1)) + #seq(min(model_C2P_full$Links), max(model_C2P_full$Links), by = 2000)
#   scale_y_continuous(breaks = round(seq(0, 100, by = 10),1), limits = c(0,100)) +
#   theme_bw() +
#   scale_color_manual(values=c("#000000")) +
#   #geom_vline(xintercept = .5, size = .5, colour = "black", linetype = "dashed") +
#   #geom_hline(yintercept = .5, size = .5, colour = "black", linetype = "dashed") +
#   labs(x='Interconnectivity (%)', y='Clustering (% Like-minded Neighbors)', linetype='Opinion Strength') +
#   theme(text = element_text(size = 20),
#         #legend.title = element_blank(),
#         panel.grid = element_blank(),
#         axis.title.y = element_text(hjust = 0.5, vjust = 1),
#         legend.position = 'right') +
#   facet_grid(. ~ P_Prop)



