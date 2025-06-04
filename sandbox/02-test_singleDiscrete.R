#this script is for testing the updated/modified singleDiscrete function
#(which is being updated/modified in the single-Discrete.R script!)

#downloading the forked nosoi version (only works for me):
devtools::load_all("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Master Project 1/nosoi_popmodel")
library(ggplot2)

# -- Define constant pExit, nContact, pTrans functions --
pExit.const <- function(t, ...) { return(rep(0.05, length(t))) }
nContact.const <- function(t, ...) { return(rep(2, length(t))) }
pTrans.const <- function(t, ...) { return(rep(0.2, length(t))) }

# -- Run your custom singleDiscrete() function --
sim.test <- singleDiscrete(
  length.sim = 25,
  max.infected = 200,
  init.individuals.A = 1,
  init.individuals.B = 1,
  pExit.A = pExit.const, param.pExit.A = NULL,
  pExit.B = pExit.const, param.pExit.B = NULL,
  nContact.A = nContact.const, param.nContact.A = NULL,
  nContact.B = nContact.const, param.nContact.B = NULL,
  pTrans.A = pTrans.const, param.pTrans.A = NULL,
  pTrans.B = pTrans.const, param.pTrans.B = NULL,
  initial.population.A = 1000,
  birth.rate.A = 0.2,
  death.rate.A = 0.1,
  initial.population.B = 500,
  birth.rate.B = 0.3,
  death.rate.B = 0.15,
  print.progress = TRUE,
  print.step = 5)

#Now I get this error again after trying the simulation: Error in CoreSanityChecksDiscrete(length.sim, max.infected, init.individuals.A,  : could not find function "CoreSanityChecksDiscrete"
#I also got this when first implementing new stuff into the singleNone, but how to fix this??


#it does work when you run the CoreSanityChecks separately in here:
CoreSanityChecksDiscrete <- function(length.sim, max.infected, init.individuals.A, init.individuals.B) {
  if (!is.numeric(length.sim) || length.sim <= 0 || length.sim %% 1 != 0) {
    stop("length.sim must be a positive integer.")
  }
  if (!is.numeric(max.infected) || max.infected <= 0 || max.infected %% 1 != 0) {
    stop("max.infected must be a positive integer.")
  }
  if (!is.numeric(init.individuals.A) || init.individuals.A <= 0 || init.individuals.A %% 1 != 0) {
    stop("init.individuals.A must be a positive integer.")
  }
  if (!is.numeric(init.individuals.B) || init.individuals.B <= 0 || init.individuals.B %% 1 != 0) {
    stop("init.individuals.B must be a positive integer.")
  }
}
#but after running the simulation again, I get this new error: Error in if (!is.list(param.pFunc) && is.na(param.pFunc)) { : missing value where TRUE/FALSE needed

sim.test <- singleDiscrete(
  length.sim = 25,
  max.infected = 200,
  init.individuals.A = 1,
  init.individuals.B = 1,
  pExit.A = pExit.const, param.pExit.A = NA,
  pExit.B = pExit.const, param.pExit.B = NA,
  nContact.A = nContact.const, param.nContact.A = NA,
  nContact.B = nContact.const, param.nContact.B = NA,
  pTrans.A = pTrans.const, param.pTrans.A = NA,
  pTrans.B = pTrans.const, param.pTrans.B = NA,
  initial.population.A = 1000,
  birth.rate.A = 0.2,
  death.rate.A = 0.1,
  initial.population.B = 500,
  birth.rate.B = 0.3,
  death.rate.B = 0.15,
  print.progress = TRUE,
  print.step = 5
)
#Error in FunctionSanityChecks(pFunc, name, param.pFunc, timeDep, diff,  : There is a probleme with your function nContact.A: you should provide a parameter list named param.nContact.A.

sim.test <- singleDiscrete(
  length.sim = 25,
  max.infected = 200,
  init.individuals.A = 1,
  init.individuals.B = 1,
  pExit.A = pExit.const, param.pExit.A = list(),
  pExit.B = pExit.const, param.pExit.B = list(),
  nContact.A = nContact.const, param.nContact.A = list(),
  nContact.B = nContact.const, param.nContact.B = list(),
  pTrans.A = pTrans.const, param.pTrans.A = list(),
  pTrans.B = pTrans.const, param.pTrans.B = list(),
  initial.population.A = 1000,
  birth.rate.A = 0.2,
  death.rate.A = 0.1,
  initial.population.B = 500,
  birth.rate.B = 0.3,
  death.rate.B = 0.15,
  print.progress = TRUE,
  print.step = 5
)
#Starting the simulation
#Initializing ... running ...
#Error in data.table::rbindlist(c(list(res$table.state), table.state.temp)) : Item 1 of input is not a data.frame, data.table or list

sim.test <- singleDiscrete(
  length.sim = 25,
  max.infected = 200,
  init.individuals.A = 1,
  init.individuals.B = 1,
  pExit.A = pExit.const, param.pExit.A = list(),
  pExit.B = pExit.const, param.pExit.B = list(),
  nContact.A = nContact.const, param.nContact.A = list(),
  nContact.B = nContact.const, param.nContact.B = list(),
  pTrans.A = pTrans.const, param.pTrans.A = list(),
  pTrans.B = pTrans.const, param.pTrans.B = list(),
  initial.population.A = 1000,
  birth.rate.A = 0.2,
  death.rate.A = 0.1,
  initial.population.B = 500,
  birth.rate.B = 0.3,
  death.rate.B = 0.15,
  print.progress = TRUE,
  print.step = 5
)
#Starting the simulation
#Initializing ...Classes ‘data.table’ and 'data.frame':	0 obs. of  0 variables
#- attr(*, ".internal.selfref")=<externalptr>
#  NULL
#running ...
#Error in data.table::rbindlist(c(list(res$table.state), table.state.temp)) :
#  Item 1 of input is not a data.frame, data.table or list

sim.test <- singleDiscrete(
  length.sim = 25,
  max.infected = 200,
  init.individuals.A = 1,
  init.individuals.B = 1,
  pExit.A = pExit.const, param.pExit.A = list(),
  pExit.B = pExit.const, param.pExit.B = list(),
  nContact.A = nContact.const, param.nContact.A = list(),
  nContact.B = nContact.const, param.nContact.B = list(),
  pTrans.A = pTrans.const, param.pTrans.A = list(),
  pTrans.B = pTrans.const, param.pTrans.B = list(),
  initial.population.A = 1000,
  birth.rate.A = 0.2,
  death.rate.A = 0.1,
  initial.population.B = 500,
  birth.rate.B = 0.3,
  death.rate.B = 0.15,
  print.progress = TRUE,
  print.step = 5
)
#Starting the simulation
#Initializing ...Classes ‘data.table’ and 'data.frame':	0 obs. of  5 variables:
#  $ time        : int
#$ population.A: num
#$ infected.A  : int
#$ population.B: num
#$ infected.B  : int
#- attr(*, ".internal.selfref")=<externalptr>
#  NULL
#running ...
#Error in data.table::rbindlist(c(list(res$table.state), table.state.temp)) :
#  Item 1 of input is not a data.frame, data.table or list

#the table is showing the correct structure now, but still error!
















# Example dummy functions
pExit.A <- function(t) 0.05
pExit.B <- function(t) 0.07
nContact.A <- function(t) 5
nContact.B <- function(t) 3
pTrans.A <- function(t) 0.1
pTrans.B <- function(t) 0.15

set.seed(123)
res <- singleDiscrete(
  length.sim = 50,
  max.infected = 500,
  init.individuals.A = 10,
  init.individuals.B = 5,
  pExit.A = pExit.A, param.pExit.A = NA,
  pExit.B = pExit.B, param.pExit.B = NA,
  nContact.A = nContact.A, param.nContact.A = NA,
  nContact.B = nContact.B, param.nContact.B = NA,
  pTrans.A = pTrans.A, param.pTrans.A = NA,
  pTrans.B = pTrans.B, param.pTrans.B = NA,
  initial.population.A = 10000,
  birth.rate.A = 0.01,
  death.rate.A = 0.005,
  initial.population.B = 2000,
  birth.rate.B = 0.02,
  death.rate.B = 0.01
)








library(data.table)

# Empty initial table
table.state <- data.table(time=integer(0),
                          population.A=numeric(0),
                          infected.A=integer(0),
                          population.B=numeric(0),
                          infected.B=integer(0))

# New data to append
table.state.temp <- data.table(time=1, population.A=10000, infected.A=10,
                               population.B=2000, infected.B=5)

# Append
table.state <- rbindlist(list(table.state, table.state.temp))

print(table.state)













#-----------
sim.test <- singleDiscrete(
  length.sim = 25,
  max.infected = 200,
  init.individuals.A = 1,
  init.individuals.B = 1,
  pExit.A = pExit.const, param.pExit.A = list(),
  pExit.B = pExit.const, param.pExit.B = list(),
  nContact.A = nContact.const, param.nContact.A = list(),
  nContact.B = nContact.const, param.nContact.B = list(),
  pTrans.A = pTrans.const, param.pTrans.A = list(),
  pTrans.B = pTrans.const, param.pTrans.B = list(),
  initial.population.A = 1000,
  birth.rate.A = 0.2,
  death.rate.A = 0.1,
  initial.population.B = 500,
  birth.rate.B = 0.3,
  death.rate.B = 0.15,
  print.progress = TRUE,
  print.step = 5
)

#Error in x(1) : could not find function "x"
#-------apparently i now need to modify another corescript to be able to fix this??
#I made a change in the: nosoi_tablesManagment.R

#NEW ERROR:
#Starting the simulation
#Error in dualStep(res, pop.A.params = ParamHost.A, pop.B.params = ParamHost.B) :
#  could not find function "dualStep"
# BUT I AM ALSO WORKING IN THE SINGLEDISCRETE?! so have i been doing this completely wrong, Idkkkkkkkkkk






