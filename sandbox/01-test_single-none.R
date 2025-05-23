#this script is for testing the updated/modified singleNone function
#(which is being updated/modified in the single-none.R script!)

library(nosoi)
#downloading the forked nosoi version (only works for me):
devtools::load_all("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Master Project 1/nosoi")
library(ggplot2)

#defining parameters
p_Exit_fct  <- function(t){return(0.08)}

n_contact_fct <- function(t, pop.size) {
  base_rate <- 10
  return(round(base_rate * pop.size / 1000))
}

p_Trans_fct <- function(t,p_max,t_incub){
  if(t < t_incub){p=0}
  if(t >= t_incub){p=p_max}
  return(p)
}

t_incub_fct <- function(x){rnorm(x,mean = 7,sd=1)}
p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}

param_pTrans = list(p_max=p_max_fct,t_incub=t_incub_fct)

#testing the simulation
test_result <- singleNone(length.sim = 100,
                          max.infected = 1000,
                          init.individuals = 1,
                          pExit = p_Exit_fct,
                          param.pExit = NA,
                          timeDep.pExit = FALSE,
                          nContact = n_contact_fct,
                          param.nContact = NA,
                          timeDep.nContact = FALSE,
                          pTrans = p_Trans_fct,
                          param.pTrans = param_pTrans,
                          timeDep.pTrans = FALSE,
                          print.progress = FALSE)

summary(test_result)
test_result$pop_model



# Example: visualize pop size dynamics
plot(test_result$pop_model, type = "l", main = "Population Size Over Time")












#------------------------------testing after the new modification
library(nosoi)

#birth rate
birth.fct <- function(t, pop.size) { return(0.5 * pop.size) }

#death rate
death.fct <- function(t, pop.size) { return(0.5) }

#pExit
p_Exit_fct <- function(t, host.info, pop.size) {
  rep(0.05, nrow(host.info))
}

#nContact
n_contact_fct <- function(t, host.info, pop.size) {
  base_rate <- 10
  scaled_mean <- base_rate * (pop.size / 1000)
  rep(abs(round(rnorm(nrow(host.info), scaled_mean, 1))), 1)
}

#pTrans
p_Trans_fct <- function(t, host.info, pop.size, p_max, t_incub) {
  p <- ifelse(t < t_incub, 0, p_max)
  return(p)
}

# Parameters for pTrans
param_pTrans <- list(p_max = function(x) { rbeta(x, 5, 2) },
                     t_incub = function(x) { rnorm(x, 5, 1) })

#Test the revised simulation
sim <- singleNone(length.sim = 100,
                  max.infected = 50000,
                  init.individuals = 1,
                  initial.population = 10000,
                  birth.rate = birth.fct,
                  param.birth.rate = list(),
                  death.rate = death.fct,
                  param.death.rate = list(),
                  pExit = p_Exit_fct,
                  param.pExit = list(),
                  timeDep.pExit = FALSE,
                  nContact = n_contact_fct,
                  param.nContact = list(),
                  timeDep.nContact = FALSE,
                  pTrans = p_Trans_fct,
                  param.pTrans = list(),
                  timeDep.pTrans = FALSE,
                  prefix.host = "TEST",
                  print.progress = TRUE,
                  print.step = 10)


#does not work for now












#--------------------------Edited nosoi_utilityFunctions.R so that nContact etc can accept extra parameters such as my population size!!! Lets see if this works.
#birth rate
birth.fct <- function(t, pop.size, ...) {
  return(0.5 * pop.size) }

#death rate
death.fct <- function(t, pop.size, ...) {
  return(0.5) } # or: return(0.01 * pop.size) if rate scales

#pExit
p_Exit_fct <- function(t, host.info, pop.size, ...) {
  if (!is.data.frame(host.info) || nrow(host.info) == 0) return(numeric(0))
  rep(0.05, nrow(host.info))
}

#nContact
n_contact_fct <- function(t, host.info, pop.size, ...) {
  if (!is.data.frame(host.info) || nrow(host.info) == 0) return(numeric(0))

  base_rate <- 10
  scaled_mean <- base_rate * (pop.size / 1000)

  rep(abs(round(rnorm(nrow(host.info), scaled_mean, 1))), 1)
}

# Define parameter functions
p_max_fct <- function(x) rbeta(x, 5, 2)
t_incub_fct <- function(x) rnorm(x, 5, 1)

# Bundle into param list
param_pTrans <- list(
  p_max = p_max_fct,
  t_incub = t_incub_fct)

# Define your transmission probability function
p_Trans_fct <- function(t, host.info, pop.size, p_max, t_incub, ...) {
  if (!is.data.frame(host.info) || nrow(host.info) == 0) {
    return(numeric(0))
  }
  p <- ifelse(t < t_incub, 0, p_max)
  return(p)
}

#> parseFunction(p_Trans_fct, param_pTrans, "pTrans", timeDep = FALSE)
#Error in FunctionSanityChecks(pFunc, name, param.pFunc, timeDep, diff,  :
#                                Parameter name in param.pTrans should match the name used in #pTrans.

#--> Solved this!

# Test with parseFunction (this shouldn't error!)
#parseFunction(p_Trans_fct, param_pTrans, "pTrans", timeDep = FALSE)
#it should work now:

#Test the revised simulation
sim <- singleNone(length.sim = 100,
                  max.infected = 50000,
                  init.individuals = 1,
                  initial.population = 10000,
                  birth.rate = birth.fct,
                  param.birth.rate = list(),
                  death.rate = death.fct,
                  param.death.rate = list(),
                  pExit = p_Exit_fct,
                  param.pExit = list(),
                  timeDep.pExit = FALSE,
                  nContact = n_contact_fct,
                  param.nContact = list(),
                  timeDep.nContact = FALSE,
                  pTrans = p_Trans_fct,
                  param.pTrans = param_pTrans,
                  timeDep.pTrans = FALSE,
                  prefix.host = "TEST",
                  print.progress = TRUE,
                  print.step = 10)

#error:
#Starting the simulation
#Initializing ... running ...
#Error in if (is.null(host.info) || nrow(host.info) == 0) return(numeric(0)) :
#  missing value where TRUE/FALSE needed

#I have solved that error, but now I have:
#Starting the simulation
#Initializing ... running ...
#Error in pasedFunction$vectArgs :
#  $ operator is invalid for atomic vectors




