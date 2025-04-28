#clear environment
rm(list=ls())

library(nosoi)

# Define your test parameters as needed
p_Exit_fct  <- function(t) { return(0.08) }
n_contact_fct <- function(t) { abs(round(rnorm(1, 0.5, 1), 0)) }
p_Trans_fct <- function(t, p_max, t_incub) {
  if(t < t_incub) { p = 0 }
  if(t >= t_incub) { p = p_max }
  return(p)
}
t_incub_fct <- function(x) { rnorm(x, mean = 7, sd = 1) }
p_max_fct <- function(x) { rbeta(x, shape1 = 5, shape2 = 2) }

param_pTrans <- list(p_max = p_max_fct, t_incub = t_incub_fct)

# Run your modified simulation
test <- singleNone(length.sim = 40,
                     max.infected = 100,
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
                     print.progress = TRUE,
                     print.step = 10)

SimulationSingle <- nosoiSim(type="single", popStructure="none",
                             length.sim=100, max.infected=100, init.individuals=1,
                             nContact=n_contact_fct,
                             param.nContact=NA,
                             timeDep.nContact=FALSE,
                             pExit = p_Exit_fct,
                             param.pExit=NA,
                             timeDep.pExit=FALSE,
                             pTrans = p_Trans_fct,
                             param.pTrans = param_pTrans,
                             timeDep.pTrans=FALSE,
                             prefix.host="H",
                             print.progress=FALSE)
