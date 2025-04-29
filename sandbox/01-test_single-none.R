#this script is for testing the updated/modified singleNone function, updated in the single-none.R script!

library(nosoi)

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







library(ggplot2)

# Example: visualize pop size dynamics
plot(test_result$pop_model, type = "l", main = "Population Size Over Time")
