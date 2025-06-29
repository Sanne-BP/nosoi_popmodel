#case study for the report! now finetuning everything to match human and mountain gorilla populations + making the final modifications to the script etc. making everything sub population dependent

#defining different (sub)populations dynamics:
subpop_names <- c("Humans", "Gorilla1", "Gorilla2")
init_pop <- list(Humans = 10000, Gorilla1 = 200, Gorilla2 = 450)
birth_rates <- list(Humans = 0.2, Gorilla1 = 0.09, Gorilla2 = 0.15)
death_rates <- list(Humans = 0.2, Gorilla1 = 0.09, Gorilla2 = 0.15)

#movement matrix
transition.matrix <- matrix(
  c(0.85,  0.1,  0.05,  # Humans mostly stay among humans, small chance to gorillas
    0.05, 0.9,  0.05,  # Gorilla1 mostly stays, small human contact, small gorilla2 contact
    0.05, 0.05, 0.9),    # Gorilla2 mostly stays, small human contact, small gorilla1 contact
  nrow = 3, ncol = 3, byrow = TRUE,
  dimnames = list(subpop_names, subpop_names))

#parameters:
#exit rate per (sub)population
p_Exit_fct <- function(t, current.in) {
  if (current.in == "Humans") return(0.02)
  if (current.in == "Gorilla1") return(0.05)
  if (current.in == "Gorilla2") return(0.06)}

#movement probability per (sub)population
p_Move_fct <- function(t, current.in) {
  if (current.in == "Humans") return(0.15)
  if (current.in == "Gorilla1") return(0.05)
  if (current.in == "Gorilla2") return(0.02)
}

#number of contacts per infected host
n_contact_fct <- function(t) {
  abs(round(rnorm(1, mean = 0.5, sd = 1), 0))
}
#use hostcount()!! make nContact dependent on hostcount(), similar in ebola continuous, make it dependent on pop size!!! its a bandaid!! to avoid stupid cases of 100 million infections

# For transmission probability, define with incubation and max prob, per host:
p_max_fct <- function(x) rbeta(x, shape1 = 5, shape2 = 2)
t_incub_fct <- function(x) rnorm(x, mean = 5, sd = 1)

proba <- function(t, p_max, t_incub) {
  if (t <= t_incub) p <- 0 else p <- p_max
  return(p)
}

param_pTrans <- list(p_max = p_max_fct, t_incub = t_incub_fct)



#Now run the wrapper function:
run_sim_and_popmodel <- function(length.sim = 300,
                                 init.pop,
                                 birth.rate,
                                 death.rate,
                                 transition.matrix,
                                 pExit,
                                 pMove,
                                 nContact,
                                 pTrans,
                                 param.pTrans) {

  #Run epidemic simulation with Nosoi
  sim <- nosoiSim(type = "single", popStructure = "discrete",
                  length.sim = length.sim,
                  max.infected = 300,
                  init.individuals = 1,
                  init.structure = names(init.pop)[1], # Start in first subpop
                  structure.matrix = transition.matrix,
                  pExit = pExit,
                  param.pExit = NA,
                  timeDep.pExit = FALSE,
                  diff.pExit = TRUE,
                  pMove = pMove,
                  param.pMove = NA,
                  timeDep.pMove = FALSE,
                  diff.pMove = TRUE, #true activates subpop-specific pMove
                  nContact = nContact,
                  param.nContact = NA,
                  timeDep.nContact = FALSE,
                  diff.nContact = FALSE,
                  pTrans = pTrans,
                  param.pTrans = param_pTrans,
                  timeDep.pTrans = FALSE,
                  diff.pTrans = FALSE,
                  prefix.host = "H",
                  print.progress = FALSE,
                  print.step = 10)

  # Initialize population size tracking list for each subpop
  PopModel <- list()
  for (loc in names(init.pop)) {
    PopModel[[loc]] <- numeric(length.sim + 1)
    PopModel[[loc]][1] <- init.pop[[loc]]
  }

  # Extract host info dataframe
  hosts_df <- sim$host.info.A$table.hosts

  # Loop over time steps and subpopulations to update population sizes
  for (t in 1:length.sim) {
    for (loc in names(PopModel)) {
      births <- rpois(1, birth.rate[[loc]] * PopModel[[loc]][t])
      deaths <- rbinom(1, PopModel[[loc]][t], death.rate[[loc]])
      epidemic_deaths <- sum(hosts_df$structure == loc & hosts_df$out.time == t & hosts_df$active == FALSE)

      PopModel[[loc]][t + 1] <- max(0, PopModel[[loc]][t] + births - deaths - epidemic_deaths)
    }
  }

  return(list(simulation = sim, population = PopModel))
}

#results:
result <- run_sim_and_popmodel(
  length.sim = 300,
  init.pop = init_pop,
  birth.rate = birth_rates,
  death.rate = death_rates,
  transition.matrix = transition.matrix,
  pExit = p_Exit_fct,
  pMove = p_Move_fct,
  nContact = n_contact_fct,
  pTrans = proba,
  param.pTrans = param_pTrans
)




