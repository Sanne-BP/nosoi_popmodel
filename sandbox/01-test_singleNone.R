#this script is for testing the updated/modified singleNone function
#(which is being updated/modified in the single-none.R script!)

#downloading the forked nosoi version (only works for me):
devtools::load_all("/Users/sanne/Library/Mobile Documents/com~apple~CloudDocs/Master Ecology & Conservation/Master Project 1/nosoi_popmodel")
library(ggplot2)





#--------------------------this does not work anymoreee
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
















#-------------------------------testing whether we now have the working singleNone file!!!

# Set parameters (if not already)
p_max_fct <- function(ID) {
  return(0.3)  # You can still return a constant, but accept an argument
}

t_incub_fct <- function(ID) {
  return(3)
}

# Transmission probability function
proba <- function(t, p_max, t_incub) {
  ifelse(t >= t_incub, p_max, 0)
}

# Exit probability function (constant exit)
p_Exit_fct <- function(t) { 0.1 }

# Contact function (constant number of contacts)
time_contact <- function(t) { 2 }

# Run the simulation
test.popdyn <- singleNone(length.sim = 100, max.infected = 1000, init.individuals = 2,
                          nContact = time_contact,
                          param.nContact = NA,
                          timeDep.nContact = FALSE,
                          pTrans = proba,
                          param.pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct),
                          timeDep.pTrans = FALSE,
                          pExit = p_Exit_fct,
                          param.pExit = NA,
                          timeDep.pExit = FALSE,
                          print.progress = TRUE)

#THIS WORKSSS YHAS
plot(test.popdyn$pop_model, type="l", xlab="Time", ylab="Population Size")
test.popdyn$pop_model  # Vector of population size per time step
test.popdyn$host.info.A$N.infected

plot(test.popdyn$pop_model, type="l", col="blue", ylab="Count", xlab="Time")
lines(test.popdyn$host.info.A$summary$N.infected, col="red")
legend("topright", legend=c("Population Size", "Infected"), col=c("blue", "red"), lty=1)










#Is it possible to determine the initital population size + birth and death rates ourselves? (the answer is, yes we can!!)
sim <- singleNone(length.sim = 100, max.infected = 10000, init.individuals = 1,
                  pExit = p_Exit_fct,
                  param.pExit = NA,
                  timeDep.pExit = FALSE,
                  nContact = time_contact,
                  param.nContact = NA,
                  timeDep.nContact = FALSE,
                  pTrans = proba,
                  param.pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct),
                  timeDep.pTrans = FALSE,
                  print.progress = TRUE,
                  initial.population = 10000,
                  birth.rate = 0.5,
                  death.rate = 0.5)

#Starting the simulation
#Initializing ... running ...
#Time: 10 (20% of maximum length). Hosts count: 10 (2% of maximum infected hosts).
#Time: 20 (40% of maximum length). Hosts count: 85 (17% of maximum infected hosts).
#Time: 30 (60% of maximum length). Hosts count: 566 (113% of maximum infected hosts).
#done.
#The simulation has run for 30 units of time and a total of 566 hosts have been infected.

plot(sim$pop_model, type = "l", ylab = "Population size", xlab = "Time", main = "Population Dynamics")

sim$pop_model





#So, next step is fine tuning this!!------------------------------------------------------------
# --> lets focus first on the interpretation and visualization

str(sim$host.info.A)

infected_over_time <- sapply(0:sim$total.time, function(t) {
  sum(sim$host.info.A$table.hosts$inf.time <= t)})

pop_df <- data.frame(time = 0:sim$total.time,
  population_size = sim$pop_model[1:(sim$total.time + 1)],
  infected = infected_over_time)

head(pop_df)

#only population size:
ggplot(pop_df, aes(x = time, y = population_size)) +
  geom_line(color = "blue", size = 1) +
  labs(title = "Population Size Over Time",
       x = "Time",
       y = "Population Size") +
  theme_minimal()

#only infected:
ggplot(pop_df, aes(x = time, y = infected)) +
  geom_line(color = "red", size = 1) +
  labs(title = "Number of Infected Over Time",
       x = "Time",
       y = "Infected Count") +
  theme_minimal()

#in the same plot:
ggplot(pop_df, aes(x = time)) +
  geom_line(aes(y = population_size, color = "Population Size"), size = 1.2) +
  geom_line(aes(y = infected, color = "Infected Individuals"), size = 1.2) +
  scale_color_manual(values = c("Population Size" = "blue",
                                "Infected Individuals" = "red")) +
  labs(title = "Population Size and Infected Individuals Over Time",
       x = "Time",
       y = "Count",
       color = "Legend") +
  theme_minimal()

#ggsave("sandbox/plots/population_dynamics2.pdf", width = 10, height = 6)

#observation from plot: infected rises while population drops, which indicates that the outbreak is actually spreading fast enough that infection-related exits are reducing population size!!



#Can we alter the parameters?------------------------------------------------------------
















