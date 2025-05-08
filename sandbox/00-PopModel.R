#This is the population size dynamics = PopModel that I have made. And this is sufficient for now and we will work with it to implement it into Nosoi!!! (see script 01). nContact was made dependent on the population size, so the bigger the population the higher the number of contacts and the smaller the population the lower the number of contacts. All these parameters can be adjusted!!

library(devtools)
library(nosoi)
library(ggplot2)
library(dplyr)

#PopModel:
time_steps <- 1000

simulate_population <- function(time_steps, initial_population, birth_rate, death_rate) {
  population <- numeric(time_steps + 1)
  population[1] <- initial_population

  for (t in 2:(time_steps + 1)) {
    births <- rpois(1, birth_rate * population[t - 1])
    deaths <- rbinom(1, population[t - 1], death_rate)
    population[t] <- population[t - 1] + births - deaths
  }

  return(population)
}

pop_size <- simulate_population(time_steps, initial_population = 100000,
                                birth_rate = 0.5, death_rate = 0.5)

ggplot(data=data.frame(Time=1:length(pop_size), Population=pop_size),
       aes(x=Time, y=Population)) +
  geom_line(color="blue", linewidth=0.5) +
  theme_minimal() +
  labs(x="Time Steps", y="Population Size", title="Population Size Dynamics")

#Parameters (can be adjusted ofcourse!):
n_contact_fct <- function(t) {
  current_pop <- pop_size[min(t + 1, length(pop_size))]
  base_rate <- 10
  scaled_mean <- base_rate * (current_pop / 1000)
  n_contacts_i <- abs(round(rnorm(1, scaled_mean, 1), 0))
  return(n_contacts_i)
}

p_Exit_fct  <- function(t) { return(0.04) }

p_Trans_fct <- function(t, p_max, t_incub) {
  if (t < t_incub) { p <- 0 }
  if (t >= t_incub) { p <- p_max }
  return(p)
}

t_incub_fct <- function(x) { rnorm(x, mean = 5, sd = 1) }
p_max_fct <- function(x) { rbeta(x, shape1 = 5, shape2 = 2) }

param_pTrans = list(p_max = p_max_fct, t_incub = t_incub_fct)
