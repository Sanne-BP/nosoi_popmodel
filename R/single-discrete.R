#' @title Single-host pathogen in a structured (discrete) host population
#'
#' @description This function, that can be wrapped within \code{\link{nosoiSim}}, runs a single-host transmission chain simulation, with a discrete host population structure (e.g. spatial, socio-economic, etc.). The simulation stops either at
#' the end of given time (specified by \code{length.sim}) or when the number of hosts infected threshold (\code{max.infected}) is crossed.
#'
#' @section Structure Matrix:
#' The structure matrix provided provided should of class \code{matrix}, with the same number of rows and columns, rows representing departure state and column the arrival state. All rows should add to 1.
#' @section Structure Parameters:
#' The \code{pMove} function should return a single probability (a number between 0 and 1).
#' @section Structure Parameters:
#' The use of \code{diff} (switch to \code{TRUE}) makes the corresponding function use the argument \code{current.in} (for "currently in"). Your function should in that case give a result for every possible discrete state.
#' @section Structure Parameters:
#' The use of \code{hostCount} (switch to \code{TRUE}) makes the corresponding function use the argument \code{host.count}.
#' @section Order of Arguments:
#' The user specified function's arguments should follow this order: \code{t} (mandatory), \code{prestime} (optional, only if timeDep is TRUE),
#' \code{current.in} (optional, only if diff is TRUE), \code{host.count} (optional, only if hostCount is TRUE) and \code{parameters} specified in the list.
#'
#' @inheritParams singleNone
#' @param init.structure in which state (e.g. location) the initially infected individuals are located.
#' @param structure.matrix transition matrix (probabilities) to go from location A (row) to B (column)
#' @param diff.pMove is pMove different between states of the structured population (TRUE/FALSE)
#' @param timeDep.pMove is pMove dependent on the absolute time of the simulation (TRUE/FALSE)
#' @param hostCount.pMove does pMove varies with the host count in the state? (TRUE/FALSE); diff.pMove should be TRUE.
#' @param pMove function that gives the probability of a host moving as a function of time.
#' @param param.pMove parameter names (list of functions) for the pMove.
#' @param diff.nContact is nContact different between states of the structured population (TRUE/FALSE)
#' @param hostCount.nContact does nContact varies with the host count in the state? (TRUE/FALSE); diff.nContact should be TRUE.
#' @param diff.pTrans is pTrans different between states of the structured population (TRUE/FALSE)
#' @param hostCount.pTrans does pTrans varies with the host count in the state? (TRUE/FALSE); diff.pTrans should be TRUE.
#' @param diff.pExit is pExit different between states of the structured population (TRUE/FALSE)
#' @param hostCount.pExit does pExit varies with the host count in the state? (TRUE/FALSE); diff.pExit should be TRUE.
#' @param initial.population initial size of the host population.
#' @param birth.rate birth rate per individual per time step.
#' @param death.rate death rate per individual per time step.
#'
#' @inherit singleNone return details
#'
#' @seealso For simulations with a structure in continuous space, see \code{\link{singleContinuous}}. For simulations without any structures, see \code{\link{singleNone}}.
#'
#' @examples
#' \donttest{
#' t_incub_fct <- function(x){rnorm(x,mean = 5,sd=1)}
#' p_max_fct <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#' p_Exit_fct  <- function(t){return(0.08)}
#' p_Move_fct  <- function(t){return(0.1)}
#'
#' proba <- function(t,p_max,t_incub){
#'  if(t <= t_incub){p=0}
#'  if(t >= t_incub){p=p_max}
#'  return(p)
#' }
#'
#' time_contact = function(t){round(rnorm(1, 3, 1), 0)}
#'
#' transition.matrix = matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),
#'                            nrow = 3, ncol = 3,
#'                            dimnames=list(c("A","B","C"),c("A","B","C")))
#'
#' set.seed(805)
#' test.nosoiA <- nosoiSim(type="single", popStructure="discrete",
#'                        length=20,
#'                        max.infected=100,
#'                        init.individuals=1,
#'                        init.structure="A",
#'                        structure.matrix=transition.matrix,
#'                        pMove=p_Move_fct,
#'                        param.pMove=NA,
#'                        nContact=time_contact,
#'                        param.nContact=NA,
#'                        pTrans = proba,
#'                        param.pTrans = list(p_max=p_max_fct,
#'                                            t_incub=t_incub_fct),
#'                       pExit=p_Exit_fct,
#'                       param.pExit=NA)
#'}
#' @export singleDiscrete
singleDiscrete <- function(length.sim,
                           max.infected = 10000,

                           # Population A
                           init.individuals.A,
                           pExit.A, param.pExit.A = NA,
                           nContact.A, param.nContact.A = NA,
                           pTrans.A, param.pTrans.A = NA,
                           initial.population.A,
                           birth.rate.A,
                           death.rate.A,

                           # Population B
                           init.individuals.B,
                           pExit.B, param.pExit.B = NA,
                           nContact.B, param.nContact.B = NA,
                           pTrans.B, param.pTrans.B = NA,
                           initial.population.B,
                           birth.rate.B,
                           death.rate.B,

                           # Simulation settings
                           print.progress = TRUE,
                           print.step = 10) {

  # Prefixes
  prefix.host.A <- "H"
  prefix.host.B <- "A"

  # Host Parameters
  ParamHost.A <- list(pExit = pExit.A, param.pExit = param.pExit.A,
                      nContact = nContact.A, param.nContact = param.nContact.A,
                      pTrans = pTrans.A, param.pTrans = param.pTrans.A)

  ParamHost.B <- list(pExit = pExit.B, param.pExit = param.pExit.B,
                      nContact = nContact.B, param.nContact = param.nContact.B,
                      pTrans = pTrans.B, param.pTrans = param.pTrans.B)

  # Initialize simulation
  res <- nosoiSimConstructor(
    total.time = 1,
    type = "dual",
    pop.A = nosoiSimOneConstructor(N.infected = init.individuals.A,
                                   table.hosts = iniTable(init.individuals.A, NA, prefix.host.A, ParamHost.A),
                                   table.state = NULL,
                                   prefix.host = prefix.host.A,
                                   popStructure = "discrete"),
    pop.B = nosoiSimOneConstructor(N.infected = init.individuals.B,
                                   table.hosts = iniTable(init.individuals.B, NA, prefix.host.B, ParamHost.B),
                                   table.state = NULL,
                                   prefix.host = prefix.host.B,
                                   popStructure = "discrete")
  )

  # ✅ Initialize custom population + infection tracking table AFTER constructor to avoid overwriting
  res$table.state <- data.table::data.table(
    time = integer(),
    population.A = numeric(),
    infected.A = integer(),
    population.B = numeric(),
    infected.B = integer()
  )

  if (print.progress) cat("Starting the simulation\n")

  for (i in 1:length.sim) {
    if (print.progress && i %% print.step == 0) {
      cat("Time step:", i, "\n")
    }

    # Update populations
    births.A <- rpois(1, birth.rate.A * initial.population.A)
    deaths.A <- rbinom(1, initial.population.A, death.rate.A)
    initial.population.A <- initial.population.A + births.A - deaths.A

    births.B <- rpois(1, birth.rate.B * initial.population.B)
    deaths.B <- rbinom(1, initial.population.B, death.rate.B)
    initial.population.B <- initial.population.B + births.B - deaths.B

    # Run one simulation step
    res <- dualStep(res, pop.A.params = ParamHost.A, pop.B.params = ParamHost.B)

    # Track population sizes and infected counts
    table.state.temp <- data.table::data.table(
      time = i,
      population.A = initial.population.A,
      infected.A = length(res$pop.A$table$host),
      population.B = initial.population.B,
      infected.B = length(res$pop.B$table$host)
    )

    # ✅ Append to tracking table
    res$table.state <- data.table::rbindlist(
      list(res$table.state, table.state.temp), use.names = TRUE, fill = TRUE
    )

    # Stop if max infected is reached
    total.infected <- length(res$pop.A$table$host) + length(res$pop.B$table$host)
    if (total.infected >= max.infected) {
      if (print.progress) {
        cat("Max infected reached at time", i, "\n")
      }
      break
    }
  }

  return(res)
}
