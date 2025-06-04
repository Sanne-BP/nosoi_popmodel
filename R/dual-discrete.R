#' @title Dual-host pathogen in structured (discrete) hosts populations
#'
#' @description This function, that can be wrapped within \code{\link{nosoiSim}}, runs a dual-host transmission chain simulation, with discrete hosts populations structures (e.g. spatial, socio-economic, etc.). The simulation stops either at
#' the end of given time (specified by \code{length.sim}) or when the number of hosts infected threshold (\code{max.infected}) is crossed.
#'
#' @section Structure Matrix:
#' The structure/transition matrix provided should be of class \code{matrix}, with the same number of rows and columns, rows representing departure state and column the arrival state. All rows should add to 1. Probability values can be different for hosts A and B (so two different matrices), but the name of the column and the rows should be shared.
#' @section Order of Arguments:
#' The user specified function's arguments should follow this order: \code{t} (mandatory), \code{prestime} (optional, only if timeDep is TRUE),
#' \code{current.in} (optional, only if diff is TRUE), \code{host.count.A} or \code{host.count.B} (optional, only if hostCount is TRUE) and \code{parameters} specified in the list.
#'
#' @inheritParams dualNone
#' @param init.structure.A in which state (e.g. location) the initially infected individuals of host-type A are located (\code{NA} if init.individual.A is 0)?
#' @param init.structure.B in which state (e.g. location) the initially infected individuals of host-type B are located (\code{NA} if init.individual.B is 0)?
#' @param structure.matrix.A transition matrix (probabilities) to go from location A (row) to B (column) for host-type A.
#' @param structure.matrix.B transition matrix (probabilities) to go from location A (row) to B (column) for host-type B.
#' @param diff.pExit.A is pExit of host-type A different between states of the structured population (TRUE/FALSE)?
#' @param hostCount.pExit.A does pExit of host-type A vary with the host count (of either host-type A or B) in the state? (TRUE/FALSE); diff.pExit.A should be TRUE.
#' @param pMove.A function that gives the probability of a host moving as a function of time for host-type A.
#' @param param.pMove.A parameter names (list of functions) for the pMove for host-type A.
#' @param timeDep.pMove.A is pMove of host-type A dependent on the absolute time of the simulation (TRUE/FALSE)?
#' @param diff.pMove.A is pMove of host-type A different between states of the structured population (TRUE/FALSE)?
#' @param hostCount.pMove.A does pMove of host-type A vary with the host count (of either host A or B) in the state? (TRUE/FALSE); diff.pMove.A should be TRUE.
#' @param diff.nContact.A is nContact of host-type A different between states of the structured population (TRUE/FALSE)?
#' @param hostCount.nContact.A does nContact of host-type A vary with the host count (of either host A or B) in the state? (TRUE/FALSE); diff.nContact.A should be TRUE.
#' @param diff.pTrans.A is pTrans of host-type A different between states of the structured population (TRUE/FALSE)?
#' @param hostCount.pTrans.A does pTrans of host-type A vary with the host count (of either host A or B) in the state? (TRUE/FALSE); diff.pTrans.A should be TRUE.
#' @param diff.pExit.B is pExit of host-type B different between states of the structured population (TRUE/FALSE)?
#' @param hostCount.pExit.B does pExit of host-type B vary with the host count (of either host A or B) in the state? (TRUE/FALSE); diff.pExit.B should be TRUE.
#' @param pMove.B function that gives the probability of a host moving as a function of time for host-type B.
#' @param param.pMove.B parameter names (list of functions) for the pMove for host-type B.
#' @param timeDep.pMove.B is sdMove of host-type B dependent on the absolute time of the simulation (TRUE/FALSE) for host-type B.
#' @param diff.pMove.B is pMove of host-type B different between states of the structured population (TRUE/FALSE)?
#' @param hostCount.pMove.B does pMove of host-type B vary with the host count (of either host A or B) in the state? (TRUE/FALSE); diff.pMove.B should be TRUE.
#' @param diff.nContact.B is nContact of host-type B different between states of the structured population (TRUE/FALSE)?
#' @param hostCount.nContact.B does nContact of host-type B vary with the host count (of either host A or B) in the state? (TRUE/FALSE); diff.nContact.B should be TRUE.
#' @param diff.pTrans.B is pTrans host-type B different between states of the structured population (TRUE/FALSE)?
#' @param hostCount.pTrans.B does pTrans of host-type B vary with the host count (of either host A or B) in the state? (TRUE/FALSE); diff.pTrans.B should be TRUE.
#'
#' @inherit singleNone return details
#' @inheritSection singleDiscrete Structure Parameters
#' @inheritSection dualNone Suffixes
#'
#' @seealso For simulations with a structure in continuous space, see \code{\link{dualContinuous}}. For simulations without any structures, see \code{\link{dualNone}}.
#'
#' @examples
#' \donttest{
#'#Host A
#'t_infectA_fct <- function(x){rnorm(x,mean = 12,sd=3)}
#'pTrans_hostA <- function(t,t_infectA){
#'  if(t/t_infectA <= 1){p=sin(pi*t/t_infectA)}
#'  if(t/t_infectA > 1){p=0}
#'  return(p)
#'}
#'
#'p_Move_fctA  <- function(t){return(0.1)}
#'
#'p_Exit_fctA  <- function(t,t_infectA){
#'  if(t/t_infectA <= 1){p=0}
#'  if(t/t_infectA > 1){p=1}
#'  return(p)
#'}
#'
#'time_contact_A = function(t){sample(c(0,1,2),1,prob=c(0.2,0.4,0.4))}
#'
#'#Host B
#'t_incub_fct_B <- function(x){rnorm(x,mean = 5,sd=1)}
#'p_max_fct_B <- function(x){rbeta(x,shape1 = 5,shape2=2)}
#'
#'p_Exit_fct_B  <- function(t,current.in){
#'  if(current.in=="A"){return(0.1)}
#'  if(current.in=="B"){return(0.2)}
#'  if(current.in=="C"){return(1)}}
#'
#'pTrans_hostB <- function(t,p_max,t_incub){
#'  if(t <= t_incub){p=0}
#'  if(t >= t_incub){p=p_max}
#'  return(p)
#'}
#'
#'time_contact_B = function(t){round(rnorm(1, 3, 1), 0)}
#'
#'transition.matrix = matrix(c(0,0.2,0.4,0.5,0,0.6,0.5,0.8,0),
#'                           nrow = 3, ncol = 3,
#'                           dimnames=list(c("A","B","C"),c("A","B","C")))
#'
#'set.seed(6262)
#'test.nosoi <- nosoiSim(type="dual", popStructure="discrete",
#'                       length.sim=40,
#'                       max.infected.A=100,
#'                       max.infected.B=200,
#'                       init.individuals.A=1,
#'                       init.individuals.B=0,
#'                       init.structure.A="A",
#'                       init.structure.B=NA,
#'                       structure.matrix.A=transition.matrix,
#'                       structure.matrix.B=transition.matrix,
#'                       pExit.A = p_Exit_fctA,
#'                       param.pExit.A = list(t_infectA = t_infectA_fct),
#'                       pMove.A=p_Move_fctA,
#'                       param.pMove.A=NA,
#'                       timeDep.pMove.A=FALSE,
#'                       diff.pMove.A=FALSE,
#'                       timeDep.pExit.A=FALSE,
#'                       nContact.A = time_contact_A,
#'                       param.nContact.A = NA,
#'                       timeDep.nContact.A=FALSE,
#'                       pTrans.A = pTrans_hostA,
#'                       param.pTrans.A = list(t_infectA=t_infectA_fct),
#'                       timeDep.pTrans.A=FALSE,
#'                       diff.pExit.A=FALSE,
#'                       diff.nContact.A=FALSE,
#'                       diff.pTrans.A=FALSE,
#'                       hostCount.pExit.A=FALSE,
#'                       hostCount.pMove.A=FALSE,
#'                       hostCount.nContact.A=FALSE,
#'                       hostCount.pTrans.A=FALSE,
#'                       pExit.B = p_Exit_fct_B,
#'                       param.pExit.B = NA,
#'                       pMove.B = function(t){return(0.1)},
#'                       param.pMove.B=NA,
#'                       timeDep.pMove.B=FALSE,
#'                       diff.pMove.B=FALSE,
#'                       timeDep.pExit.B=FALSE,
#'                       nContact.B=time_contact_B,
#'                       param.nContact.B=NA,
#'                       timeDep.nContact.B=FALSE,
#'                       pTrans.B=pTrans_hostB,
#'                       param.pTrans.B=list(p_max = p_max_fct_B,
#'                                          t_incub = t_incub_fct_B),
#'                       timeDep.pTrans.B=FALSE,
#'                       diff.pExit.B=TRUE,
#'                       diff.nContact.B=FALSE,
#'                       diff.pTrans.B=FALSE,
#'                       hostCount.pExit.B=FALSE,
#'                       hostCount.pMove.B=FALSE,
#'                       hostCount.nContact.B=FALSE,
#'                       hostCount.pTrans.B=FALSE,
#'                       structure.function.A = NULL,
#'                       structure.function.B = NULL
#')
#'}
#'
#' @export
dualDiscrete <- function(length.sim,
                         max.infected.A,
                         max.infected.B,
                         init.individuals.A,
                         init.individuals.B,
                         pExit.A, param.pExit.A, timeDep.pExit.A=FALSE,
                         pExit.B, param.pExit.B, timeDep.pExit.B=FALSE,
                         nContact.A, param.nContact.A, timeDep.nContact.A=FALSE,
                         nContact.B, param.nContact.B, timeDep.nContact.B=FALSE,
                         pTrans.A, param.pTrans.A, timeDep.pTrans.A=FALSE,
                         pTrans.B, param.pTrans.B, timeDep.pTrans.B=FALSE,
                         prefix.host.A = "A",
                         prefix.host.B = "B",
                         print.progress=TRUE,
                         print.step=10,
                         initial.population.A = 10000,
                         initial.population.B = 10000,
                         birth.rate.A = 0.5,
                         birth.rate.B = 0.5,
                         death.rate.A = 0.5,
                         death.rate.B = 0.5) {

  # Sanity check (you can extend this as needed)
  if(length.sim <= 0) stop("length.sim must be positive")
  if(init.individuals.A < 0 || init.individuals.B < 0) stop("initial individuals cannot be negative")
  if(init.individuals.A == 0 && init.individuals.B == 0) stop("At least one initial infected individual must be specified")


  # Parse functions for population A
  nContactParsed.A <- parseFunction(nContact.A, param.nContact.A, as.character(quote(nContact.A)), timeDep=timeDep.nContact.A)
  pTransParsed.A <- parseFunction(pTrans.A, param.pTrans.A, as.character(quote(pTrans.A)), timeDep=timeDep.pTrans.A)
  pExitParsed.A <- parseFunction(pExit.A, param.pExit.A, as.character(quote(pExit.A)), timeDep=timeDep.pExit.A)

  # Parse functions for population B
  nContactParsed.B <- parseFunction(nContact.B, param.nContact.B, as.character(quote(nContact.B)), timeDep=timeDep.nContact.B)
  pTransParsed.B <- parseFunction(pTrans.B, param.pTrans.B, as.character(quote(pTrans.B)), timeDep=timeDep.pTrans.B)
  pExitParsed.B <- parseFunction(pExit.B, param.pExit.B, as.character(quote(pExit.B)), timeDep=timeDep.pExit.B)

  # ParamHost constructors for A and B
  ParamHost.A <- paramConstructor(param.pExit.A, param.pMove=NA, param.nContact.A, param.pTrans.A, param.sdMove=NA)
  ParamHost.B <- paramConstructor(param.pExit.B, param.pMove=NA, param.nContact.B, param.pTrans.B, param.sdMove=NA)

  # Initialization message
  message("Starting the simulation\nInitializing ...", appendLF = FALSE)

  # Initialize simulation containers for A and B
  res <- nosoiSimConstructor(total.time = 1,
                             type = "dual",
                             pop.A = nosoiSimOneConstructor(
                               N.infected = init.individuals.A,
                               table.hosts = iniTable(init.individuals.A, NA, prefix.host.A, ParamHost.A),
                               table.state = NA,
                               prefix.host = prefix.host.A,
                               popStructure = "none"),
                             pop.B = nosoiSimOneConstructor(
                               N.infected = init.individuals.B,
                               table.hosts = iniTable(init.individuals.B, NA, prefix.host.B, ParamHost.B),
                               table.state = NA,
                               prefix.host = prefix.host.B,
                               popStructure = "none"))

  # Initialize PopModel vectors for A and B
  PopModel.A <- numeric(length.sim + 1)
  PopModel.B <- numeric(length.sim + 1)
  PopModel.A[1] <- initial.population.A
  PopModel.B[1] <- initial.population.B

  message("Starting the simulation\nInitializing ... running ...")

  for (pres.time in 1:length.sim) {

    # --- Population dynamics for A ---
    births.A <- rpois(1, birth.rate.A * PopModel.A[pres.time])
    deaths.A <- rbinom(1, PopModel.A[pres.time], death.rate.A)

    # Hosts exiting due to epidemic in A
    exiting.A <- getExitingMoving(res$host.info.A, pres.time, pExitParsed.A)
    if (!is.logical(exiting.A)) {
      exiting.A <- seq_len(nrow(res$host.info.A$table.hosts)) %in% exiting.A
    }
    res$host.info.A$table.hosts[exiting.A, `:=` (out.time = pres.time, active = FALSE)]
    epidemic_deaths.A <- sum(exiting.A)

    # Update population size for A
    PopModel.A[pres.time + 1] <- max(0, PopModel.A[pres.time] + births.A - deaths.A - epidemic_deaths.A)
    PopModel.A[pres.time + 1] <- max(0, PopModel.A[pres.time + 1])

    # --- Population dynamics for B ---
    births.B <- rpois(1, birth.rate.B * PopModel.B[pres.time])
    deaths.B <- rbinom(1, PopModel.B[pres.time], death.rate.B)

    # Hosts exiting due to epidemic in B
    exiting.B <- getExitingMoving(res$host.info.B, pres.time, pExitParsed.B)
    if (!is.logical(exiting.B)) {
      exiting.B <- seq_len(nrow(res$host.info.B$table.hosts)) %in% exiting.B
    }
    res$host.info.B$table.hosts[exiting.B, `:=` (out.time = pres.time, active = FALSE)]
    epidemic_deaths.B <- sum(exiting.B)

    # Update population size for B
    PopModel.B[pres.time + 1] <- max(0, PopModel.B[pres.time] + births.B - deaths.B - epidemic_deaths.B)
    PopModel.B[pres.time + 1] <- max(0, PopModel.B[pres.time + 1])

    # Stop if no active hosts in either population
    if (!any(res$host.info.A$table.hosts[["active"]]) && !any(res$host.info.B$table.hosts[["active"]])) {
      message("No active hosts left in either population. Stopping simulation.")
      break
    }

    # --- Transmission step ---
    # For population A (contacts and transmission)
    df.meetTransmit.A <- meetTransmit(res$host.info.A, pres.time, positions = NULL, nContactParsed.A, pTransParsed.A)
    res$host.info.A <- writeInfected(df.meetTransmit.A, res$host.info.A, pres.time, ParamHost.A)

    # For population B (contacts and transmission)
    df.meetTransmit.B <- meetTransmit(res$host.info.B, pres.time, positions = NULL, nContactParsed.B, pTransParsed.B)
    res$host.info.B <- writeInfected(df.meetTransmit.B, res$host.info.B, pres.time, ParamHost.B)

    # Progress printing
    if (print.progress && (pres.time %% print.step == 0)) {
      progressMessage(Host.count.A = res$host.info.A$N.infected,
                      Host.count.B = res$host.info.B$N.infected,
                      pres.time = pres.time,
                      print.step = print.step,
                      length.sim = length.sim,
                      max.infected.A = max.infected.A,
                      max.infected.B = max.infected.B)
    }

    # Stop if max infected exceeded in either population
    if (res$host.info.A$N.infected > max.infected.A || res$host.info.B$N.infected > max.infected.B) {
      message("Max infected exceeded in one population. Stopping simulation.")
      break
    }
  }

  # End message
  message("done.")
  message(sprintf(
    "The simulation has run for %d units of time and a total of %d hosts have been infected.",
    pres.time,
    res$host.info.A$N.infected + res$host.info.B$N.infected
  ))

  res$total.time <- pres.time
  res$pop_model.A <- PopModel.A[1:(pres.time + 1)]
  res$pop_model.B <- PopModel.B[1:(pres.time + 1)]

  return(res)
}
