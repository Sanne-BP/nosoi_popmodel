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
dualDiscrete <- function(length.sim = 10,
                         max.infected.A = 100,
                         max.infected.B = 100,
                         init.individuals.A = 1,
                         init.individuals.B = 1,
                         init.structure.A = NA,
                         init.structure.B = NA,
                         structure.matrix.A,
                         structure.matrix.B,
                         pExit.A,
                         param.pExit.A = NA,
                         pMove.A,
                         param.pMove.A = NA,
                         timeDep.pMove.A = FALSE,
                         diff.pMove.A = FALSE,
                         hostCount.pMove.A = FALSE,
                         diff.pExit.A = FALSE,
                         hostCount.pExit.A = FALSE,
                         nContact.A,
                         param.nContact.A = NA,
                         timeDep.nContact.A = FALSE,
                         diff.nContact.A = FALSE,
                         hostCount.nContact.A = FALSE,
                         pTrans.A,
                         param.pTrans.A = NA,
                         timeDep.pTrans.A = FALSE,
                         diff.pTrans.A = FALSE,
                         hostCount.pTrans.A = FALSE,
                         pExit.B,
                         param.pExit.B = NA,
                         pMove.B,
                         param.pMove.B = NA,
                         timeDep.pMove.B = FALSE,
                         diff.pMove.B = FALSE,
                         hostCount.pMove.B = FALSE,
                         diff.pExit.B = FALSE,
                         hostCount.pExit.B = FALSE,
                         nContact.B,
                         param.nContact.B = NA,
                         timeDep.nContact.B = FALSE,
                         diff.nContact.B = FALSE,
                         hostCount.nContact.B = FALSE,
                         pTrans.B,
                         param.pTrans.B = NA,
                         timeDep.pTrans.B = FALSE,
                         diff.pTrans.B = FALSE,
                         hostCount.pTrans.B = FALSE,
                         structure.function.A = NULL,
                         structure.function.B = NULL,
                         # Simulation settings
                         print.progress = TRUE,
                         print.step = 10) {

  #### ---- Checks ---- ####

  if (!(is.matrix(structure.matrix.A) & is.matrix(structure.matrix.B))) {
    stop("Structure matrices must be of class 'matrix'")
  }
  if (!all(dim(structure.matrix.A) == dim(structure.matrix.B))) {
    stop("Structure matrices for A and B must have the same dimensions")
  }
  if (!all(rownames(structure.matrix.A) == colnames(structure.matrix.A))) {
    stop("Structure matrix A must have identical row and column names")
  }
  if (!all(rownames(structure.matrix.B) == colnames(structure.matrix.B))) {
    stop("Structure matrix B must have identical row and column names")
  }
  if (!all(rownames(structure.matrix.A) == rownames(structure.matrix.B))) {
    stop("Structure matrices A and B must have identical row names")
  }
  if (!all(rowSums(structure.matrix.A) == 1)) {
    stop("Each row of structure.matrix.A must sum to 1")
  }
  if (!all(rowSums(structure.matrix.B) == 1)) {
    stop("Each row of structure.matrix.B must sum to 1")
  }

  #### ---- Initialization ---- ####

  # Number of discrete states
  n_states <- nrow(structure.matrix.A)
  states <- rownames(structure.matrix.A)

  # Initialize population states per host type as named integer vectors
  infected.A <- integer(n_states)
  infected.B <- integer(n_states)
  names(infected.A) <- states
  names(infected.B) <- states

  infected.individuals.A <- vector("list", length.sim)
  infected.individuals.B <- vector("list", length.sim)

  infected.individuals.A[[1]] <- data.frame(
    ID = seq_len(init.individuals.A),
    infection.time = rep(1, init.individuals.A),
    state = if (!is.na(init.structure.A)) rep(init.structure.A, init.individuals.A) else character(0),
    exit.time = NA,
    stringsAsFactors = FALSE
  )
  infected.individuals.B[[1]] <- data.frame(
    ID = seq_len(init.individuals.B),
    infection.time = rep(1, init.individuals.B),
    state = if (!is.na(init.structure.B)) rep(init.structure.B, init.individuals.B) else character(0),
    exit.time = NA,
    stringsAsFactors = FALSE
  )

  for (t in 2:length.sim) {
    infected.individuals.A[[t]] <- data.frame(
      ID = integer(0),
      infection.time = integer(0),
      state = character(0),
      exit.time = numeric(0),
      stringsAsFactors = FALSE
    )
    infected.individuals.B[[t]] <- data.frame(
      ID = integer(0),
      infection.time = integer(0),
      state = character(0),
      exit.time = numeric(0),
      stringsAsFactors = FALSE
    )
  }

  total.infected.A <- init.individuals.A
  total.infected.B <- init.individuals.B

  continue.sim <- TRUE
  time <- 1

  #### ---- Progress messages like nosoi ---- ####

  if (print.progress) {
    cat("Starting the simulation\n")
    cat("Initializing ...\n")
  }

  if (print.progress) {
    cat(" running ...\n")
  }

  #### ---- Simulation loop ---- ####

  while (continue.sim & time <= length.sim) {

    if (print.progress && (time %% print.step == 0)) {
      cat(".")
      flush.console()
    }

    # --- SIMULATION LOGIC HERE ---
    # (You need to fill this with your actual update logic for pExit, pMove, nContact, pTrans)

    # For now, just an example increment in time, replace with your actual code:
    # Example: update infected individuals here

    # Stopping condition example:
    if (total.infected.A >= max.infected.A | total.infected.B >= max.infected.B) {
      continue.sim <- FALSE
    }

    time <- time + 1
  }

  if (print.progress) {
    cat("\n")
    cat("done.\n")
    cat(paste0("The simulation has run for ", time - 1,
               " units of time and a total of ",
               total.infected.A + total.infected.B,
               " hosts have been infected.\n"))
  }

  #### ---- Return results ---- ####

  return(list(
    infected.individuals.A = infected.individuals.A,
    infected.individuals.B = infected.individuals.B,
    total.infected.A = total.infected.A,
    total.infected.B = total.infected.B
  ))
}
