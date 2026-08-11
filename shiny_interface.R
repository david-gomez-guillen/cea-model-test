library(ggplot2)

source('model.R')

get.overview <- function() {
  # Markdown shown in the Overview tab (see get.overview.markdown() in shiny-cea).
  # The text lives in overview.md. Everything described there is derived from
  # simulate() in model.R and from the rest of this interface, so it must be kept
  # in sync with them.
  # This function is called long after this file is sourced, when the working
  # directory is no longer the model directory, so overview.md is resolved against
  # the MODEL_PATH set by the app (falling back to the working directory when the
  # interface is used standalone).
  model.dir <- Sys.getenv('MODEL_PATH', unset='.')
  return(paste(readLines(file.path(model.dir, 'overview.md')), collapse='\n'))
}

get.strategies <- function() {
  # Hardcoded strategies for the model. In a real application, these could be loaded from a file or database.
  return(list(
    list(name='no_intervention', display.name='No intervention'),
    list(name='screening', display.name='Screening'),
    list(name='treatment', display.name='Treatment'),
    list(name='experimental_treatment', display.name='Experimental Treatment')
  ))
}

get.parameters <- function() {
  # Hardcoded parameters for the model. In a real application, these could be loaded from a file or database.
  return(list(
    list(
      name='p.healthy.cancer',
      display.name='Annual probability of developing cancer while healthy',
      base.value=0.075,
      class='General'
    ),
    list(
      name='p.healthy.death',
      display.name='Annual probability of death while healthy',
      base.value=0.00001,
      class='General'
    ),
    list(
      name='p.cancer.death',
      display.name='Annual probability of death while having cancer (except the experimental treatment)',
      base.value=0.0001,
      class='General'
    ),
    list(
      name='p.screening.effective',
      display.name='Proportion of cancer cases that are prevented by screening',
      base.value=0.05,
      class='Screening'
    ),
    list(
      name='p.treatment.effective',
      display.name='Annual probability of the regular treatment curing cancer (back to healthy)',
      base.value=0.05,
      class='Treatment'
    ),
    list(
      name='p.experimental.treatment.effective',
      display.name='Annual probability of the experimental treatment curing cancer (back to healthy)',
      base.value=0.1,
      class='Treatment'
    ),
    list(
      name='p.experimental.cancer.death',
      display.name='Annual probability of death while having cancer under the experimental treatment',
      base.value=0.005,
      class='Treatment'
    ),
    list(
      name='cost.screening',
      display.name='Annual cost per healthy person under screening',
      base.value=10000,
      class='Screening'
    ),
    list(
      name='cost.cancer.treatment',
      display.name='Annual cost per person with cancer under regular treatment',
      base.value=50000,
      class='Treatment'
    ),
    list(
      name='cost.experimental.cancer.treatment',
      display.name='Annual cost per person with cancer under the experimental treatment',
      base.value=75000,
      class='Treatment'
    ),
    list(
      name='utility.cancer',
      display.name='Utility of a year spent with cancer',
      base.value=0.6,
      class='General'
    ),
    list(
      name='discount',
      display.name='Discount rate',
      base.value=0.03,
      class='General'
    )
  ))
}

get.strata <- function() {
  # Hardcoded strata for the model. In a real application, these could be loaded from a file or database.
  return(c('30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74'))
}

run.simulation <- function(strategies, pars) {
  # The pars vector should be transformed to the format expected by the simulate function. 
  # This is a simple mapping based on the parameter names.
  results <- simulate(strategies,
                     p.healthy.cancer=pars[['p.healthy.cancer']],
                     p.healthy.death=pars[['p.healthy.death']],
                     p.cancer.death=pars[['p.cancer.death']],
                     p.screening.effective=pars[['p.screening.effective']],
                     p.treatment.effective=pars[['p.treatment.effective']],
                     p.experimental.treatment.effective=pars[['p.experimental.treatment.effective']],
                     p.experimental.cancer.death=pars[['p.experimental.cancer.death']],
                     cost.screening=pars[['cost.screening']],
                     cost.cancer.treatment=pars[['cost.cancer.treatment']],
                     cost.experimental.cancer.treatment=pars[['cost.experimental.cancer.treatment']],
                     utility.cancer=pars[['utility.cancer']],
                     discount=pars[['discount']])
  return(results)
}

get.calibration.schemes <- function() {
  return(list(
    standard=list(
      description='Example calibration',
      parameters='p.healthy.cancer',
      target=list(
        # Each target is a named list assigning a value to a specific stratum.
        # Strata not listed here are not calibrated against (e.g. burn-in strata).
        `Cancer incidence`=list(
          `30-34`=.01,
          `35-39`=.05,
          `40-44`=.08,
          `45-49`=.1,
          `50-54`=.11,
          `55-59`=.12,
          `60-64`=.13,
          `65-69`=.135,
          `70-74`=.14
        )
      ),
      strata=get.strata(),
      initial_guess=rep(.075, 9),
      error_function=calibration.error,
      latent_space_training_set=generate.training.dataset,
      latent_space_training_set_size=500,
      latent_space_training_epochs=50,
      latent_space_latent_dim=7,
      other.plots=NULL
    )))
}

calibration.error <- function(pars, target) {
  calibration.strategy <- 'no_intervention'
  # The target is a named list with one entry per stratum, so it is flattened into
  # a named numeric vector to match the simulated values by stratum name.
  target.inc <- unlist(target$`Cancer incidence`)
  result <- tryCatch({
    results <- run.simulation(calibration.strategy, pars)
    cancer.incidence <- results$incidence[[calibration.strategy]]
    names(cancer.incidence) <- get.strata()
    # Only the strata present in the target contribute to the error.
    error <- sum((cancer.incidence[names(target.inc)]-target.inc)^2)
    result <- list(
      error=error,
      output=list(cancer.incidence=cancer.incidence)
    )
    result
  }, error=function(e) {
    error <- Inf
    cancer.incidence <- rep(NA, length(get.strata()))
    names(cancer.incidence) <- get.strata()
    result <- list(
      error=error,
      output=list(cancer.incidence=cancer.incidence)
    )
    result
  })
  return(result)
}

generate.training.dataset <- function(initial_guess, n, ...) {
  f.pars <- list(...)
  variation <- f.pars$variation

  n_params <- length(initial_guess)

  dataset <- matrix(NA, nrow=n, ncol=n_params)

  for(i in 1:n) {
	  factors <- runif(n_params, min=1-variation, max=1+variation)
	  dataset[i,] <- pmin(1, initial_guess * factors)
  }

  dataset <- dataset[sample(nrow(dataset)),]

  return(dataset)
}

# ### TEST
#
# strategies <- get.strategies()
# param.info <- get.parameters()
# param.values <- sapply(param.info, function(p) p$base.value)
# names(param.values) <- sapply(param.info, function(p) p$name)
#
# results <- run.simulation(strategies, param.values)
# print(results$summary)
#
# print(
#   ggplot(results$summary, aes(x=C, y=E, color=strategy)) +
#     geom_point(size=3) +
#     coord_cartesian(xlim=c(0, max(results$summary$C)), ylim=c(0, 20)) +
#     theme_minimal()
# )

