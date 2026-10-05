library(ggplot2)

source('model.R')

get.overview <- function() {
  # Markdown shown in the Overview tab (see get.overview.markdown() in shiny-cea).
  # The text lives in overview.md. Everything described there is derived from
  # simulate() in model.R and from the rest of this interface, so it must be kept
  # in sync with them.
  return(paste(readLines('overview.md'), collapse='\n'))
}

get.strategies <- function() {
  # Hardcoded strategies for the model. In a real application, these could be loaded from a file or database.
  # The descriptions and attributes describe what each strategy changes in
  # simulate() in model.R, and must be kept in sync with it.
  return(list(
    list(
      name='no_intervention',
      display.name='No intervention',
      description='Natural history of the cancer, with no screening or treatment costs. The reference the other strategies are compared against.',
      attributes=list(intervention='None', population='Nobody', effect='None')
    ),
    list(
      name='screening',
      display.name='Screening',
      description='Every healthy person is screened every year, at cost.screening each, which prevents a share p.screening.effective of the new cancer cases.',
      attributes=list(intervention='Screening', population='Healthy', effect='Prevents cancer onset')
    ),
    list(
      name='treatment',
      display.name='Treatment',
      description='Every person with cancer is treated every year, at cost.cancer.treatment each, which adds p.treatment.effective to the annual probability of recovery.',
      attributes=list(intervention='Treatment', population='Cancer', effect='Increases recovery')
    ),
    list(
      name='experimental_treatment',
      display.name='Experimental Treatment',
      description='Every person with cancer is treated every year with a more effective but more toxic drug, at cost.experimental.cancer.treatment each, which adds p.experimental.treatment.effective to the annual probability of recovery and replaces p.cancer.death with p.experimental.cancer.death.',
      attributes=list(intervention='Experimental treatment', population='Cancer', effect='Increases recovery, changes death with cancer')
    )
  ))
}

get.strategy.attributes <- function() {
  # What describes the strategies, shown as columns of the Strategies tab. Only
  # the intervention is drawn on the base case plot, as the shape of the point.
  return(list(
    intervention=list(
      label='Intervention',
      plot='shape',
      values=c(None='x', Screening='circle', Treatment='square', `Experimental treatment`='diamond')
    ),
    population='Applied to',
    effect='Effect'
  ))
}

# Constraints: each returns TRUE when the value is acceptable, or the message
# shown for it when it is not.
prob.death.healthy.below.death.cancer <- function(par.name, params) {
  if (params[['p.healthy.death']] >= params[['p.cancer.death']]) 'Probability of death while healthy must be below probability of death while having cancer' else TRUE
}


get.parameters <- function() {
  # Hardcoded parameters for the model. In a real application, these could be loaded from a file or database.
  return(list(
    list(
      name='p.healthy.cancer',
      display.name='Annual probability of developing cancer while healthy',
      base.value=0.13,
      distribution='beta',
      class='General',
      min.value=0,
      max.value=1
    ),
    list(
      name='p.healthy.death',
      display.name='Annual probability of death while healthy',
      base.value=0.00001,
      distribution='beta',
      class='General',
      min.value=0,
      max.value=1,
      constraints=list(prob.death.healthy.below.death.cancer)
    ),
    list(
      name='p.cancer.death',
      display.name='Annual probability of death while having cancer (except the experimental treatment)',
      base.value=0.0001,
      distribution='beta',
      class='General',
      min.value=0,
      max.value=1,
      constraints=list(prob.death.healthy.below.death.cancer)
    ),
    list(
      name='p.cancer.recovery',
      display.name='Annual probability of cancer resolving on its own (back to healthy)',
      base.value=0.3,
      distribution='beta',
      class='General',
      min.value=0,
      max.value=1
    ),
    list(
      name='p.screening.effective',
      display.name='Proportion of cancer cases that are prevented by screening',
      base.value=0.05,
      distribution='beta',
      class='Screening',
      min.value=0,
      max.value=1
    ),
    list(
      name='p.treatment.effective',
      display.name='Annual probability of the regular treatment curing cancer (back to healthy)',
      base.value=0.03,
      distribution='beta',
      class='Treatment',
      min.value=0,
      max.value=1
    ),
    list(
      name='p.experimental.treatment.effective',
      display.name='Annual probability of the experimental treatment curing cancer (back to healthy)',
      base.value=0.1,
      distribution='beta',
      class='Treatment',
      min.value=0,
      max.value=1
    ),
    list(
      name='p.experimental.cancer.death',
      display.name='Annual probability of death while having cancer under the experimental treatment',
      base.value=0.005,
      distribution='beta',
      class='Treatment',
      min.value=0,
      max.value=1
    ),
    list(
      name='cost.screening',
      display.name='Annual cost per healthy person under screening',
      base.value=15000,
      distribution='gamma',
      class='Screening',
      min.value=0
    ),
    list(
      name='cost.cancer.treatment',
      display.name='Annual cost per person with cancer under regular treatment',
      base.value=200000,
      distribution='gamma',
      class='Treatment',
      min.value=0
    ),
    list(
      name='cost.experimental.cancer.treatment',
      display.name='Annual cost per person with cancer under the experimental treatment',
      base.value=300000,
      distribution='gamma',
      class='Treatment',
      min.value=0
    ),
    list(
      name='utility.cancer',
      display.name='Utility of a year spent with cancer',
      base.value=0.6,
      distribution='beta',
      class='General',
      min.value=0,
      max.value=1
    ),
    list(
      name='discount',
      display.name='Discount rate',
      base.value=0.03,
      class='General',
      min.value=0,
      max.value=1
    )
  ))
}

get.strata <- function() {
  # Hardcoded strata for the model. In a real application, these could be loaded from a file or database.
  return(c('30-34', '35-39', '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74'))
}

get.model.states <- function() {
  # State diagram shown in the Overview tab, next to the text of overview.md. It
  # describes the transition matrix of simulate() in model.R and must be kept in
  # sync with it.
  return(list(
    title='Model states',
    description='The three states of the cohort and the annual transitions between them. Hover a state or a transition for what drives it.',
    nodes=data.frame(
      id=c('healthy', 'cancer', 'dead'),
      label=c('Healthy', 'Cancer', 'Dead'),
      description=c(
        'Alive and cancer-free. The whole cohort starts here.',
        'Alive with cancer.',
        'Absorbing state, reached from both Healthy and Cancer.'
      )
    ),
    edges=data.frame(
      from=c('healthy', 'cancer', 'healthy', 'cancer'),
      to=c('cancer', 'healthy', 'dead', 'dead'),
      label=c('Onset', 'Recovery', 'Death', 'Death'),
      description=c(
        'p.healthy.cancer, reduced by p.screening.effective under screening.',
        'p.cancer.recovery, plus p.treatment.effective under treatment or p.experimental.treatment.effective under the experimental treatment.',
        'p.healthy.death',
        'p.cancer.death, replaced by p.experimental.cancer.death under the experimental treatment.'
      )
    )
  ))
}

get.code.sample <- function() {
  # Code shown in the Code panel of the Overview tab, one tab per entry, for
  # whoever wants to see how the model works. The whole of model.R is written to
  # be read, so it is shown as it is.
  return(list(
    `model.R`=paste(readLines('model.R'), collapse='\n')
  ))
}

run.simulation <- function(strategies, pars) {
  # The pars vector should be transformed to the format expected by the simulate function. 
  # This is a simple mapping based on the parameter names.
  results <- simulate(strategies,
                     p.healthy.cancer=pars[['p.healthy.cancer']],
                     p.healthy.death=pars[['p.healthy.death']],
                     p.cancer.death=pars[['p.cancer.death']],
                     p.cancer.recovery=pars[['p.cancer.recovery']],
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
      initial_guess=rep(.13, 9),
      error_function=calibration.error,
      latent_space_training_set=generate.training.dataset,
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

