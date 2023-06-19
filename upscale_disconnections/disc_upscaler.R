source("load_tool_environment.R")

DisconnectionUpscaler <- R6Class("DisconnectionUpscaler",
  public = list(
    predictor_list = NULL,
    circuit_summary = NULL,
    circuits_to_summarise = NULL,
    manufacturer_install_data = NULL,
    manufacturer_capacities = NULL,
    kw_loss_estimate = NULL,
    initialize = function(circuit_summary = NA, manufacturer_install_data = NA) {
      self$circuit_summary <- circuit_summary
      self$manufacturer_install_data <- manufacturer_install_data
    },
    set_predictors = function(predictor_list) {
      # validate inputs
      self$predictor_list <- predictor_list
    },
    assess_predictors = function() {
      # Perform an assessment to determine which predictors to recommend
    },
    clean_data = function() {
      # Clean input data/ filter as required for upscaling
      # May need to be separate for circuit data and cer data and also for each upscaling method
      # Could be run at start of upscale_x_method()
      # Will need to consider time taken
    },
    upscale_glm_method = function() {
      # Run upscaling method
      # Return kw loss etc
    },
    upscale_tranche_method = function() {
      # Run upscaling method
      # Return kw loss etc
    },
    run_bootstrap_CI = function(num_repetitions) {
      # Run bootstrap method
      # Return confidence interval
    },
    make_plots = function() {
      # Create plots used to evaluatre the predictors
    }
  )
)