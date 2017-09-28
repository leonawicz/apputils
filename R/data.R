.check_marginalize <- function(data, margin, drop=NULL){
  m <- margin
  if(!is.null(m) && !"" %in% m){
    m <- sort(m)
    m.lev <- purrr::map(m, ~levels(data[[.x]]))
    if(!is.null(drop)) m.lev <- purrr::map(m.lev, ~.x[!.x %in% drop])
    m <- m[which(purrr::map_lgl(m.lev, ~length(.x) > 1))]
    if(!length(m)) m <- NULL
  }
  m
}

#' Compile a specialized data frame based on the \code{rvtable} package
#'
#' Compile a specialized data frame based on the \code{rvtable} package using distribution data frames of SNAP downscaled climate data.
#'
#' This is a specialized function suited to preparing reactive data frames for an app where the upstream source data represents
#' an \code{rvtable}-class probability density data frame from the \code{rvtable} package.
#' Many such data frames of SNAP data are available.
#'
#' This function assumes the presence of certain data frame columns: Val, Prob, Var, RCP, Model, and Year.
#' It will insert a Decade column. It will check to ensure a valid Var column, meaning a data frame can contain only
#' one unique variable in its Var ID column and it must currently be one of \code{"pr", "tas", "tasmin", "tasmax"}.
#' This is because the current implementation makes certain assumptions about the data based on presently existing realistic use cases.
#'
#' A powerful feature of this function, given an appropriate \code{rvtable} data frame, is the ability to marginalize over
#' categorical variables (and meaningfully discrete numeric variables such as year) using the \code{margin} argument.
#' The current implementation allows marginalizing over RCPs and/or climate models.
#'
#' Arguments such as \code{variable} and \code{year.range} can be determined internally with \code{data} directly,
#' but in the app context these variables are already determined in the session environment
#' and there is no need to repeat scans of large data frames columns with every call to \code{dist_data}.
#'
#' Note that during marginalizing operations, baseline historical data sets are not integrated with climate models when integrating models
#' and historical climate models years are not integrated with future projections when integrating RCPs.
#' All categorical variables are factors with explicit levels, not character.
#'
#' If \code{limit.sample=TRUE} (default), the final sample size is reduced by a factor proportional to the number of unique RCP-GCM pairs.
#' This helps prevent massive in-app samples when users select large amounts of data from many RCPs and models.
#' A minimum sample size per group is still maintained regardless of how much data is requested.
#' Detailed progress is provided for sampling from distributions and for calculating marginal distributions.
#'
#' @param data a data frame. It does not need to be an \code{rvtable}-class data frame in advance, but it must be coercible to one.
#' @param variable character, a valid random variable. See details for currently available options.
#' @param margin variable to marginalize over. Defaults to \code{NULL}.
#' @param seed numeric or \code{NULL} (default), set random seed for reproducible sampling in app.
#' @param metric \code{NULL} or logical. Output data in metric units, otherwise in US Standard. Input data in \code{data} is assumed metric.
#' If \code{NULL} (default), no conversion or climate variable-specific rounding is performed.
#' @param year_range full range of years in data set.
#' @param rcp_min_yr minimum year for RCP, e.g., for CMIP5 data this is 2006.
#' @param base_max_yr maximum year for baseline historical comparison data set
#' that sometimes accompanies GCM data (e.g., CRU observation-based data, version 4.0 is 2015)
#' @param all_models character, vector of climate model names in data set, to include baseline model if present.
#' @param baseline_model character, name of baseline model in data set, e.g., \code{"CRU 4.0"}.
#' @param composite character, name to use for composite climate models after marginalizing over models.
#' @param baseline_scenario character, defaults to \code{"Historical"}.
#' @param general_scenario character, defaults to \code{"Projected"}.
#' @param margin_drop levels of variables to exclude from marginalizing operations on those variables.
#' Defaults to the baseline scenario and baseline model.
#' @param density_size numeric, sample size for density estimations. Defaults to \code{200}.
#' @param margin_size numeric, sample size for marginalizing operations. Defaults to \code{100}.
#' @param sample_size numeric, sample size for density estimations. Defaults to \code{margin.size}.
#' @param limit_sample logical, see details.
#' @param progress logical, include progress bar in app.
#'
#' @return a specialized data frame
#' @export
#'
#' @examples
#' #not run
dist_data <- function(data, variable, margin=NULL, seed=NULL, metric=NULL, year_range, rcp_min_yr, base_max_yr,
                      all_models, baseline_model=NULL, composite="Composite GCM",
                      baseline_scenario="Historical", general_scenario="Projected",
                      margin_drop=c(baseline_scenario, baseline_model),
                      density_size=200, margin_size=100, sample_size=margin_size,
                      limit_sample=TRUE, progress=TRUE){
  valid_variables <- c("pr", "tas", "tasmin", "tasmax")
  required_vars <- c("Val", "Prob", "RCP", "Model", "Var", "Year")
  if(!variable %in% valid_variables) stop("Invalid variable.")
  if(!all(required_vars %in% names(data)))
    stop(paste0("data must contain columns: ", paste(required_vars, collapse=", "), "."))
  if(length(unique(data$Var)) > 1) stop("Data frame must contain only one 'Var' variable.")
  if(is.numeric(seed)) set.seed(seed)
  m <- .check_marginalize(data, margin, drop=margin_drop) # determine need for marginalization
  merge_vars <- !is.null(m) && !"" %in% m
  lev.rcps <- NULL
  base <- baseline_model %in% all_models
  if(year_range[1] < rcp_min_yr || (base && year_range[1] <= base_max_yr))
    lev.rcps <- baseline_scenario
  if(year_range[2] >= rcp_min_yr) lev.rcps <- c(lev.rcps, general_scenario)

  d.args <- if(variable=="pr") list(n=density_size, adjust=0.1, from=0) else list(n=density_size, adjust=0.1)
  s.args <- list(n=margin_size) # marginalize steps
  n.samples <- sample_size # final sampling
  if(merge_vars & base){
    # split CRU from GCMs
    lev.models <- if("Model" %in% m) c(baseline_model, composite) else all_models
    data.base <- dplyr::filter(data, Model==baseline_model) %>%
      dplyr::mutate(Model=factor(Model, levels=lev.models)) %>%
      split(.$Year) %>% purrr::map(~rvtable::rvtable(.x))
    data <- dplyr::filter(data, Model!=baseline_model)
  }
  if(!merge_vars){
    n.factor <- if(limit_sample) samplesize_factor(data, baseline_model) else 1
  } else if(!base){
    n.factor <- 1
  }
  data <- data %>% split(.$Year) %>% purrr::map(~rvtable::rvtable(.x))
  n_steps <- length(data)
  step <- 0
  if(progress){
    prog <- shiny::Progress$new()
    on.exit(prog$close())
  }
  if(merge_vars){
    # marginalize (excludes baseline model and scenario, e.g. CRU and historical GCM, data)
    msg <- "Integrating variables..."
    if(progress){
      prog$set(0, message=msg, detail=NULL)
      n_steps_marginal <- if(length(m)) n_steps*length(m) else n_steps
    }
    for(i in seq_along(m)){
      for(j in seq_along(data)){
        if(progress){
          step <- step + 1
          detail <- paste0("Marginalizing over ", m[i], "s: ", round(100*step/n_steps_marginal), "%")
          if(step %% 5 == 0 || step==n_steps_marginal) prog$set(step/n_steps_marginal, msg, detail)
        }
        data[[j]] <- rvtable::marginalize(data[[j]], m[i], density.args=d.args, sample.args=s.args)
      }
    }
  }
  if(merge_vars & base){
    # update factor levels (with baseline model data)
    data.base <- dplyr::bind_rows(data.base)
    data <- dplyr::bind_rows(data) %>% dplyr::ungroup()
    if("Model" %in% m) data <- dplyr::mutate(data, Model=factor(lev.models[-1], levels=lev.models))
    if("RCP" %in% m){
      if(length(lev.rcps)==1) lev.rcps <- rep(lev.rcps, 2) # historical always present
      data.base <- dplyr::mutate(data.base, RCP=factor(as.character(RCP), levels=unique(lev.rcps)))
      data <- dplyr::mutate(data, RCP=factor(ifelse(Year < rcp_min_yr, lev.rcps[1], lev.rcps[2]), unique(lev.rcps)))
    }
    n.factor <- if(limit_sample) samplesize_factor(data, baseline_model) else 1
    data <- dplyr::bind_rows(data.base, data) %>% split(.$Year) %>% purrr::map(~rvtable::rvtable(.x))
  }
  if(progress){
    step <- 0
    msg <- "Sampling distributions..."
    prog$set(0, message=msg, detail=NULL)
  } # adjust sample based on number of RCPs and GCMs
  for(j in seq_along(data)){
    # sample distributions
    if(progress){
      step <- step + 1
      if(step %% 5 == 0 || step==n_steps)
        prog$set(step/n_steps, message=msg, detail=paste0(round(100*step/n_steps), "%"))
    }
    data[[j]] <- rvtable::sample_rvtable(data[[j]], n=max(10, round(n.samples/n.factor)))
  }
  data <- dplyr::bind_rows(data) %>% dplyr::ungroup()
  if(merge_vars & !base){
    # update factor levels (no baseline model data)
    if("Model" %in% m) data <- dplyr::mutate(data, Model=factor(composite, levels=composite))
    if("RCP" %in% m){
      if(length(lev.rcps)==1) lev.rcps <- rep(lev.rcps, 2) # projected always present
      data <- dplyr::mutate(data, RCP=factor(ifelse(Year < rcp_min_yr, lev.rcps[1], lev.rcps[2]), unique(lev.rcps)))
    }
  }
  if(nrow(data) > 0 & variable %in% valid_variables){
    # unit conversion
    if(is.logical(metric)){
      if(metric){
        data <- dplyr::mutate(data, Val=ifelse(Var=="pr", round(Val), round(Val, 1)))
      } else {
        data <- dplyr::mutate(data, Val=ifelse(Var=="pr", round(Val/25.4, 3), round((9/5)*Val + 32, 1)))
      }
    }
    if(variable=="pr") data <- dplyr::mutate(data, Val=ifelse(Val < 0, 0, Val))
    data <- dplyr::mutate(data, Decade=factor( # Add decade factor column
      paste0(Year - Year %% 10, "s"), levels=paste0(unique(Year - Year %% 10), "s")))
  }
  data
}

#' Convert units and round data
#'
#' Convert units and round climate data when the \code{Var} column in \code{data} is one of
#' \code{"pr"}, \code{"tas"}, \code{"tasmin"} or \code{"tasmax"}.
#' This function is for a specialized context.
#'
#' @param data a data frame with the appropriate columns and data.
#' @param metric whether the values in the \code{Val} column are to be output in metric or US standard.
#' Input data in column \code{Val} of \code{data} is assumed metric.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' #not run
clim_convert_round <- function(data, metric){
  if(metric) return(dplyr::mutate(data, Val=ifelse(Var=="pr", round(Val), round(Val, 1))))
  dplyr::mutate(data, Val=ifelse(Var=="pr", round(Val/25.4, 3), round((9/5)*Val + 32, 1)))
}

#' Filter data frame based on plot brush
#'
#' This function filters data frame rows to a subset corresponding to observations included in the brushed region of a plot.
#'
#' This implementation is for brushing restricted to the x-axis and to two types of data frames.
#' The annual type handles continuous x-axis data and expects a data frame with a\code{Year} column that is x-variable.
#' The decadal type handles categorical x-axis data and expects a \code{Decade} column that is the x-variable.
#' \code{x} and \code{brush} are commonly reactive values in \code{server.R}, storing the observed click and brush coordinates in \code{input},
#' rather than the \code{input} objects directly.
#'
#' @param data data frame.
#' @param x x coordinates. See details.
#' @param brush brushed coordinates. See details.
#' @param type character, \code{"annual"} or \code{"decadal"}.
#'
#' @return a data frame.
#' @export
#'
#' @examples
#' #not run
brushed_data <- function(data, x, brush, type="annual"){
  if(!type %in% c("annual", "decadal")) stop("type must be 'annual' or 'decadal'.")
  if(is.null(brush) & is.null(x)) return(data)
  if(type=="annual"){
    if(is.null(brush) & !is.null(x)){
      y <- dplyr::filter(data, Year >= x[1] & Year <= x[2])
    } else y <- shiny::brushedPoints(data, brush)
    if(nrow(y)==0) y <- data
  } else {
    dec <- data$Decade
    r <- if(is.null(brush) & !is.null(x)) c(x[1], x[2]) else c(brush$xmin, brush$xmax)
    intlev <- round(r[1]):round(r[2])
    intlev <- intlev[!intlev %in% c(0, nlevels(dec) + 1)]
    y <- dplyr::filter(data, as.integer(dec) %in% intlev)
  }
  y
}

#' Color re-indexing
#'
#' Re-index colors for grouped data, as displayed in plots and other widgets, when groups may be missing.
#'
#' This is typically used when group levels are in a data set but user selection (mouse brush action) in an interactive plot may not result
#' in a data subset that includes data from all levels of the grouping variable.
#'
#' @param x a data frame.
#' @param clrby column name of the grouping variable to be used for coloring.
#'
#' @return a re-indexed vector of colors from \code{tolpal}.
#' @export
#'
#' @examples
#' # not run
color_indexer <- function(x, clrby){
  x <- split(x, x[[clrby]])
  clrs <- as.list(tolpal(length(x)))
  idx <- which(purrr::map_lgl(x, ~nrow(.x) > 0))
  clrs <- clrs[idx]
  clrs
}
