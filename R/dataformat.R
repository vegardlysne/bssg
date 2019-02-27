#' Normalizes a variable to specified experimental period
#'
#' Subtracting mean of the specified period from every value in the variable that's being transformed, i.e.
#' mean of the baseline period
#'
#' @param .data a tibble
#' @param ID_var the ID variable, for grouping
#' @param variable name of the variable that is going to be transformed
#' @param phase_var name of the variable containing experimental phases, defaults to Experiment_period
#' @param phase_value name of the value of phase_var which you normalizes towards, defaults to Baseline
#' @param add Optional argument, a number to add to every value if you want the mean to be something
#' other than 0. Defaults to 0.
#'
#'
#' @return a tibble, where one variable is transformed
#' @export
#'
#' @examples
#' Normalizing variable bodytemperature to the Baseline period,
#' contained in variable Experiment_period in data set d
#'
#' d %>%
#'   format_normalize(variable = bodytemperature,
#'                    ID_var = Id,
#'                    phase_var = Experiment_period,
#'                    phase_value = Baseline,
#'                    add = 0)
#'
format_normalize <- function(.data,
                             ID_var,
                             variable,
                             phase_var = Experiment_period,
                             phase_value = Baseline,
                             add = 0){

  ID_var <- rlang::enquo(ID_var)
  variable <- rlang::enquo(variable)
  phase_var <- rlang::enquo(phase_var)
  phase_value <- rlang::enquo(phase_value)

  .data %>%
    dplyr::group_by(!!ID_var) %>%
    tidyr::nest() %>%
    dplyr::mutate(periodmean = purrr::map_dbl(data,
                                              . %>%
                                                dplyr::filter(!!phase_var == rlang::quo_text(phase_value)) %>%
                                                dplyr::pull(!!variable) %>%
                                                mean(., na.rm = T))) %>%
    tidyr::unnest() %>%
    dplyr::mutate(!!variable := !!variable - periodmean)

}
