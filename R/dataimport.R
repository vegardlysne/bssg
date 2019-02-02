#' Load a single .txt data file from "data quest art" software
#'
#' Load a single .txt file with two columns, and creates column names.
#' File name should be "Variablename_groupname_ratID.txt", as these are used
#' to create column names, as well as group and id columns.
#'
#' @param path path to file
#' @param baselinedays number of days in baseline condition, defaults to 4
#' @param exposuredays number of days in exposure condition, defaults to 5
#' @param zeitgebertime number indicating start of light period, defaults to 7 (= 07:00)
#'
#' @return
#' @export
#'
#' @examples
load_single_file <- function(path,
                             baselinedays = 4,
                             exposuredays = 5,
                             zeitgebertime = 7){


  readr::read_delim(path,
                  delim = ",",
                  comment = "#",
                  col_names = c("Datetime",
                                stringr::str_extract(basename(path), "\\w+?(?=_)")),
                  col_types = "cd",
                  na = "nan",
                  trim_ws = T) %>%
  purrr::modify_at(1, lubridate::as_datetime) %>%
  dplyr::mutate(Id = stringr::str_extract(basename(path), "(?<=_)[:digit:]+(?=.txt)"),
                Group = stringr::str_extract(basename(path), "(?<=_)\\w+?(?=_)"),
                Zeitgebertime = Datetime - hours(zeitgebertime),
                Day = lubridate::day(Zeitgebertime) - lubridate::day(Zeitgebertime[1]) + 1,
                Experiment_period = dplyr::case_when(Day <= baselinedays ~ "Baseline",
                                                     Day <= baselinedays + exposuredays ~ "Exposure",
                                                     TRUE ~ "Recovery"),
                Zeitgebertime = Zeitgebertime-Zeitgebertime[1]-(Day-1)*86400,
                Syncdatetime = lubridate::ymd_hms("20190101000000")+Zeitgebertime)
}



#' Combine files from same rat into a tibble
#'
#' Takes a list of .txt files from the same rat and produces a tibble with all variables
#'
#' @param filepaths a list of files
#' @param baselinedays number of days in baseline condition, defaults to 4
#' @param exposuredays number of days in exposure condition, defaults to 5
#' @param zeitgebertime number indicating start of light period, defaults to 7 (= 07:00)
#'
#' @return
#' @export
#'
#' @examples
combine_files <- function(filepaths,
                          baselinedays,
                          exposuredays,
                          zeitgebertime = 7){

  d <- load_single_file(filepaths[1], baselinedays, exposuredays, zeitgebertime)

  for(i in 2:length(filepaths)){
    d <- dplyr::left_join(d, load_single_file(filepaths[i], baselinedays, exposuredays, zeitgebertime))
  }

  d %>%
    dplyr::select(Group,
                  Id,
                  Datetime,
                  Syncdatetime,
                  Experiment_period,
                  Day,
                  Zeitgebertime,
                  everything())
}
