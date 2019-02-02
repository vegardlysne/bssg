#' Load a single .txt data file from "data quest art" software
#'
#' Load a single .txt file with two columns, and creates column names.
#' File name should be "Variablename_groupname_animalID.txt", as these are used
#' to create column names, as well as group and id columns.
#'
#' @param path path to file
#' @param baseline number of days in baseline condition, defaults to 4
#' @param exposure number of days in exposure condition, defaults to 5
#' @param zeitgebertime number indicating start of light period, defaults to 7 (= 07:00)
#' @param maxbaseline number of maximum baseline days in experiment
#'
#' @return
#' @export
#'
#' @examples
load_single_file <- function(path,
                             baseline = 4,
                             exposure = 5,
                             zeitgebertime = 7,
                             maxbaseline = 4){

  baseline <- as.numeric(baseline)
  exposure <- as.numeric(exposure)
  zeitgebertime <- as.numeric(zeitgebertime)

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
                Experiment_period = dplyr::case_when(Day <= baseline ~ "Baseline",
                                                     Day <= baseline + exposure ~ "Exposure",
                                                     TRUE ~ "Recovery"),
                Zeitgebertime = Zeitgebertime-Zeitgebertime[1]-(Day-1)*86400,
                Syncdatetime = lubridate::ymd_hms("20190101000000")+Zeitgebertime + days(Day-1 + maxbaseline-baseline))
}


#' Combine files from same animal into a tibble
#'
#' Takes a list of .txt files from the same animal and produces a tibble with all variables
#'
#' @param filepaths a character vector of file names for the same animal
#' @param baseline number of days in baseline condition, defaults to 4
#' @param exposure number of days in exposure condition, defaults to 5
#' @param zeitgebertime number indicating start of light period, defaults to 7 (= 07:00)
#' @param maxbaseline number of maximum baseline days in experiment
#'
#' @return
#' @export
#'
#' @examples
combine_files <- function(filepaths,
                          baseline = 4,
                          exposure = 5,
                          zeitgebertime = 7,
                          maxbaseline = 4){

  d <- load_single_file(filepaths[1], baseline, exposure, zeitgebertime)

  for(i in 2:length(filepaths)){
    d <- dplyr::left_join(d, load_single_file(filepaths[i], baseline, exposure, zeitgebertime))
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


d <- bind_rows(combine_files(c("./Data/Activity_B_11.txt", "./Data/Bodytemp_B_11.txt")),
          combine_files(c("./Data/Activity_B_8.txt", "./Data/Bodytemp_B_8.txt"), 4,5,7),
          combine_files(c("./Data/Activity_W_12.txt", "./Data/Bodytemp_W_12.txt"), 4,5,7))
