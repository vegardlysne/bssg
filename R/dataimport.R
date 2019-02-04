#' Load a single .txt data file from "data quest art" software
#'
#' Load a single .txt file with two columns, and creates column names.
#' File name should be "Variablename_groupname_animalID.txt", as these are used
#' to create column names, as well as group and id columns.
#'
#' @param path path to file
#' @param baseline number of days in baseline condition, defaults to 5
#' @param exposure number of days in exposure condition, defaults to 7
#' @param zeitgebertime number indicating start of light period, defaults to 7 (= 07:00)
#' @param maxbaseline number of maximum baseline days in experiment, defaults to 5
#'
#' @return a tibble
#' @export
#'
load_single_file <- function(path,
                             baseline = 5,
                             exposure = 7,
                             zeitgebertime = 7,
                             maxbaseline = 5){

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
                Zeitgebertime = Datetime - lubridate::hours(zeitgebertime),
                Day = lubridate::day(Zeitgebertime) - lubridate::day(Zeitgebertime[1]) + 1 + maxbaseline-baseline,
                Experiment_period = dplyr::case_when(Day <= maxbaseline ~ "Baseline",
                                                     Day <= maxbaseline + exposure ~ "Exposure",
                                                     TRUE ~ "Recovery"),
                Zeitgebertime = Zeitgebertime-Zeitgebertime[1]-(Day-1)*86400 + (maxbaseline-baseline)*86400,
                Syncdatetime = lubridate::ymd_hms("20190101000000")+Zeitgebertime + lubridate::days(Day-1))
}


#' Combine files from same animal into a tibble
#'
#' Takes a list of .txt files from the same animal and produces a tibble with all variables
#'
#' @param filepaths a character vector of file names for the same animal
#' @param baseline number of days in baseline condition, defaults to 5
#' @param exposure number of days in exposure condition, defaults to 7
#' @param zeitgebertime number indicating start of light period, defaults to 7 (= 07:00)
#' @param maxbaseline number of maximum baseline days in experiment, defaults to 5
#'
#' @return a tibble
#' @export
#'
combine_files <- function(filepaths,
                          baseline = 5,
                          exposure = 7,
                          zeitgebertime = 7,
                          maxbaseline = 5){

  d <- load_single_file(filepaths[1], baseline, exposure, zeitgebertime, maxbaseline)

  for(i in 2:length(filepaths)){
    d <- dplyr::left_join(d,
                          load_single_file(filepaths[i],
                                           baseline,
                                           exposure,
                                           zeitgebertime,
                                           maxbaseline))
  }

  d %>%
    dplyr::select(Group,
                  Id,
                  Datetime,
                  Syncdatetime,
                  Experiment_period,
                  Day,
                  Zeitgebertime,
                  dplyr::everything())
}

# Helperfunctions to check that argument length = filepath length
check_length <- function(argument, control){

  if(!(length(argument) == length(control)) & !(length(argument) == 1)){
    stop("baseline, exposure and zeitgebertime must be same length as filepaths or 1")
  }
  argument
}

#' Load data set
#'
#' @param filepaths A named list of character vectors, indicating the paths to all
#' the files of the same animal.
#' @param baseline A named list of the number of baselinedays for each animal,
#' or a number if it is the same for each animal, no default
#' @param exposure A number, indicating length of exposure period in days, defaults to 5
#' @param zeitgebertime A named list indicating start of light period for each animal,
#' or a number if it is the same for each animal
#' @param maxbaseline A number of maximum baseline days in experiment, defaults to 5
#'
#' @return a tibble
#' @export
#'
#' @examples
#' animals <- list(B8 = c("./Data/Activity_B_8.txt", "./Data/Bodytemp_B_8.txt"),
#'                 W12 = c("./Data/Activity_W_12.txt", "./Data/Bodytemp_W_12.txt"))
#' baselinedays <- list(B8 = 5, W12 = 4)
#'
#' create_data(animals,
#'             baseline = baselinedays,
#'             exposure = 7,
#'             zeitgebertime = 7,
#'             maxbaseline = 5)
#'
create_data <- function(filepaths,
                        baseline,
                        exposure = 7,
                        zeitgebertime,
                        maxbaseline = 5){

  baseline <- check_length(baseline, filepaths)

  exposure <- check_length(exposure, filepaths)

  zeitgebertime <- check_length(zeitgebertime, filepaths)

  purrr::pmap_df(list(filepaths, baseline, zeitgebertime),
                 function(x,y,z) combine_files(filepaths = x,
                                               baseline = y,
                                               exposure = exposure,
                                               zeitgebertime = z,
                                               maxbaseline = maxbaseline))

}


