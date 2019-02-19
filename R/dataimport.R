#' Load a single .txt data file from "data quest art" software
#'
#' Load a single .txt file with two columns, and creates column names.
#' File name should be "Variablename_groupname_animalID.txt", as these are used
#' to create column names, as well as group and id columns.
#'
#' @param path path to file
#' @param baseline number of days in baseline condition, defaults to 5
#' @param exposure number of days in exposure condition, defaults to 7
#' @param baselinestart number indicating start of baseline in hours (e.g. 7 = 07:00)
#' @param maxbaseline number of maximum baseline days in experiment, defaults to 5
#'
#' @return a tibble
#' @export
#'
load_single_file <- function(path,
                             baseline = 5,
                             exposure = 7,
                             baselinestart = 7,
                             maxbaseline = 5){
  baseline <- as.numeric(baseline)
  exposure <- as.numeric(exposure)
  zeitgebertime <- as.numeric(baselinestart)

  readr::read_delim(path,
                  delim = ",",
                  comment = "#",
                  col_names = c("Datetime",
                                stringr::str_extract(basename(path), "\\w+?(?=_)")),
                  col_types = "cd",
                  na = "nan",
                  trim_ws = T) %>%
  purrr::modify_at(1, lubridate::as_datetime) %>%
  dplyr::filter(Datetime >= lubridate::date(Datetime[1]) + lubridate::hours(baselinestart)) %>%
  dplyr::mutate(Id = stringr::str_extract(basename(path), "(?<=_)[:digit:]+(?=.txt)"),
                Group = stringr::str_extract(basename(path), "(?<=_)\\w+?(?=_)"),
                Syncdatetime = Datetime - lubridate::hours(baselinestart),
                Day = lubridate::day(Syncdatetime) - lubridate::day(Syncdatetime[1]) + 1 + maxbaseline-baseline,
                Experiment_period = dplyr::case_when(Day <= maxbaseline ~ "Baseline",
                                                     Day <= maxbaseline + exposure ~ "Exposure",
                                                     TRUE ~ "Recovery"),
                t = Syncdatetime-Syncdatetime[1] - (Day-1)*86400 + (maxbaseline-baseline)*86400,
                Syncdatetime = lubridate::ymd_hms("20190101000000") + t + lubridate::days(Day-1))
}


#' Combine files from same animal into a tibble
#'
#' Takes a list of .txt files from the same animal and produces a tibble with all variables
#'
#' @param filepaths a character vector of file names for the same animal
#' @param baseline number of days in baseline condition, defaults to 5
#' @param exposure number of days in exposure condition, defaults to 7
#' @param baselinestart number indicating start of light period, defaults to 7 (= 07:00)
#' @param maxbaseline number of maximum baseline days in experiment, defaults to 5
#'
#' @return a tibble
#' @export
#'
combine_files <- function(filepaths,
                          baseline = 5,
                          exposure = 7,
                          baselinestart = 7,
                          maxbaseline = 5){

  d <- load_single_file(filepaths[1], baseline, exposure, baselinestart, maxbaseline)

  for(i in 2:length(filepaths)){
    d <- dplyr::left_join(d,
                          load_single_file(filepaths[i],
                                           baseline,
                                           exposure,
                                           baselinestart,
                                           maxbaseline))
  }

  d %>%
    dplyr::select(Group,
                  Id,
                  Datetime,
                  Syncdatetime,
                  Experiment_period,
                  Day,
                  t,
                  dplyr::everything())
}

# Helperfunctions to check that argument length = filepath length
check_length <- function(argument, control){

  if(!(length(argument) == length(control)) & !(length(argument) == 1)){
    stop("baseline, exposure and baselinestart must be same length as filepaths or 1")
  }
  argument
}


#' Load data set
#'
#' Load list of .txt files from data quest art. All the files must be named VARIABLE_GROUP_ID,
#' where ID should be numbers only. This makes sure the data set is loaded, the new variable
#' is properly named, and the data is aligned to the correct ID. The filepaths argument must be a list, where every element of the list contains all files for
#' one animal. The other arguements may be a single number (if same value for each animal), or a list
#' of the same length as filepaths, with one value for each animal.
#'
#' @param filepaths A named list of character vectors, indicating the paths to all
#' the files of the same animal.
#' @param baseline A named list of the number of baselinedays for each animal,
#' or a number if it is the same for each animal, no default
#' @param exposure A number, indicating length of exposure period in days, defaults to 5
#' @param baselinestart A named list indicating start of baseline in hours (e.g. 7 = 07:00) for each animal,
#' or a number if it is the same for each animal
#' @param maxbaseline A number of maximum baseline days in experiment, defaults to 5
#'
#' @return a tibble
#'
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
#'             baselinestart = 7,
#'             maxbaseline = 5)
#'
create_data <- function(filepaths,
                        baseline,
                        exposure = 7,
                        baselinestart,
                        maxbaseline = 5){

  baseline <- check_length(baseline, filepaths)

  exposure <- check_length(exposure, filepaths)

  baselinestart <- check_length(baselinestart, filepaths)

  purrr::pmap_df(list(filepaths, baseline, baselinestart),
                 function(x,y,z) combine_files(filepaths = x,
                                               baseline = y,
                                               exposure = exposure,
                                               baselinestart = z,
                                               maxbaseline = maxbaseline))

}


#' Lists files for input to create_data
#'
#' The function takes all the files and organises them in a list with one element per ID,
#' containing all the files for that ID.
#'
#' How to use:
#' Collect all files in a folder, and name them VARIABLE_GROUP_ID, where ID
#' is numeric and group is characters. Make a character vector of all the filepaths
#' using the [list.files()] function, and then pass that vector to list_files().
#'
#' See example
#'
#' @param files a character vector of all the files
#'
#' @return a named list of data filepaths
#' @export
#'
#' @examples
#' List of all files using list.files()
#' allfiles <- list.files(path = "PATH TO FILES", pattern = ".txt")
#'
#' Make list containing one element per ID, which can be passed to [create_data()]
#'
#' animals <- list_files(allfiles)
list_files <- function(files){

  l <- list()

  for(i in unique(stringr::str_extract(files, "[:alpha:]+_[:digit:]+"))){

    l[[i]] <- files[stringr::str_detect(files, i)]
  }

  if(length(unique(purrr::map(l, length))) != 1){
         stop("unequal number of files per ID")
  }
  l
}
