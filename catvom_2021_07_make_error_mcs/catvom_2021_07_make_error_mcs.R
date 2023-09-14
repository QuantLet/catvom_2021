##################################################################
### STEP 1 - SET PACKAGES & OPTIONS ### 
##################################################################
load_or_install_pkgs <- function(list_of_packages){
  
  new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)){
    install.packages(new_packages)
  } else {
    lapply(list_of_packages, require, character.only = TRUE)       
    # (Thanking: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them)
  }
  print("That's done! I hope...")
}

##### Load packages ####
list_of_packages <- c("psych", "ggplot2", "reshape2", "stringr", 
                      "pageviews", "anytime", "dplyr", "tidyverse", "lubridate", 
                      "blockwatch", "stargazer", "corrplot", "tikzDevice", "vars",
                      "xtable", "MCS", "ggcorrplot", "car", "dplyr")
load_or_install_pkgs(list_of_packages)

options(scipen=999)
options("tikzLatex"="/usr/local/texlive/2023/bin/universal-darwin/latex")
#options("tikzLatex"="/usr/bin/pdflatex")
options(stringsAsFactors = FALSE)

n <- 20
#qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]

btc_per_satoshi          <- 100000000
scaling_multiplicator    <- 1000000


##################################################################
### STEP 2 - SET SETTINGS OBJECT ### 
##################################################################

SETTINGS <- list()
SETTINGS$start_date              <- "2013-06-01"
SETTINGS$end_date                <- "2019-06-01"
SETTINGS$path_data               <- "./"
SETTINGS$blockwatch_refresh      <- FALSE #
SETTINGS$file                    <- "data.csv"
SETTINGS$path_tables             <- "./"
SETTINGS$path_figs               <- "./"
SETTINGS$stor_desc_filenames     <- c("descriptives_est_and_app.tex",
                                      "descriptives_nest_and_napp.tex",
                                      "descriptives_sest_and_sapp.tex",
                                      "descriptives_other.tex")
#SETTINGS$palette_1               <- unlist(mapply(brewer.pal,
#                                                  qual_col_pals$maxcolors,
#                                                  rownames(qual_col_pals)))
SETTINGS$palette_2               <- c("#999999", "#E69F00",
                                      "#56B4E9", "#009E73",
                                      "#F0E442", "#0072B2",
                                      "#D55E00", "#CC79A7")
SETTINGS$palette_3               <- c("#332288", "#88CCEE",
                                      "#44AA99", "#117733",
                                      "#999933", "#AA4499",
                                      "#DDCC77", "#882255")
SETTINGS$titles_for_plots        <- FALSE#TRUE #
SETTINGS$btc_per_satoshi         <- btc_per_satoshi
SETTINGS$scaling_multiplicator   <- scaling_multiplicator

##################################################################
### STEP 3 - HELPER FUNCTIONS: GENERAL ### 
##################################################################


add_sig_stars <- function(df
                          , tuples_1stars
                          , tuples_2stars
                          , tuples_3stars
                          , rowadd = 0
                          , coladd = 2
){
  
  for (r in 1:nrow(df)){
    for (c in 1:ncol(df)){
      if(paste0(r,"-",c) %in% tuples_1stars){df[r+rowadd,c+coladd] <- paste0(" \\(^{\\dag}\\) ",
                                                                             df[r+rowadd,c+coladd])} 
    }
  }
  
  for (r in 1:nrow(df)){
    for (c in 1:ncol(df)){
      if(paste0(r,"-",c) %in% tuples_2stars){df[r+rowadd,c+coladd] <- paste0(" \\(^{\\ddag}\\) ",
                                                                             df[r+rowadd,c+coladd])} 
    }
  }
  
  for (r in 1:nrow(df)){
    for (c in 1:ncol(df)){
      if(paste0(r,"-",c) %in% tuples_3stars){df[r+rowadd,c+coladd] <- paste0(" \\(^{\\ast}\\) ",
                                                                             df[r+rowadd,c+coladd])} 
    }
  }
  return(df)
}


add_sig_stars_automatic <- function(vec,
                                    sigLevs = c(0.10, 0.05, 0.01)
){
  
  vec_num   <- suppressWarnings(as.numeric(vec))
  vec_char  <- vec
  vec_char[is.na(vec_num )] <- "\\( - \\)"
  for(sigLev in sigLevs){
    for(i in 1:length(vec)){
      
      if(!is.na(vec_num[i]) && vec_num[i] < sigLev){
        vec_char[i] <- paste0(vec_char[i], "\\(^{\\ast} \\)")
      }
      
    }
  }
  return(vec_char)
}

splitrownames <- function(rnames, by = " VS "){
  rnames_split <- strsplit(x=rnames, " VS ")    
  rnames_1 <- as.character(unlist(lapply(rnames_split, function(x){x[[1]]})))
  rnames_2 <- as.character(unlist(lapply(rnames_split, function(x){x[[2]]})))
  res <- list(rnames_1, rnames_2)
  names(res) <- c("left_from_BY", "right_from_BY")
  return(res)
}

##################################################################
### STEP 4 - HELPER FUNCTIONS: SPECIFIC HELPER FUNCTIONS ### 
##################################################################
#' Function executing the MCS tests and saving the resulting outputs.
#' 
#' @param mae_errordata ... deviations (MAE) between approximations and measures for velocity as dataframe
#' @param mse_errordata ... deviations (MSE) between approximations and measures for velocity as dataframe
#' @param storagepath ... path to directory where the results are to be stored.
#' @param sigLev ... the significance levels used for the MCS tests.
#' 
#' @return -
#'
mcs_from_errordata <- function(mae_errordata,
                               mse_errordata,
                               storagepath,
                               sigLev = 0.01){
  filterterms <- c("total", "circWba", "circMcaLifo", "circMcaFifo")
  
  mcs_mae_errordata <- list()
  mcs_mse_errordata <- list()
  
  for (filterterm in filterterms){
    
    mae_errordata_subset <- mae_errordata[,colnames(mae_errordata)[grepl(filterterm,
                                                                         colnames(mae_errordata))]]
    colnames(mae_errordata_subset) <- str_replace_all(str_replace_all(colnames(mae_errordata_subset), "[^[:alnum:]]", "-"),"mathtt", "")
    
    mse_errordata_subset <- mae_errordata[,colnames(mse_errordata)[grepl(filterterm,
                                                                         colnames(mse_errordata))]]
    colnames(mse_errordata_subset) <- str_replace_all(colnames(mse_errordata_subset), "[^[:alnum:]]", "-")
    
    print(paste0("<<<< START: ",storagepath ," - MAE - ", filterterm ," >>>"))
    mcs_mae_errordata[[filterterm]] <- MCSprocedure(Loss=mae_errordata_subset,
                                                    alpha=sigLev,
                                                    B=5000,
                                                    statistic='Tmax',
                                                    cl=NULL)
    print(paste0("<<<< START: ",storagepath," - MSE - ", filterterm ," >>>"))
    mcs_mse_errordata[[filterterm]] <- MCSprocedure(Loss=mse_errordata_subset,
                                                    alpha=sigLev,
                                                    B=5000,
                                                    statistic='Tmax',
                                                    cl=NULL)
  }
  
  save(mcs_mae_errordata, file = paste0(SETTINGS$path_data,
                                        "mae_appVSest_mcs_",
                                        storagepath,
                                        ".rda"))
  save(mcs_mse_errordata, file = paste0(SETTINGS$path_data,
                                        "mse_appVSest_mcs_",
                                        storagepath,
                                        ".rda"))
}


##################################################################
### STEP 5 - LOAD INPUT DATA ### 
##################################################################
setwd("/Users/ingolfpernice/Documents/catvom_2021/catvom_2021_07_make_error_mcs/")
load(file = paste0(SETTINGS$path_data, "appVSest.rda"))

##################################################################
### STEP 6 - SCRIPT ### 
##################################################################

#########################################################
## Load stored error table summaries for normal error time series


#########################################################
## Run MCS test
mcs_from_errordata(mae_errordata = error_dta[["mae_stand_ts"]],
                   mse_errordata = error_dta[["mse_stand_ts"]],
                   storagepath = "stand",
                   sigLev = 0.01)

mcs_from_errordata(mae_errordata = error_dta[["mae_stand_ts"]],
                   mse_errordata = error_dta[["mse_stand_ts"]],
                   storagepath = "stand",
                   sigLev = 0.05)

mcs_from_errordata(mae_errordata = error_dta[["mae_norm_ts"]],
                   mse_errordata = error_dta[["mse_norm_ts"]],
                   storagepath = "norm")

mcs_from_errordata(mae_errordata = error_dta[["mae_stand_ts_d1"]],
                   mse_errordata = error_dta[["mse_stand_ts_d1"]],
                   storagepath = "stand_d1")

mcs_from_errordata(mae_errordata = error_dta[["mae_norm_ts_d1"]],
                   mse_errordata = error_dta[["mse_norm_ts_d1"]],
                   storagepath = "norm_d1")
