[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="1100" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **catvom_2021_05_make_error_data** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: catvom_2021_05_make_error_data

Published in: 'Cryptocurrencies and the Velocity of Money (Pernice et al., 2021)'

Description: 'This Quantlet is primarily focused on data manipulation and error calculation. It defines a series of functions for normalizing, standardizing, and calculating mean absolute error (MAE) and mean squared error (MSE) between measured and approximated velocity data. It then applies these functions to a dataset, and saves the error data and error summaries.'

Keywords: Data Manipulation, Normalization, Standardization, Mean Absolute Error, Mean Squared Error

Author: Ingolf Pernice, Hermann Elendner, Georg Gentzen

See also: other Quantlets in this project

Submitted: 02.07.2023

Datafile: tsdata.rda

Output: appVSest_summary.rda, appVSest.rda
```

### R Code
```r

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
options("tikzLatex"="/usr/local/texlive/2018/bin/x86_64-linux/latex")
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

rename_df <- function(df){
  i_rep_all <- NULL
  for(i in 1:length(colnames(df))){
    i_rep <- switch(colnames(df)[i],
                    date = "Date",
                    v_est_m_naive = "$V^{\\mathtt{msr}}_{\\mathtt{triv}}$",
                    v_est_m_total = "$V^{\\mathtt{msr}}_{\\mathtt{total}}$",
                    v_est_m_circ_wb = "$V^{\\mathtt{msr}}_{\\mathtt{circWba}}$",
                    v_est_m_circ_mc_lifo = "$V^{\\mathtt{msr}}_{\\mathtt{circMcaLifo}}$",
                    v_est_m_circ_mc_fifo = "$V^{\\mathtt{msr}}_{\\mathtt{circMcaFifo}}$",
                    v_app_coindd = "$V^{\\mathtt{app}}_{\\mathtt{cdd}}$",
                    v_app_turnover = "$V^{\\mathtt{app}}_{\\mathtt{turn}}$",
                    v_app_naive = "$V^{\\mathtt{app}}_{\\mathtt{triv}}$",
                    days_destroyed = "INT VAR",
                    dormancy = "INT VAR",
                    turnover = "INT VAR",
                    tx_vol = "INT VAR",
                    tx_vol_infl = "$\\mathtt{Vol.(inflated)}$",
                    tx_vol_clean = "$\\mathtt{Vol.(deflated)}$",
                    tx_number = "INT VAR",
                    tx_fees = "INT VAR",
                    m_total = "$M_{\\mathtt{total}}$",
                    m_circ_wh_bill = "$M_{\\mathtt{circWba}}$",
                    m_circ_mc_lifo = "$M_{\\mathtt{circMcaLifo}}$",
                    m_circ_mc_fifo = "$M_{\\mathtt{circMcaFifo}}$",
                    marketcap = "INT VAR",
                    price_usd = "$P^{\\mathtt{USD}/\\mathtt{BTC}}$",
                    volume_usd = "$\\mathtt{Vol. (off-chain)}$",
                    returns = "$\\mathtt{Return} (\\mathtt{USD}/\\mathtt{BTC})$",
                    volatility = "$\\mathtt{Volatility} (\\mathtt{USD}/\\mathtt{BTC})$",
                    views_wiki = "INT VAR",
                    tx_cdd = "$\\mathtt{bdd}$")
    i_rep_all <- c(i_rep_all, i_rep)
  }
  colnames(df) <- i_rep_all
  return(df)
}
rename_df_with_mark_for_scaling <- function(df){
  i_rep_all <- NULL
  for(i in 1:length(colnames(df))){
    i_rep <- switch(colnames(df)[i],
                    date = "Date",
                    v_est_m_naive = "$V^{\\mathtt{msr}}_{\\mathtt{triv}}$",
                    v_est_m_total = "$V^{\\mathtt{msr}}_{\\mathtt{total}}$",
                    v_est_m_circ_wb = "$V^{\\mathtt{msr}}_{\\mathtt{circWba}}$",
                    v_est_m_circ_mc_lifo = "$V^{\\mathtt{msr}}_{\\mathtt{circMcaLifo}}$",
                    v_est_m_circ_mc_fifo = "$V^{\\mathtt{msr}}_{\\mathtt{circMcaFifo}}$",
                    v_app_coindd = "$V^{\\mathtt{app}}_{\\mathtt{cdd}}$",
                    v_app_turnover = "$V^{\\mathtt{app}}_{\\mathtt{turn}}$",
                    v_app_naive = "$V^{\\mathtt{app}}_{\\mathtt{triv}}$",
                    days_destroyed = "INT VAR",
                    dormancy = "INT VAR",
                    turnover = "INT VAR",
                    tx_vol = "INT VAR",
                    tx_vol_infl = "$^{\\ast}\\mathtt{Vol.(inflated)}$",
                    tx_vol_clean = "$^{\\ast}\\mathtt{Vol.(deflated)}$",
                    tx_number = "INT VAR",
                    tx_fees = "INT VAR",
                    m_total = "$^{\\ast}M_{\\mathtt{total}}$",
                    m_circ_wh_bill = "$^{\\ast}M_{\\mathtt{circWba}}$",
                    m_circ_mc_lifo = "$^{\\ast}M_{\\mathtt{circMcaLifo}}$",
                    m_circ_mc_fifo = "$^{\\ast}M_{\\mathtt{circMcaFifo}}$",
                    marketcap = "INT VAR",
                    price_usd = "$P^{\\mathtt{USD}/\\mathtt{BTC}}$",
                    volume_usd = "$^{\\ast}\\mathtt{Vol. (off-chain)}$",
                    returns = "$\\mathtt{Return} (\\mathtt{USD}/\\mathtt{BTC})$",
                    volatility = "$\\mathtt{Volatility} (\\mathtt{USD}/\\mathtt{BTC})$",
                    views_wiki = "INT VAR",
                    tx_cdd = "$^{\\ast}\\mathtt{bdd}$")
    i_rep_all <- c(i_rep_all, i_rep)
  }
  colnames(df) <- i_rep_all
  return(df)
}

get_sub_df <- function(df, pattern, type){
  if(type == "exact"){
    df <- df[ ,pattern]
  }else if(type == "match"){
    df <- df[,grepl(paste(pattern, collapse="|"), colnames(df))]
  }
  return(df)
}

date_indctr <- function(datevec, date){
  ind <- as.Date(datevec, format = "%m/%d/%y") == anydate(date)
  return(ind)
}
date_wiki_format <- function(x){
  paste0(str_replace_all(as.character(x), "-", ""),"00")
}

.normalize <- function(x){
  normalized = (x-min(x))/(max(x)-min(x))
  return(normalized)
}

.standardize <- function(x){
  normalized = (x-mean(x))/sd(x)
  return(normalized)
}

normalize_df <- function(x){
  x_norm = as.data.frame(sapply(x, .normalize))
  return(x_norm)
}

standardize_df <- function(x){
  x_stand = as.data.frame(sapply(x, .standardize))
  return(x_stand)
}


.mse_special <- function(x,d_app,i){
  (x-d_app[ ,i])^2
}

.mae_special <- function(x,d_app,i){
  abs(x-d_app[ ,i])
}

wDate <- function(df){
  df <- cbind(d$date,df)
  colnames(df)[1] <- "date"
  return(df)
}

diff_df <-function(x, diffname){
  x_diff <- as.data.frame(sapply(x,diff))
  colnames(x_diff) <- paste0(diffname, colnames(x_diff))
  return(x_diff)
}

splitrownames <- function(rnames, by = " VS "){
  rnames_split <- strsplit(x=rnames, " VS ")    
  rnames_1 <- as.character(unlist(lapply(rnames_split, function(x){x[[1]]})))
  rnames_2 <- as.character(unlist(lapply(rnames_split, function(x){x[[2]]})))
  res <- list(rnames_1, rnames_2)
  names(res) <- c("left_from_BY", "right_from_BY")
  return(res)
}

col_drop <- function(df, term){
  df <- df[, !grepl(term,colnames(df)), drop = F]
  return(df)
}

##################################################################
### STEP 4 - HELPER FUNCTIONS: SPECIFIC HELPER FUNCTIONS ### 
##################################################################
#' Function generating a dataframe with deviations between the approximation data and measure data for velocity
#' 
#' @param SETTINGS ... the path to the tsanalysis/SETTINGS.R config file.
#' @param df_est ... velocity data, measured
#' @param df_app ... velocity data, estimated
#' @param type ... switch variable, currently only "mse" and "mae"
#' @param datevec ... vector with time data 
#' 
#' @return a dataframe with error data
#'
make_error_data <- function(SETTTINGS = SETTTINGS,
                            df_est,
                            df_app,
                            type,
                            datevec = d$date ){
  
  if(type == "mae"){
    
    mae_from_norm <- data.frame(date = datevec)
    for(i in 1:ncol(df_app)){
      mae_from_norm_element           <- as.data.frame(sapply(df_est, .mae_special, df_app, i))
      colnames(mae_from_norm_element) <- paste0(colnames(df_app)[i]," VS ", colnames(df_est))
      mae_from_norm                   <- cbind(mae_from_norm, mae_from_norm_element)
    }
    result <- mae_from_norm
    
  }else if(type == "mse"){
    
    mse_from_norm <- data.frame(date = datevec)
    for(i in 1:ncol(df_app)){
      mse_from_norm_element           <- as.data.frame(sapply(df_est, .mse_special, df_app, i))
      colnames(mse_from_norm_element) <- paste0(colnames(df_app)[i]," VS ", colnames(df_est))
      mse_from_norm                   <- cbind(mse_from_norm, mse_from_norm_element)
    }
    result <- mse_from_norm
  }
  
  return(result)
}


#' Function summing up deviations from the error data and putting the sums into a dataframe. 
#' 
#' @param SETTINGS ... the path to the tsanalysis/SETTINGS.R config file.
#' @param errordf ... the dataframe with the deviations between measured and approximated velocity
#' @param type ... error type that is to be calculated. Just used to format column names.
#'
#' @return a dataframe with the summed up errors.
#'
make_summary_from_data <- function(type = "mse", errordf){
  
  summary_errordf <- as.data.frame(sapply(errordf[, -1], sum))
  summary_errordf <- as.data.frame(summary_errordf[order(colnames(summary_errordf))])
  colnames(summary_errordf) <- toupper(type)
  
  return(summary_errordf)
}



##################################################################
### STEP 5 - LOAD INPUT DATA ### 
##################################################################
setwd("/Users/ingolfpernice/Documents/catvom_2021/catvom_2021_05_make_error_data/")
load(file = paste0(SETTINGS$path_data, "tsdata.rda"))

##################################################################
### STEP 6 - SCRIPT ### 
##################################################################

#########################################################
## Prepare normalized, standardized and differenced data
                                        # prepare normalized estimators
nest <- rename_df(normalize_df(d$est))
napp <- rename_df(normalize_df(d$app))

                                        # prepare standardized estimators
sest <- rename_df(standardize_df(d$est))
sapp <- rename_df(standardize_df(d$app))

                                        # prepare normalized estimators
d1nest <- diff_df(rename_df(normalize_df(d$est)), "[1d]")
d1napp <- diff_df(rename_df(normalize_df(d$app)), "[1d]")

                                        # prepare standardized estimators
d1sest <- diff_df(rename_df(standardize_df(d$est)), "[1d]")
d1sapp <- diff_df(rename_df(standardize_df(d$app)), "[1d]")


#########################################################
## Loop throgh data to create error summaries and data

                                        # Prepare variables for loop
types  <- list("mse","mae")
dta_app    <- list(napp, sapp, d1napp, d1sapp)
dta_est    <- list(nest, sest, d1nest, d1sest)
naming_types  <- list("mse","mae")
naming_dta    <- list("norm_ts","stand_ts","norm_ts_d1","stand_ts_d1")
datevecs      <- list(d$date,
                      d$date,
                      d$date[-1],
                      d$date[-1])

                                        # Loop
error_dta   <- list()
summary_dta <- list()
for(type in types){
    for(i in 1:length(dta_app)){
        data_id <- paste(type, naming_dta[[i]], sep="_")
                                        # Make error data
        error_dta[[data_id]] <- make_error_data(df_est = col_drop(dta_est[[i]], "triv"),
                                                df_app = dta_app[[i]],
                                                datevec = datevecs[[i]],
                                                type   = type)
        
                                        # Make summaries
        summary_dta[[data_id]] <- make_summary_from_data(type    = type,
                                                         errordf = error_dta[[data_id]])
    }
}
                                        # Save error data
save(error_dta, file = paste0(SETTINGS$path_data,"appVSest.rda"))

                                        # Save summaries
save(summary_dta, file = paste0(SETTINGS$path_data,"appVSest_summary.rda"))

```

automatically created on 2023-09-14