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


##################################################################
### STEP 4 - HELPER FUNCTIONS: SPECIFIC HELPER FUNCTIONS ### 
##################################################################
#' Function getting converting a customized dataframe with descriptives to latex code  
#' 
#' 
#' @param SETTINGS ... the path to the tsanalysis/SETTINGS.R config file.
#'
#'
texmake_desc <- function(desc
                         #,descriptions
){
  for(i in 1:length(desc)){
    subtable_df           <- desc[[i]][ ,c("n","mean", "median","min","max","sd", "kurtosis")]
    colnames(subtable_df) <- c("Obs.", "Mean", "Med.","Min.", "Max.", "Std. Dev.", "Kurtosis")
    subtable_latex        <- xtable(subtable_df,
                                    align = c("l", rep("r", 7)),
                                    booktabs = T)
    #"lccccccc"
    print(subtable_latex,
          file       = paste0(SETTINGS$path_tables, SETTINGS$stor_desc_filenames[i]),
          floating   = FALSE,
          #scalebox='0.9',
          ## ,add.to.row = list(list(nrow(subtable_df)),
          ##                   paste0("\\hline  \\multicolumn{7}{L{9cm}}{\\textbf{Note: }",
          ##                          descriptions[i])
          sanitize.text.function = function(x) x
    )
  }
}


#' Helper function just wrappeing the describe package "describe" function 
#' 
#' 
#' @param x ... a dataframe to calculate descriptives on (see psych documentations)
#'
#' @return ... dataframe with descriptives
#'
.dscrbe <- function(x){
  y <- as.data.frame(psych::describe(x))
  return(y)
}

#' Helper function used as simplifying wrapper for putting data from different dataframes together for later calculating descriptives in the right order
#' 
#' 
#' @param SETTINGS ... the path to the tsanalysis/SETTINGS.R config file.
#' @param est_data ... velocity measures 
#' @param app_data ... velocity approximators
#' @param other_data ... additional data
#' @param col_excl ... a vector with strings of column names to exclude
#'
#' @return ... dataframe to calculate descriptives on
#'
prep_for_dscrbe <- function(est_data,
                            app_data,
                            other_data,
                            col_excl){
  dta                 <- dplyr::bind_cols(app_data,
                                          est_data,
                                          other_data)
  ordered_cols        <- colnames(dta)[order(gsub("_","",colnames(dta)))]
  dta                 <- dta[, ordered_cols]
  keep_indicator      <- !colnames(dta) %in% col_excl 
  
  dta <- dta[keep_indicator]
  
  return(dta)
  
}


#' Function getting a dataframe with descriptives from velocity estimators, approximators and "other data" (raw variables, etc.)
#' 
#' 
#' @param SETTINGS ... the path to the tsanalysis/SETTINGS.R config file.
#' @param SETTINGS ... the path to the tsanalysis/SETTINGS.R config file.
#' @param est_data ... velocity measures 
#' @param app_data ... velocity approximators
#' @param other_data ... additional data
#' @param col_excl ... a vector with strings of column names to exclude
#'
#' @return ... dataframe with descriptives. 
#'
dscrbe <- function(est_data,
                   app_data,
                   other_data,
                   col_excl = "v_app_naive"){
  dta_to_dscrbe <- prep_for_dscrbe(est_data   = est_data,
                                   app_data   = app_data,
                                   other_data = other_data,
                                   col_excl = col_excl)
  
  dta_to_dscrbe <- rename_df(dta_to_dscrbe)
  
  description <- .dscrbe(dta_to_dscrbe)
  
  return(description)
}


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


##################################################################
### STEP 5 - LOAD INPUT DATA ### 
##################################################################
setwd("/Users/ingolfpernice/Documents/catvom_2021/catvom_2021_02_make_descriptives/")
load(file = paste0(SETTINGS$path_data, "tsdata_wOutl.rda"))
load(file = paste0(SETTINGS$path_data, "tsdata.rda"))

##################################################################
### STEP 6 - SCRIPT ### 
##################################################################

##################################################################
## prepare normalized estimators
nest <- normalize_df(d_withoutl$est)
napp <- normalize_df(d_withoutl$app)

##################################################################
## prepare standardized estimators
sest <- standardize_df(d$est)
sapp <- standardize_df(d$app)

##################################################################
## prepare descriptive tables over raw, normalized and standardized data
## without counting *_naive duplicated 
                                        # initialize
desc <- list()

                                        # for raw estimators and approximators
desc$est_and_app      <- dscrbe(est_data   = d_withoutl$app,
                                app_data   = d_withoutl$est,
                                other_data = NULL,
                                col_excl   = "v_app_naive")

                                        # for normalized estimators and approximators
desc$nest_and_napp    <- dscrbe(est_data   = nest,
                                app_data   = napp,
                                other_data = NULL,
                                col_excl   = "v_app_naive")

                                        # for standardized estimators and approximators
desc$sest_and_sapp    <- dscrbe(est_data   = sest,
                                app_data   = sapp,
                                other_data = NULL,
                                col_excl   = "v_app_naive")

                                        # for other raw data
other <- get_sub_df(df       = d$all,
                    pattern  = c("v_app",
                                 "v_est",
                                 "m_total",
                                 "m_circ",
                                 "tx_vol_infl",
                                 "tx_vol_clean",
                                 "price_usd"),
                    type     = "match")
desc$other    <- dscrbe(est_data   = NULL,
                        app_data   = NULL,
                        other_data = other,
                        col_excl   = "v_app_naive")
vars_scaled <- c("$M_{\\mathtt{circMcaFifo}}$",                
                 "$M_{\\mathtt{circMcaLifo}}$",
                 "$M_{\\mathtt{circWba}}$",
                 "$M_{\\mathtt{total}}$",
                 "$\\mathtt{Vol.(deflated)}$",
                 "$\\mathtt{Vol.(inflated)}$")
rown_to_mod <- rownames(desc$other) %in% vars_scaled
rownames(desc$other)[rown_to_mod] <- paste0("$ ^{\\ast} ", str_sub(rownames(desc$other),
                                                                  start=2))[rown_to_mod]


## Write descriptive table to tex
texmake_desc(desc = desc)


