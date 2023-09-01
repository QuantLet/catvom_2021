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
get_sub_df <- function(df, pattern, type){
  if(type == "exact"){
    df <- df[ ,pattern]
  }else if(type == "match"){
    df <- df[,grepl(paste(pattern, collapse="|"), colnames(df))]
  }
  return(df)
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

col_drop <- function(df, term){
  df <- df[, !grepl(term,colnames(df)), drop = F]
  return(df)
}

date_indctr <- function(datevec, date){
  ind <- as.Date(datevec, format = "%m/%d/%y") == anydate(date)
  return(ind)
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

diff_df <-function(x, diffname){
  x_diff <- as.data.frame(sapply(x,diff))
  colnames(x_diff) <- paste0(diffname, colnames(x_diff))
  return(x_diff)
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
#' Helper function executing the MZ-regressions
#' 
#' @param estimator ... velocity measure
#' @param approximator ... velocity approximation
#'  
#' @return ... dataframe with results of the MZ-regression
#'
mzr <- function(estimator,
                approximator){
  
  mz_reg <- lm(estimator ~ approximator)
  
  adjR2 <- summary(mz_reg)$adj.r.squared
  
  alpha.est   <- summary(mz_reg)$coefficients["(Intercept)","Estimate"]
  alpha.tval  <- summary(mz_reg)$coefficients["(Intercept)","t value"]
  alpha.pval  <- summary(mz_reg)$coefficients["(Intercept)","Pr(>|t|)"]
  
  # handler for singularities and semi-singularities if approximator is straight line
  Ftest = tryCatch({linearHypothesis(mz_reg, c("(Intercept) = 0", "approximator = 1"))}, error = function(e) {e <- NA})
  Ftest.pval  <- if(all(is.na(Ftest))){NA} else {Ftest$`Pr(>F)`[2]}
  Ftest.F     <- if(all(is.na(Ftest))){NA} else {Ftest$F[2]}
  
  beta.est = tryCatch({summary(mz_reg)$coefficients["approximator","Estimate"]}, error = function(e) {e <- NA})
  beta.est    <- if(all(is.na(Ftest))){NA} else {beta.est}
  beta.tval   <- if(all(is.na(Ftest))){NA} else {summary(mz_reg)$coefficients["approximator","t value"]}
  beta.pval   <- if(all(is.na(Ftest))){NA} else {summary(mz_reg)$coefficients["approximator","Pr(>|t|)"]}
  
  
  
  result <- data.frame(
    adjR2      = adjR2,
    alpha.est  = alpha.est,
    alpha.pval = alpha.pval,
    beta.est   = beta.est,
    beta.pval  = beta.pval,
    Ftest.pval = Ftest.pval
  )
  
  names(result) <- c(
    "$R^{2}_{adj}$"
    ,"$\\alpha$"
    ,"$p^{\\alpha}$"
    ,"$\\beta$"
    ,"$p^{\\beta}$"
    ,"$p^{F-Test}$"
  )   
  
  return(result)
}


#' Function exectuing the mz-regression for the all the velocity measures and approximators
#' 
#' @param app ... dataframe with approximators for velocity
#' @param est ... dataframe with estimators for velocity
#' @param r_digits ... number of digits to be displayed in dataframe 
#' 
#' @return a dataframe collecting the results from all mz-regressions
#'
mz_table <- function(est,
                     app,
                     r_digits = 2){
  mz <- NULL
  mz_rownames <- NULL
  for(i in 1:ncol(app)){
    for(j in 1:ncol(est)){
      
      estimator           <- est[,j]
      approximator        <- app[,i]
      
      append <- mzr(estimator = estimator, approximator = approximator)
      
      if(i == 1 & j == 1){mz <- append}
      else{mz <- rbind(mz, append)}
      mz_rownames_append <- paste0(colnames(app)[i]," VS ", colnames(est)[j])
      mz_rownames        <- c(mz_rownames, mz_rownames_append)
      
    }
  }
  rownames(mz) <- mz_rownames
  mz <- format(round(mz, digits = r_digits), nsmall = 2)
  return(mz)
}

##################################################################
### STEP 5 - LOAD INPUT DATA ### 
##################################################################
setwd("/Users/ingolfpernice/Documents/catvom_2021/catvom_2021_09_make_mz_regression/")
load(file = paste0(SETTINGS$path_data, "tsdata.rda"))

##################################################################
### STEP 6 - SCRIPT ### 
##################################################################

########################################################
## Prepare normalized, standardized and differenced data
                                        # prepare normalized estimators
nest <- rename_df(col_drop(normalize_df(d$est), "VNaive"))
napp <- rename_df(normalize_df(d$app))

                                        # prepare standardized estimators
sest <- rename_df(col_drop(standardize_df(d$est), "VNaive"))
sapp <- rename_df(standardize_df(d$app))

                                        # prepare normalized estimators
d1nest <- diff_df(rename_df(col_drop(normalize_df(d$est), "naive")), "$\\Delta$ ")
d1napp <- diff_df(rename_df(normalize_df(d$app)), "$\\Delta$ ")

                                        # prepare standardized estimators
d1sest <- diff_df(rename_df(col_drop(standardize_df(d$est), "naive")), "$\\Delta$ ")
d1sapp <- diff_df(rename_df(standardize_df(d$app)), "$\\Delta$ ")



########################################################
## Gather Input for MZ-Regressions and run regressions
mz_input <- list(list(d1nest, d1napp),
                 list(d1sest, d1sapp))
mz_pathdiff <- list("normalized_d1",
                    "standardized_d1")
mz <- list()
for(i in 1:length(mz_input)){
    ## do mz regressions
    mz[[i]] <- mz_table(est      = mz_input[[i]][[1]],
                        app      = mz_input[[i]][[2]],
                        r_digits = 2)
}
mz <- do.call(cbind, mz)

########################################################

########################################################
## Add significance stars
sigCols <- c(3,5,6, 9,11,12)
for(sigCol in sigCols){
    mz[ ,sigCol] <- add_sig_stars_automatic(mz[ ,sigCol])
}

########################################################
## Make latex tables

########################################################
## Add column names in column, as xtable gets into trouble
                                        # [-1] because first rown
mz <- cbind(data.frame(`Approximation` = splitrownames(rnames = rownames(mz),
                                                       by = " VS ")[["left_from_BY"]],
                       Estimator = splitrownames(rnames = rownames(mz),
                                                 by = " VS ")[["right_from_BY"]],
                       stringsAsFactors = FALSE),
            mz)
## add colnames as row for xtables
cnames <- as.character(rbind(colnames(mz)))
mz <- rbind(cnames, mz)


## Add Multicolumn row
addtorow <- list()
addtorow$pos <- list(0)
addtorow$command <- "\\hline \\multicolumn{2}{l}{ } & \\multicolumn{6}{c}{Normalized} & \\multicolumn{6}{c}{Standardized} \\\\"

########################################################
## Print table
subtable_latex        <- xtable(mz, align = "lllrrrrrrrrrrrr")#ccccccccc
print(subtable_latex
    , file=paste0(SETTINGS$path_tables, "appVSest_mz_table.tex")
    , sanitize.text.function = function(x) x
    , floating = FALSE
    , include.rownames=FALSE
    , add.to.row=addtorow
    , include.colnames=FALSE
    , hline.after = c(0, 1, 5, 9, nrow(mz)))
