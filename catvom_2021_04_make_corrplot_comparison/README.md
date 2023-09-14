[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="1100" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **catvom_2021_04_make_corrplot_comparison** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: catvom_2021_04_make_corrplot_comparison

Published in: 'Cryptocurrencies and the Velocity of Money (Pernice et al., 2021)'

Description: 'Provides a table of the correlations between the raw data.'

Keywords: plotting

Author: Ingolf Pernice, Hermann Elendner, Georg Gentzen

See also: other Quantlets in this project

Submitted: 02.09.2023

Datafile: tsdata.rda

Output: corrtable_all.tex
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

##################################################################
### STEP 4 - HELPER FUNCTIONS: SPECIFIC HELPER FUNCTIONS ### 
##################################################################


##################################################################
### STEP 5 - LOAD INPUT DATA ### 
##################################################################
setwd("/Users/ingolfpernice/Documents/catvom_2021/catvom_2021_04_make_corrplot_comparison/")
load(file = paste0(SETTINGS$path_data, "tsdata.rda"))

##################################################################
### STEP 6 - SCRIPT ### 
##################################################################

##################################################################
## prepare extraction of data
data <- get_sub_df(df = d$all,
                   pattern = c("v_app",
                               "v_est",
                               "m_total",
                               "m_circ",
                               "tx_vol_infl",
                               "tx_vol_clean",
                               "price_usd"),
                   type = "match")


##################################################################
## prepare corr table
corrtable <- round(cor(rename_df(data)), digits = 2)
corrtable <- cbind(rownames(corrtable), corrtable)

##################################################################
## Rotate heads
rownames(corrtable) <- rownames(corrtable)
cols <- paste0("\\rothead{",colnames(corrtable), "}", collapse = " & ")
cols <- paste0(cols, "\\\\")

##################################################################
## Print table
subtable_latex <- xtable(corrtable, align = "ccccccccccccccccc")
subtable_latex <- print(subtable_latex
                      , file=paste0(SETTINGS$path_tables, "corrtable_all.tex")
                      , include.colnames = FALSE
                      , include.rownames = FALSE
                      , add.to.row = list(pos = as.list(0)
                                        , command = cols)
                      , sanitize.text.function = function(x) x
                      , floating = FALSE
                      , booktabs = FALSE
                      , hline.after = c(0)
                      , format.args = list(width = 1)
)

```

automatically created on 2023-09-14