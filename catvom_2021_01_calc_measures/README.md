[<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/banner.png" width="1100" alt="Visit QuantNet">](http://quantlet.de/)

## [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/qloqo.png" alt="Visit QuantNet">](http://quantlet.de/) **catvom_2021_01_calc_measures** [<img src="https://github.com/QuantLet/Styleguide-and-FAQ/blob/master/pictures/QN2.png" width="60" alt="Visit QuantNet 2.0">](http://quantlet.de/)

```yaml

Name of Quantlet: catvom_2021_01_calc_measures

Published in: 'Cryptocurrencies and the Velocity of Money (Pernice et al., 2021)'

Description: 'Takes raw data from Blockwatch, CoinmarketCap and the Bitcoin Blockchain (see the public repository https://github.com/trudi-group/ccurr_velocity of Weizenbaum Institut for further instructions), cleans it and prepares the variables needed for the further analysis (f.e. different velocity measures, prices, volatility, ...).' 

Keywords: data set, variable transformation, reading and writing data 

Author: Ingolf Pernice, Hermann Elendner, Georg Gentzen

See also: other Quantlets in this project

Submitted: 02.09.2023

Datafile: ts_datadump.rda

Output: tsdata.rda, tsdata_wOutl.rda
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
SETTINGS$path_tables             <- "../../../../text/ts_tables/"
SETTINGS$path_figs               <- "../../../../text/ts_figs/"
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


cleanOutliers <- function(num.sd = 10, df_raw, type = "forLapply"){
  
    if("time" %in% colnames(df_raw)){
        time <- df_raw$time
    }
    df <- df_raw[ ,!colnames(df_raw) %in% "time", drop = FALSE]
    
    threshold_upper <- function(y){thu <- mean(y, na.rm = TRUE) + num.sd*sd(y, na.rm = TRUE); return(thu)}
    
    df <- as.data.frame(lapply(df, function(x){replace(x, x > threshold_upper(x), threshold_upper(x))}))
    num_repl <- lapply(df, function(x){sum(x > threshold_upper(x), na.rm = TRUE)})
    names(num_repl) <- colnames(df)
    
    if("time" %in% colnames(df_raw)){
        df <- cbind(time, df)
    }

    if(type != "forLapply"){
    res <- list(df, num_repl)
    names(res) <- c("data", "number_of_replacements")
    }else{
        res <- df
    }
    
    return(res)
    
}


##################################################################
### STEP 4 - HELPER FUNCTIONS: SPECIFIC HELPER FUNCTIONS ### 
##################################################################
#' Helper function to scale a numeric vector with a multiplicator set in the gen. settings file and optionally calculation from BTC to Satoshis 
#' 
#' 
#' @param settings ... the path to the tsanalysis/SETTINGS.R config file.
#' @param type ... switch variable: currently only "satoshis_to_btc" as optinal scaling from Sathoshis to Bitcoin
#' @param inputvec ... a numeric vector to be scaled
#'
#' @return a scaled, numeric vector
#'
scaledown <- function(inputvec,
                      settings = SETTINGS,
                      type     = "satoshis_to_btc"){

    if(type == "satoshis_to_btc"){
        inputvec_scaled =  inputvec / (settings$scaling_multiplicator * settings$btc_per_satoshi)
    }else if(type == "no_exchangerate"){
        inputvec_scaled =  inputvec / settings$scaling_multiplicator
    }
    
    return(inputvec_scaled)
}

##################################################################
### STEP 5 - LOAD INPUT DATA ### 
##################################################################
setwd("/Users/ingolfpernice/Documents/catvom_2021/catvom_2021_01_calc_measures/")
load(file = paste0(SETTINGS$path_data, "ts_datadump.rda"))

##################################################################
### STEP 6 - SCRIPT ### 
##################################################################

##################################################################
### prepare basic variables from the dataframes
m_total        <- scaledown(dta$m_total)
m_circ_mc_lifo <- scaledown(dta$m_circ_mc_lifo_1)
m_circ_mc_fifo <- scaledown(dta$m_circ_mc_fifo_1)
m_circ_wb      <- scaledown(dta$m_circ_wh_bill_1)

tx_vol         <- scaledown(dta$tx_vol)
tx_vol_infl    <- scaledown(dta$tx_vol)
tx_vol_clean   <- scaledown(dta$tx_vol - dta$tx_vol_issues_chouts)

tx_number      <- scaledown(dta$tx_number, type="no_exchangerate")

cdd            <- scaledown(dta$days_destroyed, type="no_exchangerate")
dorm           <- cdd / tx_vol_infl
turnover       <- (1 / dorm)*365

volume_usd         <- scaledown(dta$volume_usd)
tx_fees            <- scaledown(dta$tx_fees)
marketcap          <- scaledown(dta$market_cap_by_available_supply)

price_usd          <- dta$price_usd
returns            <- dta$return_wrt_price_usd.simple
volatility         <- dta$vol.squaredreturns
views_wiki         <- dta$views_wiki

##################################################################
### prepare dataframe with target variables from the basic variables

data                      <- dta[ , "time", drop = FALSE]
data$v_est_m_naive        <- tx_vol_infl / m_total
data$v_est_m_total        <- tx_vol_clean / m_total
data$v_est_m_circ_wb      <- tx_vol_clean / m_circ_wb
data$v_est_m_circ_mc_lifo <- tx_vol_clean / m_circ_mc_lifo
data$v_est_m_circ_mc_fifo <- tx_vol_clean / m_circ_mc_fifo

data$v_app_coindd         <- cdd 
data$v_app_turnover       <- turnover
data$v_app_naive          <- tx_vol_infl/m_total

data$dormancy             <- dorm
data$turnover             <- turnover
data$m_total              <- m_total
data$m_circ_mc_lifo       <- m_circ_mc_lifo 
data$m_circ_mc_fifo       <- m_circ_mc_fifo 
data$m_circ_wh_bill       <- m_circ_wb
data$tx_vol               <- tx_vol 
data$tx_vol_infl          <- tx_vol_infl
data$tx_vol_clean         <- tx_vol_clean

data$volume_usd           <- volume_usd
data$tx_fees              <- tx_fees
data$tx_cdd               <- cdd
data$marketcap            <- marketcap

data$price_usd            <- price_usd
data$returns              <- returns
data$volatility           <- volatility
data$views_wiki           <- views_wiki

##################################################################
### Drop Inf and -Inf
data[sapply(data, is.infinite)] <- NA

##################################################################
### Save in smaller subsets for convenience
d <- list()
d$date       <- data$time
d$est        <- data[ ,str_detect(colnames(data), pattern = "v_est")]
d$est_old    <- data[ ,str_detect(colnames(data), pattern = "v_est_m_total|v_est_m_naive")]
d$est_new    <- data[ ,str_detect(colnames(data), pattern = "(v_est_m_circ)")]
d$app        <- data[ ,str_detect(colnames(data), pattern = "v_app")]
d$other      <- data[ ,!grepl("time", colnames(data)) & !grepl("v_est|v_app", colnames(data))]
d$all        <- data

##################################################################
### Clean outliers

d_nooutl_packed <- lapply(d, function(x) if(is.data.frame(x)) cleanOutliers(x, num.sd = 10) else x)

d_withoutl  <-d
d           <-d_nooutl_packed


##################################################################
### Save
save(d, file = paste0(SETTINGS$path_data, "tsdata.rda"))
save(d_withoutl, file = paste0(SETTINGS$path_data, "tsdata_wOutl.rda"))




```

automatically created on 2023-09-14