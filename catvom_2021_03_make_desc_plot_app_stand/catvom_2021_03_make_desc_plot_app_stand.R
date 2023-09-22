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

wDate <- function(df){
  df <- cbind(d$date,df)
  colnames(df)[1] <- "date"
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
#' Function getting converting a customized dataframe with descriptives to latex code  
#' 
#' 
#' @param SETTINGS ... the path to the tsanalysis/SETTINGS.R config file.
#' @param df_melted ... dataframes produced with the melt function of the reshape packae.
#' @param xaxis_legend ... legend of the x-axis
#' @param yaxis_legend ... legend of the y-axis
#' @param storage_name ... path to store figure 
#' @param legendrows ... number of rows in which to store legend
#' @param palette ... color palette
#' 
#'
#' @return ... -
#' 
make_plot_desc <- function(SETTINGS,
                           df_melted,
                           xaxis_legend,
                           yaxis_legend,
                           storage_name,
                           legendrows = 2,
                           palette = SETTINGS$palette_3,
                           diagram_title){
  if(SETTINGS$titles_for_plots == FALSE){diagram_title <- NULL}
  tikz(file = paste0(SETTINGS$path_figs, storage_name, ".tex"),
       width = 3.5,
       height = 3.5)
  print(
    ggplot(df_melted, aes(x=Date, y=value, col=variable)) +
      geom_line()+
      ggtitle(diagram_title)+
      xlab(xaxis_legend) +
      ylab(yaxis_legend)+
      scale_colour_manual(name = FALSE, values=palette) +
      theme(legend.position="bottom",
            plot.background = element_rect(fill = "white"),
            panel.background = element_rect(fill = "white"),
            axis.line.x = element_line(color = "black"),
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid', colour = "grey"),
            legend.title=element_blank()) +
      guides(colour=guide_legend(nrow=legendrows,byrow=TRUE))
    
  )
  dev.off()
}



##################################################################
### STEP 5 - LOAD INPUT DATA ### 
##################################################################
setwd("/Users/ingolfpernice/Documents/catvom_2021/catvom_2021_03_make_desc_plot/")
load(file = paste0(SETTINGS$path_data, "tsdata.rda"))

##################################################################
### STEP 6 - SCRIPT ### 
##################################################################

##################################################################
## construct melted plotting dataframes for ESTimators
v_est_melted     <- melt(rename_df(wDate(d$est)), id.var = "Date")
v_nest_melted    <- melt(rename_df(wDate(normalize_df(d$est))), id.var = "Date")
v_sest_melted    <- melt(rename_df(wDate(standardize_df(d$est))), id.var = "Date")
v_est_old_melted <- melt(rename_df(wDate(d$est_old)), id.var = "Date")
v_est_new_melted <- melt(rename_df(wDate(d$est_new)), id.var = "Date")

##################################################################
## construct plotting dataframes for APProximators
indic_excl_appnaive <- !colnames(d$app) %in% "v_app_naive"
v_app_melted        <- melt(rename_df(wDate(d$app[indic_excl_appnaive])), id.var = "Date")
v_napp_melted       <- melt(rename_df(wDate(normalize_df(d$app[indic_excl_appnaive]))), id.var = "Date")
v_sapp_melted       <- melt(rename_df(wDate(standardize_df(d$app[indic_excl_appnaive]))), id.var = "Date")
v_app_old_melted    <- melt(rename_df(wDate(d$app_old[indic_excl_appnaive])), id.var = "Date")
v_app_new_melted    <- melt(rename_df(wDate(d$app_new[indic_excl_appnaive])), id.var = "Date")

##################################################################
## construct plotting dataframes for other mixtures
# ... for different trading volume data
vol_melted  <- melt(rename_df(wDate(get_sub_df(df      = d$all,
                                               pattern = c("tx_vol_infl", "tx_vol_clean"),
                                               type    = "exact"))), id.var = "Date")

nvol_melted <- melt(rename_df(wDate(normalize_df(get_sub_df(df      = d$all,
                                                            pattern = c("tx_vol_infl", "tx_vol_clean"),
                                                            type    = "exact")))), id.var = "Date")

svol_melted <- melt(rename_df(wDate(standardize_df(get_sub_df(df      = d$all,
                                                              pattern = c("tx_vol_infl", "tx_vol_clean"),
                                                              type    = "exact")))), id.var = "Date")

comp_melted  <- melt(rename_df(wDate(get_sub_df(df       = d$all,
                                                pattern  = c("tx_vol_clean", "m_total", "m_circ_wh_bill"),
                                                type     = "exact"))), id.var = "Date")

ncomp_melted <- melt(rename_df(wDate(normalize_df(get_sub_df(df       = d$all,
                                                             pattern  = c("tx_vol_infl", "tx_vol_clean"),
                                                             type     = "exact")))), id.var = "Date")

scomp_melted <- melt(rename_df(wDate(standardize_df(get_sub_df(df      = d$all,
                                                               pattern = c("tx_vol_infl", "tx_vol_clean"),
                                                               type    = "exact")))), id.var = "Date")


##################################################################
## How Do the new measures look like
# Stnd. old and new velocity approximations
make_plot_desc(SETTINGS      = SETTINGS,
               df_melted     = v_sapp_melted,
               xaxis_legend  = 'Date',
               yaxis_legend  = 'Stnd. est. avg. turnovers',
               diagram_title = "Stnd. old and new velocity approximations",
               storage_name  = "desc_all_app_stand")



