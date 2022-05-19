#' -----------------------------------------------------------------------------
#' @title:   main.R
#' @project: Credit Shocks and Populism
#' @author:  Alessandro Pizzigolotto (NHH)
#' @version: 1.0
#' -----------------------------------------------------------------------------
#' This is the main file for running the workflow of the entire project
#' "Credit Shocks and Populism". We integrate here sequentially all the scripts
#' necessary to run all the steps of the data generating process and analysis.
#' Be aware that some of the scripts (the Stata scripts in the directory
#' `jobs`) are executed in the SOEPRemote server sending the code via email
#' through OAUTH SMTP request. I have made a function to send automatically the
#' jobs, also integrated in a script called by this file.

# TODO add scripts as source and within those scripts there is the call
# to the functions they use
