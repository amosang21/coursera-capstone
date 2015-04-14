# Load libraries and perform other overheads.
library("stylo")
library("tm")
library("data.table")
library("ggplot2")
library("dplyr")
library("shiny")

source("functions.R")  # Utility functions here.
load("vars_final.RData")  # Loads the look-up tables, "dt_cft*".
