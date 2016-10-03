library(dplyr)
library(tidyr)
library(xlsx)
library(ggplot2)



# environment setup -------------------------------------------------------

setwd("/Users/major_minor1982/Documents/programming/R/bill/mobile_bill")
dir <- getwd()

import_folder <- "files" # set the import folder
export_folder <- "export" # set the export folder

# create a folder for exported
if ("./export" %in% list.dirs()){
} else {dir.create("export")
}

# convert from pdf to txt files -------------------------------------------
source("batchpdftxt.R")
batchpdftxt(import_folder, export_folder)

# read basic information --------------------------------------------------

dir <- getwd()
files <- list.files(file.path(dir, "export"))
file_dir <- file.path(dir, "export", files)

source("readbillingtxt.R")
data <- data_frame()
file <- 1
for (i in file_dir){
        print(paste("file ", file))
        data.temp <- readbillingtxt(i)        
        data <- rbind(data, data.temp)
        file <- file + 1
        
}        

data


