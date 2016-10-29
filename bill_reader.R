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
        print(paste("file", file))
        data.temp <- readbillingtxt(i)        
        data <- rbind(data, data.temp)
        file <- file + 1
}        

data1 <- tbl_df(data)
write.csv(data1, "data1.csv")


# factor to numeric -------------------------------------------------------

data2 <- data1

names <- colnames(data2)[5:dim(data2)[2]]

for (i in names){
        data2[, i] <- as.numeric(sapply(data2[, i], as.character))
}


# add monthly base amount --------------------------------------------------

data2 <- data2 %>% mutate(phone_base = ifelse(Total.monthly.charge == 40, 40, 
                ifelse(Total.monthly.charge == 0, 0 ,15))) %>%
                mutate(share_chg = Total.monthly.charge - phone_base)

write.csv(data2, "data2.csv")

data2 <- data2[data2$Total.monthly.charge != 0, ]
data2

write.csv(data2, "data2.csv")
# monthly share charge ----------------------------------------------------

data_share <- data2 %>% 
                group_by(., billing.start) %>%
                summarize(., average = mean(share_chg), 
                          total.count = n(), 
                          share.cost = sum(share_chg))
data_share

data_share$average <- round(data_share$average, 2)
data3 <- merge(data2, data_share)

data3 <- data3 %>% mutate(., mod.Total = phone_base + average + Total.Surcharges.and.Other.Fees + Total.Government.Fees.and.Taxes)

write.csv(data3, "data3.csv")


# data checking -----------------------------------------------------------

data_total_check <- data3 %>%
                        group_by(., billing.start) %>%
                        summarize(., total_check = sum(Total),
                                  total.mod_check = sum(mod.Total),
                                  total.debited = mean(total.amount.debited),
                                  total.new.charge = mean(total.new.charge)
                                  )
data_total_check

all(data_total_check$total_check == data_total_check$total.debited)
all(data_total_check$total_check == data_total_check$total.new.charge)

# data processing ---------------------------------------------------------

 

