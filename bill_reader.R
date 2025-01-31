library(dplyr)
library(tidyr)
library(rJava)
library(xlsx)
library(ggplot2)
library(lubridate)
# environment setup -------------------------------------------------------

user <- c("Shiauhan", 
          "Mike", 
          "Yiwen",
          "Jerry",
          "Yenyu",
          "Abbie",
          "Mike's mom",
          "Zi-Fu Wang_1", 
          "Zi-Fu Wang_2")

mobile.number <- c("202 615-7195", 
                   "202 615-7277",
                   "202 812-4215", 
                   "561 568-4121",
                   "561 685-7195",
                   "919 627-3815",
                   "561 797-3675", 
                   "561 317-8725", 
                   "561 410-2979"
)

user_name <- c("SHIAUHAN LI", 
               "CHIEN CHENG SHIH", 
               "YI-WEN WANG", 
               "LI-JIE WANG", 
               "",
               "",
               "SHIH CHIEN CHENG", 
               "ZI FU WANG",
               "ZI FU WANG")

user_data <- data.frame(user, mobile.number, user_name)
write.csv(user_data, "user_data.csv")

setwd("/Users/michaelshih/Documents/code/mobile_bill/")
dir <- getwd()

pdf_input_dir <- file.path(dir, "files") # set the import folder
pdf_output_dir <- file.path(dir, "txt_export") # set the export folder

# create a folder for exported
if ("./export" %in% list.dirs()){
} else {dir.create("txt_export")
}

# convert from pdf to txt files -------------------------------------------
source("batchpdftxt.R")
batchpdftxt(pdf_input_dir, pdf_output_dir)

# read basic information --------------------------------------------------

dir <- getwd()
files <- list.files(pdf_output_dir)
file_dir <- file.path(pdf_output_dir, files)

source("readbillingtxt.R")
data <- data_frame()
file <- 1
for (i in file_dir){
        print(paste("file", file_dir[file]))
        data.temp <- readbillingtxt(i)        
        data <- rbind(data, data.temp)
        print(data.temp)
        file <- file + 1
         
}        

data1 <- tbl_df(data)
write.csv(data1, "01_before_adjusted.csv")

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

data2 <- data2[data2$Total.monthly.charge != 0, ]
data2

write.csv(data2, "02_with_phone_base.csv")

# monthly share charge ----------------------------------------------------

data_share <- data2 %>% 
                group_by(., billing.start) %>%
                summarize(., average = mean(share_chg), 
                          total.count = n(), 
                          share.cost = sum(share_chg))
data_share

data_share$average <- round(data_share$average, 2)
data3 <- merge(data2, data_share)

data3 <- data3 %>% mutate(., mod.Total = phone_base + 
                                  average + 
                                  Total.Surcharges.and.Other.Fees + 
                                  Total.Government.Fees.and.Taxes +
                                  Installment +
                                  Upgrade.Fee +
                                  Activation.Fee +
                                  Text.Instant.Msgs +
                                  Data.Overage +
                                  International.Long.Dis +
                                  Roaming +
                                  Plan.Change
                                        )

write.csv(data3, "03_after_adjusted.csv")


# data checking -----------------------------------------------------------

data_total_check <- data3 %>%
                        group_by(., billing.start) %>%
                        summarize(., total_check = sum(Total),
                                  total.mod_check = sum(mod.Total),
                                  total.debited = mean(total.amount.debited),
                                  total.new.charge = mean(total.new.charge)
                                  )


all(data_total_check$total_check == data_total_check$total.debited)
all(data_total_check$total_check == data_total_check$total.new.charge)
 
data_total_check <- data_total_check %>%
                        mutate(delta = round((total_check - total.mod_check), 2))

data_total_check

write.csv(data_total_check, "data_total_check.csv")

# data processing ---------------------------------------------------------

data_output <- data3 %>% 
                group_by(., mobile.number) %>%
                summarize(., sum(mod.Total))


name_number <- data_frame(user, mobile.number)

data_output <- merge(name_number, data_output)

data_output

write.csv(data_output, "mobile_bill.csv")
write.csv(data3, "mobile_bill_data.csv")

