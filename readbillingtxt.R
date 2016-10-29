library(stringr)
source("greptest.R")

readbillingtxt <- function(txt_file){
        #  total amount -----------------------------------------------------------
        line <- readLines(txt_file)
        # print(line[grepl("^\\$[0-9]{3,4}\\.[0-9]{2}", line)])
        total_amount_debited <- line[grepl("^\\$[0-9]{3,4}\\.[0-9]{2}", line)][2]
        total_amount_debited <- gsub("\\$", "", total_amount_debited)
        
        location <- which(grepl("Total New Charges", line))
        total_n_chg <- gsub("Total New Charges \\$", "", line[location])
        # print(total_n_chg)
        
        
        # find phone numbers ------------------------------------------------------
        
        end_list <- grep("^Total New Charges", line)[1]
        phone_string <- grep("^[0-9]{3} [0-9]{3}-[0-9]{3}", line)
        phone_string <- phone_string[phone_string < end_list]
        phone_list <- line[phone_string]
        phone_list <- gsub(" [0-9]$", "", phone_list)
        # print("this is the phone list")
        # print(phone_list)
        
        # find date ---------------------------------------------------------------
        library(lubridate)
        date_info <- strsplit(line[grep("^[0-9]{2}/[0-9]{2}/[0-9]{2}", line)][1], " ")[[1]]
        date <- date_info[grepl("^[0-9]{2}/[0-9]{2}/[0-9]{2}$", date_info)]
        date <- mdy(date)
        
        date <- if (date[2] > date [1]){ date = date
        } else { date <- c(date[2], date[1])
        }
        
        billing_start <- date[1]
        billing_end <- date[2]
        billing_day <- difftime(billing_end, billing_start, units = "days")
        billing_day <- as.numeric(billing_day) + 1
        
        # seperate phone info section ---------------------------------------------
        
        user_amount <- length(phone_list)
        
        data <- array()
        n <- 1
        for (i in phone_list){
                phone_line <- grep(i, line)
                # print(line[phone_line])
                # print(phone_line)
                b <- grep("CHIEN CHENG", line)
                # print(line[b])
                # print(b)
                start_line <- phone_line[phone_line %in% (b-1)]
                # print("start")
                # print(start_line[1])
                # print(line[start_line[1]])
                end_line <- grep(paste0("Total for ", i), line)
                # print("end")
                # print(line[end_line])
                section_line <- line[start_line[1]: end_line]

                
                # monthly charge
                mobile_share_value <- greptestvalue("Mobile Share Value(.*?)[0-9]{2}\\.[0-9]{2}$|Access for iPhone 4G LTE", 
                                          section_line)
                
                discount_for_mobile_share <- greptestvalue("Discount for Mobile Share Value Savings|Discount for Access", section_line)
                
                
                if (grepl("CR$", discount_for_mobile_share)){
                        discount_for_mobile_share <- gsub("CR$", "", discount_for_mobile_share)
                        discount_for_mobile_share <- paste0("-", discount_for_mobile_share)
                } else {
                        
                }
                
                international_roaming <- greptestvalue("International(.*?)[0-9]{1,2}", section_line)
                total_monthly <- greptestvalue("Total Monthly Charges", section_line)
                
                monthly_charge <- c(mobile_share_value, discount_for_mobile_share, 
                                    international_roaming, total_monthly)
                
                # Total Surcharges and Other Fees
                total_sur <- greptestvalue("Total Surcharges and Other Fees", section_line)
                
                # Total Government Fees and Taxes
                total_gov_tax <- greptestvalue("Total Government Fees and Taxes", section_line)
                
                # Total 
                total <- greptestvalue("Total for", section_line)
                total <- gsub("$", "", total)
                x <- c(monthly_charge, total_sur, total_gov_tax, total)
                # print(x)
                data <- rbind(data, x)
                # print(n)
                n <- n + 1
                
        }
        
        data <- data.frame(data[-1, ])
        colnames(data) <- c("Mobile.share.value", 
                            "Discount.for.mobile.share", 
                            "International.roaming", 
                            "Total.monthly.charge",
                            "Total.Surcharges.and.Other.Fees",
                            "Total.Government.Fees.and.Taxes", 
                            "Total"
        )
        data$mobile.number <- phone_list 
        data$billing.start <- billing_start
        data$billing.end <- billing_end
        data$billing.day <- billing_day 
        
        data <- data[, c(8:11, 1:7)]
        data$total.amount.debited <- total_amount_debited         
        data$total.new.charge <- total_n_chg
        return(data)
        
}


