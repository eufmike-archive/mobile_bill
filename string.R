library(stringr)

readbillingtxt <- function(txt_file){
        #  total amount -----------------------------------------------------------
        line <- readLines(txt_file)
        total_amount <- line[grepl("^\\$[0-9]{3,4}\\.[0-9]{2}", line)][1]
        
        # find phone numbers ------------------------------------------------------
        
        end_list <- grep("^Total New Charges", line)[1]
        phone_string <- grep("^[0-9]{3} [0-9]{3}-[0-9]{3}", line)
        phone_string <- phone_string[phone_string < end_list]
        phone_list <- line[phone_string]
        phone_list <- gsub(" [0-9]$", "", phone_list)
        
        
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
                b <- grep("^Mobile Share Value iPhone", line)
                start_line <- phone_line[phone_line %in% (b-2)]
                end_line <- grep(paste0("Total for ", i), line)
                
                section_line <- line[start_line: end_line]
                
                # monthly charge
                mobile_share_value <- strsplit(
                        section_line[grep("Mobile Share Value iPhone(.*?)[0-9]{2}\\.[0-9]{2}", 
                                          section_line)], " ")[[1]]
                mobile_share_value <- tail(mobile_share_value, 1)
                
                discount_for_mobile_share <- strsplit(section_line[grep("Discount for Mobile Share Value Savings", section_line)], " ")[[1]]
                discount_for_mobile_share <- tail(discount_for_mobile_share, 1)
                
                international_roaming <- strsplit(section_line[grep("International(.*?)[0-9]{1,2}", section_line)], " ")[[1]]
                international_roaming <- tail(international_roaming, 1)
                
                total_monthly <- strsplit(section_line[grep("Total Monthly Charges", section_line)], " ")[[1]]
                total_monthly <- tail(total_monthly, 1)
                
                monthly_charge <- c(mobile_share_value, discount_for_mobile_share, 
                                    international_roaming, total_monthly)
                
                # Total Surcharges and Other Fees
                total_sur <- strsplit(section_line[grep("Total Surcharges and Other Fees", section_line)], " ")[[1]]
                total_sur <- tail(total_sur, 1)
                
                # Total Government Fees and Taxes
                total_gov_tax <- strsplit(section_line[grep("Total Government Fees and Taxes", section_line)], " ")[[1]]
                total_gov_tax <- tail(total_gov_tax, 1)
                
                # Total 
                total <- strsplit(section_line[grep("Total for", section_line)], " ")[[1]]
                total <- tail(total, 1)  
                
                x <- c(monthly_charge, total_sur, total_gov_tax, total)
                data <- rbind(data, x)
                print(n)
                n <- n+1
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
        data$total.amount <- total_amount
        
        
        return()
        
}


