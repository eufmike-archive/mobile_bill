library(stringr)
source("greptest.R")

readbillingtxt <- function(txt_file){
        #  total amount -----------------------------------------------------------
        line <- readLines(txt_file)
        
        previous_balance <- line[grep("^Previous Balance", line) - 1]
        previous_balance <- as.numeric(gsub("\\$", "", previous_balance))
        print(paste("Previous Balance: $", previous_balance))

        total_amount_debited <- line[grep("^Amount to be Debited", line) - 1]
        total_amount_debited <- as.numeric(gsub("\\$", "", total_amount_debited))
        print(paste("Total Amount Debited: $", total_amount_debited))

        new_charge <- line[grep("^New Charges", line)]
        new_charge <- as.numeric(gsub("New Charges \\$", "", new_charge))
        print(paste("New Charges: $", new_charge))
        
        
        # find phone numbers ------------------------------------------------------
        
        end_list <- grep("^Total New Charges", line)[1]
        phone_string <- grep("^[0-9]{3} [0-9]{3}-[0-9]{3}", line)
        phone_string <- phone_string[phone_string < end_list]
        phone_list <- line[phone_string]
        phone_list <- gsub(" [0-9]$", "", phone_list)
        phone_list <- gsub(" CR", "", phone_list)  ##any combination of this string

        print("this is the list of phone number")
        print(phone_list)

        # # find date ---------------------------------------------------------------

        date_info <- strsplit(line[grep("^[0-9]{2}/[0-9]{2}/[0-9]{2}", line)][1], " ")[[1]]

        date_01 <- date_info[grepl("^[0-9]{2}/[0-9]{2}/[0-9]{2}$", date_info)]
        date_01 <- mdy(date_01)
        print(date_01)
        
        date_01 <- if (date_01[2] > date_01 [1]){ date_01 = date_01
        } else { date_01 <- c(date_01[2], date_01[1])
        }
        billing_start <- date_01[1]
        billing_end <- date_01[2]
        billing_day <- difftime(billing_end, billing_start, units = "days")
        billing_day <- as.numeric(billing_day) + 1
        print(billing_start)
        print(billing_end)
        print(billing_day)

        # # seperate phone info section ---------------------------------------------

        user_amount <- length(phone_list)

        data <- array()
        n <- 1
        for (i in phone_list){
                phone_line <- grep(i, line)
                print(line[phone_line])
                print(phone_line)
                
                start_line <- c()
                for (j in phone_line){
                        if (line[j + 1] %in% user_name){
                                start_line <- c(start_line, j)
                        }
                }
                print(start_line)
                print(start_line[1])
                print("start")
                print(line[start_line[1]])
                
                end_line <- grep(paste0("Total for ", i), line)
                print("end")
                print(line[end_line])
                section_line <- line[start_line[1]: end_line]

                # monthly charge
                mobile_share_value <- greptestvalue("Mobile Share Value(.*?)[0-9]{2}\\.[0-9]{2}$|Access for iPhone 4G LTE",
                                          section_line)

                discount_for_mobile_share <- greptestvalue("Discount for Mobile Share Value Savings|Discount for Access", section_line)

                international_roaming <- greptestvalue("International(.*?)[0-9]{1,2}", section_line)
                total_monthly <- greptestvalue("Total Monthly Charges", section_line)

                monthly_charge <- c(mobile_share_value, discount_for_mobile_share,
                                    international_roaming, total_monthly)

                # International Long Distance
                Inter_LD <- grep("International Long Distance", section_line)
                billed <- grep("Minutes Billed", section_line)
                if ((length(billed) == 0 || length(Inter_LD) == 0 )){
                        ILD_value = 0
                } else if (!billed %in% (Inter_LD+1)){
                        ILD_value = 0
                } else  {
                        ILD_value_temp <- section_line[billed[billed %in% (Inter_LD+1)[1]]]
                        ILD_value <- strsplit(ILD_value_temp, " ")[[1]]
                        ILD_value <- tail(ILD_value, 1)
                }

                # Roaming
                roaming <- grep("Roaming", section_line)
                billed <- grep("Minutes Billed", section_line)
                # print(roaming)
                # print(billed)
                # print(billed[billed %in% (roaming+1)])
                if ((length(billed) == 0 || length(roaming) == 0 )){
                        r_value = 0
                } else if (sum(billed %in% (roaming+1)) == 0){
                        r_value = 0
                } else  {
                        position <- billed[billed %in% (roaming+1)][1]
                        r_value_temp <- section_line[position ]
                        r_value <- strsplit(r_value_temp, " ")[[1]]
                        r_value <- tail(r_value, 1)
                }

                # Installment
                ins <- grep("Installment", section_line)
                # head(section_line)
                # print(ins)
                # print(section_line[ins[2]])
                if (length(ins) == 0){
                        ins_value = 0
                } else {
                        ins_value_temp <- section_line[ins[2]]
                        ins_value <- strsplit(ins_value_temp, " ")[[1]]
                        ins_value <- tail(ins_value, 1)
                }
                # print(ins_value)

                # Upgrade fee
                upgrade_fee <- grep("Upgrade Fee", section_line)
                # head(section_line)
                # print(upgrade_fee)
                # print(section_line[upgrade_fee[1]])
                if (length(upgrade_fee) == 0){
                        uf_value = 0
                } else {
                        uf_value_temp <- section_line[upgrade_fee[1]]
                        uf_value <- strsplit(uf_value_temp, " ")[[1]]
                        uf_value <- tail(uf_value, 1)
                }
                # print(uf_value)

                # Activation Fee
                activation_fee <- grep("Activation Fee", section_line)
                if (length(activation_fee) == 0){
                        af_value = 0
                } else {
                        af_value_temp <- section_line[activation_fee[1]]
                        af_value <- strsplit(af_value_temp, " ")[[1]]
                        af_value <- tail(af_value, 1)
                }

                # Text/Instant Msgs
                msg <- grep("Text/Instant Msgs", section_line)
                if (length(msg) == 0){
                        msg_value = 0
                } else {
                        msg_value_temp <- section_line[msg[1]]
                        msg_value <- strsplit(msg_value_temp, " ")[[1]]
                        msg_value <- tail(msg_value, 1)
                }

                # Data Overage
                Data_over <- grep("Data Overage", section_line)
                if (length(Data_over) == 0){
                        do_value = 0
                } else {
                        do_value_temp <- section_line[Data_over[1]]
                        do_value <- strsplit(do_value_temp, " ")[[1]]
                        do_value <- tail(do_value, 1)
                }

                # Plan Change
                Plan_change <- grep("Total Plan Changes", section_line)
                if (length(Plan_change) == 0){
                        pc_value = 0
                } else {
                        pc_value_temp <- section_line[Plan_change[1]]
                        pc_value <- strsplit(pc_value_temp, " ")[[1]]
                        pc_value <- tail(pc_value, 1)
                }


                # Total Surcharges and Other Fees
                total_sur <- greptestvalue("Total Surcharges and Other Fees", section_line)

                # Total Government Fees and Taxes
                total_gov_tax <- greptestvalue("Total Government Fees and Taxes", section_line)

                # Total
                total <- greptestvalue("Total for", section_line)
                total <- gsub("$", "", total)
                x <- c(monthly_charge,
                       msg_value,
                       ILD_value,
                       r_value,
                       ins_value,
                       uf_value,
                       af_value,
                       do_value,
                       pc_value,
                       total_sur,
                       total_gov_tax,
                       total)
                
                for (i in 1:length(x)){
                        if (grepl("CR", x[i])){
                                temp.x <- gsub("CR", "", x[i])
                                temp.x <- paste0("-", temp.x)
                                x[i] <- temp.x
                        } else {
                                
                        }
                        
                }

                print(x)
                data <- rbind(data, x)
                # print(n)
                n <- n + 1

        }

        data <- data.frame(data[-1, ])
        names <- c("Mobile.share.value",
                   "Discount.for.mobile.share",
                   "International.roaming",
                   "Total.monthly.charge",
                   "Text.Instant.Msgs",
                   "International.Long.Dis",
                   "Roaming",
                   "Installment",
                   "Upgrade.Fee",
                   "Activation.Fee",
                   "Data.Overage",
                   "Plan.Change",
                   "Total.Surcharges.and.Other.Fees",
                   "Total.Government.Fees.and.Taxes",
                   "Total"
        )

        colnames(data) <- names
        data$mobile.number <- phone_list
        data$billing.start <- billing_start
        data$billing.end <- billing_end
        data$billing.day <- billing_day

        data <- data[, c((length(names)+1): (length(names)+4), 1:length(names))]
        data$total.amount.debited <- total_amount_debited
        data$total.new.charge <- new_charge
        return(data)
        
}


