## greptest

greptestvalue <- function(pattern, x){
        line_number <- grep(pattern, x)
        if (length(line_number) == 0){
                value <- 0 
        } else {
                value <- strsplit(x[line_number], " ")[[1]]
                value <- tail(value, 1) 
        }
        
        return(value)
}