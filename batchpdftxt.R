## The following code converts pdf files to pdf by Xpdf. This R code is 
## dependent to Xpdf and X11. Please visit Xpdf website for more information. 
## http://www.foolabs.com/xpdf/
##
## Xpdf can also be install by Homebrew on MAC. After Homebrew is installed, 
## using follwing code to install Xpdf: brew install homebrew/x11/xpdf
## 
## Once Xpdf is installed, the code can be used for pdf converting. 
## The command for converting pdf to txt in Xpdf is "pdftotxt".

batchpdftxt <- function (import_folder, export_folder){
        files <- list.files(import_folder)
        file_dir <- file.path(import_folder, files)
        for (i in file_dir){
                command <- paste("pdftotext", "-raw", i, sep = " ")
                system(command)
        }
        
        files <- list.files(import_folder)
        files_txt <- files[grep(".txt$", files)]
        import <- file.path(import_folder, files_txt)
        export <- file.path(export_folder, files_txt)
        
        file.rename(import, export)
}
