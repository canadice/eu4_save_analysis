require(stringr)

file <- readLines(con = "C:/Users/Canadice/Documents/Paradox Interactive/Europa Universalis IV/save games/Clash_of_Europe_7.eu4", encoding = "ANSI", warn = FALSE)

clean_file <- str_replace_all(file, pattern = "[\t\"]", replacement = "")

main.list <- list()
subfun <- function(istart, txt){
  
  sub.list <- list()
  sub.list <- c(sub.list, txt[istart])
  j = istart + 1
  while( !grepl("\\}", txt[j]) ){
    
    if ( grepl("\\{", txt[j]) ){
      x <- subfun(j, txt)
      sub.list <- c(sub.list, list(x$sub) )  # add sublist to the main list
      j=x$iend
      
      # regular record    
    } else {
      sub.list <- c(sub.list, txt[j] )
    }    
    j <- j+1
  }
  sub.list <- c(sub.list, txt[j])
  return(list(sub=sub.list, iend=j))
}

temp <- subfun(1, clean_file)

