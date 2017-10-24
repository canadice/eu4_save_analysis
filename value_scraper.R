# Values from tags inside the save file 

information_finder <- function(vector, pattern){
  # Since continent is a vector of six 0s and 1s this one is different
  if(pattern != "continent"){
    if(!any(str_detect(vector, pattern))){
      return(NA)
    } else {
      data <- vector[which(str_detect(vector, pattern))]
      data <- unlist(str_split(data, pattern = "="))[2]
    }
    
  # Otherwise all the other standard values wanted should be right after the information tag (pattern) and =
  } else {
    data <- vector[which(str_detect(vector, pattern))+1]
    
    data <- str_trim(str_sub(data, start = 4), side = "right")
    
    data <- str_extract(unlist(str_split(data, " ")), "[0-9]")
    
    if(any(data == 1)){
      data <- which(data == 1)[1]  
    } else {
      return(NA)
    }
  }
  return(data)
}