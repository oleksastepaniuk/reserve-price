mode <- function(data, target, value) {
  
  # Finds mode of "value" for each unique value of "target"
  
  mode.data <- as.data.frame(unique(data[,target]))
  colnames(mode.data) <- "target"
  mode.data$value <- NA
  
  for (i in mode.data$target) {
    loop.data <- subset(data, data[,target]==i)
    
    unique.value  <- unique(loop.data[,value])
    
    mode.data$value[which(mode.data$target==i)] <- unique.value[which.max(tabulate(match(loop.data[,value], unique.value)))]
  }
  
  mode.data
}