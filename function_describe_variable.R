describe.variable <- function(type.variable, target.variable, ...) {
  
  # Creates a data frame with descriptive statistics which is used for creating graphs
  #
  # Args:
  #     target.variable:  a numeric variable for which descriptive statics is calculated
  #     type.variable:    a character or factor variable in terms of which descriptive
  #                       statistics is calculated
  #     ...:              other numeric variables for which mean is calculated in terms of
  #                       type.variable
  #     all variables must have the same length with no missing values
  #
  # Returns:
  #     90% percentile, 10% percentile, median, mean, number of observations of 
  #     target.variable, mean of "..." variables in terms of type.variable
  
  
  # 90% percentile
  sample.9 <- as.data.frame(tapply(target.variable, type.variable, quantile, na.rm=TRUE, probs=c(0.9)))
  sample.9 <- na.omit(sample.9)
  colnames(sample.9) <- c("perc.90")
  sample.9$perc.90 <- round(sample.9$perc.90, digits=1)
  sample.9$target <- rownames(sample.9)
  
  
  # 10% percentile
  sample.1 <- as.data.frame(tapply(target.variable, type.variable, quantile, na.rm=TRUE, probs=c(0.1)))
  sample.1 <- na.omit(sample.1)
  colnames(sample.1) <- c("perc.10")
  sample.1$perc.10 <- round(sample.1$perc.10, digits=1)
  sample.1$target <- rownames(sample.1)
  
  
  # median
  sample.median <- as.data.frame(tapply(target.variable, type.variable,median))
  sample.median <- na.omit(sample.median)
  colnames(sample.median) <- c("median")
  sample.median$median <- round(sample.median$median, digits=1)
  sample.median$target <- rownames(sample.median)
  
  
  # mean
  sample.mean <- as.data.frame(tapply(target.variable, type.variable,mean))
  sample.mean <- na.omit(sample.mean)
  colnames(sample.mean) <- c("mean")
  sample.mean$mean <- round(sample.mean$mean, digits=1)
  sample.mean$target <- rownames(sample.mean)
  
  
  # number of observations
  sample.count <- as.data.frame(tapply(type.variable, type.variable,length))
  sample.count <- na.omit(sample.count)
  colnames(sample.count) <- c("quantity")
  sample.count$target <- rownames(sample.count)

  
  # merge data frames
  sample.graph <- merge(sample.median, sample.mean, by="target",all=TRUE)
  sample.graph <- merge(sample.graph, sample.1, by="target",all=TRUE)
  sample.graph <- merge(sample.graph, sample.9, by="target",all=TRUE)
  sample.graph <- merge(sample.graph, sample.count, by="target",all=TRUE)
  
  other.varibales <- list(...)
  
  if (length(other.varibales)>0) {
    
      for (i in 1:length(other.varibales)) {
    
        other.mean <- as.data.frame(tapply(other.varibales[[i]], type.variable,mean))
        other.mean <- na.omit(other.mean)
        colnames(other.mean) <- c(paste("mean.",i,sep=""))
        other.mean[,1] <- round(other.mean[,1], digits=1)
        other.mean$target <- rownames(other.mean)
    
        sample.graph <- merge(sample.graph, other.mean, by="target",all=TRUE)
    }
  }
  
  
  sample.graph
}