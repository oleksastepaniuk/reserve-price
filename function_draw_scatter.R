draw.scatter <- function(dataset, lm.true,
                         x.variable, y.variable,
                         y.start, y.end, y.break,
                         subtitle.text, 
                         x.title, y.title, caption.text) {
  
  # Creates a scatter plot with a line of a mean
  #
  # Args:
  #     dataset:              data frame used for creating graph
  #     lm.true:              boolean value, if TRUE - lm method for fitting line is used,
  #                                          if FALSE - loess method is used
  #     x.variable:           numeric variable used for the x dimension
  #     y.variable:           numeric variable used for the y dimension
  #     y.start:              first value of the y axis, number
  #     y.end:                last value of the y axis, number
  #     y.break:              interval the y axis, number
  #     subtitle.text:        subtitle text, string value
  #     x.title:              x axis title, string value
  #     y.title:              y axis title, string value
  #     caption.text:         caption text, string value
  
  
  # graph theme
  theme1 <-  theme(panel.background = element_rect(fill = "#fee8c8"),
                   plot.title = element_text(size = rel(1.75), colour = "black"),
                   plot.subtitle = element_text(size = rel(1.25)),
                   axis.title = element_text(size = rel(1.25)),
                   panel.grid.minor =  element_blank())
  
  
  # create graph
  graph <- ggplot(data=dataset, aes(x=x.variable)) + 
    
           geom_point(aes(y=y.variable), color = "#fdbb84") +
           geom_smooth(aes(y=y.variable), color="#e34a33", size=1.25, linetype=2, se=FALSE, method=if(lm.true==TRUE){lm}else{loess}) +
           
           scale_y_continuous(limits = c(y.start, y.end), breaks=seq(y.start, y.end, y.break)) +
    
           labs(x = x.title, y = y.title, 
                subtitle = subtitle.text,
                caption = caption.text) +
           theme1
  
  graph
  
}



