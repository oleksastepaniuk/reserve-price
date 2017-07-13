
draw.ribbon <- function(dataset,
                        x.variable, predict.variable, ribbon.min, ribbon.max,
                        x.start, x.end, x.labels,
                        y.start, y.end, y.break,
                        subtitle.text, 
                        x.title, y.title, caption.text) {
  
  # Creates a graph with a ribbon and a line
  #
  # Args:
  #     dataset:              data frame used for creating graph
  #     x.variable:           numeric variable used for the x dimension
  #     predict.variable:     numeric variable used for the y dimension of a line
  #     ribbon.min:           numeric variable with the minimum y values of a ribbon
  #     ribbon.max:           numeric variable with the maximum y values of a ribbon
  #     x.start:              first value of the x axis, number
  #     x.end:                last value of the x axis, number
  #     x.labels:             string labels of the x axis, string variable
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
  graph <-  ggplot(data = dataset, aes(x = x.variable)) +
    
    geom_ribbon(aes(ymin = ribbon.min, ymax = ribbon.max), fill = "#fdbb84") +
    
    geom_line(aes(y=predict.variable),  size=1.5, colour="#e34a33") +
    
    scale_x_continuous(limits = c(x.start, x.end), breaks=c(x.start:x.end), labels=x.labels[x.start:x.end]) +
    
    scale_y_continuous(limits = c(y.start, y.end), breaks=seq(y.start, y.end,y.break)) +
    
    labs(x = x.title, y = y.title,
         subtitle = subtitle.text,
         caption = caption.text) +
    theme1
  
  graph
  
}