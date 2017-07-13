

################################################################################
###########     Initial Settings. Downloading data and functions    ############
################################################################################

# import packages
library(ggplot2)
library(stargazer)
library(car)

# numbers are printed in a fixed notation, unless they are more than scipen digits wider
options(scipen=50)

# set the number of digits to print when printing numeric values to 3
options(digits=3)

# set thousands deliminator
options(big.mark = ",")

# working directory
setwd("C:/Users/Oleksa/Documents/Reserve_price")

# download data
data.paper <- read.csv2("data_paper.csv")

# download functions
eval(parse("function_draw_scatter.R", encoding="UTF-8"))
eval(parse("function_draw_ribbon.R", encoding="UTF-8"))
eval(parse("function_multiplot.R", encoding="UTF-8"))
eval(parse("function_describe_variable.R", encoding="UTF-8"))







################################################################################
###########                    Calculate variables                   ###########
################################################################################

# calculate reserve price, UAH per one egg
data.paper$rp <- data.paper$expected.value / data.paper$quantity

# calculate zero round price, UAH per one egg
data.paper$r0 <- data.paper$round.0 / data.paper$quantity

# calculate third round price, UAH per one egg
data.paper$r3 <- data.paper$round.3 / data.paper$quantity



# zero round economy, %
data.paper$gain.r0 <- (1-data.paper$r0/data.paper$rp)*100

# economy of the dynamic auction, %
data.paper$gain <- (1-data.paper$r3/data.paper$r0)*100



# size of procurement - 100 of packeges
data.paper$size <- data.paper$quantity / 100



# change format of date variable from factor to Date
data.paper$date <- as.Date(as.character(data.paper$date), "%d.%m.%Y")

# create numeric varible for the progression of months: "01/2016"=1
data.paper$date.month <- format(data.paper$date, format="%m/%Y")
data.paper$date.numeric <- (as.numeric(substr(data.paper$date.month, 4, 7))-2016)*12
data.paper$date.numeric <- data.paper$date.numeric + as.numeric(substr(data.paper$date.month, 1, 2))
data.paper$date.month <- NULL





################################################################################
###########  plots of Reserve price and Zero round price over time   ###########
################################################################################


# graph: reserve price over time
reserve.time.series <- draw.scatter(data.paper, FALSE,
                                    data.paper$date, data.paper$rp,
                                    50, 110, 20,
                                    "Закупівлі паперу А4, 1011 закупівель", 
                                    "Дата оголошення аукціону", "Резервна ціна, грн. за пачку", "")


# graph: zero round price over time
zero.time.series <- draw.scatter(data.paper, FALSE,
                                 data.paper$date, data.paper$r0,
                                 50, 110, 20,
                                 "", 
                                 "Дата оголошення аукціону", "Ціна нульового раунду, грн. за пачку", "")

# graph: third round price over time
third.time.series <- draw.scatter(data.paper, FALSE,
                                 data.paper$date, data.paper$r3,
                                 50, 110, 20,
                                 "", 
                                 "Дата оголошення аукціону", "Ціна третього раунду, грн. за пачку", "Період: 01/2016-01/2017")


# draw graphs
multiplot(reserve.time.series, third.time.series, zero.time.series, cols=2)


# delete unnecessary data
rm(reserve.time.series, third.time.series, zero.time.series)


# graph: zero round economy over time
economy.time.series <- draw.scatter(data.paper, FALSE,
                                    data.paper$date, data.paper$gain.r0,
                                    0, 60, 10,
                                    "Закупівлі паперу А4, 1011 закупівель", 
                                    "Дата оголошення аукціону", "Зниження ціни в нульовому раунді, %", "Період: 01/2016-01/2017")

# draw graphs
economy.time.series

# delete unnecessary data
rm(economy.time.series)





################################################################################
### Impact of Reserve price an Number of participants on zero round price    ###
################################################################################


# regression
result <- lm(log(r0) ~ log(rp) + count + log(date.numeric), data=data.paper)
summary(result)

# check for multicollinearity
vif(result)
cor(data.paper$rp, data.paper$date.numeric)

# save regression results
stargazer(result, type="text", title="Закупівля паперу А4",
          dep.var.labels=c("Ціна нульового раунду, грн. за пачку"),
          covariate.labels=c("Резервна ціна, % зміна",
                             "Кількість учасників",
                             "Ефекту часу, % зміна"),
          out="paper_r0.txt")



###################################
############ graph: impact of reserve price on zero round price
###################################

# categories of rp for the graph
data.paper$rp.category <- ifelse(data.paper$rp<70, "<70",
                          ifelse(data.paper$rp<75, "70-75",
                          ifelse(data.paper$rp<80, "75-80",
                          ifelse(data.paper$rp<85, "80-85",">85"))))


# data set that describes relationship
paper.r0 <- describe.variable(data.paper$rp.category, data.paper$r0, 
                              data.paper$rp, data.paper$count, data.paper$date.numeric)
paper.r0$count <- c(1,5,2,3,4)
paper.r0 <- paper.r0[order(paper.r0$count),]


# predict values of r0
paper.r0$predict <- result$coefficients[[1]]

paper.r0$predict <- paper.r0$predict +
                       rowSums(cbind(result$coefficients[[2]]*log(paper.r0$mean.1),
                                     result$coefficients[[3]]*paper.r0$mean.2,
                                     result$coefficients[[4]]*log(paper.r0$mean.3)))
                                            
paper.r0$predict <- exp(paper.r0$predict)

                                       

# create graph
reserve.r0 <-  draw.ribbon(paper.r0,
                           paper.r0$count, paper.r0$predict, paper.r0$perc.10, paper.r0$perc.90,
                           1, 5, paper.r0$target,
                           60, 100, 10,
                           "Закупівлі паперу А4, 1011 закупівель",
                           "Резервна ціна, грн. за пачку", "Ціна нульового раунду, грн. за пачку", "")




###################################
############ graph: impact of number of participants on zero round price
###################################

## categories of count for the graph
data.paper$count.category <- ifelse(data.paper$count==2, "2",
                             ifelse(data.paper$count==3, "3",
                             ifelse(data.paper$count==4, "4",">4")))


# data set that describes relationship
paper.count.r0 <- describe.variable(data.paper$count.category, data.paper$r0, 
                                    data.paper$rp, data.paper$count, data.paper$date.numeric)
paper.count.r0$count <- c(4,1,2,3)
paper.count.r0 <- paper.count.r0[order(paper.count.r0$count),]


# predict values of r0
paper.count.r0$predict <- result$coefficients[[1]]

paper.count.r0$predict <- paper.count.r0$predict +
                          rowSums(cbind(result$coefficients[[2]]*log(paper.count.r0$mean.1),
                                        result$coefficients[[3]]*paper.count.r0$mean.2,
                                        result$coefficients[[4]]*log(paper.count.r0$mean.3)))

paper.count.r0$predict <- exp(paper.count.r0$predict)


# create graph
count.r0 <-  draw.ribbon(paper.count.r0,
                         paper.count.r0$count, paper.count.r0$predict, paper.count.r0$perc.10, paper.count.r0$perc.90,
                         1, 4, paper.count.r0$target,
                         60, 100, 10,
                         "",
                         "Кількість учасників аукціону", "Ціна нульового раунду, грн. за пачку", "Період: 01/2016-01/2017")

# draw graphs
multiplot(reserve.r0, count.r0, cols=2)

# delete unnecessary data
rm(paper.r0, paper.count.r0)
rm(reserve.r0,count.r0)
rm(result)





################################################################################
# IMPACT of Reserve price and Number of participants on the Economy of the auction
################################################################################


# regression
result <- lm(gain ~ log(rp) + log(count), data=data.paper)
summary(result)


# save regression results
stargazer(result, type="text", title="Закупівля паперу А4",
          dep.var.labels=c("Зменшення ціни в R3 порівнянно з R0, %"),
          covariate.labels=c("Резервна ціна, % зміна",
                             "Кількість учасників, % зміна"),
          out="paper_gain.txt")



###################################
############ graph: impact of reserve price on the economy of the auction
###################################

# data set that describes relationship
paper.gain <- describe.variable(data.paper$rp.category, data.paper$gain, 
                                data.paper$rp, data.paper$count)
paper.gain$count <- c(1,5,2,3,4)
paper.gain <- paper.gain[order(paper.gain$count),]


# predict values of gain
paper.gain$predict <- result$coefficients[[1]]

paper.gain$predict <- paper.gain$predict +
                      rowSums(cbind(result$coefficients[[2]]*log(paper.gain$mean.1),
                                    result$coefficients[[3]]*log(paper.gain$mean.2)))



# create graph
graph.rp.economy <-  draw.ribbon(paper.gain,
                                 paper.gain$count, paper.gain$predict, paper.gain$perc.10, paper.gain$perc.90,
                                 1, 5, paper.gain$target,
                                 0, 30, 5,
                                 "Закупівлі паперу А4, 1011 закупівель",
                                 "Резервна ціна, грн. за пачку", "Зменшення ціни в R3 порівнянно з R0, %", "")



###################################
############ graph: impact of number of participants on the economy of the auction
###################################


# categories of count for the graph
data.paper$count.category <- ifelse(data.paper$count==2, "2",
                             ifelse(data.paper$count==3, "3",
                             ifelse(data.paper$count==4, "4", ">4")))


# data set that describes relationship
paper.count.gain <- describe.variable(data.paper$count.category, data.paper$gain,
                                      data.paper$rp, data.paper$count)
paper.count.gain$count <- c(4,1,2,3)
paper.count.gain <- paper.count.gain[order(paper.count.gain$count),]


# predict values of gain
paper.count.gain$predict <- result$coefficients[[1]]

paper.count.gain$predict <- paper.count.gain$predict +
                            rowSums(cbind(result$coefficients[[2]]*log(paper.count.gain$mean.1),
                                    result$coefficients[[3]]*log(paper.count.gain$mean.2)))

# create graph
graph.count.economy <-  draw.ribbon(paper.count.gain,
                                    paper.count.gain$count, paper.count.gain$predict, paper.count.gain$perc.10, paper.count.gain$perc.90,
                                    1, 4, paper.count.gain$target,
                                    0, 30, 5,
                                    "",
                                    "Кількість учасників аукціону", "Зменшення ціни в R3 порівнянно з R0, %", "Період: 01/2016-01/2017")


# draw graphs
multiplot(graph.rp.economy, graph.count.economy, cols=2)

# delete unnecessary data
rm(graph.rp.economy, graph.count.economy)
rm(paper.gain,paper.count.gain)
rm(result)





################################################################################
###           IMPACT of Reserve price on Number of participants             ####
################################################################################

# regression
result <- lm(count ~ log(rp) + log(size) + date.numeric, data=data.paper)
summary(result)


# save regression results
stargazer(result, type="text", title="Закупівля паперу А4",
          dep.var.labels=c("Кількість учасників"),
          covariate.labels=c("Резервна ціна, % зміна",
                             "Розмір закупівлі, % зміна",
                             "Ефект часу"),
          out="paper_count.txt")



###################################
############ graph: impact of reserve price the on number of participants
###################################


# data set that describes relationship
paper.count <- describe.variable(data.paper$rp.category, data.paper$count, 
                                 data.paper$rp, data.paper$size, data.paper$date.numeric)
paper.count$count <- c(1,5,2,3,4)
paper.count <- paper.count[order(paper.count$count),]


# predict values of count
paper.count$predict <- result$coefficients[[1]] + 
                       result$coefficients[[2]]*log(paper.count$mean.1) +
                       result$coefficients[[3]]*log(paper.count$mean.2) +
                       result$coefficients[[4]]*paper.count$mean.3


# create graph
graph.count <-  draw.ribbon(paper.count,
                            paper.count$count, paper.count$predict, paper.count$perc.10, paper.count$perc.90,
                            1, 5, paper.count$target,
                            2, 9, 1,
                            "Закупівлі паперу А4, 1011 закупівель",
                            "Резервна ціна, грн. за пачку", "Кількість учасників аукціону", "Період: 01/2016-01/2017")

# draw graphs
graph.count


# delete unnecessary data
rm(paper.count)
rm(graph.count)
rm(result)

