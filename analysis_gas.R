

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
data.gas <- read.csv2("data_gas.csv")

# download functions
eval(parse("function_draw_scatter.R", encoding="UTF-8"))
eval(parse("function_draw_ribbon.R", encoding="UTF-8"))
eval(parse("function_multiplot.R", encoding="UTF-8"))
eval(parse("function_describe_variable.R", encoding="UTF-8"))



  



################################################################################
###########                    Calculate variables                   ###########
################################################################################

# change the measurement units of quantity from cubic meters to thou cubic meters
data.gas$quantity <- data.gas$quantity / ifelse(data.gas$measurement=="метри кубічні",1000,1)



# calculate reserve price, thou UAH per thou of cubic meters of natural gas
data.gas$rp <- data.gas$expected.value / data.gas$quantity / 1000

# calculate zero round price, thou UAH per thou of cubic meters of natural gas
data.gas$r0 <- data.gas$round.0 / data.gas$quantity / 1000

# calculate third round price, thou UAH per thou of cubic meters of natural gas
data.gas$r3 <- data.gas$round.3 / data.gas$quantity / 1000

# calculate price after additional agreement, thou UAH per thou of cubic meters of natural gas
data.gas$r.add <- data.gas$add.agreement / data.gas$quantity / 1000



# zero round economy, %
data.gas$gain.r0 <- (1-data.gas$r0/data.gas$rp)*100

# economy of the dynamic auction, %
data.gas$gain <- (1-data.gas$r3/data.gas$r0)*100

# real economy of the dynamic auction (accounting for additional agreements), %
data.gas$gain.real <- (1-data.gas$r.add/data.gas$r0)*100



# change format of date variable from factor to Date
data.gas$date <- as.Date(as.character(data.gas$date), "%d.%m.%Y")

# create numeric varible for the progression of months: "09/2016"=1
data.gas$date.month <- format(data.gas$date, format="%m/%Y")
data.gas$date.numeric <- (as.numeric(substr(data.gas$date.month, 4, 7))-2016)*12
data.gas$date.numeric <- data.gas$date.numeric + as.numeric(substr(data.gas$date.month, 1, 2))
data.gas$date.numeric <- data.gas$date.numeric - 8
data.gas$date.month <- NULL





################################################################################
###########  plots of Reserve price and Zero round price over time   ###########
################################################################################

# reserve price over time
reserve.time.series <- draw.scatter(data.gas, FALSE,
                                    data.gas$date, data.gas$rp,
                                    6, 12, 2,
                                    "Закупівлі природного газу, 677 закупівель", 
                                    "Дата оголошення аукціону", "Резервна ціна, тис. грн. за тис. м3", "")


# zero round price over time
zero.time.series <- draw.scatter(data.gas, FALSE,
                                 data.gas$date, data.gas$r0,
                                 6, 12, 2,
                                 "", 
                                 "Дата оголошення аукціону", "Ціна нульового раунду, тис. грн. за тис. м3", "Період: 09/2016-01/2017")

# draw graphs
multiplot(reserve.time.series, zero.time.series, cols=2)

# delete unnecessary data
rm(reserve.time.series, zero.time.series)
  
  
  



################################################################################
### Impact of Reserve price an Number of participants on zero round price    ###
################################################################################


# regression
result <- lm(log(r0) ~ log(rp) + count + log(date.numeric), data=data.gas)
summary(result)


# check for multicollinearity
vif(result)
cor(data.gas$rp, data.gas$date.numeric)


# save regression results
stargazer(result, type="text", title="Закупівля природного газу",
          dep.var.labels=c("Ціна нульового раунду, % зміна"),
          covariate.labels=c("Резервна ціна, % зміна",
                             "Кількість учасників",
                             "Ефект часу, % зміна"),
          out="gas_r0.txt")



###################################
############ graph: impact of reserve price on zero round price
###################################


# categories of reserve price for the graph
data.gas$rp.category <- ifelse(data.gas$rp<8.75, "<8.75",
                               ifelse(data.gas$rp<9.0, "8.75-9.00",
                                      ifelse(data.gas$rp<9.25, "9.00-9.25",
                                             ifelse(data.gas$rp<9.5, "9.25-9.50",
                                                    ifelse(data.gas$rp<9.75, "9.50-9.75",
                                                           ifelse(data.gas$rp<10.0, "9.75-10.00",">=10"))))))

# dataset that describes relationship
gas.r0 <- describe.variable(data.gas$rp.category, data.gas$r0, 
                            data.gas$rp, data.gas$count, data.gas$date.numeric)
gas.r0$count <- c(1,7,2,3,4,5,6)
gas.r0 <- gas.r0[order(gas.r0$count),]


# predict values of r0
gas.r0$predict <- result$coefficients[[1]]

gas.r0$predict <- gas.r0$predict +
                  rowSums(cbind(result$coefficients[[2]]*log(gas.r0$mean.1),
                                result$coefficients[[3]]*gas.r0$mean.2,
                                result$coefficients[[4]]*log(gas.r0$mean.3)))

gas.r0$predict <- exp(gas.r0$predict)


# create graph
reserve.r0 <-  draw.ribbon(gas.r0,
                           gas.r0$count, gas.r0$predict, gas.r0$perc.10, gas.r0$perc.90,
                           1, 7, gas.r0$target,
                           6, 10, 1,
                           "Закупівлі природного газу, 677 закупівель",
                           "Резервна ціна, тис. грн. за тис. м3", "Ціна нульового раунду, тис. грн. за тис. м3", "")

  

###################################
############ graph: impact of number of participants on zero round price
###################################

# categories of number of participants for the graph
data.gas$count.category <- ifelse(data.gas$count==2, "2",
                                  ifelse(data.gas$count==3, "3",
                                         ifelse(data.gas$count==4, "4",">4")))



# dataset that describes relationship
gas.count.r0 <- describe.variable(data.gas$count.category, data.gas$r0, 
                                  data.gas$rp, data.gas$count, data.gas$date.numeric)
gas.count.r0$count <- c(4,1,2,3)
gas.count.r0 <- gas.count.r0[order(gas.count.r0$count),]



# predict values of r0
gas.count.r0$predict <- result$coefficients[[1]]

gas.count.r0$predict <- gas.count.r0$predict +
                        rowSums(cbind(result$coefficients[[2]]*log(gas.count.r0$mean.1),
                                      result$coefficients[[3]]*gas.count.r0$mean.2,
                                      result$coefficients[[4]]*log(gas.count.r0$mean.3)))

gas.count.r0$predict <- exp(gas.count.r0$predict)


# create graph
count.r0 <-  draw.ribbon(gas.count.r0,
                         gas.count.r0$count, gas.count.r0$predict, gas.count.r0$perc.10, gas.count.r0$perc.90,
                         1, 4, gas.count.r0$target,
                         6, 10, 1,
                         "",
                         "Кількість учасників аукціону", "Ціна нульового раунду, тис. грн. за тис. м3", "Період: 09/2016-01/2017")

# draw graphs
multiplot(reserve.r0, count.r0, cols=2)

# delete unnecessary data
rm(gas.r0, gas.count.r0)
rm(reserve.r0,count.r0)
rm(result)





################################################################################
# IMPACT of Reserve price and Number of participants on the Economy of the auction
################################################################################


# regression nominal economy of the auction
result.nominal <- lm(gain ~ log(count), data=data.gas)
summary(result.nominal)


# save regression results
stargazer(result.nominal, type="text", title="Закупівля природного газу",
          dep.var.labels=c("Зменшення ціни в R3 порівнянно з R0, %"),
          covariate.labels=c("Кількість учасників, % зміна"),
          out="gas_gain_nominal.txt")


# regression real economy of the auction (accounting for additional agreements)
result.real <- lm(gain.real ~ log(count), data=data.gas)
summary(result.real)


# save regression results
stargazer(result.real, type="text", title="Закупівля природного газу",
          dep.var.labels=c("Зменшення уточненої ціни порівнянно з R0, %"),
          covariate.labels=c("Кількість учасників, % зміна"),
          out="gas_gain_real.txt")



###################################
############ graph: impact of number of participants on nominal economy
###################################

# data set that describes relationship
gas.gain <- describe.variable(data.gas$count.category, data.gas$gain, 
                              data.gas$count)
gas.gain$count <- c(4,1,2,3)
gas.gain <- gas.gain[order(gas.gain$count),]


# predict values of the nominal economy of the auction
gas.gain$predict <- result.nominal$coefficients[[1]]

gas.gain$predict <- gas.gain$predict + 
                    result.nominal$coefficients[[2]]*log(gas.gain$mean.1)


# gain changes from 0% to 16.4%
summary(data.gas$gain)


# create graph
graph.nominal.economy <-  draw.ribbon(gas.gain,
                                      gas.gain$count, gas.gain$predict, gas.gain$perc.10, gas.gain$perc.90,
                                      1, 4, gas.gain$target,
                                      -20, 20, 10,
                                      "Економія аукціону без врахування додаткових угод",
                                      "Кількість учасників аукціону", "Зменшення ціни в R3 порівнянно з R0, %", "")



###################################
############ graph: impact of number of participants on real economy
##################################


# data set that describes relationship
gas.real.gain <- describe.variable(data.gas$count.category, data.gas$gain.real, 
                                   data.gas$count)
gas.real.gain$count <- c(4,1,2,3)
gas.real.gain <- gas.real.gain[order(gas.real.gain$count),]


# predict values of the real economy of the auction
gas.real.gain$predict <- result.real$coefficients[[1]]

gas.real.gain$predict <- gas.real.gain$predict + 
                         result.real$coefficients[[2]]*log(gas.real.gain$mean.1)


# gain changes from -44% to 16.4%
summary(data.gas$gain.real)

# create graph
graph.real.economy <-  draw.ribbon(gas.real.gain,
                                   gas.real.gain$count, gas.real.gain$predict, gas.real.gain$perc.10, gas.real.gain$perc.90,
                                   1, 4, gas.real.gain$target,
                                   -20, 20, 10,
                                   "Економія аукціону з врахуванням додаткових угод",
                                   "Кількість учасників аукціону", "Зменшення ціни в R3 порівнянно з R0, %", "Період: 09/2016-01/2017")

# draw graphs
multiplot(graph.nominal.economy, graph.real.economy, cols=2)

# delete unnecessary datasets
rm(gas.gain, gas.real.gain)
rm(graph.nominal.economy,graph.real.economy)
rm(result.nominal, result.real)






################################################################################
###           IMPACT of Reserve price on Number of participants             ####
################################################################################


# regression
result <- lm(count ~ log(quantity), data=data.gas)
summary(result)


# save regression results
stargazer(result, type="text", title="Закупівля природного газу",
          dep.var.labels=c("Кількість учасників"),
          covariate.labels=c("Розмір закупівлі, % зміна"),
          out="gas_count.txt")




###################################
############ graph: impact of size of procurement on number of participants
###################################



# categories of the size of procurement
data.gas$quantity.category <- ifelse(data.gas$quantity<50, "<50",
                                 ifelse(data.gas$quantity<100, "50-100",
                                        ifelse(data.gas$quantity<250, "100-250",">=250")))



# data set that describes relationship
gas.count <- describe.variable(data.gas$quantity.category, data.gas$count, 
                               data.gas$quantity)
gas.count$count <- c(1,4,3,2)
gas.count <- gas.count[order(gas.count$count),]


# predict number of participants
gas.count$predict <- result$coefficients[[1]]

gas.count$predict <- gas.count$predict + 
                     result$coefficients[[2]]*log(gas.count$mean.1)


# count changes from 2 to 7
summary(data.gas$count)

# create graph
graph.count <-  draw.ribbon(gas.count,
                            gas.count$count, gas.count$predict, gas.count$perc.10, gas.count$perc.90,
                            1, 4, gas.count$target,
                            2, 7, 1,
                            "Закупівлі природного газу, 677 закупівель",
                            "Розмір закупівлі, тис. м3", "Кількість учасників аукціону", "Період: 09/2016-01/2017")

# draw graphs
graph.count


# delete unnecessary data
rm(gas.count)
rm(graph.count)
rm(result)


