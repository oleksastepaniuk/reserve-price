

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
data.eggs <- read.csv2("data_eggs.csv")

# download functions
eval(parse("function_draw_scatter.R", encoding="UTF-8"))
eval(parse("function_draw_ribbon.R", encoding="UTF-8"))
eval(parse("function_multiplot.R", encoding="UTF-8"))
eval(parse("function_describe_variable.R", encoding="UTF-8"))
eval(parse("function_mode.R", encoding="UTF-8"))







################################################################################
###########                    Calculate variables                   ###########
################################################################################

# calculate reserve price, UAH per one egg
data.eggs$rp <- data.eggs$expected.value / data.eggs$quantity

# calculate zero round price, UAH per one egg
data.eggs$r0 <- data.eggs$round.0 / data.eggs$quantity

# calculate third round price, UAH per one egg
data.eggs$r3 <- data.eggs$round.3 / data.eggs$quantity



# zero round economy, %
data.eggs$gain.r0 <- (1-data.eggs$r0/data.eggs$rp)*100

# economy of the dynamic auction, %
data.eggs$gain <- (1-data.eggs$r3/data.eggs$r0)*100



# change format of date variable from factor to Date
data.eggs$date <- as.Date(as.character(data.eggs$date), "%d.%m.%Y")

# create numeric varible for the progression of months: "01/2016"=1
data.eggs$date.month <- format(data.eggs$date, format="%m/%Y")
data.eggs$date.numeric <- (as.numeric(substr(data.eggs$date.month, 4, 7))-2016)*12
data.eggs$date.numeric <- data.eggs$date.numeric + as.numeric(substr(data.eggs$date.month, 1, 2))
data.eggs$date.month <- NULL


# create quarter categorical variable
data.eggs$date.q <- ifelse(data.eggs$date.numeric<4 | data.eggs$date.numeric>12 & data.eggs$date.numeric<16, "q1",
                           ifelse(data.eggs$date.numeric<7 | data.eggs$date.numeric>15, "q2",
                                  ifelse(data.eggs$date.numeric<10, "q3", "q4")))





################################################################################
###########  plots of Reserve price and Zero round price over time   ###########
################################################################################


# graph: reserve price over time
reserve.time.series <- draw.scatter(data.eggs, FALSE,
                                    data.eggs$date, data.eggs$rp,
                                    1, 4, 0.5,
                                    "Закупівлі курячих яєць, 613 закупівель", 
                                    "Дата оголошення аукціону", "Резервна ціна, грн. за шт.", "") + 
                                    scale_x_date(date_labels = "%b %Y", date_breaks="3 months")



# graph: zero round price over time
zero.time.series <- draw.scatter(data.eggs, FALSE,
                                 data.eggs$date, data.eggs$r0,
                                 1, 4, 0.5,
                                 "", 
                                 "Дата оголошення аукціону", "Ціна нульового раунду, грн. за шт.", "Період: 01/2016-04/2017") + 
                                 scale_x_date(date_labels = "%b %Y", date_breaks="3 months")

# draw graphs
multiplot(reserve.time.series, zero.time.series, cols=2)


# delete unnecessary data
rm(reserve.time.series, zero.time.series)




# graph: zero round economy over time
zero.economy.time <- draw.scatter(data.eggs, TRUE,
                                  data.eggs$date, data.eggs$gain.r0,
                                  0, 70, 10,
                                  "Закупівлі курячих яєць, 613 закупівель", 
                                  "Дата оголошення аукціону", "Зниження ціни в R0 порівняно з РЦ, %", "") + 
                                  scale_x_date(date_labels = "%b %Y", date_breaks="3 months")

# graph: economy of the auction over time
auction.time.series <- draw.scatter(data.eggs, TRUE,
                                    data.eggs$date, data.eggs$gain,
                                    0, 70, 10,
                                    "", 
                                    "Дата оголошення аукціону", "Зниження ціни в R3 порівняно з R0, %", "Період: 01/2016-04/2017") + 
                                    scale_x_date(date_labels = "%b %Y", date_breaks="3 months")

# draw graphs
multiplot(zero.economy.time, auction.time.series, cols=2)


# delete unnecessary data
rm(zero.economy.time, auction.time.series)





################################################################################
### Impact of Reserve price an Number of participants on zero round price    ###
################################################################################


# regression
result <- lm(log(r0) ~ log(rp) + count + factor(date.q), data=data.eggs)
summary(result)

# check for multicollinearity
vif(result)
cor(data.eggs$rp, data.eggs$date.numeric)

# save regression results
stargazer(result, type="text", title="Закупівля курячих яєць",
          dep.var.labels=c("Ціна нульового раунду, % зміна"),
          covariate.labels=c("Резервна ціна, % зміна",
                             "Кількість учасників",
                             "Ефект 2-го кварталу",
                             "Ефект 3-го кварталу",
                             "Ефект 4-го кварталу"),
          out="eggs_r0.txt")



###################################
############ graph: impact of reserve price on zero round price
###################################


# categories of rp for the graph
data.eggs$rp.category <- ifelse(data.eggs$rp<1.5, "<1.5",
                                ifelse(data.eggs$rp<2, "1.5-2.0",
                                       ifelse(data.eggs$rp<2.5, "2.0-2.5",">=2.5")))


# data set that describes relationship
eggs.r0 <- describe.variable(data.eggs$rp.category, data.eggs$r0, 
                             data.eggs$rp, data.eggs$count)


# data set with the mode quarter for each rp.category
eggs.mode <- mode(data.eggs, 23, 22)
eggs.mode$target <- as.character(eggs.mode$target)


# add mode quarter to the descriptive data set
eggs.r0 <- merge(eggs.r0, eggs.mode, by="target", all=TRUE)
rm(eggs.mode)

# order data
eggs.r0$count <- c(1,4,2,3)
eggs.r0 <- eggs.r0[order(eggs.r0$count),]

# predict values of r0
eggs.r0$predict <- result$coefficients[[1]]

eggs.r0$predict <- eggs.r0$predict +
                   rowSums(cbind(result$coefficients[[2]]*log(eggs.r0$mean.1),
                                 result$coefficients[[3]]*eggs.r0$mean.2))

eggs.r0$predict <- ifelse(eggs.r0$value=="q2", eggs.r0$predict + result$coefficients[[4]],
                   ifelse(eggs.r0$value=="q3", eggs.r0$predict + result$coefficients[[5]],
                   ifelse(eggs.r0$value=="q4", eggs.r0$predict + result$coefficients[[6]],
                                               eggs.r0$predict)))
  
eggs.r0$predict <- exp(eggs.r0$predict)



# create graph
reserve.r0 <-  draw.ribbon(eggs.r0,
                           eggs.r0$count, eggs.r0$predict, eggs.r0$perc.10, eggs.r0$perc.90,
                           1, 4, eggs.r0$target,
                           1, 3, 0.5,
                           "Закупівлі курячих яєць, 613 закупівель",
                           "Резервна ціна, грн. за шт.", "Ціна нульового раунду, грн. за шт.", "")




###################################
############ graph: impact of number of participants on zero round price
###################################

## categories of count for the graph
data.eggs$count.category <- ifelse(data.eggs$count==2, "2",
                                   ifelse(data.eggs$count==3, "3",">3"))


# data set that describes relationship
eggs.count.r0 <- describe.variable(data.eggs$count.category, data.eggs$r0, 
                                   data.eggs$rp, data.eggs$count)


# data set with the mode quarter for each count.category
eggs.mode <- mode(data.eggs, 24, 22)
eggs.mode$target <- as.character(eggs.mode$target)


# add mode quarter to the descriptive data set
eggs.count.r0 <- merge(eggs.count.r0, eggs.mode, by="target", all=TRUE)
rm(eggs.mode)

# order data
eggs.count.r0$count <- c(3,1,2)
eggs.count.r0 <- eggs.count.r0[order(eggs.count.r0$count),]

# predict values of r0
eggs.count.r0$predict <- result$coefficients[[1]]

eggs.count.r0$predict <- eggs.count.r0$predict +
                         rowSums(cbind(result$coefficients[[2]]*log(eggs.count.r0$mean.1),
                                       result$coefficients[[3]]*eggs.count.r0$mean.2))

eggs.count.r0$predict <- ifelse(eggs.count.r0$value=="q2", eggs.count.r0$predict + result$coefficients[[4]],
                         ifelse(eggs.count.r0$value=="q3", eggs.count.r0$predict + result$coefficients[[5]],
                         ifelse(eggs.count.r0$value=="q4", eggs.count.r0$predict + result$coefficients[[6]],
                                                           eggs.count.r0$predict)))

eggs.count.r0$predict <- exp(eggs.count.r0$predict)



# create graph
count.r0 <-  draw.ribbon(eggs.count.r0,
                         eggs.count.r0$count, eggs.count.r0$predict, eggs.count.r0$perc.10, eggs.count.r0$perc.90,
                         1, 3, eggs.count.r0$target,
                         1, 3, 0.5,
                         "",
                         "Кількість учасників аукціону", "Ціна нульового раунду, грн. за шт", "Період: 01/2016-04/2017")

# draw graphs
multiplot(reserve.r0, count.r0, cols=2)

# delete unnecessary data
rm(eggs.r0, eggs.count.r0)
rm(reserve.r0,count.r0)
rm(result)




################################################################################
# IMPACT of Reserve price and Number of participants on the Economy of the auction
################################################################################


# regression
result <- lm(gain ~ log(count) + factor(date.q), data=data.eggs)
summary(result)


# save regression results
stargazer(result, type="text", title="Закупівля курячих яєць",
          dep.var.labels=c("Зменшення ціни в R3 порівнянно з R0, %"),
          covariate.labels=c("Кількості учасників, % зміна",
                             "Ефект 2-го кварталу",
                             "Ефект 3-го кварталу",
                             "Ефект 4-го кварталу"),
          out="eggs_gain.txt")



###################################
############ graph: impact of number of participants on the economy of the auction
###################################


# dataset that describes relationship
eggs.gain <- describe.variable(data.eggs$count.category, data.eggs$gain, 
                               data.eggs$count)



# dataset with the mode quarter for each count.category
eggs.mode <- mode(data.eggs, 24, 22)
eggs.mode$target <- as.character(eggs.mode$target)


# add mode quarter to the descriptive data set
eggs.gain <- merge(eggs.gain, eggs.mode, by="target", all=TRUE)
rm(eggs.mode)


# order data
eggs.gain$count <- c(3,1,2)
eggs.gain <- eggs.gain[order(eggs.gain$count),]


# predict values of r0
eggs.gain$predict <- result$coefficients[[1]]

eggs.gain$predict <- eggs.gain$predict +
                     result$coefficients[[2]]*log(eggs.gain$mean.1)


# create graph
graph.nominal.economy <-  draw.ribbon(eggs.gain,
                                      eggs.gain$count, eggs.gain$predict, eggs.gain$perc.10, eggs.gain$perc.90,
                                      1, 3, eggs.gain$target,
                                      0, 40, 10,
                                      "Закупівля курячих яєць, 613 закупівель",
                                      "Кількість учасників аукціону", "Зменшення ціни в R3 порівнянно з R0, %", "Період: 01/2016-04/2017")


# draw graph
graph.nominal.economy


# delete unnecessary datasets
rm(eggs.gain)
rm(graph.nominal.economy)
rm(result)




################################################################################
###           IMPACT of Reserve price on Number of participants             ####
################################################################################


# regression
result <- lm(count ~ factor(date.q), data=data.eggs)
summary(result)


# save regression results
stargazer(result, type="text", title="Закупівля курячих яєць",
          dep.var.labels=c("Кількість учасників"),
          covariate.labels=c("Ефект 2-го кварталу",
                             "Ефект 3-го кварталу",
                             "Ефект 4-го кварталу"), 
          out="eggs_count.txt")




###################################
############ graph: impact of quarter on number of participants
###################################


# data set that describes relationship
eggs.count <- describe.variable(data.eggs$date.q, data.eggs$count)
eggs.count$count <- c(1,2,3,4)


# predict values of count
eggs.count$predict <- result$coefficients[[1]]

eggs.count$predict <- ifelse(eggs.count[,1]=="q2", eggs.count$predict + result$coefficients[[2]],
                      ifelse(eggs.count[,1]=="q3", eggs.count$predict + result$coefficients[[3]],
                      ifelse(eggs.count[,1]=="q4", eggs.count$predict + result$coefficients[[4]],
                                                   eggs.count$predict)))


# rename quarter variable
eggs.count$target <- c("1 квартал", "2 квартал", "3 квартал", "4 квартал")



# create graph
graph.count <-  draw.ribbon(eggs.count,
                            eggs.count$count, eggs.count$predict, eggs.count$perc.10, eggs.count$perc.90,
                            1, 4, eggs.count$target,
                            2, 5, 1,
                            "Закупівлі курячих яєць, 613 закупівель",
                            "Квартал", "Кількість учасників аукціону", "Період: 01/2016-04/2017")

# draw graphs
graph.count


# delete unnecessary data
rm(eggs.count)
rm(graph.count)
rm(result)