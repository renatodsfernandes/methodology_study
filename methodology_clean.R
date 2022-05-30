library(lars)
library(xgboost)
library(MASS)
library(ggplot2)
library(grid)
library(gridExtra)
library(RColorBrewer)
library(egg)
library(devtools)
library(patchwork)
library(vars)
library(tseries)
library(lgarch)
library(MSwM)
library(hsmm)
library(BaPreStoPro)
library(rBayesianOptimization)
library(DiffusionRjgqd)
library(neuralnet)
library(rnn)
library(microbenchmark)
library(profmem)
library(profvis)
library(VineCopula)
library(caret)
library(rcausal)
setwd("PathToDataFolder")


files <- list.files("./PDBC_stota_2015_2019", full.names = TRUE)

temp <- read.csv(files[1], skip = 2, sep=";",header = TRUE)
codificacao <- temp[,c(1,2,3)]
temp <- temp[-dim(temp)[1],-c(2,3,29)]
rownames(temp) <- temp$Codigo
temp <- temp[,-1]
temp <- t(temp)

joint_data <- temp

for (i in 2:length(files)) {
  print(files[i])
  print(i)
  temp <- read.csv(files[i], skip = 2, sep=";",header = TRUE)
  temp <- temp[-dim(temp)[1],-c(2,3,29)]
  rownames(temp) <- temp$Codigo
  temp <- temp[,-1]
  temp <- t(temp)
  
  if (i>=1368) {
    temp <- temp[,-c(1,2,3,4,5,6,7,8,9,10,11)]
  }
  
  joint_data <- rbind(joint_data,temp)
}


# Cleaning issues with summertime hours
#first
temp3 <- joint_data[seq(2201,7450,25),]
for (i in 1:23) {
  joint_data[seq(2200+i,7450,25),] <- joint_data[seq(2201+i,7450,25),]
}
joint_data[seq(2199,7448,25),] <- temp3
joint_data[7449,]<-joint_data[7450,]
#second
temp3 <- joint_data[seq(11301,16725,25),]
for (i in 1:23) {
  joint_data[seq(11300+i,16725,25),] <- joint_data[seq(11301+i,16725,25),]
}
joint_data[seq(11299,16723,25),] <- temp3
joint_data[16724,]<-joint_data[16725,]
#third
temp3 <- joint_data[seq(20401,25825,25),]
for (i in 1:23) {
  joint_data[seq(20400+i,25825,25),] <- joint_data[seq(20401+i,25825,25),]
}
joint_data[seq(20399,25823,25),] <- temp3
joint_data[25824,]<-joint_data[25825,]
#fourth
temp3 <- joint_data[seq(29501,34925,25),]
for (i in 1:23) {
  joint_data[seq(29500+i,34925,25),] <- joint_data[seq(29501+i,34925,25),]
}
joint_data[seq(29499,34923,25),] <- temp3
joint_data[34924,]<-joint_data[34925,]
#fifth
temp3 <- joint_data[seq(38776,44025,25),]
for (i in 1:23) {
  joint_data[seq(38775+i,44025,25),] <- joint_data[seq(38776+i,44025,25),]
}
joint_data[seq(38774,44023,25),] <- temp3
joint_data[44024,]<-joint_data[44025,]
joint_data <- joint_data[-seq(25,nrow(joint_data),25),]

# Selection of variable from pbdc_stota that are important for the study

use_data <- joint_data[,c(2,3,4,5,6,7,13,14,15,41,22,24,25,26,32,35)]
colnames(use_data) <- c("hydro_conv","hydro_bomb","nuclear","carvao_nac","carvao_inter","comb_cycle","imp_franca","imp_marro","imp_ando","procura","bomb","exp_franca","exp_marro","exp_ando","renov","total")
rownames(use_data) <- c()
use_data <- as.data.frame(use_data)
use_data[is.na(use_data)] <- 0
use_data <- cbind(use_data,hydro_total=use_data$hydro_conv+use_data$hydro_bomb)
use_data <- cbind(use_data,carvao_total=use_data$carvao_nac+use_data$carvao_inter)

# Reading reservoir data variables

reservoir_pt <- read.csv("Reservoirs_Portugal_2015_2019.csv",header = TRUE)
reservoir_sp <- read.csv("Reservoirs_Spain_2015_2019.csv",header = TRUE)
colnames(reservoir_pt) <- c("week","Reservoir_PT")
colnames(reservoir_sp) <- c("week","Reservoir_SP")

reservoir <- data.frame(day = c(1:365,1:366,1:365,1:365,1:300),reservoir_pt=c(rep(reservoir_pt$Reservoir_PT[1],4),rep(reservoir_pt$Reservoir_PT[-1],each=7)),reservoir_sp=c(rep(reservoir_sp$Reservoir_SP[1],4),rep(reservoir_sp$Reservoir_SP[-c(1,length(reservoir_sp$Reservoir_SP))],each=7)))

use_data <- use_data[1:(nrow(use_data)-48),] # match data length of reservoir, it will be even lower afterwards so doesn't matter

# Reading coal and gas prices

coal <- read.csv("coal_2015_2019.csv",header = TRUE,colClasses = c("character","numeric"))
gas <- read.csv("gas_2015_2019.csv",header = TRUE,colClasses = c("character","numeric"))

co2 <- read.csv("CO2_2015_2019.csv",sep = ",",header = TRUE,colClasses = c("character","numeric"))

coal_as_date <- strptime(coal$Date,format = "%d-%m-%Y", tz = "UTC")
gas_as_date <- strptime(gas$Date,format = "%d-%m-%Y", tz = "UTC")
co2_as_date <- strptime(co2$Fecha,format = "%d-%m-%Y", tz = "UTC")
coal <- cbind(coal,coal_as_date)
gas <- cbind(gas,gas_as_date)
co2 <- cbind(co2,co2_as_date)
# there are two duplicated lines in co2, lines 442/443 and 603/604. 
# I'm removing the lines with lower price
co2 <- co2[-c(442,603),]

# Computing coal_price and gas_price variables

dates_2015_2019 <- seq( as.POSIXct("2015-01-01",tz="UTC"), as.POSIXct("2019-06-30",tz="UTC"), by="+1 day")

prices_termal <- data.frame(date=dates_2015_2019)
prices_termal <- merge(x=prices_termal,y=coal,by.x = "date",by.y = "coal_as_date",all.x = TRUE)
prices_termal <- prices_termal[c("date","Price")]
colnames(prices_termal) <- c("date","close_coal")
prices_termal <- merge(x=prices_termal,y=gas,by.x = "date",by.y = "gas_as_date",all.x = TRUE)
prices_termal <- prices_termal[c("date","close_coal","Price")]
colnames(prices_termal) <- c("date","close_coal","close_gas")
prices_termal <- merge(x=prices_termal,y=co2,by.x = "date",by.y = "co2_as_date",all.x = TRUE)
prices_termal <- prices_termal[c("date","close_coal","close_gas","EUA")]

prices_termal[c(1,2,3,4),2] <- prices_termal[5,2]
prices_termal[1,c(3,4)] <- prices_termal[2,c(3,4)]

for (i in 1:nrow(prices_termal)) {
  if (is.na(prices_termal[i,2])) {
    prices_termal[i,2] <- prices_termal[i-1,2]
  }
  if (is.na(prices_termal[i,3])) {
    prices_termal[i,3] <- prices_termal[i-1,3]
  }
  if (is.na(prices_termal[i,4])) {
    prices_termal[i,4] <- prices_termal[i-1,4]
  }
}

prices_termal <- cbind(prices_termal,coal_price=(prices_termal$close_coal+prices_termal$EUA*0.9),gas_price=(prices_termal$close_gas+prices_termal$EUA*0.4))

dates_hour <- seq( as.POSIXct("2015-01-01 00:00:00",tz="UTC"), as.POSIXct("2019-06-30 23:00:00",tz="UTC"), by="+1 hour")

prices_thermal_hour <- data.frame(date=dates_hour,coal_price=rep(prices_termal$coal_price,each=24),gas_price=rep(prices_termal$gas_price,each=24))

# Reading day ahead electricity prices

prices_spain <- read.csv("Day_ahead_Prices_spain_2015_2019.csv",header = TRUE,colClasses = c("character","numeric"))
prices_spain <- prices_spain[-(1:nrow(prices_spain))*is.na(prices_spain$Day.ahead.Price..EUR.MWh.),]
prices_spain <- prices_spain[1:39408,]

# Unite all data

use_data <- use_data[1:39408,]
final_data <- prices_thermal_hour #1642 days
final_data <- cbind(final_data,reservoir_pt = rep(reservoir$reservoir_pt[1:1642],each=24),reservoir_sp = rep(reservoir$reservoir_sp[1:1642],each=24))
final_data <- cbind(final_data,net_exp_fr = (use_data$exp_franca-use_data$imp_franca),net_exp_ad = (use_data$exp_ando-use_data$imp_ando),net_exp_ma = (use_data$exp_marro-use_data$imp_marro))
final_data <- cbind(final_data,hydro = use_data$hydro_total,coal = use_data$carvao_total,comb_cycle=use_data$comb_cycle, bomb = use_data$bomb)
final_data <- cbind(final_data,net_demand = (use_data$procura-use_data$nuclear-use_data$renov),total_production = use_data$total,price = prices_spain$Day.ahead.Price..EUR.MWh.)
final_data <- cbind(final_data,demand = use_data$procura)

head(final_data)
cor_data <- final_data[,-c(1,6,14,16)] # removing duplicated dates
colnames(final_data) <- c('date','coal_price','gas_price','reservoir_pt','reservoir_sp',
                          'net_exp_fr','net_exp_ad','net_exp_ma','hydro','coal','comb_cycle',
                          'bombing','net_demand','total_production','price','demand')

#### Visualization ####

rgb2hex <- function(r,g,b) rgb(r, g, b, maxColorValue = 255)
color_find <- function(x) {
  rgb <- colorRamp(brewer.pal(5,"RdBu")[c(1,3,5)])((x+1)/2)
  result <- rgb2hex(rgb[1],rgb[2],rgb[3])
  return(result)
} 

correlation_graph <- function(data) {
  for (i in 1:(ncol(data)-1)) {
    print(i)
    txt <- paste0("v_",i,"_",i, " = ggplot(data,aes(x=data[,",i,"]))+geom_histogram(aes(y=..density..), alpha=0.5,color='dark grey')+ stat_density(geom='line',color='red') + theme(legend.position = 'none',panel.grid = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())")
    eval(parse(text = txt))
    for (j in (i+1):ncol(data)) {
      txt <- paste0("v_",i,"_",j, "<- signif(cor(data[,",i,"],data[,",j,"]),digits=3)")
      eval(parse(text = txt))
      txt <- paste0("v_",i,"_",j, "<- grobTree( rectGrob(gp=gpar(col='white',fill=color_find(v_",i,"_",j,"))), textGrob(v_",i,"_",j,"))")
      eval(parse(text = txt))
      
      txt <- paste0("v_",j,"_",i, "<- ggplot(data,aes(x=data[,",j,"],y=data[,",i,"]))+geom_point(size=0.5) + theme(legend.position = 'none',panel.grid = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())")
      eval(parse(text = txt))
    }
  }
  temp <- ggplot(data,aes(x=data[,ncol(data)]))+geom_histogram(aes(y=..density..), alpha=0.5,color="dark grey")+ stat_density(geom="line",color="red") + theme(legend.position = 'none',panel.grid = element_blank(),axis.title = element_blank(),axis.text = element_blank(),axis.ticks = element_blank())
  txt <- paste0("v_",ncol(data),"_",ncol(data), " <- temp")
  eval(parse(text = txt))
  
  txt <- "grid.arrange(rectGrob(gp=gpar(fill='white'))"
  
  for (i in 1:ncol(data)) {
    txt <- paste0(txt,",textGrob('",colnames(data)[i],"')")  
  }
  for (j in 1:ncol(data)) {
    txt <- paste0(txt,",textGrob('",colnames(data)[j],"')")  
    for (i in 1:ncol(data)) {
      txt <- paste0(txt,",v_",i,"_",j) 
    }
  }
  txt <- paste0(txt,")")
  eval(parse(text = txt))
}


full_data_visualization <- final_data[,-c(6,14,16)] # removal of net_demand_fr (always zero), total_generation and demand
full_data_visualization <- full_data_visualization[,c(1,2,3,4,5,6,7,8,11,9,10,12,13)] # reordering data for better graphs
ggplot(full_data_visualization,aes(x=date,y=coal_price)) +geom_line()

data_visualization <- function(data) {
  for (i in 1:(ncol(data)-3)) {
    txt <- paste0("g_",i,"<-ggplot(data,aes(x=",colnames(data)[1],",y=",colnames(data)[i+1],")) + geom_line() + theme(axis.title.x=element_blank())")
    eval(parse(text = txt))
  }
  for (i in (ncol(data)-2):(ncol(data)-1)) {
    txt <- paste0("g_",i,"<-ggplot(data,aes(x=",colnames(data)[1],",y=",colnames(data)[i+1],")) + geom_line()")
    eval(parse(text = txt))
  }
  
  txt <- "g_1"
  for (i in 2:(ncol(data)-1)) {
    txt <- paste0(txt," + g_",i)
  }
  txt <- paste0(txt," + plot_layout(ncol = 2)")
  eval(parse(text = txt))
}

correlation_graph(cor_dat)

#### first algorithms ####

algorithm_data <- cor_data[,-c(4,11)] # removal of reservoir_SP and net_demand

Pausa para dormir!!!!!!!!!!!!!!!!


final_data_lasso <- lars(x=as.matrix(algorithm_data[,-10]),y=as.vector(algorithm_data$price),type = "lasso")
bench_final_data_lasso <- microbenchmark(lars(x=as.matrix(algorithm_data[,-10]),y=as.vector(algorithm_data$price),type = "lasso"))

summary(bench_final_data_lasso)

save(bench_final_data_lasso,file="bench_lasso.RData")


save(final_data_lasso,file = "lasso.RData")
rm(final_data_lasso)

#####XGB#####

final_data_xgb <- xgboost(data = as.matrix(algorithm_data[,-10]),label = as.vector(algorithm_data$price),max_depth = 6, eta = 0.3, nthread = 2, nrounds = 1000,objective = "reg:squarederror")
bench_final_data_xgb <- microbenchmark(xgboost(data = as.matrix(algorithm_data[,-10]),label = as.vector(algorithm_data$price),max_depth = 6, eta = 0.3, nthread = 2, nrounds = 1000,objective = "reg:squarederror"))

summary(bench_final_data_xgb)

save(bench_final_data_xgb,file="bench_xgb.RData")


save(final_data_xgb,file = "xgb.RData")
rm(final_data_xgb)

#####Ridge#####

final_data_ridge <- lm.ridge(price~.,data = algorithm_data,lambda=150)
bench_final_data_ridge <- microbenchmark(lm.ridge(price~.,data = algorithm_data))

summary(bench_final_data_ridge)


save(bench_final_data_ridge,bench_final_data_q1_ridge,bench_final_data_q2_ridge,
     bench_final_data_q3_ridge,bench_final_data_q4_ridge,file="bench_ridge.RData")

save(final_data_ridge,file = "ridge.RData")
rm(final_data_ridge)

#####VAR#####

final_data_var <- VAR(y=as.matrix(algorithm_data[,-c(1,2,3)]),p=24,exogen=as.matrix(algorithm_data[,c(1,2,3)]))
bench_final_data_var <- microbenchmark(VAR(y=as.matrix(algorithm_data[,-c(1,2,3)]),p=24,exogen=as.matrix(algorithm_data[,c(1,2,3)])))

summary(bench_final_data_var)

save(bench_final_data_var,file="bench_var.RData")


save(final_data_var,final_data_q1_var,file = "var.RData")
rm(final_data_var)


#####SVAR/AR/GARCH/ARMAX/GARCHX#####

acf(algorithm_data) # p for arima
pacf(algorithm_data,lag.max = 200,ylim=range(-1,1)) # q for arima

amat <- diag(7)
amat[lower.tri(amat)]<-NA
bmat <- diag(7)
diag(bmat)<-NA

final_data_svar <- SVAR(final_data_var,Amat = amat,Bmat = bmat)
final_data_ar <- ar(algorithm_data$price)
final_data_garch <- garch(algorithm_data$price,order = c(24,24))
final_data_armax <- arima(algorithm_data$price,order = c(24,0,24), xreg = algorithm_data[,-10])
final_data_garchx <- garchx(algorithm_data$price,order(24,24),arch = seq(1,24),garch = seq(1,24),xreg=algorithm_data[,-10])

bench_final_data_svar <- microbenchmark(SVAR(final_data_var,Amat = amat,Bmat = bmat))
bench_final_data_ar <- microbenchmark(ar(algorithm_data$price))
bench_final_data_garch <- microbenchmark(garch(algorithm_data$price,order = c(24,24)))
bench_final_data_armax <- microbenchmark(arima(algorithm_data$price,order = c(24,0,24), xreg = algorithm_data[,-10]))
bench_final_data_garchx <- microbenchmark(garchx(algorithm_data$price,order(24,24),arch = seq(1,24),garch = seq(1,24),xreg=algorithm_data[,-10]))

summary(bench_final_data_svar)
summary(bench_final_data_ar)
summary(bench_final_data_garch)
summary(bench_final_data_armax)
summary(bench_final_data_garchx)

save(bench_final_data_svar,bench_final_data_ar,bench_final_data_garch,
     bench_final_data_armax,bench_final_data_garchx,file="bench_garch.RData")

save(final_data_svar,final_data_ar,final_data_garch,
     final_data_armax,final_data_garchx,file = "auto.RData")
rm(final_data_svar,final_data_ar,final_data_garch,
   final_data_armax,final_data_garchx)

#####Neural Network#####


final_rnn2 <- trainr(Y=array(as.vector(algorithm_data$price),dim = c(nrow(algorithm_data),1)),
                                          X=array(as.matrix(algorithm_data[,-10]),dim = c(nrow(algorithm_data),1,(ncol(algorithm_data)-1))),
                                          learningrate = 0.1,
                                          hidden_dim = 2,
                                          numepochs = 100)

bench_final_rnn2 <- microbenchmark(trainr(Y=array(as.vector(algorithm_data$price),dim = c(nrow(algorithm_data),1)),
                                          X=array(as.matrix(algorithm_data[,-10]),dim = c(nrow(algorithm_data),1,(ncol(algorithm_data)-1))),
                                          learningrate = 0.1,
                                          hidden_dim = 2,
                                          numepochs = 100))



save(bench_final_rnn2,file="bench_neural_network.RData")

save(final_rnn2,file="neural_network.RData")
rm(final_rnn2)


#### Copula ####


vine_algorithm_data <- algorithm_data
for (i in 1:ncol(algorithm_data)) {
  vine_algorithm_data[,i]<-(algorithm_data[,i]-min(algorithm_data[,i]))/(max(algorithm_data[,i])-min(algorithm_data[,i]))
}


bench_final_data_cvine <- microbenchmark(RVineStructureSelect(data=vine_algorithm_data,type = 1))


final_data_cvine <- RVineStructureSelect(data=vine_algorithm_data,type = 1)



save(bench_final_data_cvine,file="bench_copula.RData")


save(final_data_cvine,file="copula.RData")
rm(final_data_cvine)



#### Causal ####

test <- tetradrunner(algoId = 'fges',df = algorithm_data,scoreId = 'fisher-z',dataType = 'continuos',alpha=0.1,faithfulnessAssumed=TRUE,maxDegree=-1,verbose=TRUE)

save(test,file="causal.RData")
rm(test)

bench_final_data_causal <- microbenchmark(tetradrunner(algoId = 'fges',df = algorithm_data,scoreId = 'fisher-z',dataType = 'continuos',alpha=0.1,faithfulnessAssumed=TRUE,maxDegree=-1,verbose=TRUE))

save(bench_final_data_causal,file="bench_causal.RData")

back_lm <- function(data_frame) {
  m1 <- lm(price ~ .,data=data_frame)
  return(stepAIC(m1,direction = "backward",trace = FALSE))
}

bench_final_data_lm <- microbenchmark(back_lm(algorithm_data))


final_data_lm <- back_lm(algorithm_data)


descale <- function(data) {
 final <- data
 for (i in 1:(ncol(data)-1)) {
   final[,i]<-data[,i]/(sqrt(var(data[,i])))
 }
 return(final)
}
                                             
algorithm_data_scaled <- descale(algorithm_data)
final_data_scaled_lm <- back_lm(algorithm_data_scaled)

save(final_data_scaled_lm,file="scaled_lr.RData")
rm(final_data_scaled_lm)           
                                             