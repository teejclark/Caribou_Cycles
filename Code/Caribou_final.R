# Load First
library(tidyverse)
library(deSolve)
library(nimble)
library(scatterplot3d)
library(peacots)
library(imputeTS)
library(dplyr)
library(ggplot2)
library(ggpubr)

##################### Ungulate-Vegetation Model ##############

# Continuous Function to Describe Dynamics
ung_veg <- function(Time, State, Pars){
  with(as.list(c(State, Pars)), {
    dV <- u_0*(1-(V/m))-(a*V*N)/(b+V)
    dN <- xi*N*(((a*V)/(b+V)) - eta)
    return(list(c(dV,dN)))
  })
}

# params and initial conditions
p <- c(u_0 = 0.8, m = 100, a = 1.78, b = 25.4, xi = 0.30, eta = 0.89) # params
y <- c(V = 50, N = .1) # starting states
t <- seq(0,500,by=1) # time

# run the Ordinary Differential Equation
out <- as_tibble(ode(y, t, ung_veg, p)[,-1]) 

# graph
out %>% 
  ggplot(aes(t,N)) +
  geom_line(lwd = 1)

# how to calculate period
car_veg_per <- peacots::evaluate.pm(seq_along(out$N), signal = out$N)
1/car_veg_per$frequencies[car_veg_per$peakMode] 
# 125.25 period, higher than anticipated 

#### period doubles and goes to 201 for all values = 1.5 and below; hunting efficiency is worse so they are less productive, yeilding longer periods?


# function to calculate period
period_fun <- function(x){
  peak_os <- evaluate.pm(times=seq_along(x), signal=x)
  per=1/peak_os$frequencies[peak_os$peakMode]
  per
}

# function to calculate amplitude
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

amp_fun(out$N) # 3.22

############## sensitivity analysis ###############################


################ varying across u_0 #######################
#u_0 is the change in vegetation growth rate

u0_period <- rep(NA, 10)
u_0 <- seq(0,1.6,length.out=length(u0_period))

for (i in 1:length(u0_period)){
  
  # params and initial conditions
  p <- c(u_0 = u_0[i], m = 100, a = 1.78, b =25.4, xi = 0.3, eta = 0.89) # params
  y <- c(V = 50, N = .1) # starting states
  t <- seq(0,1000,by=1) # time
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, ung_veg, p)[,-1]) 
  
  u0_period[i] <- period_fun(out$N)
  
}

# let's look at the results
plot(u_0,u0_period,type="b", pch=16, main= "Variation in Period Over 10 u0 values") # no change in period
period_fun(out$N)
# period decreases to 77 when we increase growth rate, which is what we expected

#calculating amplitude - u_0
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

u0_amp <- rep(NA,10)
u_0 <- seq(0,1.6,length.out=length(u0_amp))

for (i in 1:length(u0_amp)){
  
  # params and initial conditions
  p <- c(u_0 = u_0[i], m = 100, a = 1.78, b =25.4, xi = 0.3, eta = 0.89) # params
  y <- c(V = 50, N = .1) # starting states
  t <- seq(0,1000,by=1) # time
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, ung_veg, p)[,-1]) 
  
  u0_amp[i] <- amp_fun(out_amp$N)
  
}

plot(u_0,u0_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of u0")
amp_fun(out$N)
# amplitude is 2.55 - decreased as you increase u_0 which aligns with our assumptions

################### varying across m #############################
# m is the carrying capacity for vegetation

m_period <- rep(NA, 10)
m <- seq(1,200,length.out=length(m_period))

for (i in 1:length(m_period)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = m[i], a = 1.78, b =25.4, xi = 0.3, eta = 0.89) # params
  y <- c(V = 50, N = .1) # starting states
  t <- seq(0,1000,by=1) # time
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, ung_veg, p, method="ode45")[,-1]) 
  
  m_period[i] <- period_fun(out$N)
  
}

# let's look at the results
plot(m,m_period,type="b", pch=16, main= "Variation in Period over 10 values of m") # 
period_fun(out$N)

#period decreases to 100.1 as we increase values of m

#calculating amplitude 
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

m_amp <- rep(NA,10)
m <- seq(1,200,length.out=length(m_amp))

for (i in 1:length(m_amp)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = m[i], a = 1.78, b =25.4, xi = 0.3, eta = 0.89) # params
  y <- c(V = 50, N = .1) # starting states
  t <- seq(0,1000,by=1) # time
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, ung_veg, p)[,-1]) 
  
  m_amp[i] <- amp_fun(out_amp$N)
  
}

#look at results 
plot(m,m_amp,type="b", pch=16, main= "Variation in Amplitude over 10 values of m")
amp_fun(out$N)
#the amplitude decreases to 3.57 as we increase m which deviates from hypothesis, we assumed as more standing crop was available, we would experience greater booms and busts, but this outcome does not necessaril suggest that.


############################## vary across a ################
# a is the caribou forage efficiency or search rate

a_period <- rep(NA, 10)
a <- seq(0.01,3.56,length.out=length(a_period))

for (i in 1:length(a_period)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = 100, a = a[i], b =25.4, xi = 0.3, eta = 0.89) # params
  y <- c(V = 50, N = .1) # starting states
  t <- seq(0,1000,by=1) # time
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, ung_veg, p)[,-1]) 
  
  a_period[i] <- period_fun(out$N)
  
}

# let's look at the results
plot(a,a_period,type="b", pch=16, main= "Variation in Period over 10 values of a") # 
period_fun(out$N)
# period drops down to 50.5 years


#calculating amplitude 
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

a_amp <- rep(NA,10)
a <- seq(0.01,3.56,length.out=length(a_amp))

for (i in 1:length(a_amp)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = 100, a = a[i], b =25.4, xi = 0.3, eta = 0.89) # params
  y <- c(V = 50, N = .1) # starting states
  t <- seq(0,1000,by=1) # time
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, ung_veg, p)[,-1]) 
  
  a_amp[i] <- amp_fun(out_amp$N)
  
}

plot(a,a_amp,type="b", pch=16, main= "Variation in Amplitude over 10 values of a")
amp_fun(out$N)
#amplitude decreases sharply as search rate increases, which suggests that the longer it takes for caribou to find food, the shorter the period and smaller the amplitude 


########################## vary across b ##################
# b is the caribou handling time with lichen

b_period <- rep(NA, 10)
b <- seq(0.01,50.8,length.out=length(b_period))

for (i in 1:length(b_period)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = 100, a = 1.78, b = b[i], xi = 0.3, eta = 0.89) # params
  y <- c(V = 50, N = .1) # starting states
  t <- seq(0,1000,by=1) # time
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, ung_veg, p)[,-1]) 
  
  b_period[i] <- period_fun(out$N)
  
}

# let's look at the results
plot(b,b_period,type="b", pch=16, main= "Variation in Period over 10 values of b") # 
period_fun(out$N)
# as we increase handling time the period increases which aligns with our assumptions


#calculating amplitude 
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

b_amp <- rep(NA,10)
b <- seq(0.01,50.8,length.out=length(b_amp))

for (i in 1:length(b_amp)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = 100, a = 1.78, b =b[i], xi = 0.3, eta = 0.89) # params
  y <- c(V = 50, N = .1) # starting states
  t <- seq(0,1000,by=1) # time
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, ung_veg, p)[,-1]) 
  
  b_amp[i] <- amp_fun(out_amp$N)
  
}

plot(b,b_amp,type="b", pch=16, main= "Variation in Amplitude over 10 values of b")
amp_fun(out$N)
# as we increase handling time, we see a decrease in amplitude, suggesting we have longer cycles at lower peaks and troughs.


################### Wolf-Caribou Model #########################


# Continuous Function to Describe Dynamics
caribou_wolf <- function(Time, State, Pars){
  with(as.list(c(State, Pars)), {
    dN <- r_0*N*(1-(N/k)) - ((c*P*N)/(d+N))
    dP <- chi*P*((c*N)/(d+N)-mu)
    return(list(c(dN,dP)))
  })
}

# params and initial conditions
p <- c(r_0 = 0.26, k = 0.9, c = 18.5, d = 0.5, chi = 0.114, mu = 9.25) # params
y <- c(N = .1, P = .025) # starting states
t <- seq(0,500,by=1) # time

# run the Ordinary Differential Equation
out <- as_tibble(ode(y, t, caribou_wolf, p)[,-1]) 

# graph moose
out %>% 
  ggplot(aes(t,N)) +
  geom_line(lwd=1)

# graph wolves
out %>% 
  ggplot(aes(t,P)) +
  geom_line(lwd=1, color = "red")

# calculate period
car_wolf_per <- peacots::evaluate.pm(seq_along(out$N), signal = out$N)
1/car_wolf_per$frequencies[car_wolf_per$peakMode] 
#50.25  period

# function to calculate period
period_fun <- function(x){
  peak_os <- evaluate.pm(times=seq_along(x), signal=x)
  per=1/peak_os$frequencies[peak_os$peakMode]
  per
}

amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

(max(out$N) - min(out$N)/2)/mean(out$N)

amp_fun(out$N) # 1.64 amplitude


####################### sensitivity analysis #####################

################### varying across r_0 #########################
# r_0 is the intrinsic growth rate of caribou

# r_0 period calculations
r0_period <- rep(NA, 10)
r_0 <- seq(0,0.52,length.out=length(r0_period))

for (i in 1:length(r0_period)){
  
  p <- c(r_0 = r_0[i], k = 0.9, c = 18.5, d = 0.19, chi = 0.114, mu = 9.25) # params
  y <- c(N = .1, P = .025) # starting states
  t <- seq(0,200,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, moose_wolf, p)[,-1])
  
  r0_period[i] <- period_fun(out$N)
  
}

# plot period r_0
plot(r_0,r0_period,type="b", pch=16, main= "Variation in Period Over 10 Values of r0")
#period decreases as r_0 increases - parameter was not accounted for in thess


# r_0 amplitude calculations
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

r0_amp <- rep(NA,10)
r_0 <- seq(0,0.52,length.out=length(r0_amp))

for (i in 1:length(r0_amp)){
  
  p <- c(r_0 = r_0[i], k = 0.9, c = 18.5, d = 0.19, chi = 0.114, mu = 9.25) # params
  y <- c(N = .1, P = .025) # starting states
  t <- seq(0,200,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, moose_wolf, p)[,-1])
  
  r0_amp[i] <- amp_fun(out$N)
  
}

# plot amplitude r_0
plot(r_0,r0_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of r0")
r0_amp   ### 
amp_fun(out$N) # 2.38
#amplitude decreases as r_0 increases - paameter was not accounted for in thess


################### vary over c ################
# c is the forage search rate for caribou

#c period calculations
c_period <- rep(NA, 10)
c <- seq(0.01,37,length.out=length(c_period))

for (i in 1:length(c_period)){
  
  p <- c(r_0 = 0.26, k = 0.9, c = c[i], d = 0.19, chi = 0.114, mu = 9.25) # params
  y <- c(N = .1, P = .025) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, moose_wolf, p, method="ode45")[,-1])
  
  c_period[i] <- period_fun(out$N)
  
}

# plot period c
plot(c,c_period,type="b", pch=16, main= "Variation in Period Over 10 Values of c")
# period decreases and then increases slightly as we increase search rate

#c amplitude calculations
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

c_amp <- rep(NA,10)
c <- seq(0.01,37,length.out=length(c_amp))

for (i in 1:length(c_amp)){
  
  p <- c(r_0 = 0.26, k = 0.9, c = c[i], d = 0.19, chi = 0.114, mu = 9.25) # params
  y <- c(N = .1, P = .025) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, moose_wolf, p, method="ode45")[,-1])
  
  c_amp[i] <- amp_fun(out_amp$N)
  
}

# plot amplitude c 
plot(c,c_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of c")
# amplitude increases as we increase search rate



########################## vary over d ########################
#handling time of wolf with caribou

# d period calculations
d_period <- rep(NA, 10)
d <- seq(0.01,0.38,length.out=length(d_period))

for (i in 1:length(d_period)){
  
  p <- c(r_0 = 0.26, k = 0.9, c = 18.5, d = d[i], chi = 0.114, mu = 9.25) # params
  y <- c(N = .1, P = .025) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, moose_wolf, p, method="ode45")[,-1])
  
  d_period[i] <- period_fun(out$N)
  
}

# plot period of d
plot(d,d_period,type="b", pch=16, main= "Variation in Period Over 10 Values of d")
# period decreases as we increase handling time


# d amplitude calculations
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

d_amp <- rep(NA,10)
d <- seq(0.01,0.38,length.out=length(d_amp))

for (i in 1:length(d_amp)){
  
  p <- c(r_0 = 0.26, k = 0.9, c = 18.5, d = d[i], chi = 0.114, mu = 9.25) # params
  y <- c(N = .1, P = .025) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, moose_wolf, p, method="ode45")[,-1])
  
  d_amp[i] <- amp_fun(out_amp$N)
  
}

# plot amplitude for d
plot(d,d_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of d")
amp_fun(out$N) #2.08
# amplitude is variable when we increase handling time of wolves with caribou



##################### Tri-trophic Model ######################


# Continuous Function to Describe Dynamics
veg_moose_wolf <- function(Time, State, Pars){
  with(as.list(c(State, Pars)), {
    dV <- u_0*(1-(V/m))-((a*V*N)/(b+V))
    dN <- xi*N*(((a*V)/(b+V)) - eta) - ((c*P*N)/(d+N))
    dP <- chi*P*((c*N)/(d+N)-mu) - (s_0/kappa)*(P^2)
    return(list(c(dV,dN,dP)))
  })
}

# params and initial conditions
p <- c(u_0 = 0.8, m = 100, a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
       c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
y <- c(V = 50, N = 2, P = .001) # starting states
t <- seq(0,500,by=1) # time

# run the Ordinary Differential Equation
out <- as_tibble(ode(y, t, veg_moose_wolf, p, method = "ode45")[,-1]) 

# graph moose
out %>% 
  ggplot(aes(t,N)) +
  geom_line(lwd=1)

# graph wolves
out %>% 
  ggplot(aes(t,P)) +
  geom_line(lwd=1, color = "red")

# calculate period
car_wolf_veg_per <- peacots::evaluate.pm(seq_along(out$N), signal = out$N)
1/car_wolf_veg_per$frequencies[car_wolf_veg_per$peakMode] 
#83.5  period 

# function to calculate period
period_fun <- function(x){
  peak_os <- evaluate.pm(times=seq_along(x), signal=x)
  per=1/peak_os$frequencies[peak_os$peakMode]
  per
}

amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

amp_fun(out$N) 
# amp is 3.79


############# sensitivity analysis #########################

################ varying across u_0 #######################
# u_0 is the intrinsic growth rate of vegatation


#u_0 period calculation
u0_all_period <- rep(NA, 10)
u_0 <- seq(0,1.6,length.out=length(u0_all_period))

for (i in 1:length(u0_all_period)){
  
  # params and initial conditions
  p <- c(u_0 = u_0[i], m = 100, a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_u0 <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  u0_all_period[i] <- period_fun(out_u0$N)
  
}

# plot of 0_u period
plot(u_0,u0_all_period,type="b", pch=16, main= "Variation in Period Over 10 values of u0", xlab="u_0", ylab="u0_period") # no change in period
# period decreases as we increase u_0, which is expected

#u_0 amplitude calculation
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

u0_all_amp <- rep(NA,10)
u_0 <- seq(0.01,1.6,length.out=length(u0_all_amp))

for (i in 1:length(u0_all_amp)){
  
  p <- c(u_0 = u_0[i], m = 100, a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  u0_all_amp[i] <- amp_fun(out_amp$N)
  
}

#plot of u_0 amplitude
plot(u_0,u0_all_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of u0")
u0_all_amp 
#amplitude also decreases as we increase u_0


################## vary over m #####################
# m is the carrying capacity of vegetation

#m period calculations
m_all_period <- rep(NA, 10)
m <- seq(0.01,200,length.out=length(m_all_period))

for (i in 1:length(m_all_period)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = m[i], a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  m_all_period[i] <- period_fun(out$N)
  
}

# plot of period for m 
plot(m,m_all_period,type="b", pch=16, main= "Variation in Period Over 10 values of m", xlab= "m", ylab= "m_period") # no change in period
#period decreases as we increase m

# m amplitude calculations
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

m_all_amp <- rep(NA,10)
m <- seq(0.01,200,length.out=length(m_all_amp))

for (i in 1:length(m_all_amp)){
  
  p <- c(u_0 = 0.8, m = m[i], a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  m_all_amp[i] <- amp_fun(out_amp$N)
  
}

#plot for m amplitude 
plot(m,m_all_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of m", xlab="m", ylab="m_amp")
m_all_amp
#amplitude decreases as we increase m


####################### vary over a ########################
#a is the search rate of caribou on forage

# period of a calculations
a_all_period <- rep(NA, 10)
a <- seq(0.01,3.56,length.out=length(a_all_period))

for (i in 1:length(a_all_period)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = 100, a = a[i], b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  a_all_period[i] <- period_fun(out$N)
  
}

# plot of period for a
plot(a,a_all_period,type="b", pch=16, main= "Variation in Period Over 10 values of a", xlab="a", ylab="a_period") # 
#period decreases as we increase a


# a amplitude calculations
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

a_all_amp <- rep(NA,10)
a <- seq(0.01,3.56,length.out=length(a_all_amp))

for (i in 1:length(a_all_amp)){
  
  p <- c(u_0 = 0.8, m = 100, a = a[i], b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  a_all_amp[i] <- amp_fun(out_amp$N)
  
}

#plot of amplitude for a
plot(a,a_all_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of a", xlab="a", ylab="a_amp")
a_all_amp   # no change in amplitude (3.2)
#amplitude decrease as we increase a


#################### vary over b #############################
# b is handling time of caribou with lichen

# b period calculations
b_all_period <- rep(NA, 10)
b <- seq(0.01,50.8,length.out=length(b_all_period))

for (i in 1:length(b_all_period)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = 100, a = 1.78, b = b[i], xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  b_all_period[i] <- period_fun(out$N)
  
}

# plot of period for b
plot(b,b_all_period,type="b", pch=16, main= "Variation in Period Over 10 values of b", xlab="b", ylab="b_period") # 
# period decreases abruptly and then increases as we increase handling time

# b amplitude calculations 
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

b_all_amp <- rep(NA,10)
b <- seq(0.01,50.8,length.out=length(b_all_amp))

for (i in 1:length(b_all_amp)){
  
  p <- c(u_0 = 0.8, m = 100, a = 1.78, b = b[i], xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  b_all_amp[i] <- amp_fun(out_amp$N)
  
}

#plot of amplitude for b
plot(b,b_all_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of b")
b_all_amp
# amp decreases and then gradually increases as we reach values of 30 for handling time


####################### vary over c ########################
# c is the search rate of wolves on caribou

# c period calculations
c_all_period <- rep(NA, 10)
c <- seq(0.01,37,length.out=length(c_all_period))

for (i in 1:length(c_all_period)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = 100, a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
         c = c[i], d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  c_all_period[i] <- period_fun(out$N)
  
}

# lot of period for c
plot(c,c_all_period,type="b", pch=16, main= "Variation in Period Over 10 values of c", xlab="c", ylab= "c_period") # 
# period increases after first declining 

# c amplitude calculations
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

c_all_amp <- rep(NA,10)
c <- seq(0.01,37,length.out=length(c_all_amp))

for (i in 1:length(c_all_amp)){
  
  p <- c(u_0 = 0.8, m = 100, a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
         c = c[i], d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  c_all_amp[i] <- amp_fun(out_amp$N)
  
}

#plot of amplitude for c
plot(c,c_all_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of c", xlab= "c", ylab= "c_amp")
c_all_amp 
#amplitude increases as we increase search rate of wolves on caribou


########################## vary over d ####################################
# d is the handling time of wolves with caribou

#d period calculations
d_all_period <- rep(NA, 10)
d <- seq(0.01,1,length.out=length(d_all_period))

for (i in 1:length(d_all_period)){
  
  # params and initial conditions
  p <- c(u_0 = 0.8, m = 100, a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = d[i], chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  d_all_period[i] <- period_fun(out$N)
  
}

# plot of period for d
plot(d,d_all_period,type="b", pch=16, main= "Variation in Period Over 10 values of d", xlab= "d", ylab="d_period") # 
#period decreases as we increase handling time of wolves on caribou

# d amplitude calculations 
amp_fun <- function(x){
  (max(x)-min(x)/2)/mean(x)
}

d_all_amp <- rep(NA,10)
d <- seq(0.01,1,length.out=length(d_all_amp))

for (i in 1:length(d_all_amp)){
  
  p <- c(u_0 = 0.8, m = 100, a = 1.78, b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = d[i], chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,1000,by=1) # time
  
  
  # run the Ordinary Differential Equation
  out_amp <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
  
  d_all_amp[i] <- amp_fun(out_amp$N)
  
}

# plot of amplitude for d
plot(d,d_all_amp,type="b", pch=16, main= "Variation in Amplitude Over 10 Values of d", xlab="d", ylab= "d_amp")
d_all_amp
#amplitude decrease as we increase handling time of wolves on caribou


