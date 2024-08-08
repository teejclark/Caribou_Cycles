#NOTE: not all of the code on this script is vital, but you will need to progress from from car to NEWCAR.1
# through NEWCAR.4 along the way.
#
#The Cape Bathurst and Bluenose West issues near the top here aren't vital code

library(imputeTS)
library(peacots)
library(dplyr)
library(ggplot2)
library(ggpubr)
rm(list = ls())
car=read.csv("final_cariboudata.1.csv")
car$Population=as.numeric(car$Population)
massdata=car %>% select(Population,Herd,Year) %>% group_by(Herd)%>% na_interpolation(option="stine")
View(massdata)
View(car)


#### Cape Bathurst & Bluenose West Issues####
CB=massdata %>% filter(Herd=="Cape Bathurst")%>% select(Population,Year)
ggplot(CB, aes(y = Population, x=Year))+ 
  geom_line()+
  theme_bw()+  
  geom_point(color='#2980B9', size=2) +
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y=0)+
  theme(plot.title = element_text(colour = "blue",face = "bold", size = (15),hjust=.5), 
        legend.title = element_text(colour = "steelblue",  face = "bold.italic"), 
        legend.text = element_text(colour="steelblue4",face = "italic"), 
        axis.title = element_text(colour = "steelblue4",size = (14)),
        axis.text = element_text(colour = "cornflowerblue", size = (14)))+
  ggtitle("Cape Bathurst Herd Model")+
  labs(y="Herd Population", x = "Year")
# I think I see the issue with this herd. Looking at the data it appears cyclic
# and a period of 40+ years. The problem is the first data point (1986)
# because R is treating that as a valley.

BW=massdata %>% filter(Herd=="Bluenose West")%>% select(Population,Year)
ggplot(BW, aes(y = Population, x=Year))+ 
  geom_line()+
  theme_bw()+  
  geom_point(color='#2980B9', size=2) +
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y=0)+
  theme(plot.title = element_text(colour = "blue",face = "bold", size = (15),hjust=.5), 
        legend.title = element_text(colour = "steelblue",  face = "bold.italic"), 
        legend.text = element_text(colour="steelblue4",face = "italic"), 
        axis.title = element_text(colour = "steelblue4",size = (14)),
        axis.text = element_text(colour = "cornflowerblue", size = (14)))+
  ggtitle("Bluenose West Herd Model")+
  labs(y="Herd Population", x = "Year")

fm <- car %>% filter(Herd == "Bluenose West")
fm.fit <- log(fm$Population) %>% na_interpolation(option = "stine")
fm.fit <- exp(fm.fit)
time <- seq_along(fm.fit)
plot(fm$Population)

ssp <- spectrum(fm.fit)
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)] # this is the period estimate = 72 years...
reslm <- lm(fm.fit ~ sin(2*pi/per*time) + cos(2*pi/per*time))
plot(fm.fit)
lines(predict(reslm), col = "red")
# The issue with this herd is the slight bump in population during the trough.
# R thinks that this is another "wave"

####Removing Cape Bathurst first point####
car_CB=car[-792,]
View(car_CB)

car.CB=car_CB %>% select(Population,Herd,Year) %>% group_by(Herd)%>% na_interpolation(option="stine")

CB_new=car.CB %>% filter(Herd=="Cape Bathurst")%>% select(Population,Year)
ggplot(CB_new, aes(y = Population, x=Year))+ 
  geom_line()+
  theme_bw()+  
  geom_point(color='#2980B9', size=2) +
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y=0)+
  theme(plot.title = element_text(colour = "blue",face = "bold", size = (15),hjust=.5), 
        legend.title = element_text(colour = "steelblue",  face = "bold.italic"), 
        legend.text = element_text(colour="steelblue4",face = "italic"), 
        axis.title = element_text(colour = "steelblue4",size = (14)),
        axis.text = element_text(colour = "cornflowerblue", size = (14)))+
  ggtitle("Cape Bathurst Herd Model (point removed)")+
  labs(y="Herd Population", x = "Year")

fm <- car.CB %>% filter(Herd == "Cape Bathurst")
fm.fit <- log(fm$Population) %>% na_interpolation(option = "stine")
fm.fit <- exp(fm.fit)
time <- seq_along(fm.fit)
plot(fm$Population)

ssp <- spectrum(fm.fit)
per <- 1/ssp$freq[ssp$spec==max(ssp$spec)] # 32 period est. Before it was 18
reslm <- lm(fm.fit ~ sin(2*pi/per*time) + cos(2*pi/per*time))
plot(fm.fit)
lines(predict(reslm), col = "red")

ggplot(YES, aes(x = Amplitude, y=Period))+ 
  theme_bw()+  
  geom_point(color='#2980B9', size=2) +
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y=0)+
  theme(plot.title = element_text(colour = "blue",face = "bold", size = (15),hjust=.5), 
        legend.title = element_text(colour = "steelblue",  face = "bold.italic"), 
        legend.text = element_text(colour="steelblue4",face = "italic"), 
        axis.title = element_text(colour = "steelblue4",size = (14)),
        axis.text = element_text(colour = "cornflowerblue", size = (14)))+
  ggtitle("Period v Amplitude Model")+
  labs(y="Period (years)", x = " Relative Amplitude")
# Not much of a clear relationship

####Other old Amp+Period Calculations####

# function to calculate period
periods <- function(x){
  ssp <- spectrum(x)
  per <- 1/ssp$freq[ssp$spec==max(ssp$spec)]
  per
}

# let's run it...
NEWcar.1<- car %>% group_by(Herd) %>% mutate(herd_models=exp(na_interpolation(log(Population),option="stine")))
View(NEWcar.1)

NEWcar.2 <-NEWcar.1 %>% group_by(Herd) %>% mutate(period = periods(herd_models), 
          amp = (max(herd_models)-min(herd_models))/mean(herd_models)) ### EDIT TJCW = had to divide by two
View(NEWcar.2)

####Sig of Periodigrams####
gr <- car %>% filter(Herd == "Fortymile")
# NOTE TO JACK: use log and exp to avoid negative values...
gr.fit <- log(gr$Population) %>% na_interpolation(option = "stine")
gr.fit <- exp(gr.fit)
plot(gr.fit)
# OUSS
peak_os <- evaluate.pm(times = seq_along(gr.fit), signal = gr.fit)
# plot
plotReport(times=seq_along(gr.fit),signal=gr.fit,report=peak_os)
1/(peak_os$frequencies[1]) #Period calc- is the first value always the sig one?
peak_os$P

####New periods####

best_periods <- function(x){
  peak_os <- evaluate.pm(times = seq_along(x), signal = x )
  per= 1/peak_os$frequencies[peak_os$peakMode]
  per
}

NEWcar.3 <-NEWcar.2 %>% group_by(Herd) %>% 
  mutate(new_period = best_periods(herd_models))
View(NEWcar.3)

significance= function(a){
  peak_os <- evaluate.pm(times = seq_along(a), signal = a )
  sig=peak_os$P
  sig
if(sig>=.05){
  print("NO")
  } else {
  print("YES")
  }}

NEWcar.4 = NEWcar.3 %>% group_by(Herd) %>% mutate(sig_period =significance(herd_models))
View(NEWcar.4)

####Summary of Herds+Periods####

Display = NEWcar.4 %>% select(Herd,sig_period,new_period) %>% 
  group_by(Herd) %>% summarise_each(first)
View(Display)

cyclic_herds=Display %>% filter(sig_period == "YES") %>%
  select(Herd,new_period)
View(cyclic_herds)

par(mfrow=c(1,1))

boxplot(cyclic_herds$new_period,main="Periods of Caribou Herd Cycles", 
        xlab="Period (years)",horizontal = TRUE, col=c("cadetblue2"),
        name = c("Clearly Cyclic"))

fivenum(cyclic_herds$new_period)

####Determining best AMP measure - not vital code####

NEWcar.5=NEWcar.4 %>% group_by(Herd) %>% mutate(sd_amp= sd(log10(herd_models))) %>%
  select(Herd,amp,sd_amp, sig_period) %>% summarise_each(first)
#OK to use herd_models here? it has already been log'd and exponentiated
View(NEWcar.5)

x=NEWcar.5$amp
y=NEWcar.5$sd_amp

cor.test(x,y)
#correlation is .599, p<<.001

plot(x, y, main = "Comparing Amplitude Measures",
     xlab = "Max-Min/Mean Amp", ylab = "SD log10 Amp",
     pch = 19, frame = TRUE)
abline(lm(y ~ x, data = mtcars), col = "blue")


Cyclic_amp= NEWcar.5 %>% filter(sig_period == "YES")
View(Cyclic_amp)

x1=Cyclic_amp$amp
y1=Cyclic_amp$sd_amp

plot(x1, y1, main = "Comparing Amplitude Measures (Cyclic)",
     xlab = "Max-Min/Mean Amp", ylab = "SD log10 Amp",
     pch = 19, frame = TRUE)
abline(lm(y1 ~ x1, data = mtcars), col = "blue")
cor.test(x1,y1) #r=.904

#planning on using Max-Min/Mean amp from now on

####Final compiled data####
final_data=NEWcar.4 %>% group_by(Herd) %>%
  select(-period,-Harvest,-Annual.Precip....,-Temps....,-Max.Range.Size..km.2.,-Source)
View(final_data)

cycle_analysis= final_data %>% group_by(Herd)%>% sample_n(1)%>%
  select(Herd,Subspecies,Wolves.,Predators,Semi.Domestic.,Latitude,new_period,amp,Ecotype,Biome..S.W.,sig_period)
cycle_analysis$tmin=c(-12.28508772,
                      -0.846354167,
                      -11.94923709,
                      -15.49616228,
                      -27.23784722,
                      -28.890625,
                      -27.40992647,
                      -26.1265625,
                      -10.62310606,
                      -26.23229167,
                      -27.18998016,
                      -13.97460938,
                      -15.17317708,
                      -31.39484127,
                      -10.63848039,
                      -22.48668981,
                      -20.82589286,
                      2.223958333,
                      -4.400735294,
                      -11.58738426,
                      -14.66375969,
                      -14.44462719,
                      -19.48065476,
                      -10.98690476,
                      -18.06182796,
                      -14.93229167,
                      -20.96354167,
                      -10.47152778,
                      -9.791666667,
                      -13.4380787,
                      -6.111607143,
                      -25.24768519,
                      -27.82291667,
                      -8.574869792,
                      -11.15196078,
                      -14.63377193,
                      -27.58680556,
                      -10,
                      -30.11904762,
                      -18.790625,
                      -15.37101064,
                      -20.91145833,
                      -22.15870098)

cycle_analysis$ppt=c(31.96107456,
                     108.375,
                     42.37323944,
                     34.47203947,
                     12.86921296,
                     10.92361111,
                     11.37009804,
                     11.88541667,
                     45.2834596,
                     6.538541667,
                     14.28472222,
                     12.93880208,
                     23.00325521,
                     3.958829365,
                     55.71323529,
                     5.119212963,
                     36.71056548,
                     58.75520833,
                     99.68198529,
                     46.75289352,
                     27.30135659,
                     30.20888158,
                     36.61532738,
                     52.12559524,
                     21.82728495,
                     32.1234375,
                     16.13151042,
                     59.00833333,
                     52.59440104,
                     51.31655093,
                     83.81919643,
                     3.844328704,
                     14.28422619,
                     79.01627604,
                     39.48866422,
                     27.94572368,
                     18.81944444,
                     49.40277778,
                     20.49702381,
                     14.6171875,
                     22.58510638,
                     17.92824074,
                     4.316176471)

cycle_analysis$NDVI=c(890.746181,
                      4374.618297,
                      179.8326586,
                      5913.78692,
                      2425.323529,
                      2333.800362,
                      2440.17853,
                      2042.059365,
                      64.27840112,
                      2928.753215,
                      3612.24604,
                      1181.157933,
                      4410.740809,
                      81.93887665,
                      194.9469534,
                      4197.430456,
                      2847.172083,
                      2609.032578,
                      1915.850634,
                      7765.053004,
                      2770.459009,
                      606.9634794,
                      2637.145238,
                      461.1026316,
                      2188.836884,
                      4430.079827,
                      3522.661616,
                      3624.540301,
                      721.4375848,
                      6991.332969,
                      361.9652406,
                      4940.868559,
                      2510.523098,
                      53.23583181,
                      598.1394155,
                      901.5887949,
                      385,
                      2247.947368,
                      1395.809142,
                      3142.330321,
                      376.2063927,
                      3796.807692,
                      4515.467342)
View(cycle_analysis)

final_cyclic= cycle_analysis %>% filter(sig_period == "YES")
final_cyclic$amp <- final_cyclic$amp/2
View(final_cyclic)

boxplot(final_cyclic$amp,main="Amplitudes of Caribou Herd Cycles", 
        xlab="Relative Amplitude",horizontal = TRUE, col="chartreuse4")
fivenum(final_cyclic$amp)

####ATTACH####

attach(final_cyclic)

####Significant Correlations####

plot(new_period, amp, main = "Amplitude vs Period",
     xlab = "Period (years)", ylab = "Max-Min/Mean Amp",
     pch = 19, frame = TRUE)
abline(lm(amp ~ new_period, data = final_cyclic), col = "blue")
cor.test(new_period,amp) #r=.547, p=.015 some slight correlation

####Insignificant Correlations####

plot(Latitude, amp, main = "Amplitude vs Latitude",
     xlab = "Latitude (degrees)", ylab = "Max-Min/Mean Amp",
     pch = 19, frame = TRUE)
abline(lm(amp ~ Latitude, data = final_cyclic), col = "blue")
cor.test(Latitude,amp) #r=-.29, p=.227 no significant correlation

plot(Latitude, new_period, main = "Period vs Latitude",
     xlab = "Latitude (degrees)", ylab = "Period",
     pch = 19, frame = TRUE)
abline(lm(new_period ~ Latitude, data = final_cyclic), col = "blue")
cor.test(Latitude,new_period) #r=-.295, p=.22 no significant correlation

####Categorical####

plot(Predators, new_period, main = "Period vs Predators (Cyclic)",
     xlab = "Number of Predators", ylab = "Period",
     pch = 19, frame = TRUE)
# Doesn't appear to be a relationship

plot(Predators, amp, main = "Amplitude vs Predators (Cyclic)",
     xlab = "Number of Predators", ylab = "Amplitude",
     pch = 19, frame = TRUE)
# Hard to tell here, but doesn't appear to be a relationship
# Plotting wolves was very similar to overall predators

#Note: when sd_amp was replaced with max-min/mean amp, period and amp became 
# more correlated, but amp and latitude became less correlated

####GLM prep - not necessary code?####

final_cyclic$Subspecies=factor(final_cyclic$Subspecies,levels=c("R. t. groenlandicus", 
   "R. t. fennicus", "R. t. granti","R. t. caribou", "R. t. tarandus", "R. t. platyrhynchus"))
final_cyclic$Predators=factor(final_cyclic$Predators, levels=c("0","1","2"))
final_cyclic$Wolves.=factor(final_cyclic$Wolves., levels=c("0","1"))
final_cyclic$Biome..S.W.=factor(final_cyclic$Biome..S.W.)
final_cyclic$Ecotype=factor(final_cyclic$Ecotype)
final_cyclic$Semi.Domestic.=factor(final_cyclic$Semi.Domestic.)
summary(final_cyclic)
View(final_cyclic)

#Use lm or glm function?
model=glm(new_period~amp+Latitude+Predators+Wolves.+Subspecies+Ecotype+Semi.Domestic.+Biome..S.W.+NDVI+tmin+ppt)
summary(model)

X = model.matrix(model) 
(corr = round(cor(X[,-1]), digit=2)) 

install.packages("ggcorrplot")
library(ggcorrplot)
ggcorrplot(corr,
           hc.order = TRUE,
           method = "square", # (default=square), another method=circle
           type = "lower",   # Get the lower triangle
           lab = TRUE)+       # Add correlation coefficients
  ggtitle("Visualization of a correlation matrix")+
  labs(y="Predictors", x = "Predictors")+
  theme(axis.title.x = element_text(angle = 0, color = 'grey20',size=14),
        axis.title.y = element_text(angle = 90, color = 'grey20',size=14))

install.packages("faraway")
library(faraway)            
vif(model) # Calculates the variation inflation factors of 
#     all predictors in regression models
# A general guideline is that a VIF larger than 10 is large, 
#  indicating that the model has problems estimating the coefficient.

# multiple has vifs over 10

####Map####
install.packages("maps")
library(maps)

world_map <- map_data("world")
ggplot()+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),
               fill="white",color="black",size=1.05)+
  geom_point(data=final_data, aes(x=Longitude, y = Latitude), fill = "red", size = 3, pch = 21)+
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=20),
    axis.text = element_text(size=16, color="black"),
    legend.text = element_text(size=16,color="black"),
    legend.title = element_blank())

####Plots with weather data####

#"NDVI" is the dynamic habitat index values for that area.
#"tmin" is the average winter monthly minimum temps in celsius for the area.
#"ppt" is the average winter monthly precipitation in mm for the area.

#Period 

plot(tmin, new_period, main = "Period vs Winter Temps",
     xlab = "Avg Winter Monthly Min Temp (degrees C)", ylab = "Period (years)",
     pch = 19, frame = TRUE)
abline(lm(new_period ~ tmin, data = final_cyclic), col = "blue")
cor.test(tmin,new_period) #r=-.397, p=.09275 no significant correlation but suggestive

plot(NDVI, new_period, main = "Period vs NDVI",
     xlab = "NDVI", ylab = "Period (years)",
     pch = 19, frame = TRUE)
abline(lm(new_period ~ NDVI, data = final_cyclic), col = "blue")
cor.test(NDVI,new_period) #r=-.3658, p=.1235 no significant correlation

plot(ppt, new_period, main = "Period vs Precip",
     xlab = "Avg Winter Monthly Precip (mm)", ylab = "Period (years)",
     pch = 19, frame = TRUE)
abline(lm(new_period ~ ppt, data = final_cyclic), col = "blue")
cor.test(ppt,new_period) #r=-.111, p=.6524 no significant correlation

#Amplitude

plot(tmin, amp, main = "Amplitude vs Winter Temps",
     xlab = "Avg Winter Monthly Min Temp (degrees C)", ylab = "Relative Amplitude",
     pch = 19, frame = TRUE)
abline(lm(amp ~ tmin, data = final_cyclic), col = "blue")
cor.test(tmin,amp) #r=-.137, p=.576 no significant correlation

plot(NDVI, amp, main = "Amplitude vs NDVI",
     xlab = "NDVI", ylab = "Relative Amplitude",
     pch = 19, frame = TRUE)
abline(lm(amp ~ NDVI, data = final_cyclic), col = "blue")
cor.test(NDVI,amp) #r=-.522, p=.02191 SIGNIFICANT CORRELATION

plot(ppt, amp, main = "Amplitude vs Precip",
     xlab = "Avg Winter Monthly Precip (mm)", ylab = "Relative Amplitude",
     pch = 19, frame = TRUE)
abline(lm(amp ~ ppt, data = final_cyclic), col = "blue")
cor.test(ppt,amp) #r=-.033, p=.8926 no significant correlation

# tmin and NDVI may be slightly correlated to the period length

# NDVI is the only significant weather predictor of amplitude, but this could 
# be due to the few high NDVI values skewing the results

####Final Period Model Selection####
period.model=glm(new_period~amp+Latitude+Wolves.+Predators+Subspecies+Ecotype+Biome..S.W.+Semi.Domestic.+NDVI+tmin+ppt)
#141.7248

step(period.model, direction = "backward") 

#Removed ecotype and semi-domestic. AIC didn't change- does that mean I shouldn't remove?
period.model.1=glm(new_period~amp+Latitude+Wolves.+Predators+Subspecies+Biome..S.W.+NDVI+tmin+ppt)
summary(period.model.1)#141.7248
#AICc 413.72

#Removing based on the highest p-value, except predators was removed after ppt
# because having both wolves+predators doesn't makes sense

period.model.2=update(period.model.1, .~. - ppt)
summary(period.model.2) #139.73 - very close to change in 2 for AIC
#AICc 299.73

period.model.3=update(period.model.2, .~. - Predators)
summary(period.model.3)#137.99 - again, close to change in 2...
#AICc 242.99

period.model.4=update(period.model.3, .~. - Wolves.)
summary(period.model.4)#136.06 - close to 2. remove?
#AICc 208.86

period.model.5=update(period.model.4, .~. - NDVI)
summary(period.model.5)#136.23 - AIC went up. Not worth removing
#AICc 188.23

final.period.model=glm(new_period ~ amp+Latitude+Subspecies+Biome..S.W.+NDVI+tmin)
summary(final.period.model)#AIC=136.06

####Final Amplitude Model Selection####

amp.model=glm(amp~new_period+Latitude+Wolves.+Predators+Subspecies+Ecotype+Biome..S.W.+Semi.Domestic.+NDVI+tmin+ppt)
summary(amp.model) #40.676

step(amp.model, direction = "backward") #33.01

#Removed ecotype and semi-domestic
amp.model.1=glm(amp~new_period+Latitude+Wolves.+Predators+Subspecies+Biome..S.W.+NDVI+tmin+ppt)
summary(amp.model.1)#40.676 - again didn't change when ecotype and semi-domestic removed. 
# because they are categorical variables?

amp.model.2=update(amp.model.1, .~. -Predators)
summary(amp.model.2) #38.677
#AICc 198.68

amp.model.3=update(amp.model.2, .~. -ppt)
summary(amp.model.3) #36.954 - very close to 2 again
#AICc 141.83

amp.model.4=update(amp.model.3, .~. -NDVI)
summary(amp.model.4) #35.132 
#AICc 107.86

amp.model.5=update(amp.model.4, .~. -Wolves.)
summary(amp.model.5)#33.503
#AICc 85.5

amp.model.6=update(amp.model.5, .~. -Latitude)
summary(amp.model.6)#33.006
#AICc 70.73

amp.model.7=glm(amp~new_period+Subspecies+tmin)
summary(amp.model.7)#41.003
#AICc 61.003

#Using AICc would eliminate variables even the backwards elimination didn't

backward.step.amp.model=glm(amp~new_period+Subspecies+Biome..S.W.+tmin)
summary(backward.step.amp.model)#33.01

#Maybe use both AIC and AICc in selecting a model? Pick the model with the lowest 
# AICc as long as the preceding step didn't result in an increase in AIC?
#Doesn't make a huge difference in period model, but amp it would (amp.model.6)

plot(amp.model.6)

final.amp.model=glm(amp~new_period+Subspecies+Biome..S.W.+tmin)
summary(final.amp.model)#36.825

#Seems like there are variables with extremely high p-values that when removed don't
# result in a AIC drop of at least 2



