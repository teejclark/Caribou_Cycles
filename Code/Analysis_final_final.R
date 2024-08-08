#NOTE: not all of the code on this script is vital, but you will need to progress from from car to NEWCAR.1
# through NEWCAR.4 along the way.

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

####Amp+Period Calculations####

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


####Final compiled data####
final_data=NEWcar.4 %>% group_by(Herd) %>%
  select(-period,-Harvest,-`Annual Precip (?)`,-`Temps (?)`,-`Max Range Size (km^2)`,-Source)
View(final_data)

cycle_analysis= final_data %>% group_by(Herd)%>% sample_n(1)%>%
  select(Herd,Subspecies,`Wolves?`,Predators,`Semi-Domestic?`,Latitude,new_period,amp,Ecotype,`Biome (S/W)`,sig_period)
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
