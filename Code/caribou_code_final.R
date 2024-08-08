# Analysis_final_final.R needs to be run first
# "final_cyclic" has the data for correlations/model fitting
# "NEWcar.4" has all the data for results, maps, etc.

#####################################################################################################

#### 1) PLOT DATA FOR ANNE GUNN - SHOWING TRUE CYCLES
# Herds = Alakyla, Bathurst, Beverly, Bluenose West, Cape Bathurst, Central Arctic,
# Delta, Edgeoya, Fortymile, George River, Kainuu, Kangerlussuaq-Sisimiut, Leaf River,
# Porcupine, Qamanirjuaq, Southampton Is., Taimyr, Western Arctic, White Mountains

NEWcar.4 %>% filter(Herd == "Kainuu") %>%
  ggplot(aes(x = Year, y = herd_models))+
  geom_point(size = 2)+
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y=0)+
  labs(y="Population Size", x = "Year")+
  theme_bw()
  
## Create additional graphs to add in with figure 1
NEWcar.4 %>% filter(Herd == "Kainuu") %>%
  ggplot()+
  geom_line(aes(x = Year, y = herd_models), color = "black", size = 2)+
  geom_point(aes(x = Year, y = Population), size = 6, color = "darkred")+
  scale_y_continuous(labels = scales::comma)+
  expand_limits(y=0)+
  labs(y="Population Size", x = "Year")+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=32),
    axis.text = element_text(size=24, color="black"),
    legend.text = element_text(size=20,color="black"),
    legend.title = element_blank())

ggsave("figure1_kainuu.png", dpi = 600, width = 10, height = 8)
  

#####################################################################################################
  
#### 2) MAP DATA
  
library(maps)
world_map <- map_data("world")
  
ggplot()+
  geom_polygon(data=world_map, aes(x=long, y=lat, group=group),
                 fill="white",color="black",size=1.05)+
  geom_point(data=final_data, aes(x=Longitude, y = Latitude, fill = factor(sig_period)), size = 3, pch = 21)+
  theme_bw() +
  theme(
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=20),
    axis.text = element_text(size=16, color="black"),
    legend.text = element_text(size=16,color="black"),
    legend.title = element_blank())
  
# make a map with a circular projection?
# new world map
library(rgdal)
library(raster)

data("wrld_simpl", package = "maptools")
wm <- crop(wrld_simpl, extent(-180, 180, 50, 90))

x_lines <- seq(-120,180, by = 60)

ggplot()+
  geom_polygon(data = wm, aes(x = long, y = lat, group = group), fill = "grey", color = "darkgrey", alpha = 0.5)+
  geom_point(data = final_data %>% arrange(sig_period), aes(x=Longitude, y = Latitude, color = factor(sig_period)), size = 3, pch = 19)+
  scale_color_manual(values = c("#264653", "#fb8500"))+
  # convert to polar coordinates
  coord_map("ortho", orientation = c(90, 0, 0))+
  scale_y_continuous(breaks = seq(50, 90, by = 5), labels = NULL)+
  # removes axes and labels
  scale_x_continuous(breaks = NULL)+
  xlab("")+
  ylab("")+
  # adds labels
  geom_text(aes(x = 180, y = seq(55, 85, by = 10), hjust = -0.2, vjust =-0.2, label = paste0(seq(55,85,by=10), "°N")))+
  geom_text(aes(x = x_lines, y = 42, label = c("120°W", "60°W", "0°", "60°E", "120°E", "180°W")))+
  # adds axesw
  #geom_hline(aes(yintercept = 45), size = 1)+
  geom_segment(aes(y= 45, yend = 90, x = x_lines, xend = x_lines), linetype = "dashed", size = 0.15)+
  # change theme
  theme(panel.background = element_blank(),
        legend.position = "none",
        panel.grid.major = element_line(size = 0.15, linetype = 'dashed',
                                        colour = "black"),
        axis.ticks=element_blank())
  
ggsave("figure1.png", dpi = 600, width = 10, height = 10)

#####################################################################################################

#### 3) plot NDVI and Temp vs. Period/Amplitude

a <- ggplot(final_cyclic, aes(x = NDVI/1000, y = new_period))+
  geom_point(size = 3, color = "#264653")+
  geom_smooth(method = "lm", alpha = 0.3, size = 2, color = "#fb8500", fill = "#fb8500")+
  labs(x = "", y = "Cycle Period")+
  ylim(0,70)+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=32),
    axis.text = element_text(size=24, color="black"),
    legend.text = element_text(size=20,color="black"),
    legend.title = element_blank())

b <- ggplot(final_cyclic, aes(x = tmin, y = new_period))+
  geom_point(size = 3, color = "#264653")+
  geom_smooth(method = "lm", alpha = 0.3, size = 2, color = "#fb8500", fill = "#fb8500")+
  ylim(0,70)+
  labs(x = "", y = "")+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=32),
    axis.text.x = element_text(size=24, color="black"),
    axis.text.y = element_blank(),
    legend.text = element_text(size=20,color="black"),
    legend.title = element_blank())

c <- ggplot(final_cyclic, aes(x = NDVI/1000, y = amp))+
  geom_point(size = 3, color = "#264653")+
  geom_smooth(method = "lm", alpha = 0.3, size = 2, color = "#fb8500", fill = "#fb8500")+
  labs(x = "NDVI/1000", y = "Cycle Amplitude")+
  ylim(0,1.6)+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=32),
    axis.text = element_text(size=24, color="black"),
    legend.text = element_text(size=20,color="black"),
    legend.title = element_blank())

d <- ggplot(final_cyclic, aes(x = tmin, y = amp))+
  geom_point(size = 3, color = "#264653")+
  geom_smooth(method = "lm", alpha = 0.3, size = 2, color = "#fb8500", fill = "#fb8500")+
  labs(x = "Av. Winter Min. Temp.", y = "")+
  ylim(0,1.6)+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=32),
    axis.text.x = element_text(size=24, color="black"),
    axis.text.y = element_blank(),
    legend.text = element_text(size=20,color="black"),
    legend.title = element_blank())

library(patchwork)

(a+b)/(c+d)+plot_annotation(tag_levels = "A")&
  theme(plot.tag = element_text(size = 34))

ggsave("figure2.png", dpi = 600, width = 12, height = 10)

#####################################################################################################

#### 3.5) Re-run Model Selection

# PERIOD
period.model.1=glm(new_period~amp+Latitude+Wolves.+Predators+Subspecies+Biome..S.W.+NDVI+tmin+ppt)
summary(period.model.1)#141.7248


# standardize coefficients
final_cyclic_alt <- final_cyclic
final_cyclic_alt$amp <- scale(final_cyclic_alt$amp)
final_cyclic_alt$Latitude <- scale(final_cyclic_alt$Latitude)
final_cyclic_alt$NDVI <- scale(final_cyclic_alt$NDVI)
final_cyclic_alt$tmin <- scale(final_cyclic_alt$tmin)
attach(final_cyclic_alt)

period.model.2=glm(new_period~amp+Latitude+Subspecies+Biome..S.W.+NDVI+tmin)
summary(period.model.2)

# AMPLITUDE
amp.model.1=glm(amp~new_period+Latitude+Wolves.+Predators+Subspecies+Biome..S.W.+NDVI+tmin+ppt)
summary(amp.model.1)

amp.model.2=glm(amp~new_period+Subspecies+Biome..S.W.+tmin+NDVI)
summary(amp.model.2)

amp.model.3=glm(amp~new_period+Subspecies+Biome..S.W.+tmin+ppt)
summary(amp.model.3)
  
#####################################################################################################

#### 4) Compare Results?

# another idea - do a double boxplot!
ggplot(final_cyclic, aes(x = new_period, y = amp))+
  geom_smooth(method = "lm", alpha = 0.3, size = 2, color = "#fb8500", fill = "#fb8500")+
  geom_point(size = 3, color = "#264653")+
  geom_point(aes(x = 55.55556, y = 0.9601457), size = 6, color = "darkred")+
  labs(x = "Cycle Period", y = "Cycle Amplitude")+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=32),
    axis.text = element_text(size=24, color="black"),
    legend.text = element_text(size=20,color="black"),
    legend.title = element_blank())

ggsave("figure2.5.png", dpi = 600, width = 10)

#####################################################################################################

### 5) ODE model

# function to calculate period
period_fun <- function(x){
  peak_os <- evaluate.pm(times=seq_along(x), signal=x)
  per=1/peak_os$frequencies[peak_os$peakMode]
  per
}

amp_fun <- function(x){
  ((max(x)-min(x))/mean(x))/2
}

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
#p <- c(u_0 = 0.8, m = 100, a = 2.5, b = 25.4, xi = 0.3, eta = 0.89,
#       c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
p <- c(u_0 = 0.8, m = 100, a = 2.5, b = 25.4, xi = 0.27, eta = 0.89,
       c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # new params
y <- c(V = 50, N = 2, P = .001) # starting states
t <- seq(0,3000,by=1) # time




##########
# sensitivity analysis params
p <- c(u_0 = .8, m = 100, a = 2.5, b = 25.4, xi = 0.27, eta = 0.89,
       c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.255) # new params
##########
    
# run the Ordinary Differential Equation
out <- as_tibble(ode(y, t, veg_moose_wolf, p, method = "ode45")[,-1]) 

# discard transitory dynamics
out2 <- out[1501:3000,]

# graph moose
# out2 %>% 
#   ggplot(aes(seq_along(N),N)) +
#   geom_line(lwd=1)

# calculate period
blah <- evaluate.pm(times = seq_along(out2$N), signal = out2$N)
1/blah$frequencies[blah$peakMode] 

# calculate amplitude
((max(out2$N)-min(out2$N))/mean(out2$N))/2 

################

# calculate period and amplitude vs. u0 and m (vegetation dynamics)

veg_all_period <- matrix(NA, 20, 20)
veg_all_amp <- matrix(NA, 20, 20)
u_0 <- seq(0.1,2,length.out=nrow(veg_all_period)) #row
m <- seq(10, 200, length.out = ncol(veg_all_period)) #col

colnames(veg_all_period) <- m
colnames(veg_all_amp) <- m
rownames(veg_all_period) <- u_0
rownames(veg_all_amp) <- u_0

for (i in 1:nrow(veg_all_period)){
  for (j in 1:ncol(veg_all_period)){
  
  # params and initial conditions
  p <- c(u_0 = u_0[i], m = m[j], a = 2.2, b = 25.4, xi = 0.3, eta = 0.89,
         c = 18.5, d = 0.5, chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
  y <- c(V = 50, N = 2, P = .001) # starting states
  t <- seq(0,3000,by=1) # time

  # run the Ordinary Differential Equation
  out <- as_tibble(ode(y, t, veg_moose_wolf, p, method = "ode45")[,-1]) 
  
  # discard transitory dynamics
  out2 <- out[1501:3000,]
  
  veg_all_period[i,j] <- period_fun(out2$N)
  veg_all_amp[i,j] <- ((max(out2$N)-min(out2$N))/mean(out2$N))/2 
  
  }}

# plot
library(RColorBrewer)
veg_period_mat <- reshape2::melt(veg_all_period, c("x", "y"), value.name = "z")
veg_period_mat2 <- log(veg_all_period)
veg_period_mat2 <- reshape2::melt(veg_period_mat2, c("x", "y"), value.name = "z")

# Veg - Period plot
a <- ggplot(data = veg_period_mat2, aes(x=x, y=y, fill = z))+
  geom_tile()+
  scale_fill_continuous(low = "blue", high = "orange")+
  xlab("Veg. Regrowth Rate (u0)")+
  ylab("Max. Veg. Coverage (m)")+
  labs(fill = "log(Per)")+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=24),
    axis.text = element_text(size=16, color="black"),
    legend.text = element_text(size=14,color="black"),
    legend.title = element_text(size = 24))

veg_amp_mat <- reshape2::melt(veg_all_amp, c("x", "y"), value.name = "z")
veg_amp_mat2 <- log((veg_all_amp/2.5))
veg_amp_mat2 <- reshape2::melt(veg_amp_mat2, c("x", "y"), value.name = "z")

b <- ggplot(data = veg_amp_mat2, aes(x=x, y=y, fill = z))+
  geom_tile()+
  scale_fill_continuous(low = "blue", high = "orange")+
  xlab("Veg. Regrowth Rate (u0)")+
  ylab("Max. Veg. Coverage (m)")+
  labs(fill = "log(Amp)")+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=24),
    axis.text = element_text(size=16, color="black"),
    legend.text = element_text(size=14,color="black"),
    legend.title = element_text(size = 24))

#save(veg_all_amp, veg_all_period, file = "veg_all.rda")

################

# calculate period and amplitude vs. c and d (predator dynamics)

pred_all_period <- matrix(NA, 20, 20)
pred_all_amp <- matrix(NA, 20, 20)
c <- seq(0.01,40,length.out=nrow(pred_all_period)) #row
d <- seq(0.01, 1, length.out = ncol(pred_all_period)) #col

colnames(pred_all_period) <- d
colnames(pred_all_amp) <- d
rownames(pred_all_period) <- c
rownames(pred_all_amp) <- c

for (i in 1:nrow(pred_all_period)){
  for (j in 1:ncol(pred_all_period)){
    
    # params and initial conditions
    p <- c(u_0 = 0.8, m = 100, a = 2.2, b = 25.4, xi = 0.3, eta = 0.89,
           c = c[i], d = d[j], chi = 0.114, mu = 9.25, kappa = 0.1, s_0 = 0.3) # params
    y <- c(V = 50, N = 2, P = .001) # starting states
    t <- seq(0,3000,by=1) # time
    
    # run the Ordinary Differential Equation
    out <- as_tibble(ode(y, t, veg_moose_wolf, p, method="ode45")[,-1]) 
    
    # discard transitory dynamics
    out2 <- out2[1501:3000,]
    
    pred_all_period[i,j] <- period_fun(out2$N)
    pred_all_amp[i,j] <- ((max(out2$N)-min(out2$N))/mean(out2$N))/2
    
  }}

pred_period_mat <- reshape2::melt(pred_all_period, c("x", "y"), value.name = "z")
pred_period_mat$z <- log(pred_period_mat$z) # try logging for visualization - doesn't do much

c <- ggplot(data = pred_period_mat, aes(x=x, y=y, fill = z))+
  geom_tile()+
  scale_fill_continuous(low = "blue", high = "orange")+
  xlab("Attack Rate (c)")+
  ylab("Handling Time (d)")+
  labs(fill = "log(Per)")+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=24),
    axis.text = element_text(size=16, color="black"),
    legend.text = element_text(size=14,color="black"),
    legend.title = element_text(size = 24))

pred_amp_mat <- reshape2::melt(pred_all_amp, c("x", "y"), value.name = "z")
pred_amp_mat$z <- log(pred_amp_mat$z/2.5) # try logging for visualization - doesn't do much

d <- ggplot(data = pred_amp_mat, aes(x=x, y=y, fill = z))+
  geom_tile()+
  scale_fill_continuous(low = "blue", high = "orange")+
  xlab("Attack Rate (c)")+
  ylab("Handling Time (d)")+
  labs(fill = "log(Amp)")+
  theme_bw()+  theme(
    plot.title=element_text(size=20,hjust=0.5),
    panel.border = element_blank(), panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black"),
    axis.title = element_text(size=24),
    axis.text = element_text(size=16, color="black"),
    legend.text = element_text(size=14,color="black"),
    legend.title = element_text(size = 24))

#save(pred_all_amp, pred_all_period, file = "pred_all.rda")

library(patchwork)

(a+b)/(c+d)+plot_annotation(tag_levels = "A")&
  theme(plot.tag = element_text(size = 34))

ggsave("figure3.png", dpi = 600, width = 14, height = 12)
