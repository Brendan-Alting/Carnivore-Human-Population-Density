#5.3, plotting density surface 
library(secr)


par(mfrow = c(1,1)) #set panels

plot(DensitySurface, scale = 100, breaks = seq(0,0.48,0.02), title = expression("Dingoes / km"^2))

#Then export plot to 'Figures/Manuscript Figures' 


