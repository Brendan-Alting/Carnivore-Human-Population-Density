###5.5 plotting KDE weight surface for appendix. 
library(secr)

par(mfrow=c(1,2))#SET MARGINS
options(scipen = 999) #remove exponent

plot(Mask, covariate = "kde_value", legend = TRUE)

#Then export file to 'Figures/Appendix Figures'. 

#End. 