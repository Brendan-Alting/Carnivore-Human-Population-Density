#5.5 plot resource supplement index effects on model parameters. reference level of session 1, trail type = 4wd track, for all figures, Time Period = 'A' 

library(secr)
library(ggplot2)

library(patchwork)

###################################  1)) g0 ~TrailType ##########################

###g0 with TrailType value 
gtrail <- expand.grid(kde_value = 0,
                      TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                      session = factor(c(1,2,3,4,5,6)),
                      TimePeriod = factor(c("A", "B", "C"), levels = levels(TimePeriodcov$TimePeriod)))

gtrail <- subset(gtrail, ((session %in% c(1,4)& TimePeriod == "A") | (session %in% c(2,5) & TimePeriod == "B") | (session %in% c(3,6) & TimePeriod == "C")))

gtrail <- gtrail[gtrail$TimePeriod == "A",]
gtrail <- gtrail[which(gtrail$session == "1"),]

# predict estimates into a dataframe where g0 ranges 
all_predicted_g_trail <- predict(Full, newdata = gtrail) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_g_trail <- unlist(sapply(all_predicted_g_trail, "[", "g0","estimate"))
lower_bound_gtrail <- unlist(sapply(all_predicted_g_trail, "[", "g0","lcl"))
upper_bound_gtrail <- unlist(sapply(all_predicted_g_trail, "[", "g0","ucl"))
Trailtype <- rep(factor(c("4wdtrack", "Walktrail", "Road")))###GO BACK AND FIX

plot_data_g0trail <- cbind.data.frame(predicted_values_g_trail, lower_bound_gtrail, upper_bound_gtrail, Trailtype)

##g0~trail plot

plot_g0_trail <- ggplot(plot_data_g0trail, aes(x = Trailtype, y = predicted_values_g_trail)) + 
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = lower_bound_gtrail, ymax = upper_bound_gtrail)) + 
  ylim(0, 0.3) + 
  labs(title = "", x = "Trail Type", y = bquote("g0")) +
  theme(axis.title = element_text(size = 12),       
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        panel.background = element_blank(),      # Remove background panel
        panel.grid.major = element_blank(),       # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(size = 0.5),
        axis.ticks.y = element_line(size = 0.5),
        axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
        axis.line.y = element_line(color = "black", size = 0.5)) # Add y-axis line)




################## 2)  g0 plot ~ Time Period##################

gtimeperiod <- expand.grid(kde_value = 0,
                       TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                       session = factor(c(1,2,3,4,5,6)),
                       TimePeriod = factor(c("A", "B", "C"), levels = levels(TimePeriodcov$TimePeriod)))

gtimeperiod <- gtimeperiod[gtimeperiod$TrailType == "4wdtrack",]
gtimeperiod <- gtimeperiod[gtimeperiod$session == "1",]

# predict estimates into a dataframe where g0 changes, based on the model we made from "4.Running Models"  
all_predicted_g_timeperiod <- predict(Full, newdata = gtimeperiod) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_gtimeperiod <- unlist(sapply(all_predicted_g_timeperiod, "[", "g0","estimate"))
lower_bound_gtimeperiod <- unlist(sapply(all_predicted_g_timeperiod, "[", "g0","lcl"))
upper_bound_gtimeperiod <- unlist(sapply(all_predicted_g_timeperiod, "[", "g0","ucl"))
TimePeriod <- rep(factor(c("A", "B", "C"), levels = levels(TimePeriodcov$TimePeriod)))

plot_data_g0timeperiod <- cbind.data.frame(predicted_values_gtimeperiod, lower_bound_gtimeperiod, upper_bound_gtimeperiod, TimePeriod)

##g0~season plot

plot_g0_timeperiod <- ggplot(plot_data_g0timeperiod, aes(x = TimePeriod, y = predicted_values_gtimeperiod)) + 
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = lower_bound_gtimeperiod, ymax = upper_bound_gtimeperiod)) + 
  ylim(0, 0.2) + 
  labs(title = "", x = "Survey Period", y = expression(atop("g0"))) +
  theme(axis.title = element_text(size = 12),       
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        panel.background = element_blank(),      # Remove background panel
        panel.grid.major = element_blank(),       # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(size = 0.5),
        axis.ticks.y = element_line(size = 0.5),
        axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
        axis.line.y = element_line(color = "black", size = 0.5)) # Add y-axis line)

################## 3) sigma plot ~ Time period##################

sigtimeperiod <- expand.grid(kde_value = 0,
                         TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                         session = factor(c(1,2,3,4,5,6)),
                         TimePeriod = factor(c("A", "B", "C"), levels = levels(TimePeriodcov$TimePeriod)))


sigtimeperiod <- sigtimeperiod[sigtimeperiod$TrailType == "4wdtrack",]
sigtimeperiod <- sigtimeperiod[sigtimeperiod$session == "1",]

# predict estimates into a dataframe where sigma varies with model created in "4.Running Models" 
all_predicted_sig_timeperiod <- predict(Full, newdata = sigtimeperiod) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_sigtimeperiod <- unlist(sapply(all_predicted_sig_timeperiod, "[", "sigma","estimate"))
lower_bound_sigtimeperiod <- unlist(sapply(all_predicted_sig_timeperiod, "[", "sigma","lcl"))
upper_bound_sigtimeperiod <- unlist(sapply(all_predicted_sig_timeperiod, "[", "sigma","ucl"))
TimePeriod <- rep(factor(c("A", "B", "C"), levels = levels(TimePeriodcov$TimePeriod)))

plot_data_sigtimeperiod <- cbind.data.frame(predicted_values_sigtimeperiod, lower_bound_sigtimeperiod, upper_bound_sigtimeperiod, TimePeriod)

##sigma~timeperiod plot

plot_sig_timeperiod <- ggplot(plot_data_sigtimeperiod, aes(x = TimePeriod, y = predicted_values_sigtimeperiod)) + 
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = lower_bound_sigtimeperiod, ymax = upper_bound_sigtimeperiod)) + 
  ylim(0, 6500) + 
  labs(title = "", x = "Survey Period", y = bquote("Sigma")) +
  theme(axis.title = element_text(size = 12),       
        axis.title.y = element_text(angle = 0, vjust = 0.5),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 8),
        panel.background = element_blank(),      # Remove background panel
        panel.grid.major = element_blank(),       # Remove major grid lines
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_line(size = 0.5),
        axis.ticks.y = element_line(size = 0.5),
        axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
        axis.line.y = element_line(color = "black", size = 0.5)) # Add y-axis line)

##plotting all 3 of these plots together

#fig c: Sigma and detectability by Survey Period and detectability by trail type. 

png("Figures/Manuscript Figures/Covariate Differences Figs.png", width = 7.5, height = 6, res= 600, units = "in")

((plot_g0_trail) + (plot_Density)) / (plot_g0_season+plot_sig_season) +plot_annotation(tag_levels = "a")

dev.off()

