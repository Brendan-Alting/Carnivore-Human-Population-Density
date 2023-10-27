#5.5 plot resource supplement index effects on model parameters. reference level of session 1, trail type = 4wd track, for all figures, season = summer. 

library(secr)
library(ggplot2)
library(patchwork)

###################################  1)) g0 ~TrailType ##########################

###g0 with TrailType value 
gtrail <- expand.grid(kde_value = 0,
                      Kde_value = 0,
                      TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                      session = factor(c(1,2,3,4)),
                      Season = factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))

gtrail <- subset(gtrail, !((session %in% c(1,3)& Season == "Shoulder") | (session %in% c(2,4) & Season == "Summer")))
gtrail <- gtrail[gtrail$Season == "Summer",]
gtrail <- gtrail[-which(gtrail$session == "3"),]

# predict estimates into a dataframe where g0 ranges 
all_predicted_g_trail <- predict(fit2, newdata = gtrail) 

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
  ylim(0, 0.1) + 
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

################### 2) Density varies with session#########################

Density <- expand.grid(kde_value = 0,
                       Kde_value = 0,
                       TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                       session = factor(c(1,2,3,4)),
                       Season = factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))


Density <- Density[Density$Season == "Summer",]
Density <- Density[which(Density$TrailType == "4wdtrack"),]

# predict estimates into a dataframe where g0 ranges 
all_predicted_Density <- predict(fit2, newdata = Density) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_Density <- unlist(sapply(all_predicted_Density, "[", "D","estimate"))*100
lower_bound_Density <- unlist(sapply(all_predicted_Density, "[", "D","lcl"))*100
upper_bound_Density <- unlist(sapply(all_predicted_Density, "[", "D","ucl"))*100
Session <- rep(factor(c("1", "2", "3", "4")))###GO BACK AND FIX

plot_data_Density <- cbind.data.frame(predicted_values_Density, lower_bound_Density, upper_bound_Density, Session)

##D~session plot

plot_Density <- ggplot(plot_data_Density, aes(x = Session, y = predicted_values_Density)) + 
  geom_point(size = 4) +
  geom_pointrange(aes(ymin = lower_bound_Density, ymax = upper_bound_Density)) + 
  ylim(0, 0.075) + 
  labs(title = "", x = "Session", y = expression(atop("Dingoes", "(km)"^-2))) +
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




################## 3)  g0 plot, varying with season##################

gseason <- expand.grid(kde_value = 0,
                       Kde_value = 0,
                       TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                       session = factor(c(1,2,3,4)),
                       Season = factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))

gseason <- gseason[gseason$TrailType == "4wdtrack",]
gseason <- gseason[gseason$session == "1",]

# predict estimates into a dataframe where g0 changes, based on the model we made from "4.Running Models"  
all_predicted_g_season <- predict(fit2, newdata = gseason) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_gseason <- unlist(sapply(all_predicted_g_season, "[", "g0","estimate"))
lower_bound_gseason <- unlist(sapply(all_predicted_g_season, "[", "g0","lcl"))
upper_bound_gseason <- unlist(sapply(all_predicted_g_season, "[", "g0","ucl"))
Season <- rep(factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))

plot_data_g0season <- cbind.data.frame(predicted_values_gseason, lower_bound_gseason, upper_bound_gseason, Season)

##g0~season plot

plot_g0_season <- ggplot(plot_data_g0season, aes(x = Season, y = predicted_values_gseason)) + 
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = lower_bound_gseason, ymax = upper_bound_gseason)) + 
  ylim(0, 0.1) + 
  labs(title = "", x = "Season", y = expression(atop("g0"))) +
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

################## 4) sigma plot, varying with season##################

sigseason <- expand.grid(kde_value = 0,
                         Kde_value = 0,
                         TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                         session = factor(c(1,2,3,4)),
                         Season = factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))


sigseason <- sigseason[sigseason$TrailType == "4wdtrack",]
sigseason <- sigseason[sigseason$session == "1",]

# predict estimates into a dataframe where sigma varies with model created in "4.Running Models" 
all_predicted_sig_season <- predict(fit2, newdata = sigseason) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_sigseason <- unlist(sapply(all_predicted_sig_season, "[", "sigma","estimate"))
lower_bound_sigseason <- unlist(sapply(all_predicted_sig_season, "[", "sigma","lcl"))
upper_bound_sigseason <- unlist(sapply(all_predicted_sig_season, "[", "sigma","ucl"))
Season <- rep(factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))

plot_data_sigseason <- cbind.data.frame(predicted_values_sigseason, lower_bound_sigseason, upper_bound_sigseason, Season)

##sigma~season plot

plot_sig_season <- ggplot(plot_data_sigseason, aes(x = Season, y = predicted_values_sigseason)) + 
  geom_point(size = 3) +
  geom_pointrange(aes(ymin = lower_bound_sigseason, ymax = upper_bound_sigseason)) + 
  ylim(0, 6500) + 
  labs(title = "", x = "Season", y = bquote("Sigma")) +
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

#fig c: Sigma and detectability by season and density by session

png("Figures/Manuscript Figures/Covariate Differences Figs.png", width = 7.5, height = 6, res= 600, units = "in")

((plot_g0_trail) + (plot_Density)) / (plot_g0_season+plot_sig_season) +plot_annotation(tag_levels = "a")

dev.off()

