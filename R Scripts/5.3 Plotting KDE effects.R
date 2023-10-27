#5.4 plot resource supplement index effects on model parameters, using the predict function from SECR. reference level of session 1, trail type = 4wd track, for all figures, season = summer. 

library(secr)
library(ggplot2)
library(patchwork)

################################### Fig 5.A  Density Predicted##################################

Dkde <- expand.grid(kde_value = seq(min(unlist(covariates(allsessionmasks))), max(unlist(covariates(allsessionmasks))), by = 0.05),
                    Kde_value = 0,
                    TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                    session = factor(c(1,2,3,4)),
                    Season = factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))


Dkde <- Dkde[Dkde$TrailType == "4wdtrack",]
Dkde <- Dkde[Dkde$session == "1",]

# predict estimates into a dataframe where kde ranges 
all_predicted_dkde <- predict(fit2, newdata = Dkde) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_dkde <- unlist(sapply(all_predicted_dkde, "[", "D","estimate"))*100
lower_bound_dkde <- unlist(sapply(all_predicted_dkde, "[", "D","lcl"))*100
upper_bound_dkde <- unlist(sapply(all_predicted_dkde, "[", "D","ucl"))*100
kde_value <- round(seq(min(unlist(covariates(allsessionmasks))), max(unlist(covariates(allsessionmasks))), by = 0.05), digits =2)
plot_data_dkde <- cbind.data.frame(predicted_values_dkde, lower_bound_dkde, upper_bound_dkde, kde_value)



##Density ~Kde plot

plot_D_KDE <- ggplot(plot_data_dkde, aes(x = kde_value, y = predicted_values_dkde)) + 
  geom_ribbon(aes(ymin = lower_bound_dkde, ymax = upper_bound_dkde), alpha = 0.2) +
  geom_line(linetype = 1, size = 1) + 
  labs(title = "", x = "Resource supplement index", y = expression(atop("Dingoes", "(km)"^-2))) +
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


################################### Fig 5.B  g0 Predicted##################################


gkde <- expand.grid(kde_value = 0,
                    Kde_value = seq(min(unlist(covariates(allsessionmasks))), max(unlist(covariates(allsessionmasks))), by = 0.05),
                    TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                    session = factor(c(1,2,3,4)),
                    Season = factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))


gkde <- gkde[gkde$TrailType == "4wdtrack",]
gkde <- gkde[gkde$session == "1",]

# predict estimates into a dataframe where kde ranges 
all_predicted_gkde <- predict(fit2, newdata = gkde) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_gkde <- unlist(sapply(all_predicted_gkde, "[", "g0","estimate"))
lower_bound_gkde <- unlist(sapply(all_predicted_gkde, "[", "g0","lcl"))
upper_bound_gkde <- unlist(sapply(all_predicted_gkde, "[", "g0","ucl"))
kde_value <- round(seq(min(unlist(covariates(allsessionmasks))), max(unlist(covariates(allsessionmasks))), by = 0.05), digits =2)
plot_data_gkde <- cbind.data.frame(predicted_values_gkde, lower_bound_gkde, upper_bound_gkde, kde_value)



##g0 ~kde plot

plot_gkde <- ggplot(plot_data_gkde, aes(x = kde_value, y = predicted_values_gkde)) + 
  geom_ribbon(aes(ymin = lower_bound_gkde, ymax = upper_bound_gkde), alpha = 0.2) +
  geom_line(linetype = 1, size = 1) + 
  labs(title = "", x = "Resource supplement index", y = bquote("g0")) +
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


################################### Fig 5.C  Sigma Predicted##################################
###gsig with kde  
sigkde <- expand.grid(kde_value = 0,
                      Kde_value = seq(min(unlist(covariates(allsessionmasks))), max(unlist(covariates(allsessionmasks))), by = 0.05),
                      TrailType = factor(c("4wdtrack","Walktrail","Unsealedroad")),
                      session = factor(c(1,2,3,4)),
                      Season = factor(c("Summer", "Shoulder"), levels = levels(sesscov$Season)))


sigkde <- sigkde[sigkde$TrailType == "4wdtrack",]
sigkde <- sigkde[sigkde$session == "1",]

# predict estimates into a dataframe where kde ranges 
all_predicted_sigkde <- predict(fit2, newdata = sigkde) 

# turn this into a nice dataframe for plotting, and * 100 so density is in km2 
predicted_values_sigkde <- unlist(sapply(all_predicted_sigkde, "[", "sigma","estimate"))
lower_bound_sigkde <- unlist(sapply(all_predicted_sigkde, "[", "sigma","lcl"))
upper_bound_sigkde <- unlist(sapply(all_predicted_sigkde, "[", "sigma","ucl"))
kde_value <- round(seq(min(unlist(covariates(allsessionmasks))), max(unlist(covariates(allsessionmasks))), by = 0.05), digits =2)
plot_data_sigkde <- cbind.data.frame(predicted_values_sigkde, lower_bound_sigkde, upper_bound_sigkde, kde_value)



##sig ~ kde plot

plot_sigkde <- ggplot(plot_data_sigkde, aes(x = kde_value, y = predicted_values_sigkde)) + 
  geom_ribbon(aes(ymin = lower_bound_sigkde, ymax = upper_bound_sigkde), alpha = 0.2) +
  geom_line(linetype = 1, size = 1) + 
  labs(title = "", x = "Resource supplement index", y = bquote("Sigma")) +
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
        axis.line.y = element_line(color = "black", size = 0.5)) # Add y-axis line



#####Plot all 3 figures together for figure (5).

png("Figures/Manuscript Figures/KDE Figs.png", width = 10, height = 3, res= 600, units = "in")

plot_D_KDE+plot_gkde+plot_sigkde+plot_annotation(tag_levels = "a")

dev.off()

# end
