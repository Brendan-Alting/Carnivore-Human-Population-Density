###plotting coefficients of variation for model. 
library(secr)
library(ggplot2)
library(patchwork)




Coefficients <- read.csv(file = "Derived Data/Coefficients.csv", header = TRUE)
Coefficientsnoref <- Coefficients[-c(1,8,14),] #remove rows that are the 'reference' levels for each of D, g0, sig 

###Assign factors for ggploting in order. 
Coefficientsnoref$Covariate <- factor(Coefficientsnoref$Covariate, levels = c("Session 6", "Session 5","Session 4", "Session 3",  "Session 2", "Walking Trail", "Road", "Time Period B", "Time Period C", "HPD"))
Coefficientsnoref$Cov <- factor(Coefficientsnoref$Cov, levels = c("Total", "Session", "HPD", "Time Period", "TrailType"))
Coefficientsnoref$Type <- factor(Coefficientsnoref$Type, levels = c("Sigma", "g0", "Density"))

##assign facet labels for a,b,c
facet_labels <- data.frame( 
  Type = c("Sigma", "g0", "Density"),
  label = c("a", "b", "c")
)


CoefficientPlotNoRef <- ggplot(Coefficientsnoref, aes(x = Estimate, y = Covariate, color = Cov))+
  geom_point(stat = "identity") +
  geom_pointrange(aes(xmin = LCI, xmax = UCI), lwd =1.5, fatten = 5) +
  labs(x = "Estimate",
       y = "") +
  coord_cartesian(xlim = c(-1, 1)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  theme_minimal() +
  scale_colour_manual(name = "Parameter",
                      values = c("Session" = "red", "HPD" = "blue", "Time Period" = "orange", "TrailType" = "black"),
                      labels = c("Session" = "Session",  "HPD"="Human\nPopulation Density", "Time Period" = "Time Period", "TrailType" = "TrailType")) +
  facet_wrap(~Type, ncol = 1, scales = "free_y") +
  theme(
    panel.background = element_blank(),      # Remove background panel
    panel.grid.major = element_blank(),       # Remove major grid lines
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_line(size = 0.5),
    axis.text.y = element_text(size = 17),
    axis.text.x = element_text(size = 17),
    axis.ticks.y = element_line(size = 0.5),
    axis.title.y = element_text(angle = 0, hjust = 0.5, vjust = 0.55, size = 19),
    axis.line.x = element_line(color = "black", size = 0.5), # Add x-axis line
    axis.line.y = element_line(color = "black", size = 0.5),
    axis.title.x = element_text(angle = 0, hjust = 0.5, size = 19),
    strip.text = element_text(size = 16, margin = margin(10, 0, 10, 0)),
    legend.text = element_text(size = 15),
    legend.spacing.y = unit(0.5,"cm"),
    legend.key.height = unit(2,"line"),
    legend.title = element_text(size = 18)
  )

png("Figures/Appendix Figures/SplitCoefficientsColourcov.png", width = 12, height = 8, res= 600, units = "in")

CoefficientPlotNoRef


dev.off()

#END
