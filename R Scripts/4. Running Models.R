#4. Running models using the previously created capture history, and the masks from (1.Habitat Mask)
options(scipen = 999)
library(secr)

#first make df so that sigma & g0 can vary just by season. 
sesscov <- data.frame(Season = factor(c("Summer", "Shoulder", "Summer", "Shoulder"), levels = c("Summer", "Shoulder")))

###fit2 ----- model with double observer IDs
fit2 <- secr.fit(dingoesfoursessions, 
                 mask = allsessionmasks,
                 link = "log",
                 detectfn = "HN",
                 ncores = 4,
                 trace = TRUE,
                 sessioncov = sesscov,
                 model = list(D ~ session + kde_value, sigma ~ Season + Kde_value, g0 ~ Season + Kde_value+TrailType),
                 details = list(fastproximity = FALSE))

#See results
summary(fit2)
region.N(fit2, allsessionmasks)

#From here I copied density results directly into excel- File called 'PlotResults.csv' 
#I also did the same for all the other parameter estimates, called 'Coefficients.csv'

#Both of these two files are in 'Derived Data'

#End