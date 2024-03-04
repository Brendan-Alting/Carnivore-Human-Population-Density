#4. Running models using the previously created capture history, and the masks from (1.Habitat Mask)
options(scipen = 999)
library(secr)

#first make df so that sigma & g0 can vary just by Time Period. 
TimePeriodcov <- data.frame(TimePeriod = factor(c("A", "B", "C", "A", "B", "C"), levels = c("A", "B", "C")))

#First run null models with different detection functions. 
#Null 
Null <- secr.fit(dingoessixsessions, 
                  mask = mask,
                  link = "log",
                  detectfn = "EX",
                  ncores = 4,
                  trace = TRUE,
                  sessioncov = TimePeriodcov,
                  model = list(D~1, sigma~1,g0~1),
                  details = list(fastproximity = FALSE))

NullHN <- secr.fit(dingoessixsessions, 
                   mask = mask,
                   link = "log",
                   detectfn = "HN",
                   ncores = 4,
                   trace = TRUE,
                   sessioncov = TimePeriodcov,
                   model = list(D~1, sigma~1,g0~1),
                   details = list(fastproximity = FALSE))

NullHR <- secr.fit(dingoessixsessions, 
                   mask = mask,
                   link = "log",
                   detectfn = "HR",
                   ncores = 4,
                   trace = TRUE,
                   sessioncov = TimePeriodcov,
                   model = list(D~1, sigma~1,g0~1),
                   details = list(fastproximity = FALSE))

#-----Exponential detection function is best, use that for all future models. 

###Running 7 more candidate models (total 8 including null)


#Full 
Full <- secr.fit(dingoessixsessions, 
                             mask = mask,
                             link = "log",
                             detectfn = "EX",
                             ncores = 4,
                             trace = TRUE,
                             sessioncov = TimePeriodcov,
                             model = list(D~session+kde_value, sigma~TimePeriod+kde_value,g0~TimePeriod+TrailType+kde_value),
                             details = list(fastproximity = FALSE))

#Mod A- D and Sigma vary
ModA <- secr.fit(dingoessixsessions, 
                 mask = mask,
                 link = "log",
                 detectfn = "EX",
                 ncores = 4,
                 trace = TRUE,
                 sessioncov = TimePeriodcov,
                 model = list(D~session+kde_value, sigma~TimePeriod+kde_value,g0~1),
                 details = list(fastproximity = FALSE))

#Mod B - D and Detectability vary
ModB <- secr.fit(dingoessixsessions, 
                 mask = mask,
                 link = "log",
                 detectfn = "EX",
                 ncores = 4,
                 trace = TRUE,
                 sessioncov = TimePeriodcov,
                 model = list(D~session+kde_value, sigma~1,g0~TimePeriod+TrailType+kde_value),
                 details = list(fastproximity = FALSE))

#Mod C - Detectability And Sigma vary
ModC <- secr.fit(dingoessixsessions, 
                 mask = mask,
                 link = "log",
                 detectfn = "EX",
                 ncores = 4,
                 trace = TRUE,
                 sessioncov = TimePeriodcov,
                 model = list(D~1, sigma~TimePeriod+kde_value,g0~TimePeriod+TrailType+kde_value),
                 details = list(fastproximity = FALSE))

#Mod D - Just detectability vary 
ModD <- secr.fit(dingoessixsessions, 
                 mask = mask,
                 link = "log",
                 detectfn = "EX",
                 ncores = 4,
                 trace = TRUE,
                 sessioncov = TimePeriodcov,
                 model = list(D~1, sigma~1,g0~TimePeriod+TrailType+kde_value),
                 details = list(fastproximity = FALSE))

#Mod E - Just Sigma vary
ModE <- secr.fit(dingoessixsessions, 
                 mask = mask,
                 link = "log",
                 detectfn = "EX",
                 ncores = 4,
                 trace = TRUE,
                 sessioncov = TimePeriodcov,
                 model = list(D~1, sigma~TimePeriod+kde_value,g0~1),
                 details = list(fastproximity = FALSE))

#Mod F - Just Density vary
ModF <- secr.fit(dingoessixsessions, 
                 mask = mask,
                 link = "log",
                 detectfn = "EX",
                 ncores = 4,
                 trace = TRUE,
                 sessioncov = TimePeriodcov,
                 model = list(D~session+kde_value, sigma~1,g0~1),
                 details = list(fastproximity = FALSE))

#See results
Allmodels <- secrlist(Null, Full, ModA, ModB, ModC, ModD, ModE, ModF)

#Compare AIC
AIC(ALLmodels)

#Full model is best

#View Results
Full

#Get density surface
DensitySurface <- predictDsurface(Full, cl.D = TRUE, alpha = 0.05 )

#Extract density estimates by summing predicted values from across mask 

#density estimates
DensityEst <- c(mean(covariates(DensitySurface[[1]])$D.0)*100,mean(covariates(DensitySurface[[2]])$D.0)*100,mean(covariates(DensitySurface[[3]])$D.0)*100
                      , mean(covariates(DensitySurface[[4]])$D.0)*100, mean(covariates(DensitySurface[[5]])$D.0)*100, mean(covariates(DensitySurface[[6]])$D.0)*100)

LCIs <- c(mean(covariates(DensitySurface[[1]])$lcl.0)*100,mean(covariates(DensitySurface[[2]])$lcl.0)*100,mean(covariates(DensitySurface[[3]])$lcl.0)*100
          , mean(covariates(DensitySurface[[4]])$lcl.0)*100, mean(covariates(DensitySurface[[5]])$lcl.0)*100, mean(covariates(DensitySurface[[6]])$lcl.0)*100)

UCIs <- c(mean(covariates(DensitySurface[[1]])$ucl.0)*100,mean(covariates(DensitySurface[[2]])$ucl.0)*100,mean(covariates(DensitySurface[[3]])$ucl.0)*100
          , mean(covariates(DensitySurface[[4]])$ucl.0)*100, mean(covariates(DensitySurface[[5]])$ucl.0)*100, mean(covariates(DensitySurface[[6]])$ucl.0)*100)



DingoDensity <- data.frame(
  Density = DensityEst,
  LCI = LCIs,
  UCI = UCIs, 
  Session = 1:length(DensityEst))

write.csv(DingoDensity, "Derived Data/DingoDensityEstimates.csv", row.names = FALSE)

#Copied Parameter Estimates directly into a csv called "Coefficients.csv"

#Both of these two files are in 'Derived Data'

#End