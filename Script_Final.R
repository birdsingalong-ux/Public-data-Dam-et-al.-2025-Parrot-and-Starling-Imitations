# Read in data

library(readxl)

All_Units <- read_excel("All_Units_Final.xlsx")

str(All_Units)


All_Units$R2D2_sound <- factor(All_Units$R2D2_sound, levels=c("X", "Y", "C",
                                                              "W", "H", 
                                                              "M", "N", "P"))
All_Units$Type <- factor(All_Units$Type, levels=c("Monophonic", "Polyphonic"))

All_Units$Starling <- factor(All_Units$Starling, levels=c("Nostarling", "Starling"))
summary(All_Units)

# normalize data

library(dplyr)
Norm_All <- All_Units %>%
  group_by(R2D2_sound) %>%
  mutate(normalized = (DistanceScore-mean(DistanceScore))/sd(DistanceScore))


#make model
require(lme4)

M1_NoPw <- lmer(normalized ~ Starling + Type + Starling:Type + (1|Animal_ID), data = Norm_All)

summary(M1_NoPw)
confint(M1_NoPw)

#function to plot residuals
diagnostics.plot<-function(mod.res, col=grey(level=0.25, alpha=0.5)){
  old.par = par(no.readonly = TRUE)
  par(mfrow=c(2, 2))
  par(mar=c(3, 3, 1, 0.5))
  hist(residuals(mod.res), probability=T, xlab="", ylab="", main="")
  mtext(text="histogram of residuals", side=3, line=0)
  x=seq(min(residuals(mod.res)), max(residuals(mod.res)), length.out=100)
  lines(x, dnorm(x, mean=0, sd=sd(residuals(mod.res))))
  qqnorm(residuals(mod.res), main="", pch=19)
  qqline(residuals(mod.res))
  mtext(text="qq-plot of residuals", side=3, line=0)
  plot(fitted(mod.res), residuals(mod.res), pch=19, col=col)
  abline(h=0, lty=2)
  mtext(text="residuals against fitted values", side=3, line=0)
  par(old.par)
}

require(performance)
check_normality(M1_NoPw)
diagnostics.plot(M1_NoPw) # checked the residuals assumptions

# Post-hoc test
require(emmeans)
post <- lsmeans(M1_NoPw, pairwise ~ Starling*Type, adjust = "Tukey")
post
confint(post)


M_N_TYPE <- lmer(normalized ~  Type + (1|Animal_ID), data = Norm_All)

M_N_NULL <- lmer(normalized ~  (1|Animal_ID), data = Norm_All)

summary(M_N_TYPE)

require(MuMIn)
AICc(M1_NoPw)
AICc(M_N_TYPE)
AICc(M_N_NULL)

anova(M1_NoPw, M_N_NULL)

# With Species

M2_NoPw <- lmer(normalized ~ Species + Type + Species:Type + (1|Animal_ID), data = Norm_All)

diagnostics.plot(M2_NoPw)

check_normality(M2_NoPw)

summary(M2_NoPw)

# Post-Hoc test
require(emmeans)
Species_Post_NoPw <-lsmeans(M2_NoPw, pairwise ~ Species*Type, adjust = "Tukey")
Species_Type_DF_NoPw <- as.data.frame(Species_Post_NoPw$contrasts)

Species_Type_DF_NoPw$p.value <- format(as.numeric(Species_Type_DF_NoPw$p.value), scientific = F)

require(MuMIn)
AICc(M2_NoPw)
AICc(M1_NoPw)

anova(M2_NoPw, M1_NoPw, M_N_NULLNoPw)
