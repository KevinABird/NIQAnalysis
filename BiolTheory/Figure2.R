require(ggplot2)
require(tidyverse)
require(cowplot)
require(ggsci)
require(here)

###############################################################################
# National IQ and EduPGS confounding
###############################################################################

df <- read.table(here::here("Data/Piffer2015_data.csv"),header = T, sep = ",")

df <- df %>% filter(Continent != "AA")

df$Continent <- factor(df$Continent)

Continent_R2<-summary(lm(Piffer.2015.IQ ~ Continent, data = df))$r.squared

# R² of the full model (pooled + continent)
Continent_plus_PGS_R2 <- summary(lm(Piffer.2015.IQ ~ Leeetal2018 + Continent, data = df))$r.squared

# Within-continent R² = proportion explained by PGS after continent
within_R2 <- summary(lm(Piffer.2015.IQ ~ Leeetal2018 + Continent, data = df))$r.squared - summary(lm(Piffer.2015.IQ ~ Continent, data = df))$r.squared

cat("Continent-only R²    =", round(Continent_R2, 5), "\n")
cat("Pooled + continent R² =", round(Continent_plus_PGS_R2, 5), "\n")
cat("Within-continent R²   =", round(within_R2, 5), "\n")

#Get model residuals for comparison plot
model_pgs <- lm(Leeetal2018 ~ Continent, data = df)
model_iq  <- lm(Piffer.2015.IQ ~ Continent, data = df,na.action = na.exclude)

Lee2$IQ_resid  <- residuals(model_iq)
Lee2$PGS_resid <- residuals(model_pgs)

#Make full plot, highlighting ecological correlation vs within-continent
ggplot(df,aes(y=Piffer.2015.IQ,x=scale(Leeetal2018))) +
  geom_point(aes(group=Continent,col=Continent),size=3) +
  geom_line(stat="smooth",method="lm",se=F,col="black",alpha=0.5,linetype = 2,linewidth = 2) +
  geom_line(stat="smooth",method="lm",se=F,linewidth=1,aes(group=Continent,col=Continent)) +
  scale_color_nejm(labels= c("Africa","Americas","Europe","East Asia","South Asia")) +
  xlim(c(-2,2)) + labs(x = "Educational Attainment Polygenic Score\n(Lee et al., 2018)",y = "NIQ - Lynn & Vanhanen 2012") +
  geom_text(aes(-75,105,label= "Continent-only R² = 0.95746\nPGS + continent R² = 0.95761")) +
  theme_cowplot(12) -> ConfoundPlot

#Plot IQ vs PGS residualized on continent
ggplot(df,aes(y=IQ_resid,x=PGS_resid)) +
  geom_point(aes(group=Continent,col=Continent),size=3) +
  geom_line(stat="smooth",method="lm",se=F,col="black",alpha=0.5,linetype = 2,linewidth = 2) +
  scale_color_nejm(labels= c("Africa","Americas","Europe","East Asia","South Asia")) +
  labs(x = "Educational Attainment Polygenic Score\n(residualized on continent)",y = "NIQ - Lynn & Vanhanen 2012 \n (residualized on continent)") +
  theme_cowplot(12) -> resid_Plot


Fig2 <- plot_grid(ConfoundPlot,resid_Plot)

ggsave("Bird_NIQ_Fig2.pdf", Fig2, width = 12, height = 10)
