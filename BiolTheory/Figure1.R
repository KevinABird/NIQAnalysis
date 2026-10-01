require(ggplot2)
require(tidyverse)
require(cowplot)
require(ggsci)
library(here)

###############################################################################
#NIQ dataset Comparisons
###############################################################################

NIQ <- read.table(here::here("NIQ_data_whole.csv"),header = T, sep = ",")

NIQ %>% group_by(Region) %>% reframe(LV12_corr=cor(`L.V12.GEO`,Harmonized_Learning_Outcomes,use="complete.obs"),Becker_corr=cor(`QNW.SAS.GEO`,Harmonized_Learning_Outcomes,use="complete.obs"), PK_2025_corr=cor(PK_2025,Harmonized_Learning_Outcomes,use="complete.obs"))

ggplot(data=NIQ,aes(y=`L.V12.GEO`,x=Harmonized_Learning_Outcomes,group=Region,color=Region)) +
  geom_point() +
  geom_smooth(formula = y~x, method="lm",se = F) +
  scale_color_nejm(labels=c("Rest of World","African Countries")) +
  labs(x = "Harmonized Learning Outcomes",y = "NIQ - Lynn & Vanhanen 2012") +
  theme_cowplot() -> LV12

ggplot(data=NIQ,aes(y=`QNW.SAS.GEO`,x=Harmonized_Learning_Outcomes,group=Region,color=Region)) +
  geom_point() +
  geom_smooth(formula = y~x, method="lm",se = F) +
  scale_color_nejm(labels=c("Rest of World","African Countries")) +
  labs(x = "Harmonized Learning Outcomes",y = "NIQ - Becker dataset, weighted") +
  theme_cowplot() -> BeckerLynnWeighted

ggplot(data=NIQ,aes(y=PK_2025,x=Harmonized_Learning_Outcomes,group=Region,color=Region)) +
  geom_point() +
  geom_smooth(formula = y~x, method="lm",se = F) +
  scale_color_nejm(labels=c("Rest of World","African Countries")) +
  labs(x = "Harmonized Learning Outcomes",y = "NIQ - Parra and Kirkegaard") +
  theme_cowplot() -> ParraKirkegaard

Fig1 <- plot_grid(LV12,BeckerLynnWeighted,ParraKirkegaard)
ggsave("Bird_NIQ_Figure1.pdf", Fig1, width = 10, height = 8)
