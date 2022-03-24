library(vegan)
library(ggplot2)
library(reshape2)
library(corrplot)
library(tidyverse)
library(gt)
library(gtsummary)

neon=read.csv("20211117_Neon_merged.csv",header=T)
head(neon)
dim(neon)

## NMDS
neon
neon$lat2=floor(neon$Latitude/10)*10
neon$lat2
neon$lon2=floor(neon$Longitude/10)*10
neon$lon2
neon$sand=floor(neon$sand_perc/10)*10
neon$sand

mds.neon=metaMDS(neon[,2:55],distance="bray", k=3)

data.sm=as.data.frame(scores(mds.neon))
data.sm

data.sm$lat2=as.factor(neon[,119]) 
data.sm$lon2=as.factor(neon[,120]) 
data.sm$sand=as.factor(neon[,121]) 


ggplot(data=data.sm, aes(x=NMDS1, y=NMDS2, color=sand)) + geom_point(size=5) + xlim(-0.15, 0.15) + ylim(-0.15, 0.15)+
  geom_hline(yintercept=0.0, colour="grey", lty=2)+
  geom_vline(xintercept=0.0, colour="grey",lty=2) +
  theme_bw() + scale_color_brewer(palette="Set1") + 
  theme(legend.text=element_text(size=12), legend.title=element_text(size=12)) +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_text(size=14), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))+
  theme(legend.background = element_rect(fill="white", size=0.3, linetype="solid"))


env.neon=envfit(mds.neon, neon[,56:121], permutations = 999)
env.neon

env.neon.df = as.data.frame(scores(env.neon, display="vectors"))
arrow.neon <- env.neon.df[env.neon$vectors$pvals < 0.01,]
arrow.neon.1=cbind(arrow.neon,Species=rownames(arrow.neon))
arrow.neon.1

makeStars <- function(x){
  stars <- c("****", "***", "**", "*", "NS")
  vec <- c(0, 0.0001, 0.001, 0.01, 0.05, 1.000)
  i <- findInterval(x, vec)
  stars[i]
}


factors.r2 <- env.neon$vectors$r %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(factor = 1, r2 = 2)

vectors.sig_values <- env.neon$vectors$pvals %>% 
  as.data.frame() %>%
  rownames_to_column() %>% 
  rename(factor = 1, pval = 2) %>% 
  mutate(sig_level = makeStars(pval)) %>% 
  mutate(bonferonni = makeStars(pval * 48)) %>% 
  # mutate(sig_level = if_else(is.na(sig_level), "NS", sig_level)) %>% 
  # mutate(sig_level = fct_relevel(sig_level, c("NS", "*", "**"))) %>% 
  mutate(across(c(sig_level, bonferonni), ~ if_else(is.na(.), "NS", .))) %>% 
  mutate(across(c(sig_level, bonferonni), ~ fct_relevel(., c("NS", "*", "**")))) %>% 
  left_join(factors.r2)

hist(vectors.sig_values$r2)

vectors.sig_values %>% 
  filter(r2 > 0.4)
  

bonferroni.important <- vectors.sig_values %>% 
  filter(bonferonni != "NS") %>% 
  select(factor)

vectors.sig_values %>% 
  pivot_longer(cols = sig_level:bonferonni) %>% 
  mutate(name = if_else(name == "sig_level", "None", "Bonferonni")) %>% 
  group_by(name) %>% 
  count(value) %>% 
  ggplot(aes(n, value)) +
  geom_col() +
  theme_light() +
  scale_x_continuous(expand = expansion(add = 0, mult = c(0, 0.1))) +
  facet_wrap(~ name) +
  labs(
    x = "",
    y = "Significance level",
    title = "Distribution of significance levels by multiple testing correction"
  )

arrow.neon.1


important_arrows <- arrow.neon.1 %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  rename(factor = 1) %>% 
  left_join(vectors.sig_values) %>% 
  filter(bonferonni != "NS")

# Requires the glue package
stress_label = glue::glue('Stress: { round(mds.neon$stress, 4) }')

data.sm %>%
  mutate(sand = as.numeric(sand) * 10) %>% 
  mutate(sand_class = if_else(sand < 45, "Low", "High")) %>% 
  ggplot(aes(x=NMDS1, y=NMDS2)) + geom_point(size = 5, aes(fill = sand_class), shape = 21) + xlim(-0.18, 0.18) + ylim(-0.15, 0.15)+
  geom_hline(yintercept=0.0, colour="grey", lty=2)+
  geom_vline(xintercept=0.0, colour="grey",lty=2) +
  theme_bw() + scale_color_brewer(palette="Set1") + 
  theme(legend.text=element_text(size=12), legend.title=element_text(size=12)) +
  theme(axis.text.x=element_text(size=14), axis.text.y=element_text(size=14), axis.title.x = element_text(size=12), axis.title.y = element_text(size=12))+
  theme(legend.background = element_rect(fill="white", size=0.3, linetype="solid", colour="black")) +
  geom_segment(data = important_arrows, aes(x=0, xend=0.15*NMDS1, y=0, yend=0.15*NMDS2), arrow=arrow(length=unit(0.25,"cm"))) +
  geom_text(data = important_arrows, aes(x = 0.2*NMDS1, y = 0.2*NMDS2, label = Species), size = 3) +
  labs(
    fill = "Sand Class"
  ) +
  annotate("text", x = 0.12, y = -0.14, label = stress_label, fontface = "bold", size = 5)

neon[,56:121] %>% 
  as.data.frame() %>% 
  mutate(sand = as.numeric(sand)) %>% 
  mutate(sand_class = if_else(sand < 45, "Low", "High")) %>%
  select("Longitude", "initial_soilTN", "SOC_mgC_g", 
         "FeIII_mg_g","Fe_HCl_mg_g", 
         "litterC_perc", "sand_perc", 
         "soilC_decom_mg_g", "q16S", "qamoA.012", 
         "qamoA.039")

%>%
  tbl_summary(
    by = sand_class,
    statistic = list(all_continuous() ~ "{mean}")
  ) %>% 
  add_p() %>% 
  bold_labels()
