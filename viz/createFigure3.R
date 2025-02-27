# loading FUNCTIONS
library(here)
source(paste(here(),"/analysis/NHS2-functions.R",sep=""),local=TRUE)


### LASSO 4
# Lasso-4 accuracy
filename <- paste("mir4predictions_t-48_b-10_s-25.Rds",sep="")
mir4pred <- readRDS(paste(here(),"/data/",filename,sep=""))

# lasso 4 sig
filename <- paste(here(),"/results/LASSO_4_perm-centiles.RDS",sep="")
pval4 <- readRDS(filename) %>% 
  mutate(p = 1-prop_larger) %>%
  mutate(star = case_when(
    p < .005~ "***",
    p < .01 ~ "**",
    p < .05 ~ "*",
    .default=""
  ))

# ensemble decision
mir4ens <- cbind(
  mir4pred %>% select(song,type),
  ensamble_pred = as.factor(apply(mir4pred %>% select(-song,-type,),1,get_mode))#, x
)

# calculate accuracy/confusion frequencies
lasso4acc <- table(
  mir4ens %>%
    select(ensamble_pred, type)
) %>%
  as.data.frame() %>%
  dplyr::rename(predicted = ensamble_pred, 
                actual = type) %>%
  # actual frequency
  group_by(actual) %>%
  mutate(base_freq = sum(Freq)) %>%
  ungroup() %>%
  mutate(pFreq = round((Freq/base_freq)*100, digits = 1)) %>%
  # predicted frequency
  group_by(predicted) %>%
  mutate(pred_freq = sum(Freq)) %>%
  ungroup() %>%
  mutate(marg = round((pred_freq/nrow(mir4ens))*100, digits = 1))

# get prediction margins
margins4 <- lasso4acc %>%
  select(predicted,marg) %>% 
  unique()

# calculate d'
hit_rate <- lasso4acc %>% 
  group_by(actual) %>%
  filter(actual==predicted) %>%
  summarise(hit_rate = Freq/base_freq) %>%
  rename(actual=1)

fa_rate <- lasso4acc %>% 
  group_by(predicted) %>%
  filter(actual!=predicted) %>%
  summarise(fa_rate = sum(Freq)/sum(base_freq)) %>%
  rename(actual=1)

dprime4 <- merge(hit_rate,fa_rate) %>%
  mutate(d = qnorm(hit_rate) - qnorm(fa_rate))
  # mutate(actual = factor(actual, levels = desc(d)))

# for sorting
xlvl <- dprime4 %>% 
  arrange(desc(d)) %>% pull(actual)
ylvl <- dprime4 %>% 
  arrange(d) %>% pull(actual)

gg_confusion4 <- lasso4acc %>%
  mutate(diag = predicted==actual) %>%
  mutate(predicted = factor(predicted, levels = xlvl), # ordering
         actual = factor(actual, levels = ylvl)) %>%
  ggplot(aes(x = predicted, y = actual, fill = pFreq)) +
  geom_tile(color="black", linewidth=.2) +
  geom_tile(aes(linewidth = diag), color="black", fill=NA) +
  geom_text(aes(label = paste(pFreq,"%",sep=""), color=diag)) + # percentage labels
  scale_fill_gradient(low = "white", high = "#ba77ff", limits=c(0,70)) + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 315, vjust = 0.5, hjust=1),
    panel.grid = element_blank()
  ) + 
  scale_linewidth_manual(values=c(0,.6), guide="none") +
  scale_x_discrete(position="top") +
  scale_color_manual(values=c("#444444","#000000"),guide="none") +
  labs(x = "Predicted behavioral context",
       y = "Actual behavioral context",
       fill = "%")
l4_legend <- get_legend(gg_confusion4)
gg_confusion4 <- gg_confusion4 + theme(legend.position = "none")

dfig4 <- dprime4 %>% 
  mutate(sig = case_when(
    actual=="Dance" ~ (pval4 %>% filter(type=="Dance") %>% pull(star)),
    actual=="Lullaby" ~ (pval4 %>% filter(type=="Lullaby") %>% pull(star)),
    actual=="Healing" ~ (pval4 %>% filter(type=="Healing") %>% pull(star)),
    actual=="Love" ~ (pval4 %>% filter(type=="Love") %>% pull(star)),
    .default = ""
  )) %>%
  mutate(actual = factor(actual, levels = ylvl)) %>%
  ggplot(aes(y=actual)) +
  geom_text(aes(label=round(d,digits=2), x=.1), size=4) +
  geom_text(aes(label=sig, x=1), size=4) + 
  theme_void() + 
  labs(x="d'") +
  theme(axis.title.x = element_text()) +
  scale_x_discrete(position = "top") 

mfig4 <- margins4 %>% 
  mutate(predicted = factor(predicted, levels = xlvl)) %>%
  ggplot(aes(x=predicted)) +
  geom_text(aes(label=paste(marg,"%",sep="")), size=3,
            color="#555555", y=.9) +
  theme_void()

plot_lasso4 <- gg_confusion4 #+ inset_element(mfig4,l=0, b=-.35,r=1,t=.07) #+ dfig4 + plot_layout(widths=c(.9,.1))


### LASSO 10
# load
filename <- paste("mir10predictions_t-100_b-10_s-25.Rds",sep="")
#paste("mir10predictions_t-",100,"_b-",1000,".Rds",sep="")
mir10pred <- readRDS(paste(here(),"/data/",filename,sep=""))

# lasso 4 sig
filename <- paste(here(),"/results/LASSO_10_perm-centiles.RDS",sep="")
pval10 <- readRDS(filename) %>% 
  mutate(p = 1-prop_larger) %>%
  mutate(star = case_when(
    p < .005~ "***",
    p < .01 ~ "**",
    p < .05 ~ "*",
    .default=""
  ))

# ensemble decision
mir10ens <- cbind(
  mir10pred %>% select(song,type),
  ensamble_pred = as.factor(apply(mir10pred %>% select(-song,-type),1,get_mode))#, x
)

# calculate accuracy/confusion frequencies
lasso10acc <- table(
  mir10ens %>%
    select(ensamble_pred, type)
) %>%
  as.data.frame() %>%
  dplyr::rename(predicted = ensamble_pred, 
                actual = type) %>%
  # actual frequecy
  group_by(actual) %>%
  mutate(base_freq = sum(Freq)) %>%
  ungroup() %>%
  mutate(pFreq = round((Freq/base_freq)*100, digits = 1))  %>%
  # predicted frequency
  group_by(predicted) %>%
  mutate(pred_freq = sum(Freq)) %>%
  ungroup() %>%
  mutate(marg = round((pred_freq/nrow(mir10ens))*100, digits = 1))
# get prediction margins
margins10 <- lasso10acc %>%
  select(predicted,marg) %>%
  unique()

# calculate cohen's d'
hit_rate <- lasso10acc %>% 
  group_by(actual) %>%
  filter(actual==predicted) %>%
  summarise(hit_rate = Freq/base_freq) %>%
  rename(actual=1)

fa_rate <- lasso10acc %>% 
  group_by(predicted) %>%
  filter(actual!=predicted) %>%
  summarise(fa_rate = sum(Freq)/sum(base_freq)) %>%
  rename(actual=1)

dprime10 <- merge(hit_rate,fa_rate) %>%
  mutate(d = qnorm(hit_rate) - qnorm(fa_rate)) %>%
  mutate(actual = fct_reorder(actual, desc(d)))

# for sorting
xlvl <- dprime10 %>% 
  arrange(desc(d)) %>% pull(actual)
ylvl <- dprime10 %>% 
  arrange(d) %>% pull(actual)

# plot confusion matrix
gg_confusion10 <- lasso10acc %>%
  mutate(diag = predicted==actual) %>%
  merge(pval10 %>% rename(actual=type) %>% filter(actual!="all"), by="actual", all=TRUE) %>%
  mutate(sig = diag & star!="") %>%
  mutate(predicted = factor(predicted, levels = xlvl),
         actual = factor(actual, levels = ylvl)) %>%
  ggplot(aes(x = predicted, y = actual, fill = pFreq)) +
  geom_tile(color="black", linewidth=.2) +
  geom_tile(aes(linewidth=sig), color="black", fill=NA) +
  geom_text(aes(label = paste(pFreq,"%", sep=""), color=diag), size=3.25) +
  scale_fill_gradient(low = "white", high = "#779eff", limits=c(0,70)) +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 315, vjust = 0.5, hjust=1),
    panel.grid = element_blank()
  ) + 
  scale_linewidth_manual(values=c(0,.6),guide="none") +
  scale_x_discrete(position="top") +
  scale_color_manual(values=c("#444444","#000000"), guide="none") +
  labs(x = "",
       y = "Actual behavioral context",
       fill = "%")

l10_legend <- cowplot::get_legend(gg_confusion10)
gg_confusion10 <- gg_confusion10 + theme(legend.position="none")

dfig10 <- dprime10 %>% 
  mutate(sig = case_when(
    actual=="Dance" ~ (pval10 %>% filter(type=="Dance") %>% pull(star)),
    actual=="Lullaby" ~ (pval10 %>% filter(type=="Lullaby") %>% pull(star)),
    actual=="Healing" ~ (pval10 %>% filter(type=="Healing") %>% pull(star)),
    actual=="Love" ~ (pval10 %>% filter(type=="Love") %>% pull(star)),
    actual=="Praise" ~ (pval10 %>% filter(type=="Praise") %>% pull(star)),
    actual=="Play" ~ (pval10 %>% filter(type=="Play") %>% pull(star)),
    actual=="Mourning" ~ (pval10 %>% filter(type=="Mourning") %>% pull(star)),
    actual=="Story" ~ (pval10 %>% filter(type=="Story") %>% pull(star)),
    actual=="Procession" ~ (pval10 %>% filter(type=="Procession") %>% pull(star)),
    actual=="Work" ~ (pval10 %>% filter(type=="Work") %>% pull(star)),
    .default = ""
  )) %>%
  mutate(actual = factor(actual, levels = ylvl)) %>%
  ggplot(aes(y=actual)) +
  geom_text(aes(label=paste(round(d,digits=2),sep=""), x=.1), size=4) +
  geom_text(aes(label=sig, x=1), size=4) +
  theme_void() + 
  labs(x="") +
  theme(axis.title.x = element_text()) +
  scale_x_discrete(position = "top") 

mfig10 <- margins10 %>% 
  mutate(predicted = factor(predicted, levels = xlvl)) %>%
  ggplot(aes(x=predicted)) +
  geom_text(aes(label=paste(marg,"%",sep="")), size=2.5,
            color="#555555", y=.9) +
  theme_void()
# mfig

plot_lasso10 <- gg_confusion10 + inset_element(mfig10,l=0, b=-.55,r=1,t=.1) #+ l10_legend + plot_layout(guide="collect")


### COMBINE PLOTS
lasso_plot_full <- plot_lasso4 + dfig4 + l4_legend +#plot_spacer() +
  plot_spacer() + plot_spacer() + plot_spacer() + 
  plot_lasso10 + dfig10 + l10_legend + #plot_spacer() + 
  plot_layout(ncol=3, 
              heights=unit(c(4,-.1,10),c('cm','cm')), 
              widths=c(.9,.15, .1),
              guide="collect") +
  # inset_element(l10_legend, l=1.5,r=0,t=0,b=1.9) + 
  plot_annotation(tag_levels = list(c('A','','','B')))

filename <- paste(here(),"/viz/Figure3.png", sep="")
ggsave2(filename, lasso_plot_full, height=8, width=7)
