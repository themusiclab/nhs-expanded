# loading FUNCTIONS
library(here)
source(paste(here(),"/analysis/NHS2-functions.R",sep=""),local=TRUE)


################### loading corpus, topping with everything ################### 
nhs2 <- read.csv(paste(here(),"/data/metadata_plus.csv",sep=""), skip = 0, header = TRUE)
ev_df <- readRDS(paste(here(),"/results/ICC2.Rds",sep="")) %>% 
  mutate(model = case_when(
    model=="m1" ~ "m1b",
    model=="m1b" ~ "m1",
    .default=model
  ))
# af_names <- ev_df %>% pull(feature) %>% unique()
af <- "ex_spectral_spectenropy_mean"

# plotting functions
plot_by_family_m1m2 <- function(ev_df, feature_name) {
  ev_df$feature_n <- sapply(ev_df$feature,name_of_feature)
  
  var_colors <- c('#eeeeee','#43A680','#0F7173')
  # m1 = c('#eeeeee','#43A680'),
  # m1b = c('#eeeeee','#0F7173'),
  # m2 = c('#eeeeee','#43A680','#0F7173'),
  # m3a = c('#eeeeee','#80B3FF','#43A680','#0F7173'),
  # m3b = c('#eeeeee','#74F6A7','#43A680','#0F7173'),
  # m4 = c('#eeeeee','#80B3FF','#74F6A7','#43A680','#0F7173')
  var_labels <- c('residual','culture','behaviour')
  # m1 = c('residual','language'),
  # m1b = c('residual','type'),
  # m2 = c('residual','language','type'),
  # m3a = c('residual','proximity','language','type'),
  # m3b = c('residual','phylogeny','language','type'),
  # m4 = c('residual','proximity','phylogeny','language','type')           
  
  # ordering and plotting
  plot <- ev_df %>% 
    filter(feature==feature_name) %>%
    # mutate(value = case_when(
    #   variable=="residual" ~ value-0.5,
    #   .default=value
    # )) %>%
    filter(model %in% c('m1','m2','m1b')) %>%
    mutate(model_lab = case_when(
      model=='m1' ~ 'culture only',
      model=='m1b' ~ 'behaviour only',
      model=='m2' ~ 'behaviour and culture',
      .default=model
    )) %>%
    mutate(var_ord = case_when(
      variable=='type'~1,
      variable=='phylogeny'~3,
      variable=='language'~2,
      variable=='proximity'~4,
      variable=='residual'~5,
    )) %>%
    mutate(fam_ord = case_when(
      family=='global'~1,
      family=='big3'~2,
      family=='ie'~3,
      family=='an'~4,
      family=='ac'~5,
    )) %>%
    mutate(family_lab = case_when(
      family=='global'~'Global',
      family=='big3'~'Big 3',
      family=='ie'~'Indo-European',
      family=='an'~'Austronesian',
      family=='ac'~'Antantic-Congo',
    )) %>%
    mutate(m_ord = case_when(
      model=='m1'~1,
      model=='m2'~2,
      model=='m1b'~3,
    )) %>% 
    # plotting
    ggplot(aes(y=reorder(model_lab,-m_ord),x=value,fill=reorder(variable,-var_ord))) +
    geom_bar(stat="identity") +
    scale_fill_discrete(type=var_colors,
                        labels=var_labels,
                        guide = guide_legend(reverse = TRUE)) +
    geom_vline(xintercept=1) + geom_vline(xintercept=0) + 
    geom_vline(xintercept=.5,  color="#bbbbbb", linewidth=.25) + 
    geom_vline(xintercept=.25, color="#bbbbbb", linewidth=.25, linetype = "dashed") + 
    geom_vline(xintercept=.75, color="#bbbbbb", linewidth=.25, linetype = "dashed") +
    # facet_grid(rows=vars(reorder(family,fam_ord)), scales="free", space="free") +
    facet_grid(rows=vars(reorder(model_lab,-m_ord)), scales="free", space="free", switch="y") +
    xlim(0,1) +
    labs(title=feature_name, fill="variable", x="", y="") + 
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),#element_text(hjust=1,size=8),
      strip.text.y.left = element_text(hjust=1,size=8,angle=0),
      plot.margin = unit(c(.1,.1,.1,.1),"cm")
    )
  
  return(plot)
}

plot_by_family_m3m4 <- function(ev_df, feature_name) {
  ev_df$feature_n <- sapply(ev_df$feature,name_of_feature)
  
  var_colors <- c('#eeeeee','#80B3FF','#74F6A7','#43A680','#0F7173')
  # m1 = c('#eeeeee','#43A680'),
  # m1b = c('#eeeeee','#0F7173'),
  # m2 = c('#eeeeee','#43A680','#0F7173'),
  # m3a = c('#eeeeee','#80B3FF','#43A680','#0F7173'),
  # m3b = c('#eeeeee','#74F6A7','#43A680','#0F7173'),
  # m4 = c('#eeeeee','#80B3FF','#74F6A7','#43A680','#0F7173')
  var_labels <- c('residual','proximity','phylogeny','culture','behaviour')
  # m1 = c('residual','language'),
  # m1b = c('residual','type'),
  # m2 = c('residual','language','type'),
  # m3a = c('residual','proximity','language','type'),
  # m3b = c('residual','phylogeny','language','type'),
  # m4 = c('residual','proximity','phylogeny','language','type')           
  
  # ordering and plotting
  plot <- ev_df %>% 
    filter(feature==feature_name) %>%
    # mutate(value = case_when(
    #   variable=="residual" ~ value-0.5,
    #   .default=value
    # )) %>%
    filter(model %in% c('m3a','m3b','m4')) %>%
    mutate(var_ord = case_when(
      variable=='type'~1,
      variable=='phylogeny'~3,
      variable=='language'~2,
      variable=='proximity'~4,
      variable=='residual'~5,
    )) %>%
    mutate(fam_ord = case_when(
      family=='global'~1,
      family=='big3'~2,
      family=='ie'~3,
      family=='an'~4,
      family=='ac'~5,
    )) %>%
    mutate(m_ord = case_when(
      model=='m3a'~1,
      model=='m4'~2,
      model=='m3b'~3,
    )) %>% 
    mutate(model_lab = case_when(
      model=='m3a' ~ 'proximity',
      model=='m3b' ~ 'phylogeny',
      model=='m4' ~ 'both',
      .default=model
    )) %>%
    mutate(family_lab = case_when(
      family=='global'~'Global',
      family=='big3'~'Big 3',
      family=='ie'~'Indo-European',
      family=='an'~'Austronesian',
      family=='ac'~'Antantic-Congo',
    )) %>%
    # plotting
    ggplot(aes(y=reorder(model_lab,-m_ord),x=value,fill=reorder(variable,-var_ord))) +
    geom_bar(stat="identity") +
    scale_fill_discrete(type=var_colors,
                        labels=var_labels,
                        guide = guide_legend(reverse = TRUE)) +
    geom_vline(xintercept=1) + geom_vline(xintercept=0) + 
    geom_vline(xintercept=.5,  color="#bbbbbb", linewidth=.25) + 
    geom_vline(xintercept=.25, color="#bbbbbb", linewidth=.25, linetype = "dashed") + 
    geom_vline(xintercept=.75, color="#bbbbbb", linewidth=.25, linetype = "dashed") +
    # facet_grid(rows=vars(reorder(family,fam_ord)), scales="free", space="free") +
    facet_grid(rows=vars(reorder(model_lab,-m_ord)), scales="free", space="free", switch="y") +
    xlim(0,1) +
    labs(title=feature_name, fill="variable", x="", y="") + 
    theme_minimal() +
    theme(
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      axis.text.y = element_blank(),#element_text(hjust=1,size=8),
      plot.margin = unit(c(.1,.1,.1,.1),"cm"),
      strip.text.y.left = element_text(hjust=1,size=8,angle=0)
    )
  
  return(plot)
}

plot_density_by_variable_m1m2 <- function(ev_df) {
  var_colors <- c('#0F7173','#43A680')
  var_labels <- c('behaviour','culture')
  
  plot <- ev_df %>% 
    filter(
      model %in% c('m1','m1b','m2'),
      variable != "residual"
    ) %>%
    mutate(
      var_ord = case_when(
        variable=='type'~1,
        variable=='language'~2
        ),
      variable = case_when(
        variable=='type'~'behaviour',
        variable=='language'~'culture',
        .default=variable
      )) %>%
    ggplot(aes(x=value, fill=variable)) +
    geom_density(color=NA) +
    scale_fill_discrete(type=var_colors,
                        labels=var_labels,
                        guide = guide_legend(reverse = TRUE)) +
    facet_grid(rows=vars(reorder(variable,var_ord)),scale="free_y",switch="y") +
    xlim(0,.5) +
    labs(x="ICC") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      legend.position = "none"
    ) + 
    geom_vline(xintercept=.5) +
    geom_vline(xintercept=0) + 
    # geom_vline(xintercept=.5,  color="#bbbbbb", linewidth=.25)
    geom_vline(xintercept=.25, color="#bbbbbb", linewidth=.25, linetype = "dashed")
    # geom_vline(xintercept=.75, color="#bbbbbb", linewidth=.25, linetype = "dashed")
  
  return(plot)
} 

plot_density_by_variable_m3m4 <- function(ev_df) {
  var_colors <- c('#0F7173','#43A680','#74F6A7','#80B3FF')
  var_labels <- c('behaviour','culture','phylogeny','proximity')
  
  plot <- ev_df %>% 
    filter(
      model %in% c('m4'),#c('m3','m3b','m4'),
      variable != "residual"
    ) %>%
    mutate(var_ord = case_when(
      variable=='type'~1,
      variable=='phylogeny'~3,
      variable=='language'~2,
      variable=='proximity'~4,
      variable=='residual'~5,
    )) %>%
    mutate(variable = case_when(
      variable=='type'~'behaviour',
      variable=='language'~'culture',
      .default = variable
    )) %>%
    ggplot(aes(x=value, fill=variable)) +
    geom_density(color=NA) +
    scale_fill_discrete(type=var_colors,
                        labels=var_labels,
                        guide = guide_legend(reverse = TRUE)) +
    facet_grid(rows=vars(reorder(variable,var_ord)),scale="free_y",switch="y") +
    xlim(0,.5) +
    labs(x="ICC") +
    theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      # axis.text.y = element_blank(),
      legend.position = "none"
    ) + 
    geom_vline(xintercept=0) + 
    geom_vline(xintercept=.5) +
    # geom_vline(xintercept=.5,  color="#bbbbbb", linewidth=.25) + 
    geom_vline(xintercept=.25, color="#bbbbbb", linewidth=.25, linetype = "dashed")
  
  return(plot)
}


######################################################
######################################################
######################################################
######################################################

# af <- "ex_spectral_spectenropy_mean"
var_colors <- c('#80B3FF','#74F6A7','#43A680','#0F7173')
var_labels <- c('proximity','phylogeny','culture','behaviour')

legend <- ev_df %>% filter(variable!="residual") %>%
  mutate(var_ord = case_when(
    variable=='type'~1,
    variable=='phylogeny'~3,
    variable=='language'~2,
    variable=='proximity'~4,
  )) %>%
  ggplot(aes(x=value)) + 
  geom_density(aes(fill=reorder(variable,-var_ord)),color=NA) +
  scale_fill_discrete(
      type=var_colors,
      labels=var_labels,
      guide = guide_legend(reverse = TRUE)
    ) + labs(fill="variables")
legend <- get_legend(legend)

A <- plot_by_family_m1m2(ev_df,af) + 
  labs(title="Spectral Entropy Mean", x="Proportion of Variance") +
  theme(
    legend.position = "none", 
    strip.text=element_blank())

B <- plot_by_family_m3m4(ev_df,af) + 
  labs(title="Spectral Entropy Mean", x="Proportion of Variance") + 
  theme(legend.position = "none", 
        strip.text=element_blank())

C <- plot_density_by_variable_m1m2(ev_df) +
  labs(y="", title="All Features") + 
  theme(
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(angle=0,hjust=1,size=8),
    plot.title = element_text(hjust=0)
    )

D <- plot_density_by_variable_m3m4(ev_df) + 
  labs(y="", title="All Features") +
  theme(
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(angle=0,hjust=1,size=8),
    plot.title = element_text(hjust=0)
    )

# title <- ggplot() + geom_text(aes(label="Spectral Entropy Mean", x=0,y=0),size=4.5) + theme_void()
layout <- "
ABE
CDE
"
fig4 <- A + B + C + D + legend +
  plot_layout(design=layout, heights = c(1,1.5), widths=c(1,1,.5)) +
  plot_annotation(tag_levels = list(c('A','C','B','D')))

filename <- paste(here(),"/viz/Figure5.png",sep="")
ggsave(filename,fig4,width=9,height=4)













    

