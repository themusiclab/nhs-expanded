# loading FUNCTIONS
library(here)
source(paste(here(),"/analysis/NHS2-functions.R",sep=""),local=TRUE)

################### methods ################### 
# for TREE plotting
# propagates "has_song" label up the tree through parents
# returns tree
prop_af_tree <- function(tree, df, af) {
  x <- as_tibble(tree) %>% 
    as.data.frame() %>% # turn into df
    merge(
      df %>% 
        rename("label"="glottocode", val=af) %>%
        group_by(label) %>%
        summarize(label=unique(label), mean_val=mean(val)) %>%
        select(label, mean_val),
      all.x=TRUE, by="label") # add "has_song" label
  
  for (i in 1:50) {
    # get mean value of af by parent, 
    # return parent as "node" alongside value
    nodes_to_update <- x %>% 
      group_by(parent) %>%
      summarize(mean_of_parent = mean(mean_val, na.rm=TRUE)) %>%
      mutate(mean_of_parent = case_when(is.nan(mean_of_parent) ~ NA, .default=mean_of_parent)) %>%
      rename(node=parent, mean_val=mean_of_parent)
    
    # update working dataframe to represent new mean af value
    x <- x %>% merge(nodes_to_update, by="node", all=TRUE) %>%
      mutate(mean_val = case_when(
        !is.na(mean_val.y) ~ mean_val.y, # prioritize new value
        .default = mean_val.x
      )) %>% select(!c(mean_val.x,mean_val.y))
  }
  
  x1 <- x %>% 
    select(node,mean_val) %>% 
    as_tibble()
  tree <- treedata(phylo=tree, data=x1)
  return(x1)
}


plot_af_tree <- function(tree, df, af, family="ie",lims=c(-1.71,5)) {
  # add size to tree branches
  tree <- tree 
  
  na_val <- case_when(
    family=="an" ~ "#555555",
    family=="ac" ~ "#555555",
    family=="ie" ~ "#000000",,
    .default="#000000"
  )
  
  df <- df %>%
    mutate(bigness = case_when(!is.na(mean_val) ~ .9, .default=.2),
           alph = case_when(!is.na(mean_val) ~ 1, .default=.3))
  
  plot <- tree %>%
    ggtree(layout='fan', open.angle=10, mapping=aes(size=bigness, alpha=alph)) %<+% 
    df  +
    aes(color=mean_val) +
    scale_alpha_identity(guide = "none") + 
    scale_size_identity() +
    # scale_color_gradient2(
    #     low="#0000ff",mid="#e6e200",high="#ff0000",
    #     na.value = na_val,
    #     limits=c(-1.8,3.6)
    #     ) +
    scale_color_viridis(na.value = na_val,limits=lims, option="magma") +
    labs(color="Mean Value")
  # theme(legend.position = "none")
  return(plot)
}


################### loading corpus, topping with everything ################### 
nhs2 <- read.csv(paste(here(),"/data/metadata_plus.csv",sep=""), skip = 0, header = TRUE)

# load language tree
global_tree <- read.nexus(paste(here(),"/data/global-language-tree-MCC-labelled.tree",sep=""))
labels <- global_tree$tip.label
new_labels <- str_extract(labels, '^[a-z]{4}[0-9]{4}')
global_tree$tip.label <- new_labels

# subtrees
ie_subtree <- tree_subset(global_tree,'russ1263',levels_back=7)
an_subtree <- tree_subset(global_tree,'wole1240',levels_back=19)
ac_subtree <- tree_subset(global_tree,'lito1235',levels_back=21)

# plotting func
plot_fig5 <- function(af, df, ie_tree, an_tree, ac_tree, song_min) {
  # manually handle outliers
  df <- df %>%
    rename("feature"=af)
  
  # get limit
  minf <- min(df %>% filter(type=="Dance"|type=="Lullaby") %>% pull(feature))
  maxf <- max(df %>% filter(type=="Dance"|type=="Lullaby") %>% pull(feature))
  lims <- c(minf,maxf)
  
  # create scatter plots
  splot1 <- df %>%
    filter(type=="Dance"|type=="Lullaby") %>%
    filter(family_name %in% c("Indo-European","Austronesian","Atlantic-Congo")) %>%
    group_by(family_id) %>%
    mutate(family_mean = mean(feature)) %>%
    mutate(n_val = length(feature)) %>%
    ungroup() %>%
    ggplot(aes(x=feature, label=family_name, color=type, fill=type)) +
    geom_density(alpha=.65) +
    geom_point(aes(y=-.3),position=position_jitter(height=.1)) +
    geom_hline(yintercept = 0,color="#bbbbbb") +
    geom_vline(xintercept = 0,color="#bbbbbb",linetype="dashed") +
    facet_grid(rows=vars(reorder(family_name,-n_val)), switch="y") +
    theme_classic() +
    scale_color_manual(values=c("#44bb74","#6f9acc"), breaks=c("Lullaby","Dance")) + 
    scale_fill_manual(values=c("#44bb74","#6f9acc"), breaks=c("Lullaby","Dance")) +
    theme(
      strip.text.y.left = element_text(angle = 0),
      strip.background = element_rect(fill="#ffffff", colour="#ffffff"),
      axis.line.y.left = element_blank(),
      axis.line.x.bottom = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      legend.position = "top"
    ) +
    labs(y="Deep Sample", x="", color="Behavior", fill="Behavior") +
    lims(x=lims,y=c(-1,1.4))
  # splot1
  # leg <- get_legend(splot1)
  # splot1 <- splot1 + theme(legend.position = "none")
  
  splot2den <- df %>%
    filter(type=="Dance"|type=="Lullaby") %>%
    filter(!family_name%in%c("Indo-European","Austronesian","Atlantic-Congo")) %>%
    ggplot(aes(x=feature, label=family_name, color=type, fill=type)) +
    geom_density(alpha=.65) + 
    lims(x=lims) +
    scale_color_manual(values=c("#6f9acc","#44bb74")) +
    scale_fill_manual(values=c("#6f9acc","#44bb74")) +
    theme_void() + theme(legend.position = "none")
  
  splot2 <- df %>%
    filter(type=="Dance"|type=="Lullaby") %>%
    filter(!family_name%in%c("Indo-European","Austronesian","Atlantic-Congo")) %>%
    # group_by(family_id,type) %>%
    # mutate(enuf=length(feature)>=song_min) %>%
    # ungroup() %>%
    group_by(family_id) %>%
    mutate(family_mean = mean(feature)) %>%
    # mutate(enuff = all(enuf)) %>%
    ungroup() %>%
    group_by(glottocode) %>%
    mutate(lang_mean=mean(feature)) %>%
    ungroup() %>%
    # filter(enuff) %>%
    ggplot(aes(x=feature, label=family_name, color=type, fill=type)) +
    #, y=reorder(family_name,family_mean),
    # geom_density(alpha=.55) +
    geom_hline(yintercept = 0,color="#cccccc",,linewidth=.2) +
    geom_vline(xintercept=0,color="#bbbbbb",linetype="dashed") +
    geom_point(aes(y=0)) + #, position=position_jitter(height=.2)) + #position=position_jitter(height=.01)
    facet_grid(rows=vars(reorder(family_name,-family_mean)),switch="y", margins=FALSE) +
    theme_classic() +
    scale_color_manual(values=c("#6f9acc","#44bb74")) +
    scale_fill_manual(values=c("#6f9acc","#44bb74")) +
    # scale_y_discrete(position="right") + 
    theme(
      strip.text.y.left = element_text(angle = 0,size=6,hjust=1,margin=margin(t = 0, r = 0, b = 0, l = 0, unit = "pt")),
      strip.background = element_rect(fill="#ffffff", colour="#ffffff"),
      axis.line.y = element_blank(),
      # axis.ticks.x = element_blank(),
      # axis.text.y.right = element_text(hjust=0),
      axis.text.y.left = element_blank(),
      axis.ticks.y = element_blank(),
      panel.grid = element_blank(),
      panel.spacing = unit(0,"pt"),
      legend.position="none"
    ) +
    labs(y="Wide Sample", x="Spectral Entropy Mean") +
    lims(x=lims)
  # splot2
  
  scatter_plot <- splot1 + splot2den + splot2 + 
    plot_layout(ncol=1, heights=c(2.5,.3,4),guide="collect")
  
  # filename <- paste(here(),"/viz/fig5_final_options/Ascatter_",af,".png",sep="")
  # ggsave(filename=filename,scatter_plot,width=6,height=7)
  
  # create tree plots
  # ie
  ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Dance") %>% rename(af="feature"), af)
  plot_ie_dance <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
    labs(y="Indo-European (Dance)", color="Spectral\nEntropy\nMean")
  
  ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Lullaby") %>% rename(af="feature"), af)
  plot_ie_lull <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
    labs(y="Indo-European (Lullaby)", color="Spectral\nEntropy\nMean")
  
  # ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Play") %>% rename(af="feature"), af)
  # plot_ie_play <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
  #   labs(y="Indo-European (Play)", color=af)
  # 
  # ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Procession") %>% rename(af="feature"), af)
  # plot_ie_proc <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
  #   labs(y="Indo-European (Procession)", color=af)
  # 
  # ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Praise") %>% rename(af="feature"), af)
  # plot_ie_praise <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
  #   labs(y="Indo-European (Praise)", color=af)
  # 
  # ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Healing") %>% rename(af="feature"), af)
  # plot_ie_heal <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
  #   labs(y="Indo-European (Healing)", color=af)
  # 
  # ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Love") %>% rename(af="feature"), af)
  # plot_ie_love <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
  #   labs(y="Indo-European (Love)", color=af)
  # 
  # ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Work") %>% rename(af="feature"), af)
  # plot_ie_work <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
  #   labs(y="Indo-European (Work)", color=af)
  # 
  # ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Mourning") %>% rename(af="feature"), af)
  # plot_ie_mourn <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
  #   labs(y="Indo-European (Mourning)", color=af)
  # 
  # ie_df <- prop_af_tree(ie_subtree, df %>% filter(type=="Story") %>% rename(af="feature"), af)
  # plot_ie_story <- plot_af_tree(ie_subtree, ie_df, af, family="ie", lims) +
  #   labs(y="Indo-European (Story)", color=af)
  
  # an
  an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Dance") %>% rename(af="feature"), af)
  plot_an_dance <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
    labs(y="Austronesian (Dance)", color="Spectral\nEntropy\nMean")
  
  an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Lullaby") %>% rename(af="feature"), af)
  plot_an_lull <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
    labs(y="Austronesian (Lullaby)", color="Spectral\nEntropy\nMean")
  
  # an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Play") %>% rename(af="feature"), af)
  # plot_an_play <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
  #   labs(y="Austronesian (Play)", color=af)
  # 
  # an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Procession") %>% rename(af="feature"), af)
  # plot_an_proc <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
  #   labs(y="Austronesian (Procession)", color=af)
  # 
  # an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Praise") %>% rename(af="feature"), af)
  # plot_an_praise <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
  #   labs(y="Austronesian (Praise)", color=af)
  # 
  # an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Healing") %>% rename(af="feature"), af)
  # plot_an_heal <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
  #   labs(y="Austronesian (Healing)", color=af)
  # 
  # an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Love") %>% rename(af="feature"), af)
  # plot_an_love <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
  #   labs(y="Austronesian (Love)", color=af)
  # 
  # an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Work") %>% rename(af="feature"), af)
  # plot_an_work <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
  #   labs(y="Austronesian (Work)", color=af)
  # 
  # an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Mourning") %>% rename(af="feature"), af)
  # plot_an_mourn <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
  #   labs(y="Austronesian (Mourning)", color=af)
  # 
  # an_df <- prop_af_tree(an_subtree, df %>% filter(type=="Story") %>% rename(af="feature"), af)
  # plot_an_story <- plot_af_tree(an_subtree, an_df, af, family="an", lims) +
  #   labs(y="Austronesian (Story)", color=af)
  
  # ac
  ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Dance") %>% rename(af="feature"), af)
  plot_ac_dance <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
    labs(y="Atlantic-Congo (Dance)", color="Spectral\nEntropy\nMean") 
  
  ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Lullaby") %>% rename(af="feature"), af)
  plot_ac_lull <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
    labs(y="Atlantic-Congo (Lullaby)", color="Spectral\nEntropy\nMean")
  
  # ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Play") %>% rename(af="feature"), af)
  # plot_ac_play <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
  #   labs(y="Atlantic-Congo (Play)", color=af)
  # 
  # ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Procession") %>% rename(af="feature"), af)
  # plot_ac_proc <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
  #   labs(y="Atlantic-Congo (Procession)", color=af)
  # 
  # ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Praise") %>% rename(af="feature"), af)
  # plot_ac_praise <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
  #   labs(y="Atlantic-Congo (Praise)", color=af)
  # 
  # ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Healing") %>% rename(af="feature"), af)
  # plot_ac_heal <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
  #   labs(y="Atlantic-Congo (Healing)", color=af)
  # 
  # ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Love") %>% rename(af="feature"), af)
  # plot_ac_love <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
  #   labs(y="Atlantic-Congo (Love)", color=af)
  # 
  # ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Work") %>% rename(af="feature"), af)
  # plot_ac_work <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
  #   labs(y="Atlantic-Congo (Work)", color=af)
  # 
  # ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Mourning") %>% rename(af="feature"), af)
  # plot_ac_mourn <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
  #   labs(y="Atlantic-Congo (Mourning)", color=af)
  # 
  # ac_df <- prop_af_tree(ac_subtree, df %>% filter(type=="Story") %>% rename(af="feature"), af)
  # plot_ac_story <- plot_af_tree(ac_subtree, ac_df, af, family="ac", lims) +
  #   labs(y="Atlantic-Congo (Story)", color=af)
  
  # create full plot
  scatter_plot <- splot1 + splot2den + splot2 + 
    plot_layout(nrow=3, heights=c(2.5,.3,4),guide="collect")
  
  layout_small <- "
  AB
  CD
  CD
  EF
  "
  small_tree_plot <- 
    plot_ie_dance + plot_ie_lull +
    plot_an_dance + plot_an_lull +
    plot_ac_dance + plot_ac_lull +
    plot_layout(nrow=3,guide="collect",design=layout_small,
                heights=c(1,.2,.8,1),widths=c(1,1))
  
  ## previously
  # ADE
  # BFG
  # CFG
  # CHI
  layout <- "
  A
  B
  C
  C
  "
  left_side <- 
    splot1 + splot2den + splot2 + 
    plot_layout(design=layout,heights=c(1,.2,.8,1),guides="collect")
  
  final_layout <- "
  AD
  BD
  CD
  CD
  "
  final_plot <-
    left_side + small_tree_plot +
    plot_layout(design=final_layout,guides="keep",widths=c(1,1.1)) +
    plot_annotation(tag_levels = list(c('A','','B','C')))
    # splot1 + splot2den + splot2 + 
    # plot_ie_dance + plot_ie_lull +
    # plot_an_dance + plot_an_lull +
    # plot_ac_dance + plot_ac_lull +
    # plot_layout(design=layout,heights=c(1,.2,.8,1), widths=c(2,1,1)) +
    # plot_annotation(tag_levels = list(c('A','','B','C','','','','')))
  
  return(final_plot)
}

# plotting
df <- nhs2 %>% filter(ex_tonal_mode_std<4,ex_spectral_centroid_mean<4,!is.na(family_id))
final_plot <- plot_fig5("ex_spectral_spectenropy_mean", df, ie_subtree, an_subtree, ac_subtree, song_min=2)

filename <- paste(here(),"/viz/Figure4.png",sep="")
ggsave(filename=filename,final_plot,width=11,height=8)

