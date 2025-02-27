# loading FUNCTIONS
library(here)
source(paste(here(),"/analysis/NHS2-functions.R",sep=""),local=TRUE)


################### loading corpus, topping with everything ################### 
nhs2 <- read.csv(paste(here(),"/data/metadata_plus.csv",sep=""), skip = 0, header = TRUE)
audio_data <- nhs2

# load language tree
global_tree <- read.nexus(paste(here(),"/data/global-language-tree-MCC-labelled.tree",sep=""))

# edit tree labels to only show glottocode
labels <- global_tree$tip.label
new_labels <- str_extract(labels, '^[a-z]{4}[0-9]{4}')
global_tree$tip.label <- new_labels

# load map
map <- read_sf(paste(here(),"/data/world.geo.json" ,sep=""))


# MAP PREPROCESS
# get glottocode counts, family_id, and location
lang_wloc <- nhs2 %>%
  group_by(glottocode) %>%
  summarise(
    count = length(glottocode), 
    region = max(region), 
    num_reg = length(unique(region)),
    family_id = unique(family_id),
    latitude = unique(latitude),
    longitude = unique(longitude),
    ) %>%
  mutate(family_class = case_when(
    family_id=='indo1319' ~ "Indo-European",
    family_id=='aust1307' ~ "Austronesian",
    family_id=='atla1278' ~ "Atlantic-Congo",
    TRUE ~ "Other (76)")
  ) %>% arrange(desc(count))


# PIE PREPROCESS
# calculate number of songs per-family
# 76 + 3 langauge families
family_count <- audio_data %>% 
  mutate(name=case_when(
    family_id=='indo1319' ~ "Indo-European",
    family_id=='aust1307' ~ "Austronesian",
    family_id=='atla1278' ~ "Atlantic-Congo",
    TRUE ~ "Other (76)")) %>%
  group_by(name) %>%
  summarize(n_song = n())


# TREE PREPROCESS
# calculate number of songs per language family
lang_count <- audio_data %>% group_by(glottocode) %>% summarize(n_song = length(song))

# for manually checking whether it's only IE languages in the tree
ie_subtree <- tree_subset(global_tree,'russ1263',levels_back=7)

# create df for checking representation
ie_df <- as_tibble(ie_subtree$tip.label) %>% rename(glottocode = value)
ie_df <- merge(ie_df, lang_count, all.x=TRUE)

ie_df <- ie_df %>% 
  mutate(has_song = case_when(
    !is.na(n_song) ~ TRUE,
    .default=FALSE
  ))

# austronesian subtree
an_subtree <- tree_subset(global_tree,'wole1240',levels_back=19)

# create df for checking representation
an_df <- as_tibble(an_subtree$tip.label) %>% rename(glottocode = value)
an_df <- merge(an_df, lang_count, all.x=TRUE)

an_df <- an_df %>% 
  mutate(has_song = case_when(
    !is.na(n_song) ~ TRUE,
    .default=FALSE
  ))

# atlantic-congo subtree
ac_subtree <- tree_subset(global_tree,'lito1235',levels_back=21)

# create df for checking representation
ac_df <- as_tibble(ac_subtree$tip.label) %>% rename(glottocode = value)
ac_df <- merge(ac_df, lang_count, all.x=TRUE)

ac_df <- ac_df %>% 
  mutate(has_song = case_when(
    !is.na(n_song) ~ TRUE,
    .default=FALSE
  ))


# SUMMARY PLOT PREPROCESS
# all possible region*type categories
region_list <- unique(nhs2$region)
type_list <- unique(nhs2$type)
rt_full <- expand_grid(region_list, type_list) %>%
  rename('region'='region_list','type'='type_list')

# count all songs by region*type
reg_by_type_unfilt <- nhs2 %>%
  group_by(region, type) %>%
  dplyr::summarise(n_unfilt = n())

# fill in missing region*type rows
reg_by_type_unfilt <- left_join(rt_full, reg_by_type_unfilt) %>%
  mutate(n_unfilt = ifelse(is.na(n_unfilt), 0, n_unfilt))

# calculate type marginals
type_totals <- reg_by_type_unfilt %>%
  group_by(type) %>%
  summarise(total_type = sum(n_unfilt))

# calculate region marginals
reg_totals <- reg_by_type_unfilt %>%
  group_by(region) %>%
  summarise(total = sum(n_unfilt))

reg_by_type_unfilt <- merge(reg_by_type_unfilt, reg_totals, by='region')
reg_by_type_unfilt <- merge(reg_by_type_unfilt, type_totals, by='type')
reg_by_type_unfilt <- reg_by_type_unfilt %>% mutate_if(is.integer,as.numeric)


# PLOTTING
# pie plot
# colors <- c("#2D82B7","#42E2B8","#EB8A90","#83858C")#F3DFBF")
colors <- c("#EB8A90","#42E2B8","#3da2e3","#83858C")
# plotting
pie_plot <- family_count %>% 
  mutate(ord = case_when(
    name=="Indo-European" ~ 1,
    name=="Austronesian" ~ 2,
    name=="Atlantic-Congo" ~ 3,
    name=="Other (76)" ~ 4,
  )) %>%
  mutate(name=fct_reorder(name,ord)) %>%
  ggplot(aes(x="",fill=name, y=n_song,label=n_song)) + 
  geom_bar(width = 1,stat='identity', color="#333333", linewidth=.3) +
  geom_text(position = position_stack(vjust=.65), size=2, hjust=c(-.3,.3,.3,.5)) + 
  scale_fill_manual(values=colors) +
  coord_polar("y",start=3.1*pi/2) +
  theme_void() + 
  labs(fill="Language Family") +
  theme(plot.margin = unit(c(.1,.1,.1,.1),"cm"))
pie_plot_legend <- get_legend(pie_plot)
pie_plot <- pie_plot + theme(legend.position="none")

# IE tree plot
ie_filled <- fill_tree(ie_subtree, ie_df)
ie_plot <- ie_filled %>%
  ggtree(size=.2, layout='fan', open.angle=10) %<+% ie_df +
  aes(color=child_rep) +
  geom_fruit(
    geom=geom_tile,
    mapping = aes(y=Pos, fill=has_song),
    offset=.1,
    pwidth=.1
  ) +
  scale_fill_manual(values=c('white','#a45f60')) + #ffaaaa
  scale_color_manual(values=c('grey','#a45f60'), guide=FALSE) +
  labs(y="Indo-European") + guides(fill=FALSE) + 
  theme(
    panel.background=element_blank(), 
    plot.background = element_blank(),
    plot.margin = unit(c(.1,.1,.1,.1),"cm")
  )
ie_plot

an_filled <- fill_tree(an_subtree, an_df)
# AN tree plot
an_plot <- an_filled %>%
  ggtree(size=.12, layout='fan', open.angle=10) %<+% an_df +
  aes(color=child_rep) +
  geom_fruit(
    geom=geom_tile,
    mapping = aes(y=Pos, fill=has_song),
    offset=.1,
    pwidth=.1
  ) +
  scale_fill_manual(values=c('white','#298a71')) +
  scale_color_manual(values=c('#dddddd','#298a71'), guide=FALSE) +
  labs(y="Austronesian") + guides(fill=FALSE)  + 
  theme(
    panel.background=element_blank(), 
    plot.background = element_blank(),
    plot.margin = unit(c(.1,.1,.1,.1),"cm")
  )
# an_plot

ac_filled <- fill_tree(ac_subtree, ac_df)
# AC tree plot
ac_plot <- ac_filled %>%
  ggtree(size=.15, layout='fan', open.angle=10) %<+% ac_df +
  aes(color=child_rep) +
  geom_fruit(
    geom=geom_tile,
    mapping = aes(y=Pos, fill=has_song),
    offset=.1,
    pwidth=.1
  ) +
  scale_fill_manual(values=c('white','#2D82B7')) +
  scale_color_manual(values=c('#dddddd','#2D82B7'),na.value='#dddddd', guide=FALSE) +
  labs(y="Atlantic-Congo") + guides(fill=FALSE) + 
  theme(
    panel.background=element_blank(), 
    plot.background = element_blank(),
    plot.margin = unit(c(.1,.1,.1,.1),"cm")
  )
# ac_plot

# summary plot
colour_breaks <- c(0, 1, max(reg_by_type_unfilt$n_unfilt))
colours <- c("#50808e", "#69a297", "#A3C9A8") # greens, foresty
#colours <- c("#52489C", "#4062BB", "#59C3C3") # testing colors

repr_plot <- reg_by_type_unfilt %>% mutate(region=fct_reorder(region,-total)) %>%
  mutate(type=fct_reorder(type,total_type)) %>%
  #mutate(n_unfilt = replace(n_unfilt, n_unfilt==0, NA)) %>%
  ggplot(aes(y=type, x=region, fill=n_unfilt, label=n_unfilt)) + 
  #ggplot(aes(y=type, x=region, fill=!is.na(n_unfilt))) + 
  geom_bin2d() + 
  scale_x_discrete(guide = guide_axis(n.dodge=1, angle=60)) +
  scale_fill_gradientn(
    limits  = range(reg_by_type_unfilt$n_unfilt),
    colours = colours[c(1, seq_along(colours), length(colours))],
    #values  = c(0,1,13),
    values  = c(0, scales::rescale(colour_breaks, from = range(reg_by_type_unfilt$n_unfilt)), 1)
  ) +
  geom_text(size=1.5) +
  #scale_y_discrete(guide = guide_axis(n.dodge=2)) +
  theme_minimal() +
  theme(legend.position = "right", 
        plot.margin=unit(c(0,0,0,0), 'cm'),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x= element_blank(),
        axis.text.y= element_blank()) +
  labs(fill="song count") + xlab("") + ylab("") + guides(fill=FALSE)
#repr_plot

type_plot <- type_totals %>%
  mutate(type=fct_reorder(type,total_type)) %>%
  ggplot(aes(x=type, y=total_type, label=total_type)) +
  geom_col(width=.95) +
  geom_text(size=2, nudge_y=-25, color="white") +
  geom_text(aes(label=type), nudge_y=2, hjust=0, size=2) +
  theme_minimal() +
  ylim(0,220) +
  # scale_y_continuous(expand=expansion(add=c(0,12))) +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        plot.margin=unit(c(0,1,0,0), 'cm'),
        panel.grid = element_blank(),
        axis.ticks = element_blank()) + 
  guides(fill="none") +
  labs(y="", x="") +
  coord_flip()
#type_plot

region_plot <- reg_totals %>% 
  mutate(region = case_when(
    region=="Northwestern South America" ~ "NW South America",
    region=="Northwest Coast and California" ~ "NW Coast and California",
    region=="Southeastern Europe" ~ "SE Europe",
    region=="Southeast Asia" ~ "SE Asia",
    .default = region
  )) %>% mutate(region=fct_reorder(region,-total)) %>%
  ggplot(aes(y=region, x=total, label=total)) +
  geom_col() +
  geom_text(size=2, nudge_x=-4.5, color="white") +
  geom_text(aes(label=region), angle=90, nudge_x = 2, hjust=0, size=2) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x.top = element_text("# of songs"),
        axis.text.x.bottom = element_blank(),
        plot.margin=unit(c(0,0,0,0), 'cm'),
        panel.grid = element_blank()) + 
  guides(fill="none") +
  labs(x="", y="") +
  xlim(0,100) +
  coord_flip()
# region_plot

colors <- c("#3da2e3","#42E2B8","#EB8A90","#83858C")#F3DFBF")#2D82B7 3da2e3
# plot map
map_fig <- ggplot(map) + theme_void() +
  geom_sf(data=map, colour="#cccccc", fill="#dddddd") +
  geom_point(data=lang_wloc, 
             aes(y=latitude, x=longitude, size=count, fill=family_class),
             shape=21, stroke=.5, color="black", alpha=.9
  ) +
  #geom_point(data=lang_wloc, aes(y=latitude, x=longitude, fill=region), shape=21, stroke=.3, colour="black") +
  #facet_wrap(vars(region)) +
  # scale_fill_hue(l=100, c=40) +
  scale_fill_manual(values=colors, guide="none") +
  # scale_size_continuous(range=c(1,5)) +
  labs(size="# of songs")
# theme(plot.margin = unit(c(.1,.1,.1,.1),"cm"))
map_fig_legend <- get_legend(map_fig)
map_fig <- map_fig + theme(legend.position="none")

trees_plot <- ie_plot + an_plot + ac_plot + plot_layout(design="ABC")

layout <- "
A##
###
B#C
"
full_plot <- region_plot + repr_plot + type_plot +
  plot_layout(guides='collect', design=layout,
              widths= c(22,-1.15,6), heights = c(8,-2.6,6)) +
  plot_annotation(theme=theme(plot.margin = margin(0,0,0,0,'cm')))

# full_page_fig <- map_fig + trees_plot + full_plot +
#   plot_layout(ncol=1, widths = c(15), heights = c(5,3,6)) +
#   inset_element(pie_plot, .85, 2.0, 1.25, 2.4) +
#   plot_annotation(tag_levels = list(c('A','B','C','D','','E')))
layout2 <- "
AAB
AAC
DD#
EEE
"
full_page_fig <- map_fig + map_fig_legend + pie_plot_legend + trees_plot + full_plot +
  # plot_layout(ncol=2, widths = c(15,4), heights = c(5,3,6)) +
  plot_layout(design = layout2, widths = c(15,4,2), heights = c(2.3,2.5,3.5,5)) +
  inset_element(pie_plot, .8, 1.3, 1.1, 1.97) +
  plot_annotation(tag_levels = list(c('A','','','B','C','D','F','','','E')))

# full_page_fig
filename <- paste(here(),'/viz/Figure2.png', sep="")
ggsave(filename, full_page_fig, width=8, height=8)

