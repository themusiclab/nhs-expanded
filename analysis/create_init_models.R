## initialize models for model fitting

# imports
library(tidyverse)
library(ape)
library(brms)
library(ggtree)
library(stringr)
library(treeio)
library(geosphere)
library(here)
set.seed(1)
library(ggtreeExtra)

# load datasets
# nhs2
nhs2 <- read.csv(paste(here(),"/data/metadata.csv",sep=""), skip = 0, header = TRUE)
nhs2$glottocode2 <- nhs2$glottocode
nhs2$glottocode3 <- nhs2$glottocode

# load glottolog and add family_id
glog <- read.csv(paste(here(),"/data/languoidGlottolog.csv",sep=""), skip = 0, header = TRUE)
colnames(glog)[colnames(glog)=='id'] <- 'glottocode'
nhs2 <- merge(nhs2, glog[c('glottocode','family_id','latitude','longitude')])
rm(glog)

# load audio features
audio_feats <- read.csv(paste(here(),"/data/audioExtraction_raw_median_all.csv",sep=""), skip = 0, header = TRUE)
audio_feats <- audio_feats[!audio_feats$song=="",]

# get audio feature names
af_names <- colnames(audio_feats)[-1] # remove "song"

# merge metadata with audio features
audio_data <- merge(nhs2, audio_feats, by.x="song")
rm(audio_feats)

# load language tree
global_tree <- read.nexus(paste(here(),"/data/global-language-tree-MCC-labelled.tree",sep=""))

# edit tree labels to only show glottocode
labels <- global_tree$tip.label
new_labels <- str_extract(labels, '^[a-z]{4}[0-9]{4}')
global_tree$tip.label <- new_labels

# only include languages in tree
audio_data <- audio_data[audio_data$glottocode %in% new_labels,]
# z-score acoustic features
audio_data <- audio_data
audio_data[af_names] <- scale(audio_data[af_names])
colnames(audio_data)[colnames(audio_data)=="glottocode"] <- "language"
colnames(audio_data)[colnames(audio_data)=="glottocode2"] <- "phylogeny"
colnames(audio_data)[colnames(audio_data)=="glottocode3"] <- "geospacial.pos"

#### covariance matrices
# global phylo
full_cor <- vcv.phylo(global_tree, model = "Brownian", corr = TRUE)
phyl_cov <- full_cor[colnames(full_cor) %in% audio_data$language, colnames(full_cor) %in% audio_data$language]
rm(full_cor)

# global geo
langloc <- audio_data %>% distinct(language, .keep_all=TRUE)
dmat = distm(langloc[c("longitude","latitude")])
rownames(dmat) <- langloc$language
colnames(dmat) <- langloc$language
logdmat <- log(dmat)
diag(logdmat) <- 0
logdmat <- logdmat / max(logdmat)
prox_cov <- (1 - logdmat)

#### covariance matrix for each deeply sampled family
# ie subtree
ie_subtree <- tree_subset(global_tree,'russ1263',levels_back=7)
ie_full_corr <- vcv.phylo(ie_subtree, model = "Brownian", corr = TRUE)
ie_phyl_cov <- ie_full_corr[colnames(ie_full_corr) %in% audio_data$language, colnames(ie_full_corr) %in% audio_data$language]
ie_data <- audio_data %>% filter(family_id=="indo1319")
rm(ie_full_corr)

# ie geo
langloc <- ie_data %>% distinct(language, .keep_all=TRUE)
dmat = distm(langloc[c("longitude","latitude")])
rownames(dmat) <- langloc$language
colnames(dmat) <- langloc$language
logdmat <- log(dmat)
diag(logdmat) <- 0
logdmat <- logdmat / max(logdmat)
ie_prox_cov <- (1 - logdmat)

# austronesian subtree
an_subtree <- tree_subset(global_tree,'wole1240',levels_back=19)
an_full_corr <- vcv.phylo(an_subtree, model = "Brownian", corr = TRUE)
an_phyl_cov <- an_full_corr[colnames(an_full_corr) %in% audio_data$language, colnames(an_full_corr) %in% audio_data$language]
an_data <- audio_data %>% filter(family_id=="aust1307")
rm(an_full_corr)

# austronesian geo
langloc <- an_data %>% distinct(language, .keep_all=TRUE)
dmat = distm(langloc[c("longitude","latitude")])
rownames(dmat) <- langloc$language
colnames(dmat) <- langloc$language
logdmat <- log(dmat)
diag(logdmat) <- 0
logdmat <- logdmat / max(logdmat)
an_prox_cov <- (1 - logdmat)

# atlantic-congo subtree
ac_subtree <- tree_subset(global_tree,'lito1235',levels_back=24)
ac_full_corr <- vcv.phylo(ac_subtree, model = "Brownian", corr = TRUE)
ac_phyl_cov <- ac_full_corr[colnames(ac_full_corr) %in% audio_data$language, colnames(ac_full_corr) %in% audio_data$language]
ac_data <- audio_data %>% filter(family_id=="atla1278")
rm(ac_full_corr)

# atlantic-congo geo
langloc <- ac_data %>% distinct(language, .keep_all=TRUE)
dmat = distm(langloc[c("longitude","latitude")])
rownames(dmat) <- langloc$language
colnames(dmat) <- langloc$language
logdmat <- log(dmat)
diag(logdmat) <- 0
logdmat <- logdmat / max(logdmat)
ac_prox_cov <- (1 - logdmat)

# big 3 families subtree
big3_data <- audio_data %>% filter(family_id=="atla1278" | family_id=="aust1307" | family_id=="indo1319")
full_cor <- vcv.phylo(global_tree, model = "Brownian", corr = TRUE)
big3_phyl_cov <- full_cor[colnames(full_cor) %in% big3_data$language, colnames(full_cor) %in% big3_data$language]
rm(full_cor)

# big 3 families geo
langloc <- big3_data %>% distinct(language, .keep_all=TRUE)
dmat = distm(langloc[c("longitude","latitude")])
rownames(dmat) <- langloc$language
colnames(dmat) <- langloc$language
logdmat <- log(dmat)
diag(logdmat) <- 0
logdmat <- logdmat / max(logdmat)
big3_prox_cov <- (1 - logdmat)

rm(dmat)
rm(logdmat)


# global tree models
# ... todo for release 


# indo-european models
# m1
m1 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)),
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    # (1 | gr(type)),
  # data
  data = ie_data,
  data2 = list(phyl_cov=ie_phyl_cov, prox_cov=ie_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ie/m1","/init_model.Rds",sep="")
saveRDS(m1, file=init_filename)
# m1b
m1b <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    # (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
    # data
    data = ie_data,
  data2 = list(phyl_cov = ie_phyl_cov,prox_cov = ie_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ie/m1b","/init_model.Rds",sep="")
saveRDS(m1b, file=init_filename)
# m2
m2 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
    # data
    data = ie_data,
  data2 = list(phyl_cov = ie_phyl_cov,prox_cov = ie_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ie/m2","/init_model.Rds",sep="")
saveRDS(m2, file=init_filename)
# m3a
m3a <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = ie_data,
  data2 = list(phyl_cov = ie_phyl_cov,prox_cov = ie_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ie/m3a","/init_model.Rds",sep="")
saveRDS(m3a, file=init_filename)
# m3b
m3b <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = ie_data,
  data2 = list(phyl_cov = ie_phyl_cov,prox_cov = ie_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ie/m3b","/init_model.Rds",sep="")
saveRDS(m3b, file=init_filename)
# m4
m4 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    (1 | gr(geospacial.pos, cov=prox_cov)) +
    (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = ie_data,
  data2 = list(phyl_cov = ie_phyl_cov,prox_cov = ie_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ie/m4","/init_model.Rds",sep="")
saveRDS(m4, file=init_filename)


# antlantic-congo models
# m1
m1 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)),
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    # (1 | gr(type)),
    # data
    data = ac_data,
  data2 = list(phyl_cov=ac_phyl_cov, prox_cov=ac_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ac/m1","/init_model.Rds",sep="")
saveRDS(m1, file=init_filename)
# m1b
m1b <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    # (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = ac_data,
  data2 = list(phyl_cov = ac_phyl_cov,prox_cov = ac_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ac/m1b","/init_model.Rds",sep="")
saveRDS(m1b, file=init_filename)
# m2
m2 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = ac_data,
  data2 = list(phyl_cov = ac_phyl_cov,prox_cov = ac_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ac/m2","/init_model.Rds",sep="")
saveRDS(m2, file=init_filename)
# m3a
m3a <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = ac_data,
  data2 = list(phyl_cov = ac_phyl_cov,prox_cov = ac_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ac/m3a","/init_model.Rds",sep="")
saveRDS(m3a, file=init_filename)
# m3b
m3b <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = ac_data,
  data2 = list(phyl_cov = ac_phyl_cov,prox_cov = ac_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ac/m3b","/init_model.Rds",sep="")
saveRDS(m3b, file=init_filename)
# m4
m4 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    (1 | gr(geospacial.pos, cov=prox_cov)) +
    (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = ac_data,
  data2 = list(phyl_cov = ac_phyl_cov,prox_cov = ac_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/ac/m4","/init_model.Rds",sep="")
saveRDS(m4, file=init_filename)

# austronesian models 
# m1
m1 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)),
  # (1 | gr(geospacial.pos, cov=prox_cov)) +
  # (1 | gr(phylogeny, cov=phyl_cov)) +
  # (1 | gr(type)),
  # data
  data = an_data,
  data2 = list(phyl_cov=an_phyl_cov, prox_cov=an_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/an/m1","/init_model.Rds",sep="")
saveRDS(m1, file=init_filename)
# m1b
m1b <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    # (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = an_data,
  data2 = list(phyl_cov = an_phyl_cov,prox_cov = an_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/an/m1b","/init_model.Rds",sep="")
saveRDS(m1b, file=init_filename)
# m2
m2 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = an_data,
  data2 = list(phyl_cov = an_phyl_cov,prox_cov = an_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/an/m2","/init_model.Rds",sep="")
saveRDS(m2, file=init_filename)
# m3a
m3a <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = an_data,
  data2 = list(phyl_cov = an_phyl_cov,prox_cov = an_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/an/m3a","/init_model.Rds",sep="")
saveRDS(m3a, file=init_filename)
# m3b
m3b <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = an_data,
  data2 = list(phyl_cov = an_phyl_cov,prox_cov = an_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/an/m3b","/init_model.Rds",sep="")
saveRDS(m3b, file=init_filename)
# m4
m4 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    (1 | gr(geospacial.pos, cov=prox_cov)) +
    (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = an_data,
  data2 = list(phyl_cov = an_phyl_cov,prox_cov = an_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/an/m4","/init_model.Rds",sep="")
saveRDS(m4, file=init_filename)


# big3 families
# m1
m1 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)),
  # (1 | gr(geospacial.pos, cov=prox_cov)) +
  # (1 | gr(phylogeny, cov=phyl_cov)) +
  # (1 | gr(type)),
  # data
  data = big3_data,
  data2 = list(phyl_cov=big3_phyl_cov, prox_cov=big3_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/big3/m1","/init_model.Rds",sep="")
saveRDS(m1, file=init_filename)
# m1b
m1b <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    # (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = big3_data,
  data2 = list(phyl_cov = big3_phyl_cov,prox_cov = big3_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/big3/m1b","/init_model.Rds",sep="")
saveRDS(m1b, file=init_filename)
# m2
m2 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = big3_data,
  data2 = list(phyl_cov = big3_phyl_cov,prox_cov = big3_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/big3/m2","/init_model.Rds",sep="")
saveRDS(m2, file=init_filename)
# m3a
m3a <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    (1 | gr(geospacial.pos, cov=prox_cov)) +
    # (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = big3_data,
  data2 = list(phyl_cov = big3_phyl_cov,prox_cov = big3_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/big3/m3a","/init_model.Rds",sep="")
saveRDS(m3a, file=init_filename)
# m3b
m3b <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    # (1 | gr(geospacial.pos, cov=prox_cov)) +
    (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = big3_data,
  data2 = list(phyl_cov = big3_phyl_cov,prox_cov = big3_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/big3/m3b","/init_model.Rds",sep="")
saveRDS(m3b, file=init_filename)
# m4
m4 <- brm(
  # formula
  ex_rhythm_attack_time_mean ~ 1 +
    (1 | gr(language)) +
    (1 | gr(geospacial.pos, cov=prox_cov)) +
    (1 | gr(phylogeny, cov=phyl_cov)) +
    (1 | gr(type)),
  # data
  data = big3_data,
  data2 = list(phyl_cov = big3_phyl_cov,prox_cov = big3_prox_cov),
  # set reasonable priors
  prior = c(
    prior(normal(0, .5), class = Intercept),
    prior(exponential(5), class = sd),
    prior(exponential(5), class = sigma)
  ),
  control = list(adapt_delta = 0.8),
  cores = 4, iter = 1
)
init_filename <- paste(here(),"/results","/fits/big3/m4","/init_model.Rds",sep="")
saveRDS(m4, file=init_filename)
