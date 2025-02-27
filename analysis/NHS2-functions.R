# libraries
if(!require(pacman)) {
  message("installing the 'pacman' package")
  install.packages("pacman")
}
library(pacman)
p_load(
  tidyverse,
  tidymodels,
  ape,
  brms,
  cowplot,
  ggtree,
  ggtreeExtra,
  patchwork,
  sf,
  stringr,
  treeio,
  tidytree,
  geosphere,
  viridis,
  vip
)

# edit acoustic feature name 
# from "ex_something_another_mean"
# to "something another mean"
name_of_feature <- function(feature) {
  x <- strsplit(feature,'_')
  x <- x[[1]]
  
  if (length(x)==4) {
    return(paste(x[3],x[4],sep=" "))
  } else {
    return(paste(x[3],x[4],x[5],sep=" "))
  }
}

# For PHYLO stuff:
# extract explained variance for language, lang-phylo, type, and region
# pulls samples from model, 
# feature is for label, 
# data appended to df
calc_ICC <- function(post, feature, df, func=mean) {
  
  post <- data.frame(as_draws_matrix(post))
  # calculate explained variance for each 
  type_sig <- post$sd_type__Intercept^2 / 
    (post$sd_type__Intercept^2 + 
       post$sd_phylogeny__Intercept^2 + 
       post$sd_language__Intercept^2 + 
       post$sd_geospacial.pos__Intercept^2 + 
       post$sigma^2)
  tsig <- func(type_sig)
  
  phylo_sig <- post$sd_phylogeny__Intercept^2 / 
    (post$sd_type__Intercept^2 + 
       post$sd_phylogeny__Intercept^2 + 
       post$sd_language__Intercept^2 + 
       post$sd_geospacial.pos__Intercept^2 + 
       post$sigma^2)
  psig <- func(phylo_sig)
  
  lang_sig <- post$sd_language__Intercept^2 / 
    (post$sd_type__Intercept^2 + 
       post$sd_phylogeny__Intercept^2 + 
       post$sd_language__Intercept^2 + 
       post$sd_geospacial.pos__Intercept^2 + 
       post$sigma^2)
  lsig <- func(lang_sig)
  
  region_sig <- post$sd_geospacial.pos__Intercept^2 /
    (post$sd_type__Intercept^2 +
       post$sd_phylogeny__Intercept^2 +
       post$sd_language__Intercept^2 +
       post$sd_geospacial.pos__Intercept^2 +
       post$sigma^2)
  rsig <- func(region_sig)
  
  resid <- post$sigma^2 / 
    (post$sd_type__Intercept^2 + 
       post$sd_phylogeny__Intercept^2 + 
       post$sd_language__Intercept^2 + 
       post$sd_geospacial.pos__Intercept^2 + 
       post$sigma^2)
  res <- func(resid)
  
  # append values to df
  df <- df %>% 
    add_row(feature=feature,variable='type',value=tsig) %>%
    add_row(feature=feature,variable='phylogeny',value=psig) %>%
    add_row(feature=feature,variable='language',value=lsig) %>%
    add_row(feature=feature,variable='proximity',value=rsig) %>%
    add_row(feature=feature,variable='residual',value=res)
  
  return(df)
}

# for LASSO ensemble predictions
# return mode of each row (ensamble decision)
# apply to prediction dataframe
get_mode <- function(x) {
  ux <- unique(x) # get options
  ret <- ux[which.max(tabulate(match(x,ux)))][1] # select most common option
  return(ret)
} # using get_mode looks like: apply(data_frame, 1, get_mode)


# for TREE plotting
# propagates "has_song" label up the tree
# through parents
# returns tree
fill_tree <- function(tree, df) {
  x <- as_tibble(tree) %>% 
    as.data.frame() %>% # turn into df
    merge(
      df %>% rename("label"="glottocode")
      %>% select(label,has_song), 
      all.x=TRUE, by="label") # add "has_song" label
  
  has <- x %>% filter(has_song==TRUE) %>% pull(node)
  for (i in 1:30) {
    x <- x %>%
      mutate(has_song = case_when( # mark TRUE
        node %in% has ~ TRUE,
        .default=has_song
      ))
    has <- x %>% filter(has_song==TRUE) %>% pull(parent)
  }
  
  x1 <- x %>% 
    rename(child_rep=has_song) %>% 
    select(node, child_rep) %>% 
    mutate(child_rep = case_when(
      is.na(child_rep) ~ FALSE,
      .default=child_rep
    )) %>%
    as_tibble()
  tree <- treedata(phylo=tree, data=x1)
  return(tree)
}

# for plotting PHYLO stuff
get_samples <- function(model, feat_name, skip=5, model_type="m4") {
  df <- as.data.frame(as_draws_matrix(model))
  
  if (model_type=="m1") {
    df <- df %>% rename(
        "sd_type" = "sd_type__Intercept",
        "res" = "sigma"
      ) %>% select(sd_type,res)
  } else if (model_type=="m1b") {
    df <- df %>% rename(
      "sd_language" = "sd_language__Intercept",
      "res" = "sigma"
    ) %>% select(sd_language,res)
  } else if (model_type=="m2") {
    df <- df %>% rename(
      "sd_language" = "sd_language__Intercept",
      "sd_type" = "sd_type__Intercept",
      "res" = "sigma"
    ) %>% select(sd_language,sd_type,res)
  } else if (model_type=="m3a") {
    df <- df %>% rename(
      "sd_region" = "sd_geospacial.pos__Intercept",
      "sd_language" = "sd_language__Intercept",
      "sd_type" = "sd_type__Intercept",
      "res" = "sigma"
    ) %>% select(sd_region,sd_language,sd_type,res)
  } else if (model_type=="m3b") {
    df <- df %>% rename(
      "sd_language" = "sd_language__Intercept",
      "sd_phylogeny" = "sd_phylogeny__Intercept",
      "sd_type" = "sd_type__Intercept",
      "res" = "sigma"
    ) %>% select(sd_language,sd_phylogeny,sd_type,res)
  } else if (model_type=="m4") {
    df <- df %>% rename(
      "sd_region" = "sd_geospacial.pos__Intercept",
      "sd_language" = "sd_language__Intercept",
      "sd_phylogeny" = "sd_phylogeny__Intercept",
      "sd_type" = "sd_type__Intercept",
      "res" = "sigma"
    ) %>% select(sd_region,sd_language,sd_phylogeny,sd_type,res)
  } else return(FALSE)
  
  df <- df %>%
    filter(row_number() %% skip == 0) %>%
    mutate(feature=feat_name) %>% select(feature, everything())
  return(df)
}

# for LOADING MODELS
# loading (and fitting if non existant) group-effects models
fit_or_load <- function(df, af, model_type="m4", nsamp=6000, family="global") {
  # check if model fit already exists
  filename <- paste(here(),"/results","/fits/",family,"/",model_type,"/",af,"_n",nsamp,".Rds",sep="")
  
  if (file.exists(filename)) { # load existing model fit
    print(paste("Loading from file...",filename))
    return(readRDS(filename))
  }
  print(paste("No file found, fitting model... ",af))
  
  #### for ex_rhythm_attack_time_mean, no outlier
  df_no <- df %>% filter(song != "NHS2-BLMM")
  
  # load init model
  init_filename <- paste(here(),"/results","/fits/",family,"/",model_type,"/","init_model",".Rds",sep="")
  model <- readRDS(init_filename)
  
  # set acoustic feature to be fit (af) and fit model
  model <- switch(af,
                  ex_rhythm_attack_time_mean = update(model, # remove outlier point (NHS2-BLMM)
                                                      ex_rhythm_attack_time_mean ~ .,
                                                      newdata=df_no, iter=nsamp, cores=4),
                  ex_simple_lowenergy_mean = update(model, 
                                                    ex_simple_lowenergy_mean ~ .,
                                                    newdata=df, iter=nsamp, cores=4),
                  ex_simple_brightness_mean = update(model, 
                                                     ex_simple_brightness_mean ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_simple_roughness_mean = update(model, 
                                                    ex_simple_roughness_mean ~ .,
                                                    newdata=df, iter=nsamp, cores=4),
                  ex_simple_centroid_mean = update(model, 
                                                   ex_simple_centroid_mean ~ .,
                                                   newdata=df, iter=nsamp, cores=4),
                  ex_spectral_centroid_mean = update(model, 
                                                     ex_spectral_centroid_mean ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_spectral_brightness_mean = update(model, 
                                                       ex_spectral_brightness_mean ~ .,
                                                       newdata=df, iter=nsamp, cores=4),
                  ex_spectral_spread_mean = update(model, 
                                                   ex_spectral_spread_mean ~ .,
                                                   newdata=df, iter=nsamp, cores=4),
                  ex_spectral_skewness_mean = update(model, 
                                                     ex_spectral_skewness_mean ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_spectral_kurtosis_mean = update(model, 
                                                     ex_spectral_kurtosis_mean ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_spectral_rolloff95_mean = update(model, 
                                                      ex_spectral_rolloff95_mean ~ .,
                                                      newdata=df, iter=nsamp, cores=4),
                  ex_spectral_rolloff85_mean = update(model, 
                                                      ex_spectral_rolloff85_mean ~ .,
                                                      newdata=df, iter=nsamp, cores=4),
                  ex_spectral_spectenropy_mean = update(model, 
                                                        ex_spectral_spectenropy_mean ~ .,
                                                        newdata=df, iter=nsamp, cores=4),
                  ex_spectral_flatness_mean = update(model, 
                                                     ex_spectral_flatness_mean ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_spectral_roughness_mean = update(model, 
                                                      ex_spectral_roughness_mean ~ .,
                                                      newdata=df, iter=nsamp, cores=4),
                  ex_spectral_irregularity_mean = update(model, 
                                                         ex_spectral_irregularity_mean ~ .,
                                                         newdata=df, iter=nsamp, cores=4),
                  ex_tonal_keyclarity_mean = update(model, 
                                                    ex_tonal_keyclarity_mean ~ .,
                                                    newdata=df, iter=nsamp, cores=4),
                  ex_tonal_mode_mean = update(model, 
                                              ex_tonal_mode_mean ~ .,
                                              newdata=df, iter=nsamp, cores=4),
                  ex_rhythm_tempo_mean = update(model, 
                                                ex_rhythm_tempo_mean ~ .,
                                                newdata=df, iter=nsamp, cores=4),
                  ex_rhythm_attack_time_mean = update(model, 
                                                      ex_rhythm_attack_time_mean ~ .,
                                                      newdata=df, iter=nsamp, cores=4),
                  ex_rhythm_attack_slope_mean = update(model, 
                                                       ex_rhythm_attack_slope_mean ~ .,
                                                       newdata=df, iter=nsamp, cores=4),
                  ex_dynamics_rms_mean = update(model, 
                                                ex_dynamics_rms_mean ~ .,
                                                newdata=df, iter=nsamp, cores=4),
                  ex_spectral_centroid_std = update(model, 
                                                    ex_spectral_centroid_std ~ .,
                                                    newdata=df, iter=nsamp, cores=4),
                  ex_spectral_brightness_std = update(model, 
                                                      ex_spectral_brightness_std ~ .,
                                                      newdata=df, iter=nsamp, cores=4),
                  ex_spectral_spread_std = update(model, 
                                                  ex_spectral_spread_std ~ .,
                                                  newdata=df, iter=nsamp, cores=4),
                  ex_spectral_skewness_std = update(model, 
                                                    ex_spectral_skewness_std ~ .,
                                                    newdata=df, iter=nsamp, cores=4),
                  ex_spectral_kurtosis_std = update(model, 
                                                    ex_spectral_kurtosis_std ~ .,
                                                    newdata=df, iter=nsamp, cores=4),
                  ex_spectral_rolloff95_std = update(model, 
                                                     ex_spectral_rolloff95_std ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_spectral_rolloff85_std = update(model, 
                                                     ex_spectral_rolloff85_std ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_spectral_spectenropy_std = update(model, 
                                                       ex_spectral_spectenropy_std ~ .,
                                                       newdata=df, iter=nsamp, cores=4),
                  ex_spectral_flatness_std = update(model, 
                                                    ex_spectral_flatness_std ~ .,
                                                    newdata=df, iter=nsamp, cores=4),
                  ex_spectral_roughness_std = update(model, 
                                                     ex_spectral_roughness_std ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_spectral_irregularity_std = update(model, 
                                                        ex_spectral_irregularity_std ~ .,
                                                        newdata=df, iter=nsamp, cores=4),
                  ex_tonal_keyclarity_std = update(model, 
                                                   ex_tonal_keyclarity_std ~ .,
                                                   newdata=df, iter=nsamp, cores=4),
                  ex_tonal_mode_std = update(model, 
                                             ex_tonal_mode_std ~ .,
                                             newdata=df, iter=nsamp, cores=4),
                  ex_rhythm_tempo_std = update(model, 
                                               ex_rhythm_tempo_std ~ .,
                                               newdata=df, iter=nsamp, cores=4),
                  ex_rhythm_attack_time_std = update(model, 
                                                     ex_rhythm_attack_time_std ~ .,
                                                     newdata=df, iter=nsamp, cores=4),
                  ex_rhythm_attack_slope_std = update(model, 
                                                      ex_rhythm_attack_slope_std ~ .,
                                                      newdata=df, iter=nsamp, cores=4),
                  ex_dynamics_rms_std = update(model, 
                                               ex_dynamics_rms_std ~ .,
                                               newdata=df, iter=nsamp, cores=4)
  )
  
  # save and return
  print("...saving model")
  saveRDS(model, file=filename)
  return(model)
}

### Plotting stuff

