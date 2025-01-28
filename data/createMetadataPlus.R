# loading FUNCTIONS
source(paste(here(),"/writing/NHS2-functions.R",sep=""),local=TRUE)


################### loading corpus, topping with everything ################### 
nhs2 <- read.csv(paste(here(),"/data/metadata.csv",sep=""), skip = 0, header = TRUE) %>%
  mutate(region = case_when(
    region=="Plains" ~ "Plains and Plateau",
    .default=region
  ))

# load/rm glottolog and add family_idfam_names <- glog %>% select(family_id) %>% 
glog <- read.csv(paste(here(),"/data/languoidGlottolog.csv",sep=""), skip = 0, header = TRUE)
fam_names <- glog %>% select(family_id) %>% 
  merge(glog%>%select(name,id)%>%rename(family_id=id)) %>%
  rename(family_name = name) %>% 
  unique()
glog <- merge(glog, fam_names)
colnames(glog)[colnames(glog)=='id'] <- 'glottocode'
nhs2 <- merge(nhs2, glog[c('glottocode','family_id','family_name','latitude','longitude')],all.x=TRUE)
rm(glog)

# load up new geolocations not found in glottolog
new_geoloc <- read.csv(
  paste(here(),'/data/new_geolocations.csv' ,sep=""), 
  skip=0, header=TRUE) %>%
  select(glottocode, latitude, longitude) %>%
  filter(!is.na(latitude))
nhs2 <- nhs2 %>% 
  merge(new_geoloc, by="glottocode", all=TRUE) %>%
  mutate(
    latitude.x = case_when(
      !is.na(latitude.y) ~ latitude.y,
      .default = latitude.x
    ),
    longitude.x = case_when(
      !is.na(longitude.y) ~ longitude.y,
      .default = longitude.x
    ),
  ) %>% 
  rename(latitude = latitude.x, longitude = longitude.x) %>%
  select(!latitude.y) %>% select(!longitude.y)

# load audio feature data
audio_feats <- read.csv(
  paste(here(),"/data/audio_Extraction_raw_median_all.csv",sep=""), 
  skip = 0, header = TRUE)
audio_feats <- audio_feats[!audio_feats$song=="",]
af_names <- audio_feats %>% select(starts_with("ex_")) %>% colnames()

# merge metadata with audio features
audio_data <- merge(nhs2, audio_feats, by.x="song")
audio_data[af_names] <- scale(audio_data[af_names])
rm(audio_feats)

filename <- paste(here(), "/data/metadata_plus.csv", sep="")
write.csv(audio_data, filename, row.names=FALSE)
