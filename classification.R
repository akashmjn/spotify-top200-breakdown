dt = read.csv("data.csv")

# Number of unique songs in the top x. Find the region with the maximum number
x = 20
unique_top_20 = dt %>% group_by(Region) %>% filter(Position %in% c(1:x)) %>% 
  summarise(Total_Unique = n_distinct(Track.Name)) %>% arrange(desc(Total_Unique))

# Get names of tracks in the region decided from above that are in the top at least once
top_track_names = unique((dt %>% filter((Region == "fr") & (Position %in% c(1:x))))$URL)

# Get bottom tracks
bottom_track_names = unique((dt %>% filter((Region == "fr") & !(URL %in% top_track_names)))$URL)
bottom_track_names = sample(bottom_track_names, length(top_track_names))

# Make new dataset with labels added
new_dt = song_feats %>% filter((URL %in% top_track_names) | (URL %in% bottom_track_names))
new_dt = transform(new_dt, label = if_else(URL %in% top_track_names, 1, 0))
new_col = rep("fr", nrow(new_dt))
new_dt$Region = new_col

akash_data = read.csv("data_t20regions.csv")

# Make dataframe with multiple regions included
regions_to_include = c("fi", "nl", "it", "de")
regions_to_include = c("ar", "au", "br", "ca", "cl", "de", "dk", "es", "fi", "fr", "gb", "global", "id", "it", "mx", "nl", "no", "pe", "ph", "se", "us")
for (i in (1 : length(regions_to_include))){
  top_track_names = unique((dt %>% filter((Region == regions_to_include[i]) & (Position %in% c(1:x))))$URL)
  bottom_track_names = unique((dt %>% filter((Region == regions_to_include[i]) & !(URL %in% top_track_names)))$URL)
  bottom_track_names = sample(bottom_track_names, length(top_track_names))
  curr_dt = song_feats %>% filter((URL %in% top_track_names) | (URL %in% bottom_track_names))
  print(nrow(curr_dt))
  curr_dt = transform(curr_dt, label = if_else(URL %in% top_track_names, 1, 0))
  new_col = rep(regions_to_include[i], nrow(curr_dt))
  curr_dt$Region = new_col
  new_dt = rbind(new_dt, curr_dt)
  print(nrow(new_dt))
}
new_dt <- new_dt[sample(nrow(new_dt)),]

# Logistic regression
classification = glm(label ~ Region + energy + liveness + tempo + speechiness + acousticness + instrumentalness + danceability + loudness + valence, data = new_dt, family = binomial)
to_predict = new_dt[,c("Region", "energy","liveness","tempo","speechiness","acousticness","instrumentalness","danceability","loudness", "valence")]
fitted.results <- predict(classification, newdata=to_predict, type='response')
fitted.results <- ifelse(fitted.results > 0.5,1,0)

misClasificError <- mean(fitted.results != new_dt$label)
print(paste('Accuracy',1-misClasificError))
num_false_positive = 0
num_false_negative = 0
num_true_negative = 0
num_true_positive = 0
for (i in 1:length(fitted.results)){
  if ((fitted.results[i] == 1) & (new_dt$label[i]) == 0){
    num_false_positive = num_false_positive + 1
  }
  if ((fitted.results[i] == 0) & (new_dt$label[i]) == 1){
    num_false_negative = num_false_negative + 1
  }
  if ((fitted.results[i] == 0) & (new_dt$label[i]) == 0){
    num_true_negative = num_true_negative + 1
  }
  if ((fitted.results[i] == 1) & (new_dt$label[i]) == 1){
    num_true_positive = num_true_positive + 1
  }
}
print(paste('False Positive Rate: ',num_false_positive/length(fitted.results)))
print(paste('False Negative Rate: ',num_false_negative/length(fitted.results)))
print(paste('True Negative Rate: ',num_true_negative/length(fitted.results)))
print(paste('True Positive Rate: ',num_true_positive/length(fitted.results)))



# Doing a trends analysis for the song "Chantaje" 
time_series = dt %>% group_by(Track.Name, Region) %>% filter(Track.Name == "Chantaje")
top_regions = (time_series %>% group_by(Region) %>% 
                 summarise(total = n()) %>% filter(total == max(total)))$Region

time_series %>% filter(Region %in% top_regions) %>% 
  ggplot() + geom_point(mapping = aes(x = Date, y = Position, color = Region))

# Make new dataset with song related features
song_feats = read_csv("song_feats.csv")
reduced_data = unique(dt %>% filter(Region == "is"))[,c("Track.Name")]
merged = merge(dt, song_feats, by="URL")
merged$label = cut(merged$Position, breaks=c(0,20,200), labels=c(1,0))

# Run logistic regression
classification = glm(label ~ energy + liveness + tempo + speechiness + acousticness + instrumentalness + danceability + loudness, data = merged, family = binomial)
to_predict = merged[,c("Region","energy","liveness","tempo","speechiness","acousticness","instrumentalness","danceability","loudness")]

# correlation plots
top_track_names = unique((dt %>% filter((Region == "fr") & (Position %in% c(1:x))))$URL)
temp_dt = song_feats %>% filter(URL %in% top_track_names)
temp_dt_1 = dt %>% group_by(URL) %>% summarise(mean = mean(Streams))
new_temp_dt = merge(temp_dt, temp_dt_1, by= "URL")

ggplot(new_temp_dt) + facet_wrap(mapping = aes(x=danceability, y=mean))

songSummaries5Regions = merge(new_dt,temp_dt_1,by="URL")

pivoted = melt(songSummaries5Regions,id.vars = c("mean","URL","Region"),
               value.name = "value")

ggplot(pivoted)+geom_point(aes(x=value,y=mean,color=Region))+
  facet_wrap(~variable,scales = "free_x")

dtpivoted = data.table(pivoted)

ggplot(dtpivoted[!(variable %in% c("duration_ms","mean","tempo",
                                   "key","loudness","label","time_signature"))])+
  geom_boxplot(aes(x=variable,y=value,fill=Region))+
  theme(axis.text.x=element_text(angle=45, hjust=1))



