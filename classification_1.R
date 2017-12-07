# split based on mean position in the "US" region

mean_positions = dt %>% filter(Region == "us") %>% group_by(URL) %>% summarise(Mean_position = mean(Position))
top_track_URLS = (mean_positions %>% filter(Mean_position <= 100))$URL

bottom_track_URLS = (mean_positions %>% filter(Mean_position > 100))$URL
bottom_track_URLS = sample(bottom_track_URLS, length(top_track_URLS))

new_dt = song_feats %>% filter((URL %in% top_track_URLS) | (URL %in% bottom_track_URLS))
new_dt = transform(new_dt, label = if_else(URL %in% top_track_URLS, 1, 0))

classification = glm(label ~ energy + liveness + tempo + speechiness + acousticness + instrumentalness + danceability + loudness + valence, data = new_dt, family = binomial)
to_predict = new_dt[,c("energy","liveness","tempo","speechiness","acousticness","instrumentalness","danceability","loudness", "valence")]
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




temp_dt_1 = dt %>% group_by(URL) %>% summarise(mean = mean(Streams))

songSummaries5Regions = merge(new_dt,temp_dt_1,by="URL")

pivoted = melt(songSummaries5Regions,id.vars = c("mean","URL"),
               value.name = "value")

ggplot(pivoted)+geom_point(aes(x=value,y=mean))+
  facet_wrap(~variable,scales = "free_x")
