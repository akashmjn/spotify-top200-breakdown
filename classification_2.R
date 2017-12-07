# split by number of regions that a song was in the top 20

akash_data = read.csv("data_t20regions.csv")

confusion_matrix = function(fitted.results, new_dt){
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
}

x = 20
unique_songs = akash_data %>% group_by(URL) %>% filter(Position %in% c(1:x)) %>% summarise(num_unique = n_distinct(Region)) %>% arrange(desc(num_unique))

top_track_URLS = (unique_songs %>% filter(num_unique > 4))$URL

#bottom_track_URLS = (unique_songs %>% filter(num_unique == 1))$URL
bottom_track_URLS = (song_feats %>% filter(!(URL %in% unique_songs$URL)))$URL
#bottom_track_URLS = sample(bottom_track_URLS, length(top_track_URLS))

percent_test = 0.2
top_test_URLS = sample(top_track_URLS, percent_test*length(top_track_URLS))
top_train_URLS = top_track_URLS[!top_track_URLS %in% top_test_URLS]

bottom_test_URLS = sample(bottom_track_URLS, length(top_test_URLS))
bottom_train_URLS = bottom_track_URLS[!bottom_track_URLS %in% bottom_test_URLS]
bottom_train_URLS = sample(bottom_train_URLS, length(top_train_URLS))

# Create train and test set
train_data = song_feats %>% filter((URL %in% top_train_URLS) | (URL %in% bottom_train_URLS))
train_data = transform(train_data, label = if_else(URL %in% top_train_URLS, 1, 0))

test_data = song_feats %>% filter((URL %in% top_test_URLS) | (URL %in% bottom_test_URLS))
test_data = transform(test_data, label = if_else(URL %in% top_test_URLS, 1, 0))

classification = glm(label ~ energy + liveness + tempo + speechiness + acousticness + instrumentalness + danceability + loudness + valence, data = train_data, family = binomial)
to_predict_train = train_data[,c("energy","liveness","tempo","speechiness","acousticness","instrumentalness","danceability","loudness", "valence")]
to_predict_test = test_data[,c("energy","liveness","tempo","speechiness","acousticness","instrumentalness","danceability","loudness", "valence")]

# Train model and predict on train and test set
fitted.results.train <- predict(classification, newdata=to_predict_train, type='response')
fitted.results.train <- ifelse(fitted.results.train > 0.5,1,0)

misClasificError.train <- mean(fitted.results.train != train_data$label)
print(paste('Training Accuracy',1-misClasificError.train))

fitted.results.test <- predict(classification, newdata=to_predict_test, type='response')
fitted.results.test <- ifelse(fitted.results.test > 0.5,1,0)

misClasificError.test <- mean(fitted.results.test != test_data$label)
print(paste('Testing Accuracy',1-misClasificError.test))


# Print confusion matrix
print("Training")
confusion_matrix(fitted.results.train, train_data)

print("Testing")
confusion_matrix(fitted.results.test, test_data)

temp_dt_1 = dt %>% group_by(URL) %>% summarise(mean = mean(Streams))

songSummaries5Regions = merge(new_dt,temp_dt_1,by="URL")

pivoted = melt(songSummaries5Regions,id.vars = c("mean","URL"),
               value.name = "value")

ggplot(pivoted)+geom_point(aes(x=value,y=mean))+
  facet_wrap(~variable,scales = "free_x")
