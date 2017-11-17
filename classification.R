# Number of unique songs in the top x 

x = 20
unique_top_20 = dt %>% group_by(Region) %>% filter(Position %in% c(1:x)) %>% 
  summarise(Total_Unique = n_distinct(Track.Name))

# Doing a trends analysis for the song "Chantaje" 
time_series = dt %>% group_by(Track.Name, Region) %>% filter(Track.Name == "Chantaje")
top_regions = (time_series %>% group_by(Region) %>% 
                 summarise(total = n()) %>% filter(total == max(total)))$Region

time_series %>% filter(Region %in% top_regions) %>% 
  ggplot() + geom_point(mapping = aes(x = Date, y = Position, color = Region))

# Make new dataset with song related features
song_feats = read_csv("song_feats.csv")
merged = merge(dt, song_feats, by="URL")
merged$label = cut(merged$Position, breaks=c(0,20,200), labels=c(1,0))

# Run logistic regression
classification = glm(label ~ energy + liveness + tempo + speechiness + acousticness + instrumentalness + danceability + loudness, data = merged, family = binomial)