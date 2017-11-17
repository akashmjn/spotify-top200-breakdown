'''
This script generates a csv file with all song related features for all songs in the dataset
'''
import csv
import subprocess
import json

# Make a list of unique songs that need to be queried
unique_songs = {}
with open("data.csv", "rb") as csv_file:
	reader = csv.reader(csv_file)
	for line in reader:
		if line[1] not in unique_songs:
			track_id = line[4].split("/")[-1]
			unique_songs[line[1]] = track_id

#Access token provided by Spotify
access_token = "BQD0iWaC8QZT2k5_8NBqKwlAIsYK4ZcMCtcEB9FCbGWvZKh8wnvrfdfnk-Y9QLVGK1gOGURaIKyEOvIwkKwPvdNJnlFHLqcOWjOBo-fe-4jTy8HwHcgPMqDSdhmVgXyoFHmR1wyIb6J4Zu-idNs"

# Use the song ids stored in unique_songs and get features of the songs. Keep adding to a csv file
all_feats =[]
num_songs = len(unique_songs.values())
# num_songs = 200
with open("song_feats.csv", "wb") as f:
	spamwriter = csv.writer(f, delimiter=',',
                            quotechar='|', quoting=csv.QUOTE_MINIMAL)
	spamwriter.writerow(["URL","energy","liveness","tempo","speechiness","acousticness","instrumentalness","time_signature","danceability","key","duration_ms","loudness","valence"])
	for i in xrange(0,num_songs,100):
		songs = unique_songs.values()[i:i+100]
		songs = ",".join(songs)
		output = subprocess.Popen(["curl", "-X", "GET", "https://api.spotify.com/v1/audio-features/?ids="+songs, "-H", "Authorization: "+"Bearer "+access_token+"\""], stdout=subprocess.PIPE, stderr=subprocess.PIPE)
		output = output.stdout.read()
		try:
			json_output = json.loads(output)
			final = json_output["audio_features"]
			for entry in final:
				print entry
				csv_entry = []
				csv_entry.append("https://open.spotify.com/track/"+entry["id"])
				csv_entry.append(entry["energy"])
				csv_entry.append(entry["liveness"])
				csv_entry.append(entry["tempo"])
				csv_entry.append(entry["speechiness"])
				csv_entry.append(entry["acousticness"])
				csv_entry.append(entry["instrumentalness"])
				csv_entry.append(entry["time_signature"])
				csv_entry.append(entry["danceability"])
				csv_entry.append(entry["key"])
				csv_entry.append(entry["duration_ms"])
				csv_entry.append(entry["loudness"])
				csv_entry.append(entry["valence"])
				spamwriter.writerow(csv_entry)
		except:
			pass

# with open("song_feats.json", "wb") as f:
# 	json.dump(all_feats, f)

# with open("song_feats.json", "rb") as f:
# 	contents = json.loads(f)
# 	for entry in contents:
# 		print entry


# songs = ["1CUVN2kn7mW5FjkqXTR2W1","0sOpGWXWkxNDev51Dst3fQ"]
# songs = ",".join(songs)

# data = subprocess.Popen(["curl", "-X", "GET", "https://api.spotify.com/v1/audio-features/?ids="+songs, "-H", "Authorization: "+"Bearer "+access_token+"\""], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

# print "here: "
# print data.stdout.read()