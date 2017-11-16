
# Filter and return top N regions 
getTopNRegions <- function(dt,N){
  # Mean of streams by region
  dailyStreamsByRegion = dt[,.(MeanStreams=mean(Streams)),by=Region]
  # filter by region - only top 20 regions
  topNRegions = dailyStreamsByRegion[order(-MeanStreams)][1:(N+1)]
  dtFiltered = merge(dt,topNRegions,by = "Region")
  return(dtFiltered)
}

# Filter only tracks that have been in the top N 
# charts by region. N=200 will just return the entire dataset
# Returns: list( dtFiltered, topNTracksStats )
getTopNTracks <- function(dt,N){
  # filter tracks getting <= N ranking at some point
  # groupby TrackName, Region gives a count of dates 
  topNTracksByRegion = dt[Position<=N][,
                               .(DaysInTopN=.N),
                               by=.(TrackName,Region)]
  topNTracks = topNTracksByRegion[,.(NumberOfRegions=.N),by=TrackName]
  # join and filter only tracks in this topN list
  dtFiltered = merge(dt,topNTracks,by="TrackName")
  return(list(dtFiltered,topNTracksByRegion))
}

# filter and only keep tracks that have made a rise in the charts
# i.e. min Position of track is less that threshold
filterRisenTracks <- function(dt,minPosition){
  
}