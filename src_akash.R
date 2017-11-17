
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
                               by=.(URL,Region)]
  topNTracks = topNTracksByRegion[,.(NumberOfRegions=.N),by=URL]
  # join and filter only tracks in this topN list
  dtFiltered = merge(dt,topNTracks,by="URL")
  return(list(dtFiltered,topNTracksByRegion))
}

# filter valid time series 
# Returns: list( dtFiltered, dtTSDurations )
filterValidTS <- function(dt,minDaysThresh){
  # pulling out time series with minimum days present 
  dtTSDurations = dt[,.(DaysInCharts=.N),by=.(URL,Region)][DaysInCharts>=minDaysThresh]
  # giving each time series and ID
  dtTSDurations[,TSID:=.I]
  # joining and filtering back on original dataset
  dtFiltered = merge(dt,dtTSDurations[,.(TSID,Region,URL)],by = c("URL","Region"))
  return(list(dtFiltered,dtTSDurations))
}

