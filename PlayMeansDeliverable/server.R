library(shiny)
library(spotifyr)
library(DT)
library(htmltools)
library(plyr)
library(stringr)
library(cluster)

Login = function(client, secret){
  # Populate with own credientials, return access_token
  # If blank, log in with Liam's credentials.
  # For production, remove blank login

  if (client == ''){
    Sys.setenv(SPOTIFY_CLIENT_ID = "f4d35cf0a8014ae4bcc98a755f98b7b4")
    Sys.setenv(SPOTIFY_CLIENT_SECRET = "31d309abeb4b4b568e0f3d6876fcb723")
  }
  else{
  Sys.setenv(SPOTIFY_CLIENT_ID = client)
  Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
  }
  
  access_token = get_spotify_access_token()
  
  return(access_token)
}


songs_by_artist = function(band){
  # Given a band name, returns a dataframe:
  # Track_Name, Columns of Audio Features:
  # 4th row comprises interesting categorical data
  
  columns <- c("track_name","energy", "duration_ms","acousticness",
               "danceability","tempo","speechiness",
               "liveness","loudness","valence", "instrumentalness",
               "mode","time_signature", "key", "track_preview_url")
  
  # Call API and select columns
  print("Calling API...")
  groups = c("album", "single")
  response <- get_artist_audio_features(band, include_groups = groups, dedupe_albums = TRUE)
  response = response[,columns]
  
  # Remove Duplicates (filter by track_name and duration_ms to avoid floating point comparisons, but not exclude non-duplicate live versions)
  response = response[!duplicated(response[c(1,3)]),]
  response = response[!(str_detect(tolower(response$track_name), "remix")),]
  
  # Remove missin values (NA for track_name)
  response = response[!(is.na(response$track_name)),]
  
  # Removes explicit remixes, but:
  # Some songs labelled as edit/mix/etc...
  # This could clash with song names like "Gronlandic Edit" - Of Montral or "Mixtape" - Chance the Rapper
  
  return(response[,columns])
}


stackedBar = function(mydata, clustNum, catVar){
  # Code by Davit Khachatryan
  # Adapted for use here by Liam Earley
  
  for.stacked.bar=table(mydata[,catVar], mydata[,clustNum])
  
  totalc<-colSums(for.stacked.bar)
  propt<-for.stacked.bar
  for (i in 1:ncol(for.stacked.bar))
  {propt[,i]<-for.stacked.bar[,i]/totalc[i]}
  
  return(renderPlot({
    par(bg="white")
    plot(3, 3, type="n", ann=FALSE, axes=FALSE)
    u <- par("usr") # The coordinates of the plot area
    rect(u[1], u[3], u[2], u[4], col="white", border=NA)
    par(new=TRUE)
    
    
    stat<-barplot(propt, main=paste("Distribution of", catVar, "by cluster"),
                  ylab="Proportion",
                  xlab="Cluster #",
                  col=terrain.colors(length(unique(mydata[,catVar]))),
                  ylim=c(0,1.1),xlim=c(0,9),cex.axis=1.2,col.axis="darkslategray",
                  cex.lab=1.2,col.lab="darkslategray",
                  legend = rownames(for.stacked.bar))
    text(x = stat, y = rep(1.05,6), label = totalc,
         col = "darkslategray")
  }))
}

run_kmeans = function(data, numclust, standardize=F, remove_outliers=F){
  # Runs K-means, returning a list of:
  # (Cluster Membership Vector, Cluster Centroid Plot, Categorical Plots)
  # K-Means and Centroid Plot code by Davit Khachatryan, adapted for use by Liam Earley
  
  # Load Data and remove Categorical Variables
  mydata = data
  mydata$track_name = as.factor(mydata$track_name)
  mydata$mode = as.factor(mydata$mode)
  mydata$time_signature = as.factor(mydata$time_signature)
  mydata$key = as.factor(mydata$key)
  
  # Designate Numeric Columns
  vars_to_cluster = colnames(num_data)
  cat_vars = colnames(cat_data)
  
  # Handle Standardization and Outlier Removal
  if (standardize){
    cols_for_standard = as.matrix(mydata[,vars_to_cluster])
    mydata_s = scale(cols_for_standard)
    mydata_s = as.data.frame(mydata_s)
    mydata_s = cbind(mydata_s, mydata[,cat_vars])
    
    mydata = mydata_s
  }
  
    if (remove_outliers) {
      std = scale(mydata[,colnames(num_data)])
      vals = rowSums(abs(std) < 3) == 10
      mydata = mydata[vals,]
    }
  
  # Set seed for consistent results
  set.seed(42)
  
  # Run K-means algorithm
  kmeans_clusters = kmeans(mydata[,vars_to_cluster], centers=numclust, nstart=25)

  for_summary.numclust=cbind(mydata,as.vector(kmeans_clusters$cluster))
  
  colnames(for_summary.numclust)[dim(for_summary.numclust)[2]]="cluster"
  
  # Generate Centroid Plot
  varnum=seq(1:length(vars_to_cluster))
  x=names(kmeans_clusters$centers[1,])
  ymin=min(kmeans_clusters$centers)
  ymax=max(kmeans_clusters$centers)
  
  CentroidPlot = renderPlot({
    par(mar=c(9.5,5,5,0.5))
    plot(varnum, kmeans_clusters$centers[1,], xlim=c(1,max(varnum)+1.5), type="l",lty=1, lwd=2,xaxt = "n", xlab="", main="Cluster Centroid Profile Plot", ylab="Cluster Centroids (Standardized)", ylim=c(ymin,ymax))
    axis(1, at = varnum, labels = x, las = 2, cex.axis = 0.6)
    for (i in 2:numclust){
      lines(varnum, kmeans_clusters$centers[i,], type="l", col=i, lwd=2,lty=i)
    }
    leg=c()
    for (i in 1:numclust){
      leg=c(leg,paste("Cluster",i))
    legend(list(x = max(varnum)+0.0001, y = ymax), leg, col=1:numclust, lty = 1:numclust, cex=0.8)
      
    }
    
  })
  
  # Breakdown of each cluster by each categorical variable
  CatPlots = lapply(colnames(cat_data),function(vname){
    # Using lapply, generates a stacked bar chart for each categorical variable
    # except track_name and returns a list of plots
    
    if (!(vname=="track_name")){
    return(stackedBar(for_summary.numclust, "cluster",vname))
    }
    })
  return (list(kmeans_clusters$cluster, list(CentroidPlot, unlist(CatPlots))))
  
  }
  

  play_track = function(track_name, track_preview_link){
    # Given track_name and preview link:
    # Generate HTML taglist to play link
    # Adapted from https://github.com/charlie86/sentify/blob/master/server.R

    if (!is.na(track_preview_link)) {
      outp = renderUI(tagList(
        tags$h2(track_name),
        tags$audio(id = 'song_preview', src = track_preview_link, type = 'audio/mp3', autoplay = NA, controls = NA),
        tags$script(JS("myAudio=document.getElementById('song_preview'); myAudio.play();"))
      ))
    } else {
      outp = renderUI(tagList(
        tags$h2(track_name),
        h5('No preview for this track on Spotify')))
    }
    return (outp)
  }
  
  get_bar_chart_track = function(mouse_event, sort_by, rmv_out=F){
    # Initial Data
    cluster_val = round(mouse_event$x, 0)
    y_val = mouse_event$y
    k_membership = get("k_membership", envir=globalenv())
    temp_data = song_data
    
    # Remove Outliers
    # Standardizing doesn't affect this, only need to remove outliers if we already are
    if (rmv_out) {
      std = scale(temp_data[,colnames(num_data)])
      vals = rowSums(abs(std) < 3) == 10
      temp_data = temp_data[vals,]
    }
    
    # Combine temp data and membership vector
    temp_data$k_membership = k_membership
    
    # Select only songs from the proper cluster
    temp_data = temp_data[temp_data$k_membership == cluster_val,]
    
    # Sort songs in ascending order by categorical integer label
    temp_data = temp_data[order(temp_data[,sort_by]),]
    
    # Get closest track to where the user clicked on the bar
    track_idx = round(y_val*nrow(temp_data))
    if (track_idx == 0){
      track_idx = 1
    }
    
    # Render Audio Player
    track_name = temp_data[track_idx, "track_name"]
    track_preview_url = temp_data[track_idx, "track_preview_url"]
    
    return (list(track_name, track_preview_url))
  }

  

# Server Function


function(input, output) {
  # Initial Output Values
  defaultmessage = renderText("No data, please login and search artist.")
  output$dist_page = defaultmessage
  output$k_page = defaultmessage
  output$k_select_page = renderText("Please run K-Means before exploring different values of K")
  output$kmeans_plots = renderUI({})
  output$silhouette_plots = renderUI({})
  output$hist_audio = renderUI({})
  output$kmeans_audio = renderUI({})
  
  # List which gets populated 1 by 1 via running k-means
  silhouette_plot_list = list(NULL, NULL, NULL, NULL)
  
  # Ensure audio players don't continue with old data
  reset_audio_players = function() {
    output$hist_audio = renderUI({})
    output$kmeans_audio = renderUI({})
  }

  
  # Login to Spotify Button
  # After successful Login, renders the Enter Artist and Get Songs UI elements
  observeEvent(input$send_creds, 
               tryCatch({
                 access_token = Login(input$client, input$secret)
                 assign("access_token", access_token, envir=.GlobalEnv)
                 output$login_status = renderText("Successfully Logged In")
                if (input$send_creds == 1){
                 insertUI(selector="#song_status",where="beforeBegin",
                          ui = textInput("band_name", label="Enter Artist", placeholder="Enter Artist"))
                 
                 insertUI(selector="#song_status",where="beforeBegin",
                          ui = actionButton("get_songs", label="Get Songs!"))
                  }
               },
               warning = function(w){
                 print(w)
               },
               error = function(e){
                 print("An Error Occured:")
                 print(e)
                 output$login_status = renderText("Login Failed")
               },
               finally = {
                
               }))
  
  
  # Handler for Get Songs Button
  # Calls API, gets+stores song data, and displays data in table in app
  # Stores cat_data, num_data, and song_data in globalenv to work with later
  observeEvent(input$get_songs,
   tryCatch({
     # Get data, store in vars
    song_data <- songs_by_artist(tolower(input$band_name))
    print(colnames(song_data))
    catcols = c("track_name","mode","time_signature","key")
    numcols = c("energy", "duration_ms","acousticness",
                "danceability","tempo","speechiness",
                "liveness","loudness","valence","instrumentalness")
    
    song_data[,"track_preview_url"] = as.character(song_data[,"track_preview_url"])
    assign("track_preview_url", song_data[,"track_preview_url"], envir=.GlobalEnv)
    
    assign("cat_data", song_data[,catcols], envir=.GlobalEnv)
    assign("num_data", song_data[,numcols], envir=.GlobalEnv)
    
    song_data = cbind(cat_data, num_data)
    song_data["track_preview_url"] = get("track_preview_url", globalenv())
    assign("song_data", song_data, envir=.GlobalEnv)
    
    # Make sure to remove any previous Silhouette Plots and Audio
    assign("silhouette_plot_list", list(), envir=.GlobalEnv)
    reset_audio_players()
  
    # Render Output Table
    datacontainer = htmltools::withTags(table(
      class='display',
      thead(tr(
        th('#', title="#"),
        th('Track Name', title="Name of the Song"),
        th('Mode', title="Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0."),
        th('Time Signature', title="An estimated overall time signature of a track. The time signature (meter) is a notational convention to specify how many beats are in each bar (or measure)."),
        th('Key', title="The estimated overall key of the track. Integers map to pitches using standard Pitch Class notation. E.g. 0 = C, 1 = C sharp, and so on. If no key was detected, the value is -1."),
        th('Energy', title="Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy. For example, death metal has high energy, while a Bach prelude scores low on the scale. Perceptual features contributing to this attribute include dynamic range, perceived loudness, timbre, onset rate, and general entropy."),
        th('Duration_ms', title="The duration of the track in milliseconds."),
        th('Acousticness', title='A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic.'),
        th('Danceability', title="Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
        th('Tempo', title="The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."),
        th('Speechiness', title="Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks."),
        th('Liveness', title='Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live.'),
        th('Loudness', title="The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks. Loudness is the quality of a sound that is the primary psychological correlate of physical strength (amplitude). Values typical range between -60 and 0 db."),
        th('Valence', title="	A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."),
        th('Instrumentalness', title="Predicts whether a track contains no vocals. 'Ooh' and 'aah' sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly 'vocal'. The closer the instrumentalness value is to 1, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.")
      ))))
    
    output$song_data <- DT::renderDataTable({
      datatable(song_data[,!(names(song_data) %in% c("track_preview_url"))], 
                options = list(pageLength = 10, dom="tp"),
                container = datacontainer,
                ) %>% formatStyle(columns = 0:16, backgroundColor = "white")
    })
    
    # Update Text below "Get Artist"
    output$song_status = renderText("Artist Found!")
    
    # Generate UI for Distributional Analysis Page
    output$dist_page = renderUI(
      fluidPage(
      # Title
      titlePanel("Distributional Analysis"),
      
      # Sidebar
      sidebarLayout(
        sidebarPanel(radioButtons(inputId="dist_selected",
                                  label="Variables to Plot",
                                  choices=c("none",
                                            "acousticness",
                                            "danceability",
                                            "duration_ms",
                                            "energy",
                                            "instrumentalness",
                                            "loudness",
                                            "liveness",
                                            "speechiness",
                                            "tempo",
                                            "valence")),
                     uiOutput("hist_audio")
                     ),
        
        # Histograms
        mainPanel(
          uiOutput("dist_hists")
        )
      )
    )
    )
    
    # Generate K-Means Page
    output$k_page = renderUI(
      fluidPage(
        # Title
        titlePanel("K-Means"),
        
        # Sidebar
        sidebarLayout(
          sidebarPanel(
            checkboxInput("standardize",
                          label="Standardize Variables?"),
            checkboxInput("remove_outliers",
                          label="Remove Outliers?"),
            
            sliderInput("k_clusters",
                        label="Number of Clusters (k):",
                        min=2,
                        max=10,
                        value=1,
                        step=1,
                        round=TRUE),
            
            actionButton("run_kmeans",
                         label="Run K-Means"),
            uiOutput("kmeans_audio")
                    ),
          
          # Plots
          mainPanel(
            plotOutput("centroid_plot", click="centroid_click"),
            plotOutput("Bar1", click="bar1_click"),
            plotOutput("Bar2", click="bar2_click"),
            plotOutput("Bar3", click="bar3_click")
          )
        )
      )
    )
    
   },
  warning = function(w){
    print(w)
    output$song_status = renderText("The artist was not found. Please make another search.")
  },
  error = function(e){
    print("An Error has Occurred:")
    print(e)
    output$song_status = renderText("The artist was not found. Please make another search.")
  },
  finally = function(){
    
  })
  )
  
  # Handles Distributional Analysis Page
  # input = list of selected variables
  # output dist_plot as plot to render
  observeEvent(input$dist_selected,
               {
                 if (!("none" %in% input$dist_selected)){
                   for (name in input$dist_selected){
                     data = as.numeric(song_data[,name])
                     histogram = hist(data, main=name, xlab=name)
                     output$histogram = renderPlot({plot(histogram,
                                                  main=paste("Histogram of", name),
                                                  xlab=name)
                                              abline(v=mean(data),
                                                          lty=6,
                                                          col="red")
                                              legend("topright", 
                                                     legend=paste("mean", round(mean(data), 3)),
                                                     col="red",
                                                     lty=6)})
                   }
                   output$dist_hists = renderUI(plotOutput("histogram", click="hist_click")
                     )
                 }
                 else {
                   output$dist_hists = renderUI({})
                 }
               }
               )
  
  # K-Means Page 
  observeEvent(input$run_kmeans, {
    # Reset audio
    reset_audio_players()
    
    # Casting input variables to new variables prevents auto-updating, so the button works as intended
    k_clust = input$k_clusters
    stnd = input$standardize
    rmv_out = input$remove_outliers
    
    # returns 1d list (cluster membership, centroid plot, cat plots)
    kmeans_output = run_kmeans(song_data,k_clust,stnd,rmv_out)
    
    # Unpack kmeans_output + update kmeans UI
    membership = kmeans_output[1]
    plots = kmeans_output[2:length(kmeans_output)]
    assign("plots", plots, envir=globalenv())
    
    output$kmeans_plots = renderUI({list(
      plotOutput("centroid_plot", click="centroid_click"),
      plotOutput("Bar1", click="bar1_click"),
      plotOutput("Bar2", click="bar2_click"),
      plotOutput("Bar3", click="bar3_click")
    )})
    
    # Unpack list returned by run_kmeans
    output$centroid_plot = plots[[c(1, 1)]]
    output$Bar1 = plots[[c(1, 2, 1)]]
    output$Bar2 = plots[[c(1, 2, 2)]]
    output$Bar3 = plots[[c(1, 2, 3)]]
    

    
    # Generate Silhouette Plot and pass to UI
    silhouette_plot = function(k_membership){
      nd = num_data
      k_membership = unlist(k_membership)
      assign("k_membership", k_membership, globalenv())
      
      # Standardizatoin and Outliers
      if(stnd){
        nd = scale(nd)
      }
      if (rmv_out) {
        std = scale(nd[,colnames(num_data)])
        vals = rowSums(abs(std) < 3) == 10
        nd = nd[vals,]
      }
      
      distance = dist(nd)
      sil = cluster::silhouette(k_membership, dist=distance)
      plot(cluster::silhouette(k_membership, dist=distance), border=NA, main=paste("Silhouette for k =", k_clust))
      return (renderPlot({
        plot(cluster::silhouette(k_membership, dist=distance), border=NA, main=paste("Silhouette for k =", k_clust))
        abline(v=mean(sil[,"sil_width"]))
        }))
    }
    
    # Generate Plot, Retrieve Cached Plots, Create new list, update Cache
    new_silhouette_plot = silhouette_plot(membership)
    splist = get("silhouette_plot_list", globalenv())
    splist[4] = splist[3]
    splist[3] = splist[2]
    splist[2] = splist[1]
    splist[1] = list(new_silhouette_plot)
    splist = splist[1:4]
    assign("silhouette_plot_list", splist, .GlobalEnv)
    

    # Populate K-Selection Page (k_select_page)
    output$k_select_page = renderUI({
      fluidPage(
        fluidRow(
          column(12, new_silhouette_plot)),
        fluidRow(
          column(4, get("silhouette_plot_list", globalenv())[2]),
          column(4, get("silhouette_plot_list", globalenv())[3]),
          column(4, get("silhouette_plot_list", globalenv())[4])
        ))})
    print("from global:")
    print(get("silhouette_plot_list", globalenv()))
  })
  
  observeEvent(input$hist_click,{
    # 1 Get X Coordinate
    # 2 Since X is in feature Space, find closest track
    # 5 Play Snippet of Song (need to go back and grab links to spotify songs)
    
    # Find Closest Song
    num_var = input$dist_selected
    x_val = input$hist_click$x
    temp_data = song_data[,c("track_name", num_var, "track_preview_url")]
    closest = which.min(abs(temp_data[,num_var] - x_val))

    # Render with data from Closest Value
    track_link = temp_data[closest,"track_preview_url"]
    track_name = temp_data[closest,"track_name"]
    output$hist_audio = play_track(track_name, track_link)
    
    })
  
  
  # Bar Chart on-click Events
  # 1 for each categorical plot
  observeEvent(input$bar1_click,{
    # Retrieve closest track
    tr = get_bar_chart_track(input$bar1_click, "mode", input$remove_outliers)
    
    # Render+Play Audio
    track_name = tr[1]
    track_preview_url = tr[2]
    output$kmeans_audio = play_track(track_name, track_preview_url)
  })
  

  observeEvent(input$bar2_click,{
    
    tr = get_bar_chart_track(input$bar2_click, "time_signature", input$remove_outliers)
    track_name = tr[1]
    track_preview_url = tr[2]
    
    output$kmeans_audio = play_track(track_name, track_preview_url)
  })
  
  
  observeEvent(input$bar3_click,{
    
    tr = get_bar_chart_track(input$bar3_click, "key", input$remove_outliers)
    track_name = tr[1]
    track_preview_url = tr[2]
    
    output$kmeans_audio = play_track(track_name, track_preview_url)
  })
}

  
  