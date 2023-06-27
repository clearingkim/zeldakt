alpha_pred_dist<-function(data,startyear,endyear){
  years = startyear:endyear
  actor_year_df = NULL
  actors_list=unique(c(unique(data$ACTOR1),unique(data$ACTOR2)))
  
  for(ii in 1:length(actors_list)){
    for(yy in 1:length(years)){
      tmp = data.frame(year = years[yy],
                       date = as.Date(paste0(years[yy],"-12-01")),
                       actor = actors_list[ii])
      actor_year_df = bind_rows(actor_year_df, tmp);rm(tmp)
    }
  }
  actor_year_df$lat.pred = NA
  actor_year_df$long.pred= NA
  result_out = NULL
  
  for(yy in 1:length(years) ){ 
    
    # estimate the best alpha in t-1
    #startValue = 0.5
    
    # load the estimated parameter
    alpha_value = 0.5
    # load data + observations (up til t-1)
    data_t = data %>% filter(YEAR <= years[yy] -1 ) %>% 
      dplyr::select(ccode = YEAR, EVENT_DATE, ACTOR1, ACTOR2,LATITUDE, LONGITUDE) 
    
    # loop actors
    for(ii in 1:length(actors_list)){ 
      
      if( nrow(data_t)>0 ){
        
        focal_actor = actors_list[ii]
        
        # predict actor location on December 1st every year
        focal_time_fixedPredict = as.Date(paste0(years[yy],"-12-01")) 
        
        # find focal_actor's partners
        partners <- unique(c(data_t$ACTOR1[data_t$ACTOR2==focal_actor],data_t$ACTOR2[data_t$ACTOR1==focal_actor] ) )
        
        # find focal actor's x-location history
        focal_location_history_x <- data_t$LATITUDE[c(which(data_t$ACTOR1==focal_actor),which(data_t$ACTOR2==focal_actor) )] 
        
        # find focal actor's y-location history
        focal_location_history_y <- data_t$LONGITUDE[c(which(data_t$ACTOR1==focal_actor),which(data_t$ACTOR2==focal_actor) )] 
        
        # find focal actor's time history 
        focal_ages <- focal_time_fixedPredict - data_t$EVENT_DATE[c(which(data_t$ACTOR1==focal_actor),which(data_t$ACTOR2==focal_actor) )]
        
        # (partner's) find focal's alter x history and alter's time history
        alter_history_x <- NULL 
        alter_history_y <- NULL
        alter_ages <- NULL
        
        # loop over alters
        for(a in partners){
          
          # find alter actor's x-location history
          alter_history_x <- c(alter_history_x, data_t$LATITUDE[c(which(data_t$ACTOR1==a),which(data_t$ACTOR2==a) )])
          
          # find alter actor's y-location history
          alter_history_y <- c(alter_history_y,data_t$LONGITUDE[c(which(data_t$ACTOR1==a),which(data_t$ACTOR2==a) )])
          
          # find alter actor's time history
          alter_ages <- c(alter_ages, focal_time_fixedPredict - data_t$EVENT_DATE[c(which(data_t$ACTOR1==a),which(data_t$ACTOR2==a) )])
        }
        
        gamma <- 0
        eta <- 0
        alpha <- exp(alpha_value)
        beta <- 0
        
        focal_ages <- as.numeric(focal_ages)
        alter_ages <- as.numeric(alter_ages)
        
        weights_i <- 1/(focal_ages^alpha+.01)
        weights_i <- weights_i/sum(weights_i)
        weights_k <- 1/(alter_ages^beta+.01)
        weights_k <- 1/(sum(weights_k))
        
        length(focal_ages)/length(alter_ages)
        a <- length(focal_ages)
        e <- length(alter_ages)
        
        relev <- (a/e)^(1/sqrt(e))
        
        pi <- plogis(gamma + eta*relev) 
        pi = 0 # force it to only consider focal's info
        
        projected_focal_x = (1-pi)*sum(weights_i*focal_location_history_x) + pi*sum(alter_history_x*weights_k)
        projected_focal_y = (1-pi)*sum(weights_i*focal_location_history_y) + pi*sum(alter_history_y*weights_k)
        
        # save the projected location in t (actor-year)
        actor_year_df$lat.pred[actor_year_df$actor == focal_actor & actor_year_df$year == (years[yy]) ] <- projected_focal_x
        actor_year_df$long.pred[actor_year_df$actor == focal_actor & actor_year_df$year == (years[yy]) ] <- projected_focal_y
        
        rm(weights_i, weights_k, focal_location_history_x, focal_location_history_y)
        
      } # if any condition
      
    } # ii actor loop
    cat("\r",yy)
    flush.console()
    
  } # end of year loop
  
  # fill in the unpredicted with NA
  actor_year_df$lat.pred[actor_year_df$lat.pred == 0] <- NA
  actor_year_df$long.pred[actor_year_df$long.pred == 0] <- NA
  
  # rename the df
  actorYearDF_alpha = actor_year_df
  return(actorYearDF_alpha)
}