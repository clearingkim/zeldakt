#' Predicted Distance Function(4para)
#'
#' This function allows you to produce the predicted distance (4para) of moving actors in your dataset
#' @param data 'data' should be a data object with following variables: YEAR (the year of event), EVENT_DATE (the date of event),
#' ACTOR1 (focal actor name), ACTOR2 (alter actor name), LATITUDE (latitude), LONGITUDE (logitude). Note: the variable names should be EXACT.
#' @param startyear The first year you want to produce the predicted distance. It should be later than the earliest year of your data.
#' @param endyear The last year you want to produce the predicted distance. It should be earlier than the lastest year of your data.
#' @keywords practice
#' @export
#' @examples
#' data(acled)
#' data<-acled
#' pred_four<-four_pred_dist(data,1999,2000)

four_pred_dist<-function(data,startyear,endyear){
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

    # load the estimated parameter
    gamma_eta_alpha_beta = c(-10,-10,-0.5,-5)
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

      # Find focal actor's y-location history
      focal_location_history_y <- data_t$LONGITUDE[c(which(data_t$ACTOR1==focal_actor),which(data_t$ACTOR2==focal_actor) )]
      # focal_location_history_y

      # find focal actor's time history
      focal_ages <- focal_time_fixedPredict - data_t$EVENT_DATE[c(which(data_t$ACTOR1==focal_actor),which(data_t$ACTOR2==focal_actor) )]
      # focal_ages

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

      gamma <- gamma_eta_alpha_beta[1]
      eta <- gamma_eta_alpha_beta[2]
      alpha <- exp(gamma_eta_alpha_beta[3])
      beta <- exp(gamma_eta_alpha_beta[4])

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

      pi <- plogis(gamma + eta*relev) # NaN
      # pi = 0 # force it to only consider focal's info

      projected_focal_x = (1-pi)*sum(weights_i*focal_location_history_x) + pi*sum(alter_history_x*weights_k)
      projected_focal_x
      projected_focal_y = (1-pi)*sum(weights_i*focal_location_history_y) + pi*sum(alter_history_y*weights_k)
      projected_focal_y

      # save the projected location in t (actor-year)
      actor_year_df$lat.pred[actor_year_df$actor == focal_actor & actor_year_df$year == (years[yy]) ] <- projected_focal_x
      actor_year_df$long.pred[actor_year_df$actor == focal_actor & actor_year_df$year == (years[yy]) ] <- projected_focal_y

      rm(weights_i, weights_k, focal_location_history_x, focal_location_history_y)

    }

  }

  cat("\r",yy)
  flush.console()
} # end of year loop

# fill in the unpredicted with NA
actor_year_df$lat.pred[actor_year_df$lat.pred == 0] <- NA
actor_year_df$long.pred[actor_year_df$long.pred == 0] <- NA
actor_year_df$lat.pred[is.nan(actor_year_df$lat.pred)] <- NA
actor_year_df$long.pred[is.nan(actor_year_df$long.pred)] <- NA

# rename the df
actorYearDF_4para = actor_year_df

# remove unused stuff
rm(data_t, actor_year_df)
return(actorYearDF_4para)


}

