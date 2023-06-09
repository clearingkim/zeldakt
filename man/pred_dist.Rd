% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pred_dist.R
\name{pred_dist}
\alias{pred_dist}
\title{Predicted Distance Function(4para)}
\usage{
pred_dist(data, startyear, endyear, gamma, eta, alpha, beta)
}
\arguments{
\item{data}{'data' should be a data object with following variables: YEAR (the year of event), EVENT_DATE (the date of event),
ACTOR1 (focal actor name), ACTOR2 (alter actor name), LATITUDE (latitude), LONGITUDE (logitude). Note: the variable names should be EXACT.}

\item{startyear}{The first year you want to produce the predicted distance. It should be later than the earliest year of your data.}

\item{endyear}{The last year you want to produce the predicted distance. It should be earlier than the lastest year of your data.}

\item{gamma}{Input gamma value}

\item{eta}{Input eta value}

\item{alpha}{Input alpha value}

\item{beta}{Input beta value}
}
\description{
This function allows you to produce the predicted distance (4para) of moving actors in your dataset
}
\examples{
#four para model
data(acled)
data<-acled
pred_four<-pred_dist(data,1999,2000,-10,-10,-0.5,-5)
#alpha only model
pred_alpha<-pred_dist(data,1999,2000,0,0,-0.5,0)
#NA would be generated if there is no location history of an actor until the endyear you set up
}
\keyword{practice}
