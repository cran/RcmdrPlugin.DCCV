\name{dccvPlot}
\alias{dccvPlot}

\title{Draw Survival Function}

\description{
Draw a survival function of the fitted model.
}

\details{
This dialog box draws a survival function of the fitted model using 
function \code{\link[DCchoice]{plot.sbchoice}} or 
\code{\link[DCchoice]{plot.dbchoice}} in \pkg{DCchoice} package. 
This dialog box is activated if the fitted model 
in the \sQuote{\code{Fit Model to DCCV Data}} dialog box is selected 
as the active model. 
For details on the results of this dialog box, refer to the help content 
for function \code{\link[DCchoice]{plot.sbchoice}} or 
\code{\link[DCchoice]{plot.dbchoice}}. 

GUI elements of this dialog box are as follows.

\bold{Title and labels (optional)}

\itemize{
\item \code{Title}: Main title of the plot. 
No main title is displayed if missing (default).

\item \code{x-axis}: A label of the x-axis of the plot. 
A label \sQuote{\code{Bid}} is displayed if missing (default).

\item \code{y-axis}: A label of the y-axis of the plot. 
A label \sQuote{\code{Probability of selecting yes}} is displayed 
if missing (default).
}

\bold{Ranges (optional)}
\itemize{
\item \code{x-axis}: The x limits of the plot. The lower limit and 
the upper limit are specified in the \sQuote{\code{from}} box and 
the \sQuote{\code{to}} box, respectively.

\item \code{y-axis}: The y limits of the plot. The lower limit and 
the upper limit are specified in the \sQuote{\code{from}} box and 
the \sQuote{\code{to}} box, respectively.

\item \code{Bid from} and \code{to}: The ranges of bids to be drawn 
on the plot. The lower bound and the upper bound are specified in 
the \sQuote{\code{from}} box and the \sQuote{\code{to}} box, 
respectively. The minimum and maximum bid values in the data set are
used as the default lower and upper bounds.
}
}


