\name{dccvCollectResponse}
\alias{dccvCollectResponse}
\alias{dccvCollectResponseSet}

\title{Collect Responses to DCCV Questions}

\description{
Collect user's responses to DCCV questions.
}

\details{
Three types of dialog boxes are provided to collect user's responses to
DCCV questions.

The first dialog box (\sQuote{\code{Set Options for Response Collection}})
sets four options for response collections:

\itemize{
\item \code{Import bid table}: Press the button to import a CSV file 
containing bids to Rcmdr. 

\item \code{Currency}: Select a currency for bids.

\item \code{Bid id}: Select a bid/bid-combination used for the questions. 
When selecting \sQuote{\code{Random}}, a bid/bid-combination is selected randomly.

\item \code{Save to file}: When the user's responses should be saved
to an external file in CSV format (\code{.csv}), check this box.
}

After confirming all the settings on the first dialog box, press
the \sQuote{\code{OK}} button to continue.

A series of dialog boxes (\sQuote{\code{Collect Responses to DCCV Questions}})
sequentially opens to collect the user's responses to DCCV questions 
(For SB-DCCV and some cases of OOHB-DCCV, a single dialog box opens).
The user is asked to select \sQuote{\code{Yes}} or \sQuote{\code{No}} 
from the drop-down list box. After making the selection,
the user clicks the \sQuote{\code{OK}} button to open a new dialog box
corresponding to the next DCCV question.

When all user responses to DCCV questions have been collected, a dialog
box (\sQuote{\code{Save As}}) opens if the \sQuote{\code{Save to file}}
option was selected in the first dialog box. This saves the user's
responses to an external file in CSV format.
}
