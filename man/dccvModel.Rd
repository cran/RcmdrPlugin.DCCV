\name{dccvModel}
\alias{dccvModel}
\alias{resetDccvModel}
\alias{dccvModelP}

\title{Fit Model to DCCV Data}

\description{
Fit a model to DCCV data.
}

\details{
This dialog box implements a parametric analysis of DCCV data using 
function
\code{\link[DCchoice]{sbchoice}} (single-bounded DCCV), 
\code{\link[DCchoice]{oohbchoice}} (one-and-one-half-bounded DCCV), or 
\code{\link[DCchoice]{dbchoice}} (double-bounded DCCV) 
in package \pkg{DCchoice}. 
This dialog box is activated if a data frame is selected as the active
data set.
For details on the results of this dialog box, refer to the help content
for functions 
\code{\link[DCchoice]{sbchoice}}, 
\code{\link[DCchoice]{oohbchoice}}, and 
\code{\link[DCchoice]{dbchoice}}.

GUI elements of this dialog box are as follows.

\itemize{
\item \code{Name for model}: An R object name into which the results 
of a fitted model obtained through this dialog box are stored. 
The default name \sQuote{\code{DCCVmodel.x}}, where \code{x} indicates
the integer number assigned automatically from \code{1}, can be
changed following R's rules for naming objects.

\item \code{Choice format}: A choice format type for DCCV to be analyzed.
Select one of the three radio buttons corresponding to 
\sQuote{\code{Single-bounded (SB)}},
\sQuote{\code{One-and-one-half-bounded (OOHB)}}, and
\sQuote{\code{Double-bounded (DB)}}.

\item \code{1st response variable}: A response variable for 
the suggested price in the first stage of an SB-, OOHB-, or 
DB-DCCV question. Select it from a drop-down list of variables.

\item \code{2nd response variable}: A response variable for 
the suggested price in the second stage of an OOHB- or DB-DCCV 
question. Select it from a drop-down list of variables.

\item \code{Covariates (optional)}: Variables to be used as 
covariates in the model to be fitted. Select none or more 
variables from a list box of variables.

\item \code{1st bid variable}: A bid variable containing 
the suggested price in the first stage of an SB-, OOHB-, 
or DB-DCCV question. Select it from a drop-down list of variables.

\item \code{2nd bid variable}: A bid variable containing 
the suggested price in the second stage of an OOHB- or DB-DCCV
question. Select it from a drop-down list of variables.

\item \code{Take the log of bid}: A check box for taking the 
logarithm of the bid variables. When checked, take the logarithm
of the bid variable(s) selected by the \sQuote{\code{1st bid variable}}
and \sQuote{\code{2nd bid variable}} drop-down lists.

\item \code{Distribution}: An error distribution of the model. 
Select one from radio buttons corresponding to \sQuote{\code{Logistic}},
\sQuote{\code{Normal}}, \sQuote{\code{Log-logistic}}, 
\sQuote{\code{Log-normal}}, and \sQuote{\code{Weibull}}.

\item \code{Subset expression}: A logical expression for specifying 
a subset of the observations in the active data set to be used for 
the fitting. If necessary, type a valid logical expression into this box.

\item \code{Initial parameters (optional)}: Initial parameters of 
independent variables (covariates and bid variables selected above) 
used in the optimization process of fitting a model. If necessary, 
type comma-separated values (e.g., 0.3, 1.5, -0.9, ...) into this box.
}
}

