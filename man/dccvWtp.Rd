\name{dccvWtp}
\alias{dccvWtp}

\title{Calculate Confidence Intervals for WTP}

\description{
Calculate confidence intervals for a willingness to pay (WTP) from 
the fitted model.
}

\details{
This dialog box uses either function \code{\link[DCchoice]{krCI}} 
or \code{\link[DCchoice]{bootCI}} in package \pkg{DCchoice} to
calculate confidence intervals for a WTP from the fitted model. 
This dialog box is activated if the fitted model in 
the \sQuote{\code{Fit Model to DCCV Data}} dialog box is selected
as the active model. 
For details on the results of this dialog box, refer to the help 
content for \code{\link[DCchoice]{krCI}} or 
\code{\link[DCchoice]{bootCI}}. 

GUI elementsof this dialog box are as follows.

\itemize{
\item \code{Name for output}: An R object name into which the results 
are stored. The default name is \sQuote{\code{WTP}}. Any name that is 
valid for R is typed into this box, if necessary.

\item \code{Calculation method}: A method used to calculate confidence
intervals for a WTP. Select one of the two radio buttons corresponding
to \sQuote{\code{Krinsky and Robb}} and \sQuote{\code{Bootstrap}}. 
For the details on these methods, see help for functions 
\code{\link[DCchoice]{krCI}} and \code{\link[DCchoice]{bootCI}}.

\item \code{Confidence level}: A confidence level used for calculating
the confidence intervals for a WTP. The default is \code{0.95}.

\item \code{Number of replications}: Total number of replications.
The term \sQuote{\code{replication}} corresponds to 
\sQuote{\code{resampling}} for \sQuote{\code{Bootstrap}} and 
\sQuote{\code{drawing}} for \sQuote{\code{Krinsky and Robb}}. 
The default is \code{200}, which may be small.

\item \code{Seed for random number generator (optional)}: An integer
value used as a seed for generating random numbers. If reproducibility
is needed, type an arbitrary value into this box.
}
}


