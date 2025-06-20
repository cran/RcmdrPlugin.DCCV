\docType{package}
\name{RcmdrPlugin.DCCV-package}
\alias{RcmdrPlugin.DCCV-package}
\alias{RcmdrPlugin.DCCV}

\title{R Commander Plug-in for Parametric Analysis of Dichotomous Choice 
Contingent Valuation Data}

\description{
This package adds menu items to the R Commander for parametric analysis 
of dichotomous choice contingent valuation (DCCV) data. CV is a 
question-based survey method to elicit individuals' preferences for 
goods and services. This package depends on functions regarding parametric 
DCCV analysis in the package DCchoice. See Carson and Hanemann (2005) 
<doi:10.1016/S1574-0099(05)02017-6> for DCCV.
}

\details{
This package is an R Commander plug-in package for parametric analysis of 
dichotomous choice contingent valuation (DCCV) data. It depends on 
\pkg{DCchoice} (Nakatani et al. 2020) and \pkg{Rcmdr} (Fox 2005, 2017; 
Fox and Bouchet-Valat 2020). Refer to \pkg{DCchoice}, 
Aizaki et al. (2014, 2022), and 
\sQuote{Non-Market Valuation with R} <\url{https://nmvr.skr.jp/}> 
for a brief introduction to DCCV in R. 
A manual for this package is available at <\url{https://sites.google.com/view/r4sp/rcmdrplugin}> 

After successfully installing and loading \pkg{RcmdrPlugin.DCCV}, 
the R Commander window will appear, and then you will find \sQuote{\code{DCCV}} 
added to the top-level menus in the R Commander window.

Selecting \sQuote{\code{DCCV}} displays some menu items. When selecting a menu 
item, the corresponding dialog box opens. The following dialog boxes 
are provided by \pkg{RcmdrPlugin.DCCV}:
\itemize{
\item The dialog box for \sQuote{\code{Collect responses...}} displays
three types of dialog boxes to collect user's responses to DCCV questions.

\item The dialog box for \sQuote{\code{Fit Model...}} implements a 
parametric analysis of DCCV data using \code{\link[DCchoice]{sbchoice}} 
(single-bounded DCCV),
\code{\link[DCchoice]{oohbchoice}} (one-and-one-half-bounded DCCV), or 
\code{\link[DCchoice]{dbchoice}} (double-bounded DCCV) in \pkg{DCchoice}.

\item The dialog box for \sQuote{\code{Calculate confidence intervals for 
WTP...}} 
calculates confidence intervals for willigness to pay (WTP) from the 
fitted model using \code{\link[DCchoice]{krCI}} or 
\code{\link[DCchoice]{bootCI}} in \pkg{DCchoice}.

\item The dialog box for \sQuote{\code{Draw survival function...}} 
draws a survival function of the fitted model using 
\code{\link[DCchoice]{plot.sbchoice}} or \code{\link[DCchoice]{plot.dbchoice}} 
in \pkg{DCchoice}.
}
}

\section{Acknowledgments}{
This work was supported by JSPS KAKENHI Grant Number JP20K06251.
}

\author{
Hideo Aizaki
}

\references{
Aizaki H, Nakatani T, Sato K, Fogarty J (2022)
R Package DCchoice for Dichotomous Choice Contingent Valuation: 
A Contribution to Open Scientific Software and its Impact.
\emph{Japanese Journal of Statistics and Data Science}, 5: 871--884.
DOI: 10.1007/s42081-022-00171-1.

Aizaki H, Nakatani T, Sato K (2014) 
\emph{Stated Preference Methods Using R}. 
Chapman and Hall/CRC. 
DOI: 10.1201/b17292.

Carson RT, Hanemann WM (2005) Contingent valuation. 
In KG Maler, JR Vincent (eds), 
\emph{Handbook of Environmental Economics}, 
Volume 2, Chapter 17, pp. 821--936. Elsevier.
DOI: 10.1016/S1574-0099(05)02017-6.

Fox J (2005) The R Commander: A Basic Statistics Graphical User Interface to R. 
\emph{Journal of Statistical Software}, 14(9): 1--42. 
DOI: 10.18637/jss.v014.i09.

Fox J (2017) \emph{Using the R Commander: A Point-and-Click Interface for R}.
Chapman and Hall/CRC. 
DOI: 10.1201/9781315380537.

Fox J, Bouchet-Valat M (2020) Rcmdr: R Commander. R package version 2.7-1.
 \url{https://cran.r-project.org/package=Rcmdr}.

Nakatani T, Aizaki H, Sato K (2020) 
DCchoice: Analyzing Dichotomous Choice Contingent Valuation Data. 
R package 0.0.17. 
\url{https://cran.r-project.org/package=DCchoice}.
}

\examples{
if (interactive()) {
  library(RcmdrPlugin.DCCV)
}
}

\keyword{
package
}
