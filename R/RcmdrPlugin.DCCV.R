.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  putRcmdr("slider.env", new.env())    
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}

###############################################################################
DCCVp <- function() {
  initializeDialog(title = gettextRcmdr("Fit Parametric Model"))
 
  UpdateModelNumber()
  modelName  <- tclVar(paste("Model.", getRcmdr("modelNumber"), sep = ""))
  currentModel <- FALSE
  modelFrame <- tkframe(top)
  model      <- ttkentry(modelFrame, width = "20", textvariable = modelName)
   
  outputFrame  <- tkframe(top)
  
  dcTypelFrame <- tkframe(top)
  resVarsFrame <- tkframe(top)
  bidVarsFrame <- tkframe(top)
  optionsFrame <- tkframe(top)
  initParFrame <- tkframe(optionsFrame)

  radioButtons(dcTypelFrame, name = "dc", 
               buttons = c("SB", "OOHB", "DB"),
               values  = c("1", "2", "3"),
               labels  = gettextRcmdr(c("Single-bounded (SB)",
                                        "One-and-one-half-bounded (OOHB)",
                                        "Double-bounded (DB)")),
               initialValue = "1",
               title = gettextRcmdr("Choice format"))   

  resVar1BOX <- variableComboBox(
    resVarsFrame,
    Variables(),
    initialSelection = gettextRcmdr("<no variable selected>"),
    title = gettextRcmdr("1st response variable"))
  
  resVar2BOX <- variableComboBox(
    resVarsFrame,
    Variables(),
    initialSelection = gettextRcmdr("<no variable selected>"),
    title = gettextRcmdr("2nd response variable"))
  
  modelFormula(hasLhs = FALSE, rhsExtras = NULL)

  bidVar1BOX <- variableComboBox(
    bidVarsFrame,
    Variables(),
    initialSelection = gettextRcmdr("<no variable selected>"),
    title = gettextRcmdr("1st bid variable"))
  
  bidVar2BOX <- variableComboBox(
    bidVarsFrame,
    Variables(),
    initialSelection = gettextRcmdr("<no variable selected>"),
    title = gettextRcmdr("2nd bid variable"))
  
  logCheckVar <- tclVar("0")
  logCheckButton <- ttkcheckbutton(bidVarsFrame, variable = logCheckVar)
  
  radioButtons(optionsFrame, name = "dist", 
               buttons = c("log", "nor", "loglog", "lognor", "wei"),
               values  = c("'logistic'", "'normal'", "'log-logistic'",
                           "'log-normal'", "'weibull'"),
               labels  = gettextRcmdr(c("Logistic", "Normal", "Log-logistic",
                                        "Log-normal", "Weibull")),
               initialValue = "'logistic'",
               title = gettextRcmdr("Distribution"))   

  subsetBox(optionsFrame, model = TRUE)
  
  initParName <- tclVar("")
  initPar     <- ttkentry(initParFrame, width = 25, textvariable = initParName)
       
  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))
      
    resVar1 <- getSelection(resVar1BOX)
    resVar2 <- getSelection(resVar2BOX)
    bidVar1 <- getSelection(bidVar1BOX)
    bidVar2 <- getSelection(bidVar2BOX)
    covVar  <- tclvalue(rhsVariable)
    
    SBDC <- OOHB <- DBDC <- FALSE
    if (tclvalue(dcVariable) == 1) {
      SBDC <- TRUE
    } else if (tclvalue(dcVariable) == 2) {
      OOHB <- TRUE
    } else {
      DBDC <- TRUE
    }

    if (isTRUE(SBDC)) {
      if (trim.blanks(resVar2) != gettextRcmdr("<no variable selected>") ||
          trim.blanks(bidVar2) != gettextRcmdr("<no variable selected>")) {
        errorCondition(
          recall = DCCVp,
          message = gettextRcmdr(
            "2nd response and bid variables should not be selected for SBDC"),
          model = TRUE)
        return()
      }
    } else {
      if (trim.blanks(resVar2) == gettextRcmdr("<no variable selected>") ||
          trim.blanks(bidVar2) == gettextRcmdr("<no variable selected>")) {
        errorCondition(
          recall = DCCVp,
          message = gettextRcmdr(
            "2nd response and bid variables should be selected for OOHB/DBDC"),
          model = TRUE)
        return()
      }
    }
    
    if (covVar == "") {
      covVar <- "1"
    }
      
    if (tclvalue(initParName) == "") {
      cmd.initPar <- ""
    } else {
      cmd.initPar <- paste(", par = c(", tclvalue(initParName), ")", sep = "")
    }
                  
    closeDialog()

    subset <- tclvalue(subsetVariable)
    if (trim.blanks(subset) == gettextRcmdr("<all valid cases>") ||
        trim.blanks(subset) == "") {
      subset <- ""
      putRcmdr("modelWithSubset", FALSE)
    } else {
      subset <- paste(", subset = ", subset, sep = "")
      putRcmdr("modelWithSubset", TRUE)
    }
            
    # Set a model formula for dbchoice()
    if (isTRUE(SBDC)) {
      resVars <- paste(resVar1)
      if (tclvalue(logCheckVar) == 0) {
        bidVars <- paste(bidVar1)
      } else {
        bidVars <- paste("log(",bidVar1, ")", sep = "")
      }
    } else {
      resVars <- paste(resVar1, resVar2, sep = " + ")
      if (tclvalue(logCheckVar) == 0) {
        bidVars <- paste(bidVar1, bidVar2, sep = " + ")
      } else {
        bidVars <- paste("log(", bidVar1, ") + log(", bidVar2, ")", sep = "")
      }
    }
    rhsVars <- paste(covVar, bidVars, sep = " | ")
    formula <- paste(resVars, rhsVars, sep = " ~ ")
      
    # Fit model
    if (isTRUE(SBDC)) {
      cmd.function <- paste("sbchoice(")
    } else if (isTRUE(DBDC)) {
      cmd.function <- paste("dbchoice(")
    } else {
      cmd.function <- paste("oohbchoice(")
    }
    command <- paste(cmd.function, formula, ", data = ", ActiveDataSet(),
                     subset, ", dist = ", tclvalue(distVariable), 
                     cmd.initPar, ")", sep = "")
    doItAndPrint(paste(modelValue, " <- ", command, sep = ""))
    doItAndPrint(paste("summary(", modelValue, ")", sep = ""))
    activeModel(modelValue)
    tkfocus(CommanderWindow())
  }


  OKCancelHelp(helpSubject = "dbchoice",
               reset       = "DCCVp",
               apply       = NULL)

  tkgrid(labelRcmdr(modelFrame, text = gettextRcmdr("Name for model ")),
         model, sticky = "w")
  tkgrid(modelFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

  tkgrid(dcFrame, sticky = "w")
  tkgrid(dcTypelFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))
  
  tkgrid(labelRcmdr(top, text = gettextRcmdr("Model formula"),
                    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"),
         sticky = "w")   

  tkgrid(getFrame(resVar1BOX),
         labelRcmdr(resVarsFrame, text = gettextRcmdr(" + ")),
         getFrame(resVar2BOX),
         sticky = "sw")
  tkgrid(resVarsFrame, sticky = "w")
   
  tkgrid(getFrame(xBox), sticky = "w")
  tkgrid(outerOperatorsFrame, sticky = "w")
  tkgrid(formulaFrame, sticky = "w")

  tkgrid(getFrame(bidVar1BOX),
         labelRcmdr(bidVarsFrame, text = gettextRcmdr(" + ")),
         getFrame(bidVar2BOX),
         labelRcmdr(bidVarsFrame, text = "  "),
         logCheckButton,
         labelRcmdr(bidVarsFrame, text = gettextRcmdr("Take the log of bid")),
         sticky = "sw")

  tkgrid(bidVarsFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))

  tkgrid(labelRcmdr(initParFrame,
                    text = gettextRcmdr("Initial parameters (optional)"),
                    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"),
         sticky = "w")
  tkgrid(initPar, sticky = "w")
  tkgrid(distFrame, labelRcmdr(optionsFrame, text = " "),
         subsetFrame, labelRcmdr(optionsFrame, text = " "),
         initParFrame, sticky = "nw")
  tkgrid(optionsFrame, sticky = "w")
   
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix(preventDoubleClick = TRUE)
}

###############################################################################
DCCVpCIWTP <- function() {
  initializeDialog(title = gettextRcmdr("Calculate Confidence Intervals for WTP"))
  env <- environment()
  
  outputFrame <- tkframe(top)
  inputsFrame <- tkframe(top)
  methodFrame <- tkframe(inputsFrame)
  settingFrame <- tkframe(inputsFrame)
  RNGseedFrame <- tkframe(top)
  
  outputName <- tclVar("WTP") 
  output     <- ttkentry(outputFrame, width = "20", textvariable = outputName)

  # Bootstrap method
  radioButtons(methodFrame, 
               name    = "methodtype",
               buttons = c("Parametric", "Nonparametric"),
               values  = c("1", "2"),
               labels  = gettextRcmdr(c("Krinsky and Robb", "Bootstrap")),
               initialValue = "1",
               title   = gettextRcmdr("Simulation method"))

  # Confidence level
  confLevelName <- tclVar("0.95")
  NdrawsValue   <- tclVar("200")
  Ndraws        <- ttkentry(settingFrame, width = "6", textvariable = NdrawsValue)
  confLevel     <- ttkentry(settingFrame, width = "6", textvariable = confLevelName)

  # Random number generator seed
  RNGseedName <- tclVar("")
  RNGseed     <- ttkentry(RNGseedFrame, width = "10", textvariable = RNGseedName)

  onOK <- function() {
    outputValue <- trim.blanks(tclvalue(outputName))
    closeDialog()
    
    if (tclvalue(methodtypeVariable) == "1") {
      cmd.method <- paste("krCI(obj = ", ActiveModel(), ", nsim = ", sep = "")
    } else {
      cmd.method <- paste("bootCI(obj = ", ActiveModel(), ", nboot = ", sep = "")
    }
    
    if (!is.na(as.numeric(tclvalue(RNGseedName)))) {
      cmd.seed <- paste("set.seed(", as.numeric(tclvalue(RNGseedName)), ")", sep = "")
    }

    cmd <- paste(cmd.method, as.numeric(tclvalue(NdrawsValue)),
                 ", CI = ", as.numeric(tclvalue(confLevelName)), ")", sep = "")
    
    if (!is.na(as.numeric(tclvalue(RNGseedName)))) {
      doItAndPrint(paste(cmd.seed, ";", outputValue, " <- ", cmd, sep = ""))
    } else {
      doItAndPrint(paste(outputValue, " <- ", cmd, sep = ""))
    }
    doItAndPrint(paste(outputValue))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "krCI",
               reset       = "DCCVpCIWTP",
               apply       = NULL)

  tkgrid(labelRcmdr(outputFrame,
                    text = gettextRcmdr("Name for output ")),
         output, sticky = "w")
  tkgrid(outputFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))
  
  tkgrid(methodtypeFrame, sticky = "w")
  tkgrid(methodFrame, sticky = "w")

  tkgrid(labelRcmdr(settingFrame,
                    text = gettextRcmdr("Confidence level ")),
         confLevel, sticky = "w")
  tkgrid(labelRcmdr(settingFrame,
                    text = gettextRcmdr("Number of replications ")),
         Ndraws, sticky = "w")
  tkgrid(settingFrame, sticky = "w")
  tkgrid(inputsFrame, sticky = "w")

  tkgrid(
    labelRcmdr(
      RNGseedFrame,
      text = gettextRcmdr("Seed for random number generator (optional) ")),
    RNGseed, sticky = "w")
  tkgrid(RNGseedFrame, sticky = "w")

  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()       
}

###############################################################################
DCCVpPlot <- function() {
  defaults <- list(initial.scoretype = "bw",
                   initial.positiontype = "1")
  dialog.values <- getDialog("bws1CountPlot", defaults)
  initializeDialog(title = gettextRcmdr("Draw Survival Function"))
  
  # Titles
  titleFrame     <- tkframe(top)
  titleSub1Frame <- tkframe(titleFrame)

  mainVar   <- tclVar("")
  mainEntry <- ttkentry(titleSub1Frame, width = "50", textvariable = mainVar)
  xlabVar   <- tclVar("")
  xlabEntry <- ttkentry(titleSub1Frame, width = "50", textvariable = xlabVar)
  ylabVar   <- tclVar("")
  ylabEntry <- ttkentry(titleSub1Frame, width = "50", textvariable = ylabVar)
  
  # Ranges
  rangeFrame   <- tkframe(top)
  xyRangeFrame <- tkframe(rangeFrame)
  
  xFromVar <- tclVar("")
  xFrom    <- ttkentry(xyRangeFrame, width = "6", textvariable = xFromVar)
  xToVar   <- tclVar("")
  xTo      <- ttkentry(xyRangeFrame, width = "6", textvariable = xToVar)
  
  yFromVar <- tclVar("")
  yFrom    <- ttkentry(xyRangeFrame, width = "6", textvariable = yFromVar)
  yToVar   <- tclVar("")
  yTo      <- ttkentry(xyRangeFrame, width = "6", textvariable = yToVar)

  bFromVar <- tclVar("")
  bFrom    <- ttkentry(xyRangeFrame, width = "6", textvariable = bFromVar)
  bToVar   <- tclVar("")
  bTo      <- ttkentry(xyRangeFrame, width = "6", textvariable = bToVar)

  # OK button function
  onOK <- function() {
    closeDialog()
    
    if (tclvalue(mainVar) == "") {
      cmd.main <- ""
    } else {
      cmd.main <- paste(", main = '", tclvalue(mainVar), "'", sep = "")
    }
    
    if (tclvalue(xlabVar) == "") {
      cmd.xlab <- ""
    } else {
      cmd.xlab <- paste(", xlab = '", tclvalue(xlabVar), "'", sep = "")
    }
    
    if (tclvalue(ylabVar) == "") {
      cmd.ylab <- ""
    } else {
      cmd.ylab <- paste(", ylab = '", tclvalue(ylabVar), "'", sep = "")
    }
    
    if (tclvalue(xFromVar) == "" & tclvalue(xToVar) == "") {
      cmd.xlim <- ""
    } else {
      cmd.xlim <- paste(", xlim = c(", tclvalue(xFromVar), ", ",
                        tclvalue(xToVar), ")", sep = "")
    }
    
    if (tclvalue(yFromVar) == "" & tclvalue(yToVar) == "") {
      cmd.ylim <- ""
    } else {
      cmd.ylim <- paste(", ylim = c(", tclvalue(yFromVar), ", ",
                        tclvalue(yToVar), ")", sep = "")
    }

    if (tclvalue(bFromVar) == "" & tclvalue(bToVar) == "") {
      cmd.bid <- ""
    } else {
      cmd.bid <- paste(", bid = c(", tclvalue(bFromVar), ", ",
                       tclvalue(bToVar), ")", sep = "")
    }

    doItAndPrint(
      paste("plot(x = ", ActiveModel(),
            cmd.main, cmd.xlab, cmd.ylab, cmd.xlim, cmd.ylim, cmd.bid, ")",
            sep = ""))
    
    tkfocus(CommanderWindow())
  }
  
  
  OKCancelHelp(helpSubject = "plot.dbchoice",
               reset       = "DCCVpPlot",
               apply       = NULL)

  tkgrid(labelRcmdr(titleFrame, 
                    text = gettextRcmdr("Title and labels (optional)")),
         sticky = "w")
  tkgrid(labelRcmdr(titleSub1Frame, text = gettextRcmdr("title ")),
         mainEntry, sticky = "w")
  tkgrid(labelRcmdr(titleSub1Frame, text = gettextRcmdr("x-axis ")),
         xlabEntry, sticky = "w")
  tkgrid(labelRcmdr(titleSub1Frame, text = gettextRcmdr("y-axis ")),
         ylabEntry, sticky = "w")
  tkgrid(titleSub1Frame, sticky = "w")
  tkgrid(titleFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = ""))
  
  tkgrid(labelRcmdr(rangeFrame, text = gettextRcmdr("Ranges (optional)")),
         sticky = "w")
  tkgrid(labelRcmdr(xyRangeFrame, text = gettextRcmdr("x-axis from ")), xFrom,
         labelRcmdr(xyRangeFrame, text = gettextRcmdr(" to ")), xTo,
         sticky = "w")
  tkgrid(labelRcmdr(xyRangeFrame, text = gettextRcmdr("y-axis from ")), yFrom,
         labelRcmdr(xyRangeFrame, text = gettextRcmdr(" to ")), yTo,
         sticky = "w")
  tkgrid(labelRcmdr(xyRangeFrame, text = gettextRcmdr("bid     from ")), bFrom,
         labelRcmdr(xyRangeFrame, text = gettextRcmdr(" to ")), bTo,
         sticky = "w")
  tkgrid(xyRangeFrame, sticky = "w")
  tkgrid(rangeFrame, sticky = "w")
  
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}

###############################################################################
