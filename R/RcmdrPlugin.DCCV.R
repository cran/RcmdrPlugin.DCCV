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
dccvModel <- function() {
  initializeDialog(title = gettextRcmdr("Fit Model to DCCV Data"))
  defaults <- list(
    ini.dcVariable    = "1",
    ini.resVar1BOX    = gettextRcmdr("<no variable selected>"),
    ini.resVar2BOX    = gettextRcmdr("<no variable selected>"),
    ini.covariatesVar = NULL,
    ini.bidVar1BOX    = gettextRcmdr("<no variable selected>"),
    ini.bidVar2BOX    = gettextRcmdr("<no variable selected>"),
    ini.logCheckVar   = "0",
    ini.distVar       = "'logistic'",
    ini.initParName   = "")
  dialog.values <- getDialog("dccvModel", defaults)


  .activeModel <- ActiveModel()
  currentModel <- if (!is.null(.activeModel)) {
    any(class(get(.activeModel, envir = .GlobalEnv))[1] == c("dbchoice", "sbchoice", "oohbchoice"))
  } else {
    FALSE
  }
  if (currentModel) {
    currentFields <- formulaFields(get(.activeModel, envir = .GlobalEnv))
    if (currentFields$data != ActiveDataSet()) currentModel <- FALSE
  }

  # remove a second part of rhs from the current model formula
  if (currentModel) {
    currentRhs <- currentFields$rhs
    currentRhs <- unlist(strsplit(currentRhs, "\\|"))[1]
    currentFields$rhs <- currentRhs
  }

  if (isTRUE(getRcmdr("reset.model"))) {
    currentModel <- FALSE
    putRcmdr("reset.model", FALSE)
  }

 
  UpdateModelNumber()
  modelName  <- tclVar(paste("DCCVmodel.", getRcmdr("modelNumber"), sep = ""))
  modelFrame <- tkframe(top)
  model      <- ttkentry(modelFrame, width = "14", textvariable = modelName)
   
  outputFrame  <- tkframe(top)
  
  dcTypelFrame <- tkframe(top)
  resVarsFrame <- tkframe(top)

  covariatesFrame <- tkframe(top)

  bidVarsFrame <- tkframe(top)
  optionsFrame <- tkframe(top)
  initParFrame <- tkframe(optionsFrame)

  radioButtons(dcTypelFrame, name = "dc", 
               buttons = c("SB", "OOHB", "DB"),
               values  = c("1", "2", "3"),
               labels  = gettextRcmdr(c("Single-bounded (SB)",
                                        "One-and-one-half-bounded (OOHB)",
                                        "Double-bounded (DB)")),
               initialValue = dialog.values$ini.dcVariable,
               title = gettextRcmdr("Choice format"))   

  resVar1BOX <- variableComboBox(
    resVarsFrame,
    Variables(),
    initialSelection = dialog.values$ini.resVar1BOX,
    title = gettextRcmdr("1st response variable"))
  
  resVar2BOX <- variableComboBox(
    resVarsFrame,
    Variables(),
    initialSelection = dialog.values$ini.resVar2BOX,
    title = gettextRcmdr("2nd response variable"))
  
  covariatesBox <- variableListBox(
                     covariatesFrame,
                     Variables(),
                     title = gettextRcmdr("Covariates (pick zero or more)"),
                     selectmode = "multiple",
                     listHeight = 5,
                     initialSelection = varPosn(dialog.values$ini.covariatesVar,
                                                vars = Variables()))
    
  bidVar1BOX <- variableComboBox(
    bidVarsFrame,
    Variables(),
    initialSelection = dialog.values$ini.bidVar1BOX,
    title = gettextRcmdr("1st bid variable"))
  
  bidVar2BOX <- variableComboBox(
    bidVarsFrame,
    Variables(),
    initialSelection = dialog.values$ini.bidVar2BOX,
    title = gettextRcmdr("2nd bid variable"))
  
  logCheckVar <- tclVar(dialog.values$ini.logCheckVar)
  logCheckButton <- ttkcheckbutton(bidVarsFrame, variable = logCheckVar)
  
  radioButtons(optionsFrame, name = "dist", 
               buttons = c("log", "nor", "loglog", "lognor", "wei"),
               values  = c("'logistic'", "'normal'", "'log-logistic'",
                           "'log-normal'", "'weibull'"),
               labels  = gettextRcmdr(c("Logistic", "Normal", "Log-logistic",
                                        "Log-normal", "Weibull")),
               initialValue = dialog.values$ini.distVar,
               title = gettextRcmdr("Distribution"))   

  subsetBox(optionsFrame, model = TRUE)
  
  initParName <- tclVar(dialog.values$ini.initParName)
  initPar     <- ttkentry(initParFrame, width = 25, textvariable = initParName)
       
  onOK <- function() {
    modelValue <- trim.blanks(tclvalue(modelName))
      
    resVar1 <- getSelection(resVar1BOX)
    resVar2 <- getSelection(resVar2BOX)
    bidVar1 <- getSelection(bidVar1BOX)
    bidVar2 <- getSelection(bidVar2BOX)
    covVar  <- getSelection(covariatesBox)
    
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
          recall = dccvModel,
          message = gettextRcmdr(
            "2nd response and bid variables should not be selected for SBDC"),
          model = TRUE)
        return()
      }
    } else {
      if (trim.blanks(resVar2) == gettextRcmdr("<no variable selected>") ||
          trim.blanks(bidVar2) == gettextRcmdr("<no variable selected>")) {
        errorCondition(
          recall = dccvModel,
          message = gettextRcmdr(
            "2nd response and bid variables should be selected for OOHB/DBDC"),
          model = TRUE)
        return()
      }
    }
    
    if (length(covVar) == 0) {
      covVar <- "1"
    }
      
    if (tclvalue(initParName) == "") {
      cmd.initPar <- ""
    } else {
      cmd.initPar <- paste(", par = c(", tclvalue(initParName), ")", sep = "")
    }


    putDialog("dccvModel", list(
      ini.dcVariable    = tclvalue(dcVariable),
      ini.resVar1BOX    = resVar1,
      ini.resVar2BOX    = resVar2,
      ini.covariatesVar = covVar,
      ini.bidVar1BOX    = bidVar1,
      ini.bidVar2BOX    = bidVar2,
      ini.logCheckVar   = tclvalue(logCheckVar),
      ini.distVar       = tclvalue(distVariable),
      ini.initParName   = tclvalue(initParName)))    

                  
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
    
    if (length(covVar) > 1) {
      covVar <- paste(covVar, collapse = " + ")
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


  OKCancelHelp(helpSubject = "dccvModel",
               model       = TRUE,
               reset       = "resetDccvModel",
               apply       = "dccvModel")

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
   
  tkgrid(getFrame(covariatesBox), sticky = "nw")
  tkgrid(covariatesFrame, sticky = "w")

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


resetDccvModel <- function(){
  putRcmdr("reset.model", TRUE)
  putDialog("dccvModel", NULL)
  putDialog("dccvModel", NULL, resettable = FALSE)
  dccvModel()
}


###############################################################################
dccvWtp <- function() {
  initializeDialog(
    title = gettextRcmdr("Calculate Confidence Intervals for WTP"))
  defaults <- list(
    ini.outputName    = "WTP",
    ini.methodtypeVar = "1",
    ini.confLevelName = "0.95",
    ini.NdrawsValue   = "200",
    ini.RNGseedName   = "")
  dialog.values <- getDialog("dccvWtp", defaults)

  env <- environment()
  
  outputFrame <- tkframe(top)
  inputsFrame <- tkframe(top)
  methodFrame <- tkframe(inputsFrame)
  settingFrame <- tkframe(inputsFrame)
  RNGseedFrame <- tkframe(top)
  
  outputName <- tclVar(dialog.values$ini.outputName)
  output     <- ttkentry(outputFrame, width = "14", textvariable = outputName)

  # Bootstrap method
  radioButtons(methodFrame, 
               name    = "methodtype",
               buttons = c("Parametric", "Nonparametric"),
               values  = c("1", "2"),
               labels  = gettextRcmdr(c("Krinsky and Robb", "Bootstrap")),
               initialValue = dialog.values$ini.methodtypeVar,
               title   = gettextRcmdr("Calculation method"))

  # Confidence level
  confLevelName <- tclVar(dialog.values$ini.confLevelName)
  NdrawsValue   <- tclVar(dialog.values$ini.NdrawsValue)
  Ndraws        <- ttkentry(settingFrame, width = "6",
                            textvariable = NdrawsValue)
  confLevel     <- ttkentry(settingFrame, width = "6",
                            textvariable = confLevelName)

  # Random number generator seed
  RNGseedName <- tclVar(dialog.values$ini.RNGseedName)
  RNGseed     <- ttkentry(RNGseedFrame, width = "10",
                          textvariable = RNGseedName)

  onOK <- function() {
    outputValue <- trim.blanks(tclvalue(outputName))


    putDialog("dccvWtp", list(
      ini.outputName    = tclvalue(outputName),
      ini.methodtypeVar = tclvalue(methodtypeVariable),
      ini.NdrawsValue   = tclvalue(NdrawsValue),
      ini.confLevelName = tclvalue(confLevelName),
      ini.RNGseedName   = tclvalue(RNGseedName)))


    closeDialog()
    
    if (tclvalue(methodtypeVariable) == "1") {
      cmd.method <- paste("krCI(obj = ", ActiveModel(), ", nsim = ",
                          sep = "")
    } else {
      cmd.method <- paste("bootCI(obj = ", ActiveModel(), ", nboot = ",
                          sep = "")
    }
    
    if (!is.na(as.numeric(tclvalue(RNGseedName)))) {
      cmd.seed <- paste("set.seed(", as.numeric(tclvalue(RNGseedName)), ")",
                        sep = "")
    }

    cmd <- paste(cmd.method, as.numeric(tclvalue(NdrawsValue)),
                 ", CI = ", as.numeric(tclvalue(confLevelName)), ")", sep = "")
    
    if (!is.na(as.numeric(tclvalue(RNGseedName)))) {
      doItAndPrint(paste(cmd.seed, sep = ""))
      doItAndPrint(paste(outputValue, " <- ", cmd, sep = ""))
    } else {
      doItAndPrint(paste(outputValue, " <- ", cmd, sep = ""))
    }
    doItAndPrint(paste(outputValue))
    tkfocus(CommanderWindow())
  }

  OKCancelHelp(helpSubject = "dccvWtp",
               reset       = "dccvWtp",
               apply       = "dccvWtp")

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
dccvPlot <- function() {
  initializeDialog(title = gettextRcmdr("Draw Survival Function"))
  defaults <- list(
    ini.mainVar  = "",
    ini.xlabVar  = "",
    ini.ylabVar  = "",
    ini.xFromVar = "",
    ini.xToVar   = "",
    ini.yFromVar = "",
    ini.yToVar   = "",
    ini.bFromVar = "",
    ini.bToVar   = "")
  dialog.values <- getDialog("dccvPlot", defaults)

  
  # Titles
  titleFrame     <- tkframe(top)
  titleSub1Frame <- tkframe(titleFrame)

  mainVar   <- tclVar(dialog.values$ini.mainVar)
  mainEntry <- ttkentry(titleSub1Frame, width = "50", textvariable = mainVar)
  xlabVar   <- tclVar(dialog.values$ini.xlabVar)
  xlabEntry <- ttkentry(titleSub1Frame, width = "50", textvariable = xlabVar)
  ylabVar   <- tclVar(dialog.values$ini.ylabVar)
  ylabEntry <- ttkentry(titleSub1Frame, width = "50", textvariable = ylabVar)
  
  # Ranges
  rangeFrame   <- tkframe(top)
  xyRangeFrame <- tkframe(rangeFrame)
  
  xFromVar <- tclVar(dialog.values$ini.xFromVar)
  xFrom    <- ttkentry(xyRangeFrame, width = "6", textvariable = xFromVar)
  xToVar   <- tclVar(dialog.values$ini.xToVar)
  xTo      <- ttkentry(xyRangeFrame, width = "6", textvariable = xToVar)
  
  yFromVar <- tclVar(dialog.values$ini.yFromVar)
  yFrom    <- ttkentry(xyRangeFrame, width = "6", textvariable = yFromVar)
  yToVar   <- tclVar(dialog.values$ini.yToVar)
  yTo      <- ttkentry(xyRangeFrame, width = "6", textvariable = yToVar)

  bFromVar <- tclVar(dialog.values$ini.bFromVar)
  bFrom    <- ttkentry(xyRangeFrame, width = "6", textvariable = bFromVar)
  bToVar   <- tclVar(dialog.values$ini.bToVar)
  bTo      <- ttkentry(xyRangeFrame, width = "6", textvariable = bToVar)

  # OK button function
  onOK <- function() {

    putDialog("dccvPlot", list(
      ini.mainVar  = tclvalue(mainVar),
      ini.xlabVar  = tclvalue(xlabVar),
      ini.ylabVar  = tclvalue(ylabVar),
      ini.xFromVar = tclvalue(xFromVar),
      ini.xToVar   = tclvalue(xToVar),
      ini.yFromVar = tclvalue(yFromVar),
      ini.yToVar   = tclvalue(yToVar),
      ini.bFromVar = tclvalue(bFromVar),
      ini.bToVar   = tclvalue(bToVar)))

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
  
  
  OKCancelHelp(helpSubject = "dccvPlot",
               reset       = "dccvPlot",
               apply       = "dccvPlot")

  tkgrid(labelRcmdr(titleFrame, 
                    text = gettextRcmdr("Title and labels (optional)")),
         sticky = "w")
  tkgrid(labelRcmdr(titleSub1Frame, text = gettextRcmdr("Title ")),
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
  tkgrid(labelRcmdr(xyRangeFrame, text = gettextRcmdr("Bid     from ")), bFrom,
         labelRcmdr(xyRangeFrame, text = gettextRcmdr(" to ")), bTo,
         sticky = "w")
  tkgrid(xyRangeFrame, sticky = "w")
  tkgrid(rangeFrame, sticky = "w")
  
  tkgrid(buttonsFrame, columnspan = 2, sticky = "w")
  dialogSuffix()
}

###############################################################################
dccvModelP <- function() {
  activeModelP() && any(class(get(ActiveModel()))[1] == c("dbchoice", "sbchoice", "oohbchoice"))
}

###############################################################################
dccvCollectResponseSet <- function() {
  initializeDialog(title = gettextRcmdr("Set Options for Response Collection"))
  defaults <- list(ini.designName   = "DCCVdesign",
                   ini.bidRow       = "Random",
                   ini.saveVariable = "1")
  dialog.values = getDialog("dccvCollectResponseSet", defaults)
  
  inputsFrame      <- tkframe(top)
  tableFrame       <- tkframe(inputsFrame)
  importFrame      <- tkframe(inputsFrame)
  currencyFrame    <- tkframe(inputsFrame)
  selectBidIdFrame <- tkframe(inputsFrame)
  saveFrame        <- tkframe(inputsFrame)
  
  if (exists("bidTable")) {
    nRowsBids <- nrow(bidTable)
    nColsBids <- ncol(bidTable)
    bidIDs    <- as.character(1:nRowsBids)
  } else {
    nRowsBids <- 0
    nColsBids <- 0
    bidIDs    <- NULL
  }
  
  bidIdBox <- variableComboBox(
    selectBidIdFrame,
    bidIDs,
    nullSelection = "Random",
    initialSelection = dialog.values$ini.bidRow,
    title = gettextRcmdr("Bid id")
  )
  
  currencyBox <- variableComboBox(
    currencyFrame,
    ISOcodes::ISO_4217$Letter,
    nullSelection = NULL,
    initialSelection = "USD",
    title = gettextRcmdr("Currency")
  )
  
  saveVariable <- tclVar(dialog.values$ini.saveVariable)
  saveCheckBox <- ttkcheckbutton(saveFrame, variable = saveVariable)
  
  onOK <- function() {
    if (!exists("bidTable")) {
      Message(gettextRcmdr("Please import bid table"), type = "warning")
      closeDialog()
      dccvCollectResponseSet()
      return()
    }
    
    currency <- getSelection(currencyBox)
    bidId    <- getSelection(bidIdBox)
    
    if (tclvalue(saveVariable) == 1) {
      SAVE <- TRUE
    } else {
      SAVE <- FALSE
    }
    
    closeDialog()
    
    if (bidId == "Random") {
      bidId <- sample(x = 1:nRowsBids, size = 1)
    }
    
    putRcmdr("DCCVcurrency.SAVE", currency)
    putRcmdr("DCCVresponse.SAVE", SAVE)
    putRcmdr("DCCVbidId.SAVE", bidId)
    
    dccvCollectResponse()
    
    tkfocus(CommanderWindow())
  }
  
  onImport <- function() {
    closeDialog()
    
    file <- tclvalue(tkgetOpenFile(filetypes = gettextRcmdr(
      '{"CSV" {".csv" ".CSV"}}')))
    if (file == "") {
      return()
    }
    setBusyCursor()
    on.exit(setIdleCursor)
    
    cmd <- paste0('bidTable <-read.csv("', file, '", header = FALSE)')
    loadedObjects <- justDoIt(cmd)
    logger(cmd)
    
    dccvCollectResponseSet()
    
    tkfocus(CommanderWindow())
  }
  
  OKCancelHelp(helpSubject = "dccvCollectResponse")
  
  importButton <- buttonRcmdr(
    importFrame,
    text = gettextRcmdr("Import bid table"),
    foreground = "darkgreen",
    width = "18",
    command = onImport,
    default = "normal",
    borderwidth = 3
  )
  
  if (exists("bidTable")) {
    if (nColsBids == 1) {
      tkgrid(labelRcmdr(tableFrame, text = paste0("id")),
             labelRcmdr(tableFrame, text = paste0("bid1")),
             sticky = "w")
    } else if (nColsBids == 2) {
      tkgrid(labelRcmdr(tableFrame, text = paste0("id")),
             labelRcmdr(tableFrame, text = paste0("bidL")),
             labelRcmdr(tableFrame, text = paste0("bidH")),
             sticky = "w")
      
    } else {
      tkgrid(labelRcmdr(tableFrame, text = paste0("id")),
             labelRcmdr(tableFrame, text = paste0("bid1")),
             labelRcmdr(tableFrame, text = paste0("bid2L")),
             labelRcmdr(tableFrame, text = paste0("bid2H")),
             sticky = "w")
    }
    for (i in 1:nRowsBids) {
      cmd <- paste0("labelRcmdr(tableFrame, text = paste0('", i, "')), ")
      for (j in 1:nColsBids) {
        cmd <- paste0(cmd,
                      paste0("labelRcmdr(tableFrame, text = paste0(",
                             bidTable[i, j],
                             ")), "))
      }
      cmd <- paste0(cmd, paste0("sticky = 'w'"))
      eval(parse(text = paste0("tkgrid(", cmd, ")")))
    }
  } else {
    tkgrid(labelRcmdr(tableFrame,
                      text = paste0("No bid table imported"),
                      fg = "red"),
           sticky = "w")
  }
  
  tkgrid(tableFrame, sticky = "w")
  
  tkgrid(importButton, sticky = "w")
  tkconfigure(importButton, takefocus = 0)
  tkgrid(importFrame, sticky = "w")
  
  tkgrid(getFrame(currencyBox), sticky = "nw", pady = c(5, 0))
  tkgrid(currencyFrame, sticky = "w")
  
  tkgrid(getFrame(bidIdBox), sticky = "nw", pady = c(5, 0))
  tkgrid(selectBidIdFrame, sticky = "w")
  
  tkgrid(
    saveCheckBox,
    labelRcmdr(
      saveFrame,
      text = gettextRcmdr("Save to file")),
    sticky = "w",
    pady = c(5, 0)
  )
  
  tkgrid(selectBidIdFrame, sticky = "nw")
  tkgrid(saveFrame, sticky = "nw")
  tkgrid(inputsFrame, sticky = "nw")
  tkgrid(buttonsFrame, columnspa = 2, sticky = "w")
  
  dialogSuffix()
}

###############################################################################

dccvCollectResponse <- function() {
  initializeDialog(title = gettextRcmdr("Collect Responses to DCCV Questions"))
  defaults <- list(
    ini.Q = 1,
    ini.R = NULL,
    ini.responseName = "<no response selected>"
  )
  dialog.values <- getDialog("dccvCollectResponse", defaults)
  
  currency <- getRcmdr("DCCVcurrency.SAVE")
  save     <- getRcmdr("DCCVresponse.SAVE")
  bidrow   <- getRcmdr("DCCVbidId.SAVE")
  
  if (ncol(bidTable) == 1) {
    nQues = 1
    dccvFormat = "SB"
  } else if (ncol(bidTable) == 2) {
    nQues = 2
    dccvFormat = "OOHB"
    if (dialog.values$ini.Q == 1) {
      LorH = sample(x = c(1, 2), size = 1)
      putRcmdr("DCCVLorH.SAVE", LorH)
    } else {
      LorH = getRcmdr("DCCVLorH.SAVE")
    }
  } else {
    nQues = 2
    dccvFormat = "DB"
  }
  
  inputsFrame   <- tkframe(top)
  responseFrame <- tkframe(inputsFrame)
  okcancelFrame <- tkframe(top)
  okFrame       <- tkframe(okcancelFrame)
  cancelFrame   <- tkframe(okcancelFrame)
  
  response <- variableComboBox(
    responseFrame,
    variableList = c("Yes", "No"),
    nullSelection = "<no response selected>",
    adjustWidth = TRUE)
  
  onOK <- function() {
    responseName <- getSelection(response)
    
    if (responseName == "<no response selected>") {
      Message(gettextRcmdr("Please respond to the question"), type = "warning")
      closeDialog()
      dccvCollectResponse()
      return()
    }
    
    if (dialog.values$ini.Q == 1) {
      set.seed(seed = NULL)
      justDoIt(paste0("MyDCCVresponses <- c(", sample.int(1e10, 1), ")"))
    }
    
    if (responseName == "Yes") {
      R = 1
    } else {
      R = 0
    }
    
    putDialog("dccvCollectResponse", list(
      ini.Q = dialog.values$ini.Q + 1,
      ini.R = R,
      ini.responseName = "<no response selected>"))
    
    justDoIt(paste0("MyDCCVresponses <- c(MyDCCVresponses, ", R, ")"))
    
    closeDialog()
    
    if (dccvFormat == "SB") {
      COMPLETE = TRUE
    } else if (dccvFormat == "DB") {
      if (dialog.values$ini.Q < nQues) {
        COMPLETE = FALSE
      } else {
        COMPLETE = TRUE
      }
    } else {
      if (LorH == 1 & dialog.values$ini.Q == 1 & R == 0) {
        COMPLETE = TRUE
        justDoIt(paste0("MyDCCVresponses <- c(MyDCCVresponses, -9)"))
      } else if (LorH == 2 & dialog.values$ini.Q == 1 & R == 1) {
        COMPLETE = TRUE
        justDoIt(paste0("MyDCCVresponses <- c(MyDCCVresponses, -9)"))
      } else if (dialog.values$ini.Q < nQues) {
        COMPLETE = FALSE
      } else {
        COMPLETE = TRUE
      }
    }
    
    if (!isTRUE(COMPLETE)) {
      dccvCollectResponse()
    } else {
      putDialog("dccvCollectResponse", list(
        ini.Q = 1,
        ini.R = NULL,
        ini.responseName = "<no response selected>"))
      
      if (dccvFormat == "SB") {
        justDoIt(paste0("MyDCCVresponses <- c(MyDCCVresponses, ",
                        bidTable[bidrow, 1],
                        ")"))
        cmd <- paste0('names(MyDCCVresponses) <- c("id", "R1", "bid1")')
      } else if (dccvFormat == "OOHB") {
        justDoIt(paste0("MyDCCVresponses <- c(MyDCCVresponses, ",
                        bidTable[bidrow, 1], ", ", bidTable[bidrow, 2], ")"))
        cmd <- paste0('names(MyDCCVresponses) <- c("id", "R1", "R2", "bidL", "bidH")')
      } else {
        if (dialog.values$ini.R == 1) {
          bidCOL = 3
        } else {
          bidCOL =2
        }
        justDoIt(paste0("MyDCCVresponses <- c(MyDCCVresponses, ",
                        bidTable[bidrow, 1], ", ",
                        bidTable[bidrow, bidCOL], ")"))
        cmd <- paste0('names(MyDCCVresponses) <- c("id", "R1", "R2", "bid1", "bid2")')
      }
      
      justDoIt(cmd)
      
      doItAndPrint(paste0("MyDCCVresponses"))
      
      if (isTRUE(save)) {
        saveFile <- tclvalue(tkgetSaveFile(
          filetypes = gettextRcmdr(
            '{"CSV Files" {".csv" ".CSV"}}'),
          defaultextension = ".csv",
          initialfile = "MyDCCVresponses.csv",
          parent = CommanderWindow()))
        if(saveFile == "") {
          tkfocus(CommanderWindow())
          return()
        }
        cmd <- paste0('write.csv(t(MyDCCVresponses), file = "', saveFile,
                      '", row.names = FALSE)')
        justDoIt(cmd)
        logger(cmd)
        Message(
          paste0(
            gettextRcmdr("Your responses to DCCV questions were exported to file: "),
            saveFile),
          type = "note")
      }
    }
    tkfocus(CommanderWindow())
  }
  
  onCancel <- function() {
    closeDialog()
    
    putDialog("dccvCollectResponse", list(
      ini.Q = 1,
      ini.R = NULL,
      ini.responseName = "<no response selected>"))
    
    tkfocus(CommanderWindow())
  }
  
  tkgrid(
    labelRcmdr(
      inputsFrame,
      text = gettextRcmdr(paste0("Question ", dialog.values$ini.Q))),
    sticky = "w")
  
  if (dialog.values$ini.Q == 1) {
    if (dccvFormat == "OOHB") {
      bidcol <- LorH
    } else {
      bidcol <- 1
    }
  } else {
    if (dccvFormat == "OOHB") {
      if (LorH == 1) {
        bidcol <- 2
      } else {
        bidcol <- 1
      }
    } else {
      if (dialog.values$ini.R == 1) {
        bidcol <- 3
      } else {
        bidcol <- 2
      }
    }
  }
  
  Qdescription <- paste0("Are you willing to pay ", bidTable[bidrow, bidcol],
                         " ", currency, " for the good/service/plan?")
  
  tkgrid(
    labelRcmdr(
      inputsFrame,
      text = gettextRcmdr(Qdescription)),
    sticky = "w")
  
  tkgrid(
    labelRcmdr(
      responseFrame,
      text = "My response: "),
    getFrame(response),
    sticky = "w",
    pady = c(10, 0))
  
  tkgrid(responseFrame, sticky = "w")
  
  okButton <- buttonRcmdr(
    okFrame,
    text = gettextRcmdr("OK"),
    foreground = "darkgreen",
    width = 10,
    command = onOK,
    default = "active",
    borderwidth = 3,
    image = "::image::okIcon",
    compound = "left")
  
  cancelButton <- buttonRcmdr(
    cancelFrame,
    text = gettextRcmdr("Cancel"),
    foreground = "darkgreen",
    width = 10,
    command = onCancel,
    default = "normal",
    borderwidth = 3,
    image = "::image::cancelIcon",
    compound = "left")
  
  tkgrid(okButton, sticky = "w")
  tkconfigure(okButton, takefocus = 0)
  
  tkgrid(cancelButton, sticky = "w")
  tkconfigure(cancelButton, takefocus = 0)
  
  tkgrid(
    labelRcmdr(
      inputsFrame,
      text = ""),
    sticky = "w")
  
  tkgrid(inputsFrame, sticky = "nw")
  
  tkgrid(okFrame, cancelFrame, sticky = "nw")
  
  tkgrid(okcancelFrame, sticky = "w")
  
  dialogSuffix()
}

###############################################################################