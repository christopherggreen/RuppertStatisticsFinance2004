############################################################
#                                                          #
# Copyright (c) Christopher G. Green, 2006                 #
#                                                          #
############################################################
############################################################
#                                                          #
# pages239-240.q                                           #
#                                                          #
############################################################

# s-plus can import sas data files
# normally you'd do this in the GUI using "Import Data"
#
guiImportData(FileName = "..\\common\\data\\capm.sas7bdat", 
	FileTypes = "SAS Version 7/8/9 (sas7bdat)", 
	TargetDataFrame = "sasuser.capm", 
	TargetStartCol = "<END>", 
	TargetInsertOverwrite = "Create new data set", 
	NameRowAuto = "Auto", 
	NameColAuto = "Auto", 
	StartCol = 1, 
	EndCol = "<END>", 
	StartRow = 1, 
	EndRow = "<END>", 
	PageNumberAuto = "Auto", 
	StringsAsFactors = T, 
	SortFactorLevels = T, 
	LabelsAsNumbers = F, 
	CenturyCutoffYear = 1930, 
	KeepOrDropList = "<ALL>", 
	SeparateDelimiters = T, 
	ASCIIDateInFormat = "M/d/yyyy", 
	ASCIITimeInFormat = "h:mm:ss tt", 
	ASCIIDecimalPoint = "Period (.)", 
	ASCIIThousandsSeparator = "None")

#
# in R you need library(foreign)?
#
	
EXlogR.sp500 <- c(NA,diff(log(sasuser.capm$Close.sp500))) - sasuser.capm$Close.tbill/(100*253)	
EXlogR.msft  <- c(NA,diff(log(sasuser.capm$Close.msft ))) - sasuser.capm$Close.tbill/(100*253)	

msft.reg <- lm( EXlogR.msft ~ EXlogR.sp500, na.action=na.omit )
summary(msft.reg)
anova(msft.reg)
summary.proc.reg(msft.reg)

msft.reg.noint <- lm( EXlogR.msft ~ EXlogR.sp500 - 1, na.action=na.omit )
summary(msft.reg.noint)
anova(msft.reg.noint)
summary.proc.reg(msft.reg.noint)
