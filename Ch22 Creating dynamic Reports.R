#--------------------------------------------------------------------------#
# R in Action (2nd ed): Chapter 22                                         #
# Creating dynamic reports                                                 #
# requires packages knitr, odfWeave, r2wd, car (for the dataset)           #
# install.packages(c("knitr", "odfWeave", "r2wd", "car", "rmarkdown"),     # 
#                    depend=TRUE)                                          #
# Windows users will need                                                  #
#   MikTeX (http://miktex.org/)                                            #
# Mac users will need                                                      #
#   MacTeX (http://www.tug.org/mactex/)                                    #
# Linux users should just need the knitr package                           #
#--------------------------------------------------------------------------#

# download http://www.statmethods.net/RiA/DynamicReports.zip
# and unzip the contents in the R current working directory

# Creating web page with rmarkdown
library(rmarkdown)
render("women.Rmd", "html_document")

## Creating publication ready documents with R and LaTeX

library(knitr)
knit2pdf("drugs.Rnw")


## Creating dynamic reports with R and OpenOffice
# you will need to have OpenOffice (http://www.openoffice.org) installed
library(odfWeave)
infile <- "salaryTemplate.odt"
outfile <- "salaryReport.odt"
odfWeave(infile, outfile)

## Creating dynamic reports with R and Microsoft Word
# you will need to have Microsoft Word installed

# Listing 22.3 - R script for inserting results in salary.docx
require(R2wd)
require(car)

df <- Salaries
n <- nrow(df)
fit <- lm(salary ~ rank*sex, data=df)
aovTable <- Anova(fit, type=3)
aovTable <- round(as.data.frame(aovTable), 3)
aovTable[is.na(aovTable)] <- ""

wdGet("salaryTemplate2.docx", method="RDCOMClient")
wdGoToBookmark("n")                                          
wdWrite(n)

wdGoToBookmark("aovTable")                                   
wdTable(aovTable, caption="Two-way Analysis of Variance", 
        caption.pos="above", pointsize=12, autoformat=4)

wdGoToBookmark("effectsPlot")                                
myplot <- function(){
  require(effects)
  par(mar=c(2,2,2,2))
  plot(allEffects(fit), main="")
}
wdPlot(plotfun=myplot, caption="Mean Effects Plot",                      
       height=4, width=5, method="metafile")
wdSave("SalaryReport2.docx")                                 
wdQuit()

