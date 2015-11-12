setwd("/Users/ben/dropbox/Chicago Booth/41100 Regressions/Project")

# Import compensation data from Compustat ExecuComp dataset
ExecuComp <- read.csv("Execucomp GVKey All.csv")

# Import financials data from Compustat North America dataset
Financials <- read.csv("NA GVKey 1.csv")

# Merge two dataset
combined <- merge(ExecuComp,Financials, by = "GVKEY")

