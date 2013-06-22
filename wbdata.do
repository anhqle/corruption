clear
capture log close
cap cd "D:/Projects/corruption/rawdata"

wbopendata, indicator(BN.KLT.DINV.CD.ZS;BX.KLT.DINV.WD.GD.ZS) year(1996:2011) long nometadata clear

save fdi.dta
