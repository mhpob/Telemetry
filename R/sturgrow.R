#' Bioenergetics model for juvenile Atlantic sturgeon
#' 
#' \code{sturgrow} executes the bioenergetics model from Niklitschek &
#' Secor 2009.
#' 
#' For more information on model development and full explanation of
#' parameters, see the supporting material of Niklitschek E.J., Secor D.H.,
#' 2009. Dissolved oxygen, temperature and salinity effects on the
#' ecophysiology and survival of juvenile Atlantic sturgeon in estuarine
#' waters: II. Model development and testing. Journal of Experimental Marine
#' Biology and Ecology 381: S161-S172.
#' 
#' Script was originally written on 20110803, last update on 20140702.
#' Manipulated into R fuction on 20150330 by M. O'Brien 
#' \email{obrien@@umces.edu}.
#' 
#' @author Edwin Niklitschek \email{edwin.niklitschek@@ulagos.cl}
#' 
#' @param TEMP Numeric vector. Recorded temperature (celsius).
#' @param SAL Numeric vector. Corresponding recorded salinity.
#' @param DO Numeric vector. Corresponding recorded dissolved oxygen
#'    (percent saturation)
#' @param GR Numeric. Weight of fish (kilograms). Defaults to 14 kg.
#' @param TL Numeric. Total length of fish (cm). Defaults to NULL.
#' @param CJ_OBS Numeric. If you declare this variable equal to 0, the model
#'    assumes maximum consumption rate (CMAX). Defaults to 0.
#' @param RATION Numeric. If you declare this variable equal to 1, the model
#'    assumes p-vaule equal to 1. Defaults to 1.
#'    
#' @param t1 Numeric. Lowest tested temperature (celsius). Constant in the
#'    temperature portion of the routine metabolism and food consumption
#'    models. Defaults to 6.
#' @param t4 Numeric. Highest tested temperature (celsius). Constant in the
#'    temperature portion of the routine metabolism and food consumption
#'    models. Defaults to 28.
#' @param tk1rm Numeric. Reaction rate multiplier at the lowest tested
#'    temperature (6 C). Parameter of temperature portion of routine metabolism
#'    model. Defaults to 0.141.
#' @param tk4rm Numeric. Lower temperature threshold where f_rm(TEMP) >= 0.98.
#'    Parameter of temperature portion of routine metabolism model. Defaults to
#'    0.796.
#'    
#' @param b1 Numeric. Specific gill surface area. Constant in salinity portion
#'    of routine metabolism and food consumption models. Defaults to -0.158
#'    after work of Burggren et al (1979) on \strong{Acipenser transmontanus}.
#'  @param hrm Numeric. Hyperosmotic response coefficient. Parameter of 
#'    salinity portion of routine metabolism model. Defaults to 0.268.
#' @param irm Numeric. Hyposmotic response coefficient. Parameter of
#'    salinity portion of routine metabolism model. Defaults to 0.352.
#' @param smin Numeric. Salinity at which minimum osmoregulation cost is
#'    predicted. Parameter of salinity portion of routine metabolism and food
#'    consumption models. Defaults to 9.166.
#' 
#' @param do1 Numeric. Constant in the dissolved oxygen portion of the routine
#'    metabolism and food consumption models. ***UPDATE***   
#' @param grm Numeric. Proportionality constant for DOC_rm. Parameter of
#'    dissolved oxygen portion of routine metabolism model. Defaults to 0.748.
#' @param drm Numeric. Proportionality constant for reaction rate at lowest
#'    DO_sat. Parameter of dissolved oxygen portion of routine metabolism
#'    model. Defaults to 1.048.
#' @param crm Numeric. Dissolved oxygen response shape parameter. Parameter of
#'    dissolved oxygen portion of routine metabolism model. Defaults to 1.0.
#'    
#' @param ox Numeric. Constant in routine metabolism model ***UPDATE***
#' @param arm Numeric. Allometric intercept (scaling coefficient), parameter of
#'    routine metabolism model. Defaults to 0.522.
#' @param brm Numeric. Allometric slope, parameter of routine metabolism model.
#'    Defaults to -0.17.
#'    
#' @param tl98fc Numeric. Lower temperature threshold where f_fc(T) >= 0.98.
#'    Parameter of temperature portion of food consumption model. Defaults to
#'    26.09.
#' @param tk1fc Numeric. Reaction rate multiplier at the lowest tested
#'    temperature (6 C). Parameter of temperature portion of food consumption
#'    model. Defaults to 0.195.
#' @param tk4fc Numeric. Reaction rate multiplier at the highest tested
#'    temperature (28 C). Parameter of temperature portion of food consumption
#'    model. Defaults to 0.556.
#'  
#' @param s1 Numeric. Lowest tested salinity. Constant in the salintiy portion
#'    of the food consumption model. Defaults to 1.
#' @param s4 Numeric. Highest tested salinity. Constant in the salinity portion
#'    of the food consumption model. Defaults to 29.  
#' @param jfc Numeric. Size-dependent intercept for reaction rate at the lowest
#'    salinity. Parameter of salinity portion of food consumption model.
#'    Defaults to 0.359.
#' @param kfc Numeric. Size-dependent intercept for reaction rate at the
#'    highest salinity. Parameter of salinity portion of food consumption
#'    model. Defaults to 0.247.
#'    
#' @param gfc Numeric. Proportionality constant for DOC_fc. Parameter of
#'    dissolved oxygen portion of food consumption model. Defaults to 0.733.
#' @param dfc Numeric. Proportionality constant for reaction rate at lowest
#'    DO_sat. Parameter of dissolved oxygen portion of food consumption model.
#'    Defaults to 2.516.
#' @param cfc Numeric. Dissolved oxygen response shape parameter. Parameter of
#'    dissolved oxygen portion of food consumption model. Defaults to 1.0.
#'    
#' @param afc Numeric. Allometric intercept (scaling coefficient), parameter of
#'    food consumption model. Defaults to 1.028.
#' @param bfc Numeric. Allometric slope, parameter of food consumption model.
#'    Defaults to -0.197.
#'    
#' @param aeg Numeric. Scale parameter for egestion. Parameter of egestion
#'    model. Defaults to 0.335.
#' @param ceg Numeric. Dissolved oxygen effect exponent. Parameter of egestion
#'    model. Defaults to -0.75.
#' @param deg Numeric. Temperature effect exponent. Parameter of egestion
#'    model. Defaults to -0.62.
#' @param geg Numeric. Ration size effect exponent. Parameter of egestion
#'    model. Defaults to 0.
#'    
#' @param aex Numeric. RNE, scaling factor. Parameter of excretion model.
#'    Defaults to 0.055703.
#' @param bex Numeric. RNE, exponent. Parameter of excretion model. Defaults to
#'    -0.29.
#' @param cex Numeric. XNE, food consumption proportionality coefficient.
#'    Parameter of excretion model. Defaults to 0.0392.
#'    
#' @param asda Numeric. Proportionality constant (to assimilated energy).
#'    Constant of post-prandial metabolism (SDA) model. Defaults to 0.1657.
#'
#' @param aact Numeric. Proportionality constant to food consumption. Constant
#'    of active metabolism (ACT) model. Defaults to 0.29.
#' 
#' @return Output is the predicted growth in the respective temperature,
#'    salintiy, and dissolved oxygen combinations.
#' @export

sturgrow <- function(TEMP, SAL, DO, GR = 14, TL = NULL, CJ_OBS = 0, RATION = 1,
                     t1 = 6, t4 = 28, tk1rm = 0.141, tk4rm = 0.796,
                     b1 = -0.158, hrm = 0.268, irm = 0.352, smin = 9.166,
                     do1 = 25, grm = 0.748, crm = 1.0, drm = 1.048,
                     arm = 0.522, brm = -0.17, ox = 13.55, tl98fc = 26.09,
                     tk1fc = 0.195, tk4fc = 0.556, jfc = 0.359, kfc = 0.247,
                     s1 = 1, s4 = 29, gfc = 0.733, dfc = 2.516, cfc = 1,
                     afc = 1.028, bfc = -0.197, aeg = 0.335, ceg = -0.75,
                     deg = -0.62, geg = 0, aex = 0.055703, bex = -0.29,
                     cex = 0.0392, asda = 0.1657, aact = 0.29){
## RM Model ----
  ## FT
  y1 <- (1 / (t4 - t1)) * log(tk4rm * (1 - tk1rm) / (tk1rm * (1 - tk4rm)))
	ey1 <- exp(y1 * (TEMP - t1))
  FTrm <- tk1rm * ey1 / (1 + tk1rm * (ey1 - 1))
  
  ## FS
	FSArm <- 1 + 0.01 * exp(hrm * GR ^ b1 * (SAL - smin))
	FSBrm <- 1 + 0.01 * exp(irm * GR ^ b1 * (smin - SAL))
	FSrm <- FSArm * FSBrm / (1.0201)
  
  ## FO
	DOCrm <- 100 * (1 - crm * exp(-FTrm * FSrm))
	KO1rm <- 1 - drm * exp(FTrm * FSrm - 1)
	dorel <- (DOCrm - DO) / 100
	SLrm <- (0.98 - KO1rm) / ((0.02 * (DOCrm - do1)) ^ crm)
	FOrm <- ifelse(dorel > 0, (0.98 - SLrm * dorel ^ crm) / 0.98, 1)

	KRM <- FOrm * FSrm * FTrm		
	RM <- GR * (arm * GR ^ brm) * KRM * 24 * ox / 1000
  
## FC Model ----
  ## FT
	cy1 <- (1 / (tl98fc - t1)) * log(0.98 * (1 - tk1fc) / (tk1fc * 0.02))
	ecy1 <- exp(cy1 * (TEMP - t1))
  cka <- tk1fc * ecy1 / (1 + tk1fc * (ecy1 - 1))

	cy2 <- (1 / (t4 - tl98fc)) * log(0.98 * (1 - tk4fc) / (tk4fc * 0.02))
	ecy2 <- exp(cy2 * (t4 - TEMP))
	ckb <- tk4fc * ecy2 / (1 + tk4fc * (ecy2 - 1))

	FTfc <- cka * ckb 
  
  ## FS		
	CKS1 <- jfc * GR ^ -b1
	CKS4 <- kfc * GR ^ -b1

	YA <- (1 / (smin - s1)) * log(0.98 * (1 - CKS1) / (CKS1 * 0.02))
	EYA <- exp(YA * (SAL - s1))
  KSA <- CKS1 * EYA / (1 + CKS1 * (EYA - 1))

	YB <- (1 / (s4 - smin)) * log(0.98 * (1 - CKS4) / (CKS4 * 0.02))
	EYB <- exp(YB * (s4 - SAL))
  KSB <- CKS4 * EYB / (1 + CKS4 * (EYB - 1))
	FSfc <- (KSA * KSB) / (1) 
  
  ## FO
	DOCfc <- 100 * (1 - gfc * exp(-KRM))
	KO1fc <- 1 - dfc * exp(KRM - 1)
	dorel <- (DOCfc - DO) / 100
	SLfc <- (0.98 - KO1fc)  /((0.02 * (DOCfc - do1)) ^ cfc)
	FOfc <- ifelse(dorel > 0, (0.98 - SLfc * dorel ^ cfc) / 0.98, 1)
	
	CJG_MAX <- (afc * GR ^ bfc) * FTfc * FSfc * FOfc
	CJ_MAX <- GR * CJG_MAX
	CJG_PRED <- RATION * CJG_MAX
	CJ_PRED <- CJG_PRED * GR

	if (CJ_OBS == 0) CJ = CJ_PRED else CJ = CJ_OBS
  
## Egestion Model ----
  E <- aeg * (TEMP / 6) ^ ceg * (DO / DOCrm) ^ deg * RATION ^ geg 
	EG <- E * CJ

## Excretion Model ----
	EX <- aex * GR ^ bex * RM + cex * CJ

## SDA Model----
	SDA <- (CJ - EG) * asda
	
## Active Medtabolism ----
	AM <- CJ_MAX * aact 

## Energy Content ---- 
	TL <- ifelse(is.null(TL), exp((log(GR) + 6.1488) / 3.113), TL)
	LNL <- log(TL)
	WS <- TL / exp((log(GR) + 6.1488) / 3.113)
	ENEC <- exp(-1.0065 + 1.0336 * log(WS) + 0.6807 * LNL)
 
## Growth ----
	GKJ_PRED <- CJ - RM - EG - SDA - EX - AM
	log((GR + GKJ_PRED / ENEC) / GR)
}