#    ATLANTIC STURGEON MODEL
# 	REFERENCE: Niklitschek E.J. & Secor D.H. 2009. Dissolved oxygen, temperature and salinity effects on the ecophysiology and survival 
# 			of juvenile Atlantic sturgeon in estuarine waters: II. Model development and testing. Journal of Experimental Marine Biology 
# 			and Ecology 381: S161-S172.
# 	SCRIPT : 20110803
# 	LAST UPDATE: 20140702
#   Programmer: Edwin Niklitschek; edwin.niklitschek@ulagos.cl


#*** READING DATA ***;
# REPLACE data1 BY YOUR OWN ENVIRONMENTAL DATA SET;
data1<-expand.grid(SAL=1:29,DO=25:100,TEMP=6:29)

#DEFINING FISH WEIGHT AND OBSERVED CONSUMPTION;
GR=14; #REPLACE THIS BY YOUR ACTUAL FISH WEIGHT IF NEEDED;
TL=NA; #REPLACE THIS BY TOUR ACTUAL FISH TOTAL LENGTH OR LEAVE IT AS NA IF YOU DO NOT HAVE IT;
CJ_OBS=0; #IF YOU DECLARE THIS VARIABLE EQUAL TO 0, THE MODEL ASSUMES MAXIMUM CONSUMPTION RATE (CMAX); 
RATION=1; #IF YOU DECLARE THIS VARIABLE EQUAL TO 1, THE MODEL ASSUMES p-value=1;
#----------------------------------------------------------------------	
#BIOENERGETICS PARAMETERS
#----------------------------------------------------------------------
## ROUTINE METABOLISM;
arm = 0.522;
brm = -0.17;
tk1rm = 0.141;
tk4rm = 0.796;
crm = 1.0;
drm = 1.048;
grm = 0.748;
hrm = 0.268;
irm = 0.352;
smin = 9.166;
b1=-.158;
## FOOD CONSUMPTION;
afc = 1.028;
bfc = -0.197;
tk1fc = 0.195;
tk4fc = 0.556;
tl98fc = 26.09;
cfc=1;
dfc = 2.516;
gfc = 0.733;
jfc = 0.359;
kfc = 0.247;

## EGESTION;
aeg = 0.335;
ceg = -0.75;
deg = -0.62;
geg=0;

## EXCRETION;
aex=0.055703;
bex=-0.29;
cex=0.0392;

## SDA;
asda=0.1657;

## AM;
aact=0.29;

## Constants
s4=29; s1=1; do1=25;
#----------------------------------------------------------------------
# MODELING
#----------------------------------------------------------------------
attach(data1)
#----------------------------------------------------------------------  
# RM MODEL;
#----------------------------------------------------------------------  
## RM Model Constants;
ox=13.55; rt1=6; rt4=28; 

## FT;
	y1=(1/(rt4-rt1))*log(tk4rm*(1-tk1rm)/(tk1rm*(1-tk4rm)));
	ey1=exp(y1*(TEMP-rt1));
  FTrm=tk1rm*ey1/(1+tk1rm*(ey1-1));
## FS;
	FSArm=1+0.01*exp(hrm*GR**b1*(SAL-smin));
	FSBrm=1+0.01*exp(irm*GR**b1*(smin-SAL));
	FSrm=FSArm*FSBrm/(1.0201);
## FO;
	DOCrm=100*(1-crm*exp(-FTrm*FSrm));
	KO1rm=1-drm*exp(FTrm*FSrm-1);
	dorel=(DOCrm-DO)/100;
	SLrm=(.98-KO1rm)/((.02*(DOCrm-do1))**crm);
	FOrm=ifelse(dorel>0,(0.98-SLrm*dorel**crm)/0.98,1)

	KRM=FOrm*FSrm*FTrm;		
	RM=GR*(arm*GR**brm)*KRM*24*ox/1000;
#----------------------------------------------------------------------  
# FC MODEL;
#----------------------------------------------------------------------  
## FC model constants;		
	ct1=6; ct4=28;
## FT;
	cy1=(1/(tl98fc-ct1))*log(0.98*(1-tk1fc)/(tk1fc*0.02));
	ecy1=exp(cy1*(TEMP-ct1));
  cka=tk1fc*ecy1/(1+tk1fc*(ecy1-1));

	cy2=(1/(ct4-tl98fc))*log(0.98*(1-tk4fc)/(tk4fc*0.02));
	ecy2=exp(cy2*(ct4-TEMP));
	ckb=tk4fc*ecy2/(1+tk4fc*(ecy2-1));

	FTfc=cka*ckb; 
## FS;		
	CKS1=jfc*GR**-b1;
	CKS4=kfc*GR**-b1;

	YA=(1/(smin-S1))*log(0.98*(1-CKS1)/(CKS1*0.02));
	EYA=exp(YA*(SAL-S1));
  KSA=CKS1*EYA/(1+CKS1*(EYA-1));

	YB=(1/(S4-smin))*log(0.98*(1-CKS4)/(CKS4*0.02));
	EYB=exp(YB*(S4-SAL));
  KSB=CKS4*EYB/(1+CKS4*(EYB-1));
	FSfc=(KSA*KSB)/(1); 

## FO;
	DOCfc=100*(1-gfc*exp(-KRM));
	KO1fc=1-dfc*exp(KRM-1);
	dorel=(DOCfc-DO)/100;
	SLfc=(.98-KO1fc)/((.02*(DOCfc-do1))**cfc);
	FOfc=ifelse(dorel>0,(0.98-SLfc*dorel**cfc)/0.98,1)
	
	CJG_MAX=(afc*GR**bfc)*FTfc*FSfc*FOfc;
	CJ_MAX=GR*CJG_MAX;
	CJG_PRED=RATION*CJG_MAX;
	CJ_PRED=CJG_PRED*GR;

	if (CJ_OBS==0) CJ=CJ_PRED else CJ=CJ_OBS;
#----------------------------------------------------------------------  
# EGESTION MODEL
#----------------------------------------------------------------------  
	E=aeg*(TEMP/6)**ceg*(DO/DOCrm)**deg*RATION**geg; 
	EG=E*CJ;
#----------------------------------------------------------------------  
# EXCRETION MODEL;
#----------------------------------------------------------------------  
	EX=aex*GR**bex*RM+cex*CJ;
#----------------------------------------------------------------------  
# SDA MODEL;
#----------------------------------------------------------------------  
	SDA=(CJ-EG)*asda;
#----------------------------------------------------------------------  	
# ACTIVE METABOLISM ;
#----------------------------------------------------------------------  
	AM=CJ_MAX*aact; 
#----------------------------------------------------------------------  
# ENERGY CONTENT;
#----------------------------------------------------------------------  
	TL=ifelse(is.na(TL),exp((log(GR)+6.1488)/3.113),TL);
	LNL=log(TL);
	WS=TL/exp((log(GR)+6.1488)/3.113);
	ENEC=exp(-1.0065+1.0336*log(WS)+0.6807*LNL);
#----------------------------------------------------------------------  
# GROWTH;
#----------------------------------------------------------------------  
	GKJ_PRED=CJ-RM-EG-SDA-EX-AM;
	G_PRED=log((GR+GKJ_PRED/ENEC)/GR);