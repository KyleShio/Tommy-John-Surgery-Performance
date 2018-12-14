# What are the effects of TJ?

library('Lahman')
library('useful')
library('glmnet')
library('leaps')
library('magrittr')
library('dplyr')
library('ggplot2')

TJ_DF <- read.csv('/Users/Tyler/Downloads/Tommy John Surgery List (@MLBPlayerAnalys) - TJ List.csv')
data('Pitching')
Pitching$SH <- NULL
Pitching$SF <- NULL
Pitching$GIDP <- NULL
Pitching$IP <- Pitching$IPouts/3
View(Pitching)
View(TJ_DF)
mean(TJ_DF$Age)
LongBall_pitchDF <- Pitching %>%
  filter(yearID >= 1994) %>%
  mutate(BBper9 = ((BB*9)/IP),
         Hper9 = ((H*9)/IP),
         HRper9 = ((HR*9)/IP),
         SOper9 = ((SO*9)/IP)
         )
View(LongBall_pitchDF)

TJ <- TJ_DF %>%
  filter(Level == "MLB" & Year <= 2016 & Position == "P" & Year >= 2000) 
mean(TJ$Age)
median(TJ$Age)
TJ$Young <- ifelse(TJ$Age < median(TJ$Age), 1, 0)
TJ$Recover <- ifelse(TJ$Post.TJ.MLB.G != 0, 1, 0)

View(TJ)
set.seed(1861)
sample(1:311, size = 100) 
#hitchst01, 2000
TJ[309,1:2] 
subset(LongBall_pitchDF, playerID == 'hitchst01')
hitch.before <- LongBall_pitchDF %>%
  filter(playerID == 'hitchst01' & yearID <= 2000)
hitch.after <- LongBall_pitchDF %>%
  filter(playerID == 'hitchst01' & yearID > 2000)
sapply(hitch.before, class)
#soriara01, 2004
TJ[243,1:2] 
subset(LongBall_pitchDF, playerID == 'soriara01')
sor.before <- LongBall_pitchDF %>%
  filter(playerID == 'soriara01' & yearID <= 2004)  
sor.after <- LongBall_pitchDF %>%
  filter(playerID == 'soriara01' & yearID > 2004) 

#zagurmi01, 2008
TJ[186,1:2] 
subset(LongBall_pitchDF, playerID == 'zagurmi01')
zag.before <- LongBall_pitchDF %>%
  filter(playerID == 'zagurmi01' & yearID <= 2008)  
zag.after <- LongBall_pitchDF %>%
  filter(playerID == 'zagurmi01' & yearID > 2008) 
#oflaher01, 2013
TJ[83,1:2] 
subset(LongBall_pitchDF, playerID == 'oflaher01')
ofla.before <- LongBall_pitchDF %>%
  filter(playerID == 'oflaher01' & yearID <= 2013)  
ofla.after <- LongBall_pitchDF %>%
  filter(playerID == 'oflaher01' & yearID > 2013) 


#hanrajo01, 2015
TJ[38, 1:2]
#subset(LongBall_pitchDF, playerID == 'zimmeje02')
#Zimmerman.before <- LongBall_pitchDF %>%
#filter(playerID == 'zimmeje02' & yearID <= 2002)  
#Zimmerman.after <- LongBall_pitchDF %>%
#filter(playerID == 'zimmeje02' & yearID > 2002) 
### DID NOT RETURN FROM INJURY

#zimmeje01, 2002
TJ[271, 1:2]
#subset(LongBall_pitchDF, playerID == 'hanrajo01')
#han.before <- LongBall_pitchDF %>%
#filter(playerID == 'hanrajo01' & yearID <= 2015)
#han.after <- LongBall_pitchDF %>%
#filter(playerID == 'hanrajo01' & yearID > 2015)
###DID not return from injury
#wolfra01, 2005
TJ[232, 1:2]
subset(LongBall_pitchDF, playerID == 'wolfra02')
wolf.before <- LongBall_pitchDF %>%
  filter(playerID == 'wolfra02' & yearID <= 2005)  
wolf.after <- LongBall_pitchDF %>%
  filter(playerID == 'wolfra02' & yearID > 2005) 
#villaos01, 2009
TJ[173, 1:2]
#subset(LongBall_pitchDF, playerID == 'villaos01')
#vill.before <- LongBall_pitchDF %>%
#filter(playerID == 'villaos01' & yearID <= 2009)  
#vill.after <- LongBall_pitchDF %>%
#filter(playerID == 'villaos01' & yearID > 2009) 
### DID NOT RETURN FROM INJURY
#strasst01, 2010
TJ[146, 1:2]
subset(LongBall_pitchDF, playerID == 'strasst01')
stras.before <- LongBall_pitchDF %>%
  filter(playerID == 'strasst01' & yearID <= 2010)  
stras.after <- LongBall_pitchDF %>%
  filter(playerID == 'strasst01' & yearID > 2010) 
#harnipe01, 2002
TJ[277, 1:2]
#subset(LongBall_pitchDF, playerID == 'harnipe01')
#harn.before <- LongBall_pitchDF %>%
#filter(playerID == 'harnipe01' & yearID <= 2002)  
#harn.after <- LongBall_pitchDF %>%
#filter(playerID == 'harnipe01' & yearID > 2002) 
### DID NOT RETURN FROM INJURY

#Eric Milton       6/15/2007  miltoer01
TJ[198,1:2] 
subset(LongBall_pitchDF, playerID == 'miltoer01')
milton.before <- LongBall_pitchDF %>%
  filter(playerID == 'miltoer01' & yearID <= 2007)  
milton.after <- LongBall_pitchDF %>%
  filter(playerID == 'miltoer01' & yearID > 2007) 
#Sean Nolin        8/1/2016   nolinse01
TJ[7,1:2] 
subset(LongBall_pitchDF, playerID == 'nolinse01')
nolinse01.before <- LongBall_pitchDF %>%
  filter(playerID == 'nolinse01' & yearID <= 2016)  
nolinse01.after <- LongBall_pitchDF %>%
  filter(playerID == 'nolinse01' & yearID > 2016) 
#Bill Simas      12/22/2000   simasbi01
TJ[297,1:2] 
#subset(LongBall_pitchDF, playerID == 'simasbi01')
#simasbi01.before <- LongBall_pitchDF %>%
 # filter(playerID == 'simasbi01' & yearID <= 2000)  
#simasbi01.after <- LongBall_pitchDF %>%
 # filter(playerID == 'simasbi01' & yearID > 2000) 
#Jason Christiansen       5/30/2002   chrisja01
TJ[274,1:2] 
subset(LongBall_pitchDF, playerID == 'chrisja01')
chrisja01.before <- LongBall_pitchDF %>%
  filter(playerID == 'chrisja01' & yearID <= 2002)  
chrisja01.after <- LongBall_pitchDF %>%
  filter(playerID == 'chrisja01' & yearID > 2002) 
#Ricky Bones        5/1/2003    bonesri01
TJ[263, 1:2]
#subset(LongBall_pitchDF, playerID == 'bonesri01')
#bonesri01.before <- LongBall_pitchDF %>%
 # filter(playerID == 'bonesri01' & yearID <= 2003)  
#bonesri01.after <- LongBall_pitchDF %>%
 # filter(playerID == 'bonesri01' & yearID > 2003) 
#Orber Moreno        1/1/2000   morenor01
TJ[307, 1:2]
subset(LongBall_pitchDF, playerID == 'morenor01')
morenor01.before <- LongBall_pitchDF %>%
  filter(playerID == 'morenor01' & yearID <= 2000)  
morenor01.after <- LongBall_pitchDF %>%
  filter(playerID == 'morenor01' & yearID > 2000) 
#Jeremy Bonderman       4/24/2012   bondeje01
TJ[117, 1:2]
subset(LongBall_pitchDF, playerID == 'bondeje01')
bondeje01.before <- LongBall_pitchDF %>%
  filter(playerID == 'bondeje01' & yearID <= 2012)  
bondeje01.after <- LongBall_pitchDF %>%
  filter(playerID == 'bondeje01' & yearID > 2012) 
#Matt Harvey      10/22/2013    harvema01
TJ[74, 1:2]
subset(LongBall_pitchDF, playerID == 'harvema01')
harvema01.before <- LongBall_pitchDF %>%
  filter(playerID == 'harvema01' & yearID <= 2013)  
harvema01.after <- LongBall_pitchDF %>%
  filter(playerID == 'harvema01' & yearID > 2013) 
#Sergio Escalona       3/27/2012    escalse01
TJ[126, 1:2]
#subset(LongBall_pitchDF, playerID == 'escalse01')
#escalse01.before <- LongBall_pitchDF %>%
 # filter(playerID == 'escalse01' & yearID <= 2012)  
#escalse01.after <- LongBall_pitchDF %>%
 # filter(playerID == 'escalse01' & yearID > 2012) 
#Jarrod Parker       3/24/2014    parkeja02
TJ[68, 1:2]
#subset(LongBall_pitchDF, playerID == 'parkeja02')
#parkeja01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'parkeja02' & yearID <= 2014)  
#parkeja01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'parkeja02' & yearID > 2014) 

#John Foster        6/2/2006    fostejo01
TJ[219,1:2] 
#subset(LongBall_pitchDF, playerID == 'fostejo01')
#fostejo01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'fostejo01' & yearID <= 2006)  
#fostejo01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'fostejo01' & yearID > 2006) 
#Brett Anderson       7/14/2011   anderbr01
TJ[134,1:2] 
subset(LongBall_pitchDF, playerID == 'anderbr04')
anderbr01.before <- LongBall_pitchDF %>%
  filter(playerID == 'anderbr04' & yearID <= 2011)  
anderbr01.after <- LongBall_pitchDF %>%
  filter(playerID == 'anderbr04' & yearID > 2011) 
#Victor Zambrano       5/15/2006    zambrvi01
TJ[220,1:2] 
subset(LongBall_pitchDF, playerID == 'zambrvi01')
zambrvi01.before <- LongBall_pitchDF %>%
  filter(playerID == 'zambrvi01' & yearID <= 2006)  
zambrvi01.after <- LongBall_pitchDF %>%
  filter(playerID == 'zambrvi01' & yearID > 2006) 
#Octavio Dotel        6/6/2005    doteloc01
TJ[234,1:2]
subset(LongBall_pitchDF, playerID == 'doteloc01')
doteloc01.before <- LongBall_pitchDF %>%
  filter(playerID == 'doteloc01' & yearID <= 2005)  
doteloc01.after <- LongBall_pitchDF %>%
  filter(playerID == 'doteloc01' & yearID > 2005) 
#Billy Traber       9/21/2003   trabebi01
TJ[252, 1:2]
subset(LongBall_pitchDF, playerID == 'trabebi01')
trabebi01.before <- LongBall_pitchDF %>%
  filter(playerID == 'trabebi01' & yearID <= 2003)  
trabebi01.after <- LongBall_pitchDF %>%
  filter(playerID == 'trabebi01' & yearID > 2003) 
#Chris Spurling        3/4/2004   spurlch01
TJ[250, 1:2]
subset(LongBall_pitchDF, playerID == 'spurlch01')
spurlch01.before <- LongBall_pitchDF %>%
  filter(playerID == 'spurlch01' & yearID <= 2004)  
spurlch01.after <- LongBall_pitchDF %>%
  filter(playerID == 'spurlch01' & yearID > 2004) 
#Adam Wainwright       2/28/2011    wainwad01
TJ[142, 1:2]
subset(LongBall_pitchDF, playerID == 'wainwad01')
wainwad01.before <- LongBall_pitchDF %>%
  filter(playerID == 'wainwad01' & yearID <= 2011)  
wainwad01.after <- LongBall_pitchDF %>%
  filter(playerID == 'wainwad01' & yearID > 2011) 
#Fernando Rodney       4/29/2004    rodnefe01
TJ[247, 1:2]
subset(LongBall_pitchDF, playerID == 'rodnefe01')
rodnefe01.before <- LongBall_pitchDF %>%
  filter(playerID == 'rodnefe01' & yearID <= 2004)  
rodnefe01.after <- LongBall_pitchDF %>%
  filter(playerID == 'rodnefe01' & yearID > 2004) 
#Rubby De La Rosa        8/9/2011   delarru01 
TJ[132, 1:2]
subset(LongBall_pitchDF, playerID == 'delarru01')
delarru01.before <- LongBall_pitchDF %>%
  filter(playerID == 'delarru01' & yearID <= 2011)  
delarru01.after <- LongBall_pitchDF %>%
  filter(playerID == 'delarru01' & yearID > 2011) 
#Daisuke Matsuzaka       6/10/2011    matsuda01
TJ[136, 1:2]
subset(LongBall_pitchDF, playerID == 'matsuda01')
matsuda01.before <- LongBall_pitchDF %>%
  filter(playerID == 'matsuda01' & yearID <= 2011)  
matsuda01.after <- LongBall_pitchDF %>%
  filter(playerID == 'matsuda01' & yearID > 2011) 


#Peter Moylan        5/8/2008   moylape01
TJ[184,1:2] 
subset(LongBall_pitchDF, playerID == 'moylape01')
moylape01.before <- LongBall_pitchDF %>%
  filter(playerID == 'moylape01' & yearID <= 2008)  
moylape01.after <- LongBall_pitchDF %>%
  filter(playerID == 'moylape01' & yearID > 2008) 
#Daniel Hudson        7/9/2012    hudsoda01
TJ[102,1:2] 
subset(LongBall_pitchDF, playerID == 'hudsoda01')
hudsoda01.before <- LongBall_pitchDF %>%
  filter(playerID == 'hudsoda01' & yearID <= 2012)  
hudsoda01.after <- LongBall_pitchDF %>%
  filter(playerID == 'hudsoda01' & yearID > 2012) 
#Shaun Marcum       9/30/2008   marcush01
TJ[176,1:2] 
subset(LongBall_pitchDF, playerID == 'marcush01')
marcush01.before <- LongBall_pitchDF %>%
  filter(playerID == 'marcush01' & yearID <= 2008)  
marcush01.after <- LongBall_pitchDF %>%
  filter(playerID == 'marcush01' & yearID > 2008) 
#Doug Brocail       4/27/2001   brocado01
TJ[291,1:2] 
subset(LongBall_pitchDF, playerID == 'brocado01')
brocado01.before <- LongBall_pitchDF %>%
  filter(playerID == 'brocado01' & yearID <= 2001)  
brocado01.after <- LongBall_pitchDF %>%
  filter(playerID == 'brocado01' & yearID > 2001) 
#Joe Mays       9/11/2003   maysjo01
TJ[254, 1:2]
subset(LongBall_pitchDF, playerID == 'maysjo01')
maysjo01.before <- LongBall_pitchDF %>%
  filter(playerID == 'maysjo01' & yearID <= 2003)  
maysjo01.after <- LongBall_pitchDF %>%
  filter(playerID == 'maysjo01' & yearID > 2003) 
#Brandon Beachy       3/21/2014   beachbr01
TJ[70, 1:2]
subset(LongBall_pitchDF, playerID == 'beachbr01')
beachbr01.before <- LongBall_pitchDF %>%
  filter(playerID == 'beachbr01' & yearID <= 2014)  
beachbr01.after <- LongBall_pitchDF %>%
  filter(playerID == 'beachbr01' & yearID > 2014) 
#Edinson Volquez        8/3/2009    volqued01
TJ[158, 1:2]
subset(LongBall_pitchDF, playerID == 'volqued01')
volqued01.before <- LongBall_pitchDF %>%
  filter(playerID == 'volqued01' & yearID <= 2009)  
volqued01.after <- LongBall_pitchDF %>%
  filter(playerID == 'volqued01' & yearID > 2009) 
#Jose Cisnero       5/28/2014   cisnejo01
TJ[54, 1:2]
#subset(LongBall_pitchDF, playerID == 'cisnejo01')
#cisnejo01.before <- LongBall_pitchDF %>%
# filter(playerID == 'cisnejo01' & yearID <= 2014)  
#cisnejo01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'cisnejo01' & yearID > 2014) 
#Luke Hochevar       3/18/2014    hochelu01
TJ[72, 1:2]
subset(LongBall_pitchDF, playerID == 'hochelu01')
hochelu01.before <- LongBall_pitchDF %>%
  filter(playerID == 'hochelu01' & yearID <= 2014)  
hochelu01.after <- LongBall_pitchDF %>%
  filter(playerID == 'hochelu01' & yearID > 2014) 
#Adam Bernero        3/1/2007   bernead01
TJ[206, 1:2]
#subset(LongBall_pitchDF, playerID == 'bernead01')
#bernead01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'bernead01' & yearID <= 2007)  
#bernead01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'bernead01' & yearID > 2007) 


#Shawn Kelley        9/1/2010   kellesh01
TJ[147,1:2] 
subset(LongBall_pitchDF, playerID == 'kellesh01')
kellesh01.before <- LongBall_pitchDF %>%
  filter(playerID == 'kellesh01' & yearID <= 2010)  
kellesh01.after <- LongBall_pitchDF %>%
  filter(playerID == 'kellesh01' & yearID > 2010) 
#Scott Radinsky       6/10/2000   radinsc01
TJ[300,1:2] 
subset(LongBall_pitchDF, playerID == 'radinsc01')
radinsc01.before <- LongBall_pitchDF %>%
  filter(playerID == 'radinsc01' & yearID < 2000)  
radinsc01.after <- LongBall_pitchDF %>%
  filter(playerID == 'radinsc01' & yearID > 2000) 
#Colin Rea      11/10/2016    reaco01
TJ[1,1:2] 
#subset(LongBall_pitchDF, playerID == 'reaco01')
#reaco01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'reaco01' & yearID <= 2016)  
#reaco01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'reaco01' & yearID > 2016) 
#Joel Zumaya       3/29/2012    zumayjo01
TJ[125,1:2] 
#subset(LongBall_pitchDF, playerID == 'zumayjo01')
#zumayjo01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'zumayjo01' & yearID <= 2012)  
#zumayjo01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'zumayjo01' & yearID > 2012) 
#Michael Kohn       4/12/2012   kohnmi01
TJ[120, 1:2]
subset(LongBall_pitchDF, playerID == 'kohnmi01')
kohnmi01.before <- LongBall_pitchDF %>%
  filter(playerID == 'kohnmi01' & yearID <= 2012)  
kohnmi01.after <- LongBall_pitchDF %>%
  filter(playerID == 'kohnmi01' & yearID > 2012) 
#Alex White       4/11/2013   whiteal01
#TJ[89, 1:2]
#subset(LongBall_pitchDF, playerID == 'whiteal01')
#whiteal01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'whiteal01' & yearID <= 2013)  
#whiteal01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'whiteal01' & yearID > 2013) 
#Bruce Chen        1/1/2007   chenbr01
TJ[207, 1:2]
subset(LongBall_pitchDF, playerID == 'chenbr01')
chenbr01.before <- LongBall_pitchDF %>%
  filter(playerID == 'chenbr01' & yearID <= 2007)  
chenbr01.after <- LongBall_pitchDF %>%
  filter(playerID == 'chenbr01' & yearID > 2007) 
#Jamie Moyer       12/1/2010    moyerja01
TJ[143, 1:2]
subset(LongBall_pitchDF, playerID == 'moyerja01')
moyerja01.before <- LongBall_pitchDF %>%
  filter(playerID == 'moyerja01' & yearID <= 2010)  
moyerja01.after <- LongBall_pitchDF %>%
  filter(playerID == 'moyerja01' & yearID > 2010) 
# Matt Wise       3/25/2003   wisema01
TJ[265, 1:2]
subset(LongBall_pitchDF, playerID == 'wisema01')
wisema01.before <- LongBall_pitchDF %>%
  filter(playerID == 'wisema01' & yearID <= 2003)  
wisema01.after <- LongBall_pitchDF %>%
  filter(playerID == 'wisema01' & yearID > 2003) 
#A.J. Griffin       4/30/2014   griffaj01
TJ[57, 1:2]
subset(LongBall_pitchDF, playerID == 'griffaj01')
griffaj01.before <- LongBall_pitchDF %>%
  filter(playerID == 'griffaj01' & yearID <= 2014)  
griffaj01.after <- LongBall_pitchDF %>%
  filter(playerID == 'griffaj01' & yearID > 2014) 


#Brian Wilson       4/19/2012   wilsobr01
TJ[118,1:2] 
subset(LongBall_pitchDF, playerID == 'wilsobr01')
wilsobr01.before <- LongBall_pitchDF %>%
  filter(playerID == 'wilsobr01' & yearID <= 2012)  
wilsobr01.after <- LongBall_pitchDF %>%
  filter(playerID == 'wilsobr01' & yearID > 2012) 
#Fernando Rodriguez       3/27/2013   rodrife02
TJ[91,1:2] 
subset(LongBall_pitchDF, playerID == 'rodrife02')
rodrife02.before <- LongBall_pitchDF %>%
  filter(playerID == 'rodrife02' & yearID <= 2013)  
rodrife02.after <- LongBall_pitchDF %>%
  filter(playerID == 'rodrife02' & yearID > 2013) 
#Doug Creek       6/15/2003   creekdo01
TJ[261,1:2] 
subset(LongBall_pitchDF, playerID == 'creekdo01')
creekdo01.before <- LongBall_pitchDF %>%
  filter(playerID == 'creekdo01' & yearID <= 2003)  
creekdo01.after <- LongBall_pitchDF %>%
  filter(playerID == 'creekdo01' & yearID > 2003) 
#Tim Collins       3/11/2015    colliti01
TJ[41,1:2] 
#subset(LongBall_pitchDF, playerID == 'colliti01')
#colliti01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'colliti01' & yearID <= 2015)  
#colliti01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'colliti01' & yearID > 2015) 
#Rocky Coppinger        1/1/2000    coppiro01
TJ[308, 1:2]
subset(LongBall_pitchDF, playerID == 'coppiro01')
coppiro01.before <- LongBall_pitchDF %>%
  filter(playerID == 'coppiro01' & yearID <= 2000)  
coppiro01.after <- LongBall_pitchDF %>%
  filter(playerID == 'coppiro01' & yearID > 2000) 
#Anthony Reyes       6/12/2009    reyesan01
TJ[165, 1:2]
#subset(LongBall_pitchDF, playerID == 'reyesan01')
#reyesan01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'reyesan01' & yearID <= 2009)  
#reyesan01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'reyesan01' & yearID > 2009) 
#Gavin Floyd        5/8/2013    floydga01
TJ[86, 1:2]
subset(LongBall_pitchDF, playerID == 'floydga01')
floydga01.before <- LongBall_pitchDF %>%
  filter(playerID == 'floydga01' & yearID <= 2013)  
floydga01.after <- LongBall_pitchDF %>%
  filter(playerID == 'floydga01' & yearID > 2013) 
#Josh Kinney       3/13/2007    kinnejo01
TJ[205, 1:2]
subset(LongBall_pitchDF, playerID == 'kinnejo01')
kinnejo01.before <- LongBall_pitchDF %>%
  filter(playerID == 'bernead01' & yearID <= 2007)  
kinnejo01.after <- LongBall_pitchDF %>%
  filter(playerID == 'kinnejo01' & yearID > 2007) 
#Jose Arredondo        2/2/2010   arredjo01
TJ[155, 1:2]
subset(LongBall_pitchDF, playerID == 'arredjo01')
arredjo01.before <- LongBall_pitchDF %>%
  filter(playerID == 'arredjo01' & yearID <= 2010)  
arredjo01.after <- LongBall_pitchDF %>%
  filter(playerID == 'arredjo01' & yearID > 2010) 
#Logan Kensing       8/31/2006    kensilo01
TJ[213, 1:2]
subset(LongBall_pitchDF, playerID == 'kensilo01')
kensilo01.before <- LongBall_pitchDF %>%
  filter(playerID == 'kensilo01' & yearID <= 2006)  
kensilo01.after <- LongBall_pitchDF %>%
  filter(playerID == 'kensilo01' & yearID > 2006) 


#Zach Miner       5/28/2010   minerza01
TJ[151,1:2] 
subset(LongBall_pitchDF, playerID == 'minerza01')
minerza01.before <- LongBall_pitchDF %>%
  filter(playerID == 'minerza01' & yearID <= 2010)  
minerza01.after <- LongBall_pitchDF %>%
  filter(playerID == 'minerza01' & yearID > 2010) 
#Felix Doubront       4/12/2016   doubrfe01
TJ[15,1:2] 
#subset(LongBall_pitchDF, playerID == 'doubrfe01')
#doubrfe01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'doubrfe01' & yearID <= 2016)  
#doubrfe01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'doubrfe01' & yearID > 2016) 
#Rod Beck        1/1/2001   beckro01
TJ[294,1:2] 
subset(LongBall_pitchDF, playerID == 'beckro01')
beckro01.before <- LongBall_pitchDF %>%
  filter(playerID == 'beckro01' & yearID <= 2001)  
beckro01.after <- LongBall_pitchDF %>%
  filter(playerID == 'beckro01' & yearID > 2001) 
#Yu Darvish       3/17/2015   darviyu01
TJ[40,1:2] 
subset(LongBall_pitchDF, playerID == 'darviyu01')
darviyu01.before <- LongBall_pitchDF %>%
  filter(playerID == 'darviyu01' & yearID <= 2015)  
darviyu01.after <- LongBall_pitchDF %>%
  filter(playerID == 'darviyu01' & yearID > 2015) 
#Odalis Perez        1/1/2000   perezod01
TJ[304, 1:2]
subset(LongBall_pitchDF, playerID == 'perezod01')
perezod01.before <- LongBall_pitchDF %>%
  filter(playerID == 'perezod01' & yearID <= 2000)  
perezod01.after <- LongBall_pitchDF %>%
  filter(playerID == 'perezod01' & yearID > 2000) 
#Frank Herrmann       3/13/2013   herrmfr01
TJ[92, 1:2]
subset(LongBall_pitchDF, playerID == 'herrmfr01')
herrmfr01.before <- LongBall_pitchDF %>%
  filter(playerID == 'herrmfr01' & yearID <= 2013)  
herrmfr01.after <- LongBall_pitchDF %>%
  filter(playerID == 'herrmfr01' & yearID > 2013) 
# Danys Baez      10/26/2007    baezda01
TJ[188, 1:2]
subset(LongBall_pitchDF, playerID == 'baezda01')
baezda01.before <- LongBall_pitchDF %>%
  filter(playerID == 'baezda01' & yearID <= 2007)  
baezda01.after <- LongBall_pitchDF %>%
  filter(playerID == 'baezda01' & yearID > 2007) 
#Jason Grilli        1/1/2002   grillja01
TJ[280, 1:2]
subset(LongBall_pitchDF, playerID == 'grillja01')
grillja01.before <- LongBall_pitchDF %>%
  filter(playerID == 'grillja01' & yearID <= 2002)  
grillja01.after <- LongBall_pitchDF %>%
  filter(playerID == 'grillja01' & yearID > 2002) 
#Neftali Feliz        8/1/2012    felizne01
TJ[97, 1:2]
subset(LongBall_pitchDF, playerID == 'felizne01')
felizne01.before <- LongBall_pitchDF %>%
  filter(playerID == 'felizne01' & yearID <= 2012)  
felizne01.after <- LongBall_pitchDF %>%
  filter(playerID == 'felizne01' & yearID > 2012) 
#Darren Dreifort        7/9/2001    dreifda01
TJ[287, 1:2]
subset(LongBall_pitchDF, playerID == 'dreifda01')
dreifda01.before <- LongBall_pitchDF %>%
  filter(playerID == 'dreifda01' & yearID <= 2001)  
dreifda01.after <- LongBall_pitchDF %>%
  filter(playerID == 'dreifda01' & yearID > 2001) 


#Luis Ayala       3/30/2006   ayalalu01
TJ[224,1:2] 
subset(LongBall_pitchDF, playerID == 'ayalalu01')
ayalalu01.before <- LongBall_pitchDF %>%
  filter(playerID == 'ayalalu01' & yearID <= 2006)  
ayalalu01.after <- LongBall_pitchDF %>%
  filter(playerID == 'ayalalu01' & yearID > 2006) 
#Juan Pena        1/1/2000    penaju01
TJ[305,1:2] 
#subset(LongBall_pitchDF, playerID == 'penaju01')
#penaju01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'penaju01' & yearID <= 2000)  
#penaju01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'penaju01' & yearID > 2000) 
#Ryan Brasier        6/1/2014   braisry01
#TJ[53,1:2] 
#subset(LongBall_pitchDF, playerID == 'braisry01')
#braisry01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'braisry01' & yearID <= 2014)  
#braisry01.after <- LongBall_pitchDF %>%
 # filter(playerID == 'braisry01' & yearID > 2014)
# went overseas to play (now on Red Sox 2018)
#David Cortes        1/1/2000   corteda01
TJ[303,1:2] 
subset(LongBall_pitchDF, playerID == 'corteda01')
corteda01.before <- LongBall_pitchDF %>%
  filter(playerID == 'corteda01' & yearID <= 2000)  
corteda01.after <- LongBall_pitchDF %>%
  filter(playerID == 'corteda01' & yearID > 2000) 
#Drew Hutchison        8/9/2012   hutchdr01
TJ[96, 1:2]
subset(LongBall_pitchDF, playerID == 'hutchdr01')
hutchdr01.before <- LongBall_pitchDF %>%
  filter(playerID == 'hutchdr01' & yearID <= 2012)  
hutchdr01.after <- LongBall_pitchDF %>%
  filter(playerID == 'hutchdr01' & yearID > 2012) 
#Carlos Carrasco       9/14/2011    carraca01
TJ[129, 1:2]
subset(LongBall_pitchDF, playerID == 'carraca01')
carraca01.before <- LongBall_pitchDF %>%
  filter(playerID == 'carraca01' & yearID <= 2011)  
carraca01.after <- LongBall_pitchDF %>%
  filter(playerID == 'carraca01' & yearID > 2011) 
#Lance Painter       10/1/2001    paintla01
TJ[283, 1:2]
subset(LongBall_pitchDF, playerID == 'paintla01')
paintla01.before <- LongBall_pitchDF %>%
  filter(playerID == 'paintla01' & yearID <= 2001)  
paintla01.after <- LongBall_pitchDF %>%
  filter(playerID == 'paintla01' & yearID > 2001) 
#Cla Meredith        3/2/2011   meredcl01
TJ[141, 1:2]
#subset(LongBall_pitchDF, playerID == 'meredcl01')
#meredcl01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'meredcl01' & yearID <= 2011)  
#meredcl01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'meredcl01' & yearID > 2011) 
#Jason Vargas        8/5/2015   vargaja01
TJ[23, 1:2]
subset(LongBall_pitchDF, playerID == 'vargaja01')
vargaja01.before <- LongBall_pitchDF %>%
  filter(playerID == 'vargaja01' & yearID <= 2015)  
vargaja01.after <- LongBall_pitchDF %>%
  filter(playerID == 'vargaja01' & yearID > 2015) 
#Dan Giese        6/9/2009    gieseda01
TJ[167, 1:2]
#subset(LongBall_pitchDF, playerID == 'gieseda01')
#gieseda01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'gieseda01' & yearID <= 2009)  
#gieseda01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'gieseda01' & yearID > 2009) 


#Casey Kelly        4/2/2013    kellyca01
TJ[90,1:2] 
subset(LongBall_pitchDF, playerID == 'kellyca01')
kellyca01.before <- LongBall_pitchDF %>%
  filter(playerID == 'kellyca01' & yearID <= 2013)  
kellyca01.after <- LongBall_pitchDF %>%
  filter(playerID == 'kellyca01' & yearID > 2013) 
#Michael Gonzalez       5/31/2007   gonzami02
TJ[200,1:2]
subset(LongBall_pitchDF, playerID == 'gonzami02')
gonzami01.before <- LongBall_pitchDF %>%
  filter(playerID == 'gonzami02' & yearID <= 2007)  
gonzami01.after <- LongBall_pitchDF %>%
  filter(playerID == 'gonzami02' & yearID > 2007) 
#Chris Carpenter       7/24/2007    carpech01
TJ[196,1:2] 
subset(LongBall_pitchDF, playerID == 'carpech01')
carpech01.before <- LongBall_pitchDF %>%
  filter(playerID == 'carpech01' & yearID <= 2007)  
carpech01.after <- LongBall_pitchDF %>%
  filter(playerID == 'carpech01' & yearID > 2007) 
#Sean Burnett        6/5/2014   burnese01
TJ[51,1:2] 
subset(LongBall_pitchDF, playerID == 'burnese01')
burnese01.before <- LongBall_pitchDF %>%
  filter(playerID == 'burnese01' & yearID <= 2014)  
burnese01.after <- LongBall_pitchDF %>%
  filter(playerID == 'burnese01' & yearID > 2014) 
#Brian Anderson       7/14/2006   anderbr02
TJ[214, 1:2]
#subset(LongBall_pitchDF, playerID == 'anderbr02')
#anderbr02.before <- LongBall_pitchDF %>%
#  filter(playerID == 'anderbr02' & yearID <= 2006)  
#anderbr02.after <- LongBall_pitchDF %>%
#  filter(playerID == 'anderbr02' & yearID > 2006) 
#Tyler Chatwood       7/23/2014   chatwty01
TJ[48, 1:2]
subset(LongBall_pitchDF, playerID == 'chatwty01')
chatwty01.before <- LongBall_pitchDF %>%
  filter(playerID == 'chatwty01' & yearID <= 2014)  
chatwty01.after <- LongBall_pitchDF %>%
  filter(playerID == 'chatwty01' & yearID > 2014) 
#Jason Marquis       7/30/2013    marquja01
TJ[78, 1:2]
subset(LongBall_pitchDF, playerID == 'marquja01')
marquja01.before <- LongBall_pitchDF %>%
  filter(playerID == 'marquja01' & yearID <= 2013)  
marquja01.after <- LongBall_pitchDF %>%
  filter(playerID == 'marquja01' & yearID > 2013) 
#Denny Neagle       7/30/2003   neaglde01
TJ[258, 1:2]
#subset(LongBall_pitchDF, playerID == 'neaglde01')
#neaglde01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'neaglde01' & yearID <= 2003)  
#neaglde01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'neaglde01' & yearID > 2003) 
#Chris Withrow        6/3/2014    withrch01
TJ[52, 1:2]
subset(LongBall_pitchDF, playerID == 'withrch01')
withrch01.before <- LongBall_pitchDF %>%
  filter(playerID == 'withrch01' & yearID <= 2014)  
withrch01.after <- LongBall_pitchDF %>%
  filter(playerID == 'withrch01' & yearID > 2014) 
#J.B. Wendelken      10/12/2016   wendejb01
TJ[2, 1:2]
#subset(LongBall_pitchDF, playerID == 'wendejb01')
#wendejb01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'wendejb01' & yearID <= 2016)  
#wendejb01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'wendejb01' & yearID > 2016) 


#Francisco Liriano       11/6/2006    liriafr01
TJ[208,1:2] 
subset(LongBall_pitchDF, playerID == 'liriafr01')
liriafr01.before <- LongBall_pitchDF %>%
  filter(playerID == 'liriafr01' & yearID <= 2006)  
liriafr01.after <- LongBall_pitchDF %>%
  filter(playerID == 'liriafr01' & yearID > 2006) 
#Joey Devine       4/21/2009    devinjo01
TJ[172,1:2] 
subset(LongBall_pitchDF, playerID == 'devinjo01')
devinjo01.before <- LongBall_pitchDF %>%
  filter(playerID == 'devinjo01' & yearID <= 2009)  
devinjo01.after <- LongBall_pitchDF %>%
  filter(playerID == 'devinjo01' & yearID > 2009) 
# Eddie Guardado        9/8/2006    guarded01
TJ[211,1:2] 
subset(LongBall_pitchDF, playerID == 'guarded01')
guarded01.before <- LongBall_pitchDF %>%
  filter(playerID == 'guarded01' & yearID <= 2006)  
guarded01.after <- LongBall_pitchDF %>%
  filter(playerID == 'guarded01' & yearID > 2006) 
#Danny Kolb       6/13/2000   kolbda01
TJ[299,1:2] 
subset(LongBall_pitchDF, playerID == 'kolbda01')
kolbda01.before <- LongBall_pitchDF %>%
  filter(playerID == 'kolbda01' & yearID <= 2000)  
kolbda01.after <- LongBall_pitchDF %>%
  filter(playerID == 'kolbda01' & yearID > 2000) 
#Jordan Zimmermann       8/19/2009    zimmejo02
TJ[157, 1:2]
subset(LongBall_pitchDF, playerID == 'zimmejo02')
zimmejo02.before <- LongBall_pitchDF %>%
  filter(playerID == 'zimmejo02' & yearID <= 2009)  
zimmejo02.after <- LongBall_pitchDF %>%
  filter(playerID == 'zimmejo02' & yearID > 2009) 
#Joel Hanrahan       5/16/2013    hanrajo01
TJ[85, 1:2]
#subset(LongBall_pitchDF, playerID == 'hanrajo01')
#hanrajo01.before <- LongBall_pitchDF %>%
#  filter(playerID == 'hanrajo01' & yearID <= 2013)  
#hanrajo01.after <- LongBall_pitchDF %>%
#  filter(playerID == 'hanrajo01' & yearID > 2013) 
#Carl Pavano        6/5/2007    pavanca01
TJ[199, 1:2]
subset(LongBall_pitchDF, playerID == 'pavanca01')
pavanca01.before <- LongBall_pitchDF %>%
  filter(playerID == 'pavanca01' & yearID <= 2007)  
pavanca01.after <- LongBall_pitchDF %>%
  filter(playerID == 'pavanca01' & yearID > 2007) 
#Adam Ottavino        5/7/2015    ottavad01
TJ[31, 1:2]
subset(LongBall_pitchDF, playerID == 'ottavad01')
ottavad01.before <- LongBall_pitchDF %>%
  filter(playerID == 'ottavad01' & yearID <= 2015)  
ottavad01.after <- LongBall_pitchDF %>%
  filter(playerID == 'ottavad01' & yearID > 2015) 
#Brian Anderson       7/21/2005   anderbr02
TJ[230, 1:2]
#subset(LongBall_pitchDF, playerID == 'anderbr02')
#anderbr02.before <- LongBall_pitchDF %>%
#  filter(playerID == 'anderbr02' & yearID <= 2005)  
#anderbr02.after <- LongBall_pitchDF %>%
#  filter(playerID == 'anderbr02' & yearID > 2005) 
#Billy Wagner       9/10/2008   wagnebi02
TJ[177, 1:2]
subset(LongBall_pitchDF, playerID == 'wagnebi02')
wagnebi02.before <- LongBall_pitchDF %>%
  filter(playerID == 'wagnebi02' & yearID <= 2008)  
wagnebi02.after <- LongBall_pitchDF %>%
  filter(playerID == 'wagnebi02' & yearID > 2008) 
View(wagnebi02.before)

BEFORE_DF <- as.data.frame(rbind(hitch.before,sor.before,zag.before,ofla.before,wolf.before,stras.before,milton.before,nolinse01.before,chrisja01.before,morenor01.before,bondeje01.before,harvema01.before,anderbr01.before,zambrvi01.before,doteloc01.before,trabebi01.before,spurlch01.before,wainwad01.before,rodnefe01.before,delarru01.before,matsuda01.before,moylape01.before,hudsoda01.before,marcush01.before,brocado01.before,maysjo01.before,beachbr01.before,volqued01.before,hochelu01.before,kellesh01.before,radinsc01.before,kohnmi01.before,chenbr01.before,moyerja01.before,wisema01.before,griffaj01.before,wilsobr01.before,rodrife02.before,creekdo01.before,coppiro01.before,floydga01.before,kinnejo01.before,arredjo01.before,kensilo01.before,minerza01.before,beckro01.before,darviyu01.before,perezod01.before,herrmfr01.before,baezda01.before,grillja01.before,felizne01.before,dreifda01.before,ayalalu01.before,corteda01.before,hutchdr01.before,carraca01.before,paintla01.before,vargaja01.before,kellyca01.before,gonzami01.before,carpech01.before,burnese01.before,chatwty01.before,marquja01.before,withrch01.before,liriafr01.before,devinjo01.before,guarded01.before,kolbda01.before,zimmejo02.before,pavanca01.before,ottavad01.before,wagnebi02.before))
View(BEFORE_DF)

AFTER_DF <- as.data.frame(rbind(hitch.after,sor.after,zag.after,ofla.after,wolf.after,stras.after,milton.after,nolinse01.after,chrisja01.after,morenor01.after,bondeje01.after,harvema01.after,anderbr01.after,zambrvi01.after,doteloc01.after,trabebi01.after,spurlch01.after,wainwad01.after,rodnefe01.after,delarru01.after,matsuda01.after,moylape01.after,hudsoda01.after,marcush01.after,brocado01.after,maysjo01.after,beachbr01.after,volqued01.after,hochelu01.after,kellesh01.after,radinsc01.after,kohnmi01.after,chenbr01.after,moyerja01.after,wisema01.after,griffaj01.after,wilsobr01.after,rodrife02.after,creekdo01.after,coppiro01.after,floydga01.after,kinnejo01.after,arredjo01.after,kensilo01.after,minerza01.after,beckro01.after,darviyu01.after,perezod01.after,herrmfr01.after,baezda01.after,grillja01.after,felizne01.after,dreifda01.after,ayalalu01.after,corteda01.after,hutchdr01.after,carraca01.after,paintla01.after,vargaja01.after,kellyca01.after,gonzami01.after,carpech01.after,burnese01.after,chatwty01.after,marquja01.after,withrch01.after,liriafr01.after,devinjo01.after,guarded01.after,kolbda01.after,zimmejo02.after,pavanca01.after,ottavad01.after,wagnebi02.after))
View(AFTER_DF)

#get career stats, randomly select years to analyze, comment out players that do not return from injury

#ALL_Before_ERA <- c(stras.before$ERA, hitch.before$ERA, sor.before$ERA, zag.before$ERA, ofla.before$ERA, wolf.before$ERA)
#ALL_After_ERA <- c(stras.after$ERA, hitch.after$ERA, sor.after$ERA, zag.after$ERA, ofla.after$ERA, wolf.after$ERA)
#mean(ALL_Before_ERA)
#mean(ALL_After_ERA)

#ALL_Before_SOper9 <- c(stras.before$SOper9, hitch.before$SOper9, sor.before$SOper9, zag.before$SOper9, ofla.before$SOper9, wolf.before$SOper9)
#ALL_After_SOper9 <- c(stras.after$SOper9, hitch.after$SOper9, sor.after$SOper9, zag.after$SOper9, ofla.after$SOper9, wolf.after$SOper9)
#mean(ALL_Before_SOper9)
#mean(ALL_After_SOper9)

#ALL_Before_Hper9 <- c(stras.before$Hper9, hitch.before$Hper9, sor.before$Hper9, zag.before$Hper9, ofla.before$Hper9, wolf.before$Hper9)
#ALL_After_Hper9 <- c(stras.after$Hper9, hitch.after$Hper9, sor.after$Hper9, zag.after$Hper9, ofla.after$Hper9, wolf.after$Hper9)
#mean(ALL_Before_Hper9)
#mean(ALL_After_Hper9)

#ALL_Before_HRper9 <- c(stras.before$HRper9, hitch.before$HRper9, sor.before$HRper9, zag.before$HRper9, ofla.before$HRper9, wolf.before$HRper9)
#ALL_After_HRper9 <- c(stras.after$HRper9, hitch.after$HRper9, sor.after$HRper9, zag.after$HRper9, ofla.after$HRper9, wolf.after$HRper9)
#mean(ALL_Before_HRper9)
#mean(ALL_After_HRper9)
#summary(ALL_Before_HRper9)
#summary(ALL_After_HRper9)

#ALL_Before_BBper9 <- c(stras.before$BBper9, hitch.before$BBper9, sor.before$BBper9, zag.before$BBper9, ofla.before$BBper9, wolf.before$BBper9)
#ALL_After_BBper9 <- c(stras.after$BBper9, hitch.after$BBper9, sor.after$BBper9, zag.after$BBper9, ofla.after$BBper9, wolf.after$BBper9)
#mean(ALL_Before_BBper9)
#mean(ALL_After_BBper9)

#Before.DF <- as.data.frame(cbind(ALL_Before_ERA, ALL_Before_BBper9, ALL_Before_Hper9, ALL_Before_HRper9, ALL_Before_SOper9))
#After.DF <- as.data.frame(cbind(ALL_After_ERA, ALL_After_BBper9, ALL_After_Hper9, ALL_After_HRper9, ALL_After_SOper9))

# want ERA, BBper9, Hper9, SOper9, HRper9

attach(BEFORE_DF)
attach(AFTER_DF)
library('doBy')
summaryBy(SOper9+BBper9+HRper9+Hper9+ERA ~ G, data = BEFORE_DF)
summaryBy(SOper9+BBper9+HRper9+Hper9+ERA ~ G, data = AFTER_DF)

BEFORE_DF$playerID <- NULL
BEFORE_DF$teamID <- NULL
BEFORE_DF$lgID <- NULL
BEFORE_DF$stint <- NULL
BEFORE_DF$yearID <- NULL

AFTER_DF$playerID <- NULL
AFTER_DF$teamID <- NULL
AFTER_DF$lgID <- NULL
AFTER_DF$stint <- NULL
AFTER_DF$yearID <- NULL

trainSize1 <- 0.75
trainInd1 <- sample(1:nrow(BEFORE_DF), size = floor(nrow(BEFORE_DF) * trainSize1))       
train1 <- BEFORE_DF[trainInd1,]
test1 <- BEFORE_DF[-trainInd1,]

View(train1)
View(BEFORE_DF)

trainSize2 <- 0.75
trainInd2 <- sample(1:nrow(AFTER_DF), size = floor(nrow(AFTER_DF) * trainSize2))       
train2 <- AFTER_DF[trainInd2,]
test2 <- AFTER_DF[-trainInd2,]



library("corrplot")
CorB <- cor(train1)
CorB
corb <- cor(train1$ERA, train1)
corb
corrplot(train1)

cora <- cor(AFTER_DF)
cora1 <- cor(ERA,AFTER_DF)

# LM against ERA and then calculating MSE 

lm.before <- lm(ERA~Hper9+HRper9+BBper9+SOper9, data = train1)
lm.after <- lm(ERA~Hper9+BAOpp+HRper9+SOper9, data = train2)
summary(lm.before)
summary(lm.after)
predict_lm <- predict(lm.before)
predict_lm2 <- predict(lm.after)
MSE <- function(yhat, ytrue)
  mean((yhat - ytrue)^2)   
MSE_lm_before <- log(MSE(ERA, predict_lm))
MSE_lm_after <- log(MSE(ERA, predict_lm2))
MSE_lm_after
MSE_lm_before
#MSE Before = 3.326, After = 3.068

# building Lasso Model
#BEFORE_DF
formula1 <- formula(ERA~Hper9+HRper9+BBper9+SOper9, data = train1)
Xvar <- build.x(formula1, data = train1)
Yvar <- build.y(formula1, data = train1)
LassoFit1 <- cv.glmnet(Xvar, Yvar, alpha = 1)
plot(LassoFit1)
coef(LassoFit1, s = "lambda.min")
coef(LassoFit1, s = "lambda.1se")

#AFTER_DF
formula2 <- formula(ERA~Hper9+BAOpp+HRper9+SOper9, data = train2)
Xvar1 <- build.x(formula2, data = train2)
Yvar1 <- build.y(formula2, data = train2)
LassoFit2 <- cv.glmnet(Xvar1, Yvar1, alpha = 1)
plot(LassoFit2)
coef(LassoFit2, s = "lambda.min")
coef(LassoFit2, s = "lambda.1se")


# forward stepwise model
library('leaps')
before_fregress <-  regsubsets(ERA~., data = train1, nvmax = 4, method = 'forward')
summary(before_fregress)
# Hper9, BBper9, HRper9, and BAOpp were most significant (in that order)

after_fregress <- regsubsets(ERA~., data = train2, nvmax = 4, method = 'forward')
summary(after_fregress)
# Hper9, BBper9, BAOpp, and SOper9 were most significant (in that order)

#build Tree model
library("rpart")
library("tree")
tree.before = tree(ERA~Hper9+BBper9+HRper9+BAOpp, data = train1)
tree.after = tree(ERA~Hper9+BBper9+BAOpp+SOper9, data = train2)
summary(tree.before)
plot(tree.before)
text(tree.before)
plot(tree.after)
text(tree.after)


#calculating MSE 
MSE = function(yhat, ytrue){
  mean((yhat - ytrue)^2)
}
ERA_predict_before <- predict(tree.before, data = test1)
ERA_predict_after <- predict(tree.after, newdata = test2)

ERA_predict_before
ERA_predict_after


ggplot(train1, aes(ERA,Hper9)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("Before - Hits per 9 innings vs ERA")
ggplot(train2, aes(ERA,Hper9)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("After - Hits per 9 innings vs ERA")
ggplot(train1, aes(ERA,SOper9)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("Before - Strikeouts per 9 innings vs ERA")
ggplot(train2, aes(ERA,SOper9)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("After - Strikeouts per 9 innings vs ERA")
ggplot(train1, aes(ERA,BBper9)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("Before - Walks per 9 innings vs ERA")
ggplot(train2, aes(ERA,BBper9)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("After - Walks per 9 innings vs ERA")
ggplot(train1, aes(ERA,HRper9)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("Before - Home Runs per 9 innings vs ERA")
ggplot(train2, aes(ERA,HRper9)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("After - Home Runs per 9 innings vs ERA")
ggplot(train1, aes(ERA,BAOpp)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("Before - Batting Average Against vs ERA")
ggplot(train2, aes(ERA,BAOpp)) + geom_point(color = "blue") + coord_cartesian(ylim =c(0,20), xlim = c(0,15)) + geom_smooth() + ggtitle("After - Batting Average Against vs ERA")



