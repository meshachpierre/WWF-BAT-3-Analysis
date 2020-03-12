#CODE FOR BOOTS PLOT OF RAIs
#pd=Position_Dodge???
  
  
boots_C<-read.table('Boots_AllLoc2015_Carn.txt', header=T)
boots_S<-read.table('Boots_AllLoc2015_PreyS2.txt', header=T)
boots_L<-read.table('Boots_AllLoc2015_PreyL2.txt', header=T)

boots_C<-read.table('Boots_AllLoc2015_Carn_RUPS.txt', header=T)
boots_L<-read.table('Boots_AllLoc2015_Larg_RUPS.txt', header=T)
boots_S<-read.table('Boots_AllLoc2015_Small_RUPS.txt', header=T)


pd <- position_dodge(.8)
Carn<-ggplot(boots_C,aes(Species, Mean, color=Location))+
  geom_point(position=pd)+
  geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+
  theme_bw() + 
  opts(axis.text.x=theme_text(angle=90,hjust=1))

ggplot(boots_S,aes(Species, Mean, color=Location))+
  geom_point(position=pd)+
  geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+
  theme_bw() + 
  opts(axis.text.x=theme_text(angle=90,hjust=1))

ggplot(boots_L,aes(Species, Mean, color=Location))+
  geom_point(position=pd)+
  geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+
  theme_bw() + 
  opts(axis.text.x=theme_text(angle=90,hjust=1))

#PRESENTATION FIGURES

pd <- position_dodge(.6)
carn<-ggplot(boots_C,aes(colour=Location,y=Mean,x=Species))+geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max),width=0.4,position=pd)+theme(axis.ticks.x=element_blank(),axis.text.x=element_text(angle=45,hjust=1),panel.background=element_rect(fill="transparent", color=NA))

carn2<-carn+scale_colour_discrete(name=("Locations"))
carn3<-carn2+scale_colour_manual(values=c("darkkhaki","darkolivegreen3","darkred","firebrick"), breaks=c("KBO","DDA","KUS","PAR"))



pd <- position_dodge(.6)
small<-ggplot(boots_S,aes(colour=Location,y=Mean,x=Species))+geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max),width=0.4,position=pd)+theme(axis.ticks.x=element_blank(),axis.text.x=element_text(angle=45,hjust=1),panel.background=element_rect(fill="transparent", color=NA))

small2<-small+scale_colour_discrete(name=("Locations"))
small3<-small2+scale_colour_manual(values=c("darkkhaki","darkolivegreen3","darkred","firebrick"), breaks=c("KBO","DDA","KUS","PAR"))

large<-ggplot(boots_L,aes(colour=Location,y=Mean,x=Species))+geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max),width=0.4,position=pd)+theme(axis.ticks.x=element_blank(),axis.text.x=element_text(angle=45,hjust=1),panel.background=element_rect(fill="transparent", color=NA))

large2<-large+scale_colour_discrete(name=("Locations"))
large3<-large2+scale_colour_manual(values=c("darkkhaki","darkolivegreen3","darkred","firebrick"), breaks=c("KBO","DDA","KUS","PAR"))








#Figure out the 'theme' function'
ggplot(boots_L,aes(Species, Mean, color=Location))+
  geom_point(position=pd)+
  geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_text(angle=90,hjust=1),panel.background=element_rect(fill="transparent", color=NA),plot.background=element_rect(fill="transparent",color=NA))+geom_vline(xintercept=1:3+0.5,linetype=1,size=0.1,color="grey20")

ggplot(boots_C,aes(Species, Mean, color=Location))+
  geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+
  theme(axis.ticks.x=element_blank(),axis.text.x=element_text(angle=90,hjust=1),panel.background=element_rect(fill="transparent", color=NA),plot.background=element_rect(fill="transparent",color=NA))+geom_vline(xintercept=1:3+0.5,linetype=1,size=0.1,color="grey20")


ggplot(boots_L,aes(Species, Mean, color=Hunt))+
  geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+
  theme_bw() + 
  opts(axis.text.x=theme_text(angle=90,hjust=1))
ggplot(boots_S,aes(Species, Mean, color=Hunt))+
  geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+
  theme_bw() + 
  opts(axis.text.x=theme_text(angle=90,hjust=1))
ggplot(boots_C,aes(Species, Mean, color=Hunt))+
  geom_point(position=pd)+
  geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+
  theme_bw() + 
  opts(axis.text.x=theme_text(angle=90,hjust=1))



Re-categorizing hunting pressure
boots_C<-read.table('Boots_AllLoc2015_Carn2.txt', header=T)
boots_S<-read.table('Boots_AllLoc2015_PreyS2.txt', header=T)
boots_L<-read.table('Boots_AllLoc2015_PreyL2.txt', header=T)
pd <- position_dodge(.5)

LargePrey<-ggplot(boots_L,aes(Species, Mean, color=Location))+geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+theme_bw() + opts(axis.text.x=theme_text(angle=90,hjust=1))

LargePrey+scale_colour_discrete(name=("Hunting pressure"))+scale_colour_manual(values=c("goldenrod","firebrick","forestgreen"),breaks=c("Me","Lo","VL"),labels=c("Medium","Low","Very low"))

SmallPrey<-ggplot(boots_S,aes(Species, Mean, color=Hunt2))+geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+theme_bw() + opts(axis.text.x=theme_text(angle=90,hjust=1))
SmallPrey+scale_colour_discrete(name=("Hunting pressure"))+scale_colour_manual(values=c("goldenrod","firebrick","forestgreen"),breaks=c("Me","Lo","VL"),labels=c("Medium","Low","Very low"))

Predator<-ggplot(boots_C,aes(Species, Mean, color=Hunt2))+geom_point(position=pd)+geom_errorbar(aes(ymin=Min,ymax=Max,position=pd),width=1.0,position=pd)+theme_bw() + opts(axis.text.x=theme_text(angle=90,hjust=1))

Predator+scale_colour_discrete(name=("Hunting pressure"))+scale_colour_manual(values=c("goldenrod","firebrick","forestgreen"),breaks=c("Me","Lo","VL"),labels=c("Medium","Low","Very low"))

