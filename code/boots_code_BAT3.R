BAT3<-read.table('BAT3.txt',header=T)
fox<-BAT3$Cthous
crax<-BAT3$Calector
labba<-BAT3$Cpaca
agouti<-BAT3$Dleporina
opos<-BAT3$Dmarsupialis
tayra<-BAT3$Ebarbara
ocelot<-BAT3$Lpardalis
margay<-BAT3$Lwiedii
oncilla<-BAT3$Ltigrinus
bdeer<-BAT3$Mazama
acouchy<-BAT3$Macouchy
anteater<-BAT3$Mtridactyla
coati<-BAT3$Nnasua
sdeer<-BAT3$Oviriginianus
jaguar<-BAT3$Ponca
cpeccari<-BAT3$Ptajacu
garmadillo<-BAT3$Pmaximus
raccoon<-BAT3$Pcancrivorous
rat<-BAT3$Proechimys
puma<-BAT3$Pconcolor
yag<-BAT3$Pyagouaroundi
tapir<-BAT3$Tterrestris
wlp<-BAT3$Tpecari
armadillo<-BAT3$Dasypus

summary(armadillo)
qqnorm(armadillo)
R=1000
n1=length(armadillo)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(armadillo,n1,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(fox)
qqnorm(fox)
R=1000
n1=length(fox)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(fox,n1,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(crax)
n2=length(crax)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(crax,n2,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(labba)
n3=length(labba)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(labba,n3,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(agouti)
n4=length(agouti)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(agouti,n4,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(opos)
n5=length(opos)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(opos,n5,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(tayra)
n6=length(tayra)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(tayra,n6,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(ocelot)
n7=length(ocelot)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(ocelot,n7,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(margay)
n8=length(margay)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(margay,n8,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(oncilla)
n9=length(oncilla)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(oncilla,n9,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(bdeer)
n10=length(bdeer)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(bdeer,n10,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(acouchy)
n11=length(acouchy)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(acouchy,n11,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(anteater)
n12=length(anteater)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(anteater,n12,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(coati)
n13=length(coati)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(coati,n13,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(sdeer)
n14=length(sdeer)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(sdeer,n14,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(jaguar)
n15=length(jaguar)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(jaguar,n15,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(cpeccari)
n16=length(cpeccari)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(cpeccari,n16,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(garmadillo)
n17=length(garmadillo)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(garmadillo,n17,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(raccoon)
n18=length(raccoon)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(raccoon,n18,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(puma)
n19=length(puma)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(puma,n19,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(yag)
n20=length(yag)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(yag,n20,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(rat)
n21=length(rat)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(rat,n21,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(tapir)
n22=length(tapir)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(tapir,n22,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))

summary(wlp)
n23=length(wlp)
boot.means=numeric(R)
for(i in 1:R){
  boot.sample=sample(wlp,n23,replace=T)
  boot.means[i]=mean(boot.sample)
}
quantile(boot.means,c(0.025,0.975))



