#Fitting of the number of protoscoleces per cyst from Torgerson et al.
Age <- 1:6
nCyst <- c(2.6,3.11,2.5,4.61,7.1,10.28)
nProt <- c(21,99,711,1726,6899,9774)

#Fitting the number of protoscoleces per cyst
estProt <- function(param){
  nCystPerYear <- param[1]
  ProtPerCyst <- param[2]
  nProtEst <- nCystPerYear*cumsum(ProtPerCyst*Age^3)
  return(sum(nProt-nProtEst)^2)
}

optim(c(1,10),estProt)

plot(Age, nProt)


nCystPerYear <-  2.21
ProtPerCyst <-  10.68
nCystPerYear*cumsum(ProtPerCyst*Age^3)



(ProtPerCyst*1^3)
