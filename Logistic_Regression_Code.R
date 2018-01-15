#load the csv file into R
dataSet <- read.csv("Arrowsmith.csv", header = T, na.strings ="?")
summary(dataSet)
dim(dataSet)

#construct variables
asSearch <- dataSet$Arrowsmith.search
aLitSize <- dataSet$A.lit.size
cLitSize <- dataSet$C.lit.size
target <- dataSet$target
nA <- dataSet$nA
nC <- dataSet$nC
nMesh <- dataSet$nof.MeSH.in.common
nSeman <- dataSet$nof.semantic.categories
cohesion <- dataSet$cohesion.score
nMedline <- dataSet$n.in.MEDLINE
firstYrMedline <- dataSet$X1st.year.in.MEDLINE
pAc <- dataSet$pAC
onMediStop <- dataSet$on.medium.stoplist.
onLongStop <- dataSet$on.long.stoplist.

#construct attributes
x1 <- ifelse((nA > 1 | aLitSize < 1000) & (nC > 1 | cLitSize < 1000), 1, 0)
x2 <- ifelse(nMesh > 0 & Mesh < 99999, 1 ,ifelse(Mesh == 99999, 0.5, 0))
x3 <- ifelse(nSeman > 0, 1, 0)
x4 <- ifelse(cohesion < 0.3, cohesion, 0.3)
x5 <- -abs(log10(nMedline) - 3)
x6 <- ifelse(firstYrMedline > 2005, 2005, ifelse(firstYrMedline > 1950, firstYrMedline, 1950))
x7 <- ifelse(-log10(pAc + 0.000000001) > 8, 8, -log10(pAc + 0.000000001))

I1 <- ifelse(asSearch =='retinal detachment vs aortic aneurysm', 1, 0)
I2 <- ifelse(asSearch =='NO and mitochondria vs PSD', 1, 0)
I3 <- ifelse(asSearch =='mGluR5 vs lewy bodies', 1, 0)
I4 <- ifelse(asSearch =='magnesium vs migraine', 1, 0)
I5 <- ifelse(asSearch =='Calpain vs PSD', 1, 0)
I6 <- ifelse(asSearch =='APP vs reelin', 1, 0)

y <- ifelse(target == 0 | target == 2, 1, 0)

# create a datafram with raw data 
df1 <- data.frame(target, aLitSize, cLitSize, nA, nC, nMesh, 
                  nSeman, cohesion, nMedline, firstYrMedline, 
                  pAc, onMediStop, onLongStop)

# create a new dataframe with x,y,i
df2 <- data.frame(y, x1, x2, x3,x4,x5,x6,x7,I1,I2,I3,I4,I5,I6)

# create a multiple linear regression with x,y
asGlm <- glm(y~x1+x2+x3+x4+x5+x6+x7+I1+I2+I3+I4+I5+I6, family ='binomial')
summary(asGlm)

#summary statistics
summary(X1)
summary(X2)
summary(X3)
summary(X4)
summary(X5)
summary(X6)
summary(X7)
summary(I1)
summary(I2)
summary(I3)
summary(I4)
summary(I5)
summary(I6)
summary(I6)
summary(asSearch)
summary(aLitSize)
summary(cLitSize)









#histograms
hist(x1)
hist(x2)
hist(x3)
hist(x4)
hist(x5)
hist(x6)
hist(x7)
hist(I1)
hist(I2)
hist(I3)
hist(I4)
hist(I5)
hist(I6)


#Pairwise scatter plots 
pairs(df1[, -1], gap = 0, pch = ".")
pairs(df2[, -1], gap = 0, pch = ".")