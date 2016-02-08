## generate the letters P, C and A in three dimensions, third
## dimension uniform noise, later we will add other dimensions and mix
## the whole stuff so that the letters are no longer visible. Then PCA
## should disentangle the stuff...

P <- matrix(runif(5001), ncol = 3)
botright <- P[,1] > .4 & P[,2] < .4
P <- P[!botright,]
left <- P[,1] < .1
P <- P[!left,]

bigcirc <- P[,1] > .7 & ((P[,1]-.7)^2 + (P[,2] - 0.7)^2 > 0.3^2)
P <- P[!bigcirc,]
smallcirc <- ##P[,1] > .6 &
    ((P[,1]-.6)^2 + (P[,2] - 0.7)^2 < 0.12^2)
P <- P[!smallcirc,]
## plot(P)
## save(P, file = "P.RData")

C <- matrix(runif(5001), ncol = 3)
bigcirc <- (C[,1]-.5)^2 + (C[,2] - 0.5)^2 > 0.5^2
C <- C[!bigcirc,]
smallcirc <- (C[,1]-.5)^2 + (C[,2] - 0.5)^2 < 0.25^2
C <- C[!smallcirc,]
openright <- C[,1] > .7 & (C[,2] > .3 & C[,2] < .7)
##points(C[openright,], col = 2)
C <- C[!openright,]
## save(C, file = "C.RData")

A <- matrix(runif(5001), ncol = 3)
topcirc <- A[,2] > .5 & ((A[,1]-.5)^2 + (A[,2] - .5)^2 > .5^2)
A <- A[!topcirc,]
botblock <- A[,1] > .3 & A[,1] < .7 & A[,2] < .3
A <- A[!botblock,]
topcircsmall <- A[,2] > .5 & ((A[,1]-.5)^2 + (A[,2] - .5)^2 < .2^2)
## points(A[topcircsmall,], col = 2)
A <- A[!topcircsmall,]
## save(A, file = "A.RData")

chardist <- .15
Cprime <- C
Cprime[,1] <- Cprime[,1] + max(P[,1]) + chardist
Aprime <- A
Aprime[,1] <- Aprime[,1] + max(Cprime[,1]) + chardist
PCAdat <- rbind(P,
                Cprime,
                Aprime)
colors <- rep(1:3, c(nrow(P), nrow(C), nrow(A)))
plot(PCAdat, col = colors)
save(PCAdat, colors, file = "PCAletters.RData")

## Now add a rotation matrix so that the letters are scrambled and can
## no longer be distinguished. But first: check whether the letters
## survive a PCA...
library(PCA)

load("PCAletters.RData")
PCAdat[,2] <- PCAdat[,2]*1.2
huhn <- PCA(scale(PCAdat, scale = FALSE))
scoreplot(huhn, col = colors)
## Bingo! Maybe upside down, maybe from right to left, but that only
## serves to indicate how PCA works.

huhn <- cbind(PCAdat, matrix(runif(nrow(PCAdat)*7), ncol = 7))
huhn.PCA <- PCA(scale(huhn, scale = FALSE))
scoreplot(huhn.PCA, col = colors)

library(rgl)
plot3d(scores(huhn.PCA, 3), col = colors)
## Oh! This is nice :-)

## now rotate so that the info is no longer in the first two
## variables. Simple: just choose an axis perpendicular to the plane
## of the first two.
## rotmat <- matrix(c(0.36, -.8, 0.48, 0.48, .6, .64, -.8, 0, .6), ncol = 3)
rotmat <- matrix(c(0, 0, -1, 0, 1, 0, 1, 0, 0), ncol = 3)
## this is just a matrix way to swap two columns...
library(GPArotation)
rotmat <- Random.Start(10)
PCAdat2 <- huhn %*% rotmat
plot(PCAdat2[,2:3], col = colors)

woppa <- PCA(scale(PCAdat2, scale = FALSE))
scoreplot(woppa, col = colors)

PCADATA <- PCAdat2
save(PCADATA, colors, file = "PCADATA.RData")
