
curve(dnorm(x,0,1), xlim=c(-4,4), main="Normal Density z=1.65")
from.z <- -.75
to.z <- 2
S.x <- c(from.z, seq(from.z, to.z, 0.01), to.z)
S.y <- c(0, dnorm(seq(from.z, to.z, 0.01)), 0)
polygon(S.x,S.y, col="red")

