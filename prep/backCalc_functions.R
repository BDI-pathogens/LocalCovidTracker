# zeta is the distribution for the delay from infection to case confirmation
zeta.max <- 35 # hard-code that no delays are ever longer than that
delays.considered <- seq(0, zeta.max)

# incubation period
s.meanlog <- 1.58
s.sdlog   <- 0.47
s <- function(tau) {dlnorm(tau, sdlog = s.sdlog, meanlog = s.meanlog)}
s.discrete <- vapply(delays.considered, s, numeric(1))
s.discrete <- s.discrete / sum(s.discrete)

symptom.to.swab.mean <- 1.2
symptom.to.swab.sd   <- 1
symptom.to.swab.shape <- symptom.to.swab.mean^2 / symptom.to.swab.sd^2
symptom.to.swab.rate <- symptom.to.swab.shape / symptom.to.swab.mean
symptom.to.swab.pdf <- function(t) {
  dgamma(t, shape = symptom.to.swab.shape, rate = symptom.to.swab.rate)
}
symptom.to.swab.discrete <- vapply(delays.considered, symptom.to.swab.pdf, numeric(1))
symptom.to.swab.discrete <- symptom.to.swab.discrete / sum(symptom.to.swab.discrete)



# Zeta is the convolution of s and symptom to swab time.
# Careful: we include day zero but vector indexing is 1-based
zeta <- rep(NA, zeta.max + 1)
for (t in delays.considered) {
  convolution.sum.range <- seq(0, t)
  zeta[[t + 1]] <- sum(s.discrete[convolution.sum.range + 1] *
                         symptom.to.swab.discrete[t - convolution.sum.range + 1])
}
zeta <- zeta / sum(zeta) # unit normalise