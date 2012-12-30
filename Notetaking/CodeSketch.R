
# initial code for the Note Taking problem
# How much time should be spent taking notes in class?
# Assume a quadratic relationship beween an activity and learning.

a = -200/100^2
b = 200^2/100^2
c = 0

d = a
e = b
f = c

Learning.Notes = function(a, b, c, x) {
  a*x^2 + b*x + c
}

Learning.Other = function(d, e, f, y) {
  d*y^2 + e*y + f
}

Learning = function(a, b, c, d, e, f, x) {
  Learning.Notes(a, b, c, x) + Learning.Other(d, e, f, (100-x))
}

xs = 0:100
par(mfrow = c(2, 2))
plot(xs, Learning.Notes(a, b, c, xs), type = "l",
     main = "Learning from Notes",
     xlab = "Time taking notes",
     ylab = "Learning")

plot(xs, Learning.Other(d, e, f, (100-xs)), type = "l",
     main = "Learning from Other Activities",
     xlab = "Time doing other activities",
     ylab = "Learning")

plot(xs, Learning(a, b, c, d, e, f, xs), type = "l",
     ylim = c(0, max(Learning(a, b, c, d, e, f, xs))),
     main = "Learning from All Activities",
     xlab = "Time taking notes",
     ylab = "Learning")
lines(xs, Learning.Notes(a, b, c, xs), col = "blue")
lines(xs, Learning.Other(d, e, f, (100-xs)), col = "red")

xstar = (e - b + 200*d)/2/(a+d)
lines(c(xstar, xstar), c(0, max(Learning(a, b, c, d, e, f, xs))))

# Marginal Plot
MB = function(a, b, x) {
  2*a*x + b
}

MC = function(d, e, y) {
  2*d*y + e
}

MBs = MB(a, b, xs)
MCs = MC(d, e, (100-xs))
plot(c(0, 100), range(c(MBs, MCs)), type = "l",
     main = "Marginal Benefit and Cost of Note Taking",
     xlab = "Time taking notes",
     ylab = "Learning / Time taking notes"
     )
lines(xs, MBs, col = "blue")
lines(xs, MCs, col = "red")
