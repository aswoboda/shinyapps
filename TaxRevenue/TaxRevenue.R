
a = 50; b = .01
c = 0; d = .01

TR = function(TAX) TAX^2*(1/(-d-b))+TAX*((c-a)/(-d-b))

TR(0:50)

plot(0:50, TR(0:50))


QuadForm = function(a, b, c) {
  root1 = (-b + sqrt(b^2 - 4*a*c))/(2*a)
  root2 = (-b - sqrt(b^2 - 4*a*c))/(2*a)
  cbind(root1, root2)
}
  
QuadForm(1/(-d-b), (c-a)/(-d-b), -20000)
