
y <-  "(T+R<->Y)*(A+Y*K<->C)*(C*H+C*I<->E)"
is.inus(y)
x <- "H*T+A*I<->E"
x <- "C*H+I*T<->E"
is_compatible(x,y)
y <- "(C*A+B<->R)*(R+B+X<->Y)*(Y*N+B*n<->L)*(L*a*b+U<->E)"
x <- "A+B<->E"
is_compatible(x,y)
y <- "(A+B<->C)*(C+X<->E)*(E*R*T*x+X*a*b+Y*a*b<->Z)"
x <- "A+X<->Z"
is_compatible(x,y)

y <- "(T+R<->Y)*(A+B<->C)*(t*C+a*Y<->E)"
x <- "A+T<->E"
is_compatible(x,y)
y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(T*C+A<->E)"
x <- "L+T<->E"
is_compatible(x,y)
y <- "(A+B<->C)*(T+R<->Y)*(C*Y+c*y<->E)"
x <-  "t*r+C<->E"
is_compatible(x,y)
y <- "(A*X+B<->C)*(C+A*R<->Y)*(C*a*U+Z*y<->E)"
is.inus(y)
x <- "R<->E"
is_compatible(x,y)
y <- "(A*b+B*a+A*C<->D)*(D+E<->F)"
x <- "A*b+B*a+B*C<->F"
is_compatible(x,y)


y <- "(A+B*F<->C)*(D+B*f<->E)*(C+E*T<->G)"
x <- "(A+B*F<->C)*(D+B*f<->E)*(C+E+F<->G)"
x <- "A+B*F+E<->G"
is_compatible(x,y)
is_compatible(x,y)
is.inus(x, selectCases(y))
y <- "(D*F+a*b<->C)*(F*c<->G)*(B*f+D*c+a*c<->E)"
x <-"D*f+d*B<->E"
is_compatible(x,y)

y <- "(C*e*f+E*F*c+F*c*g<->B)*(B*e+E*G*c<->D)*(F*G+d*e<->A)"

x <- "e*b+F*G<->A"
is_compatible(x,y)

y <- "(E*c*d+a*c+a*d*g<->F)*(A*F+D*E*g+a*d*f<->B)"
x <- "A*F+a*d*G+E*D*g<->B"
is_compatible(x,y)


### is_compatible()
y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(C+U<->E)"
x <- "(L+B<->Y)*(X+Y<->E)"
is_compatible(x,y)

y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(T*C+A<->E)"
x <- "(L+T<->A)*(X+Y<->E)"
is_compatible(x,y)

x <- "L+T<->Y"
x <- "R+C<->E"
is_compatible(x, y)

y <- "(C+D<->E)*(A+B<->C)*(E+F<->G)*(G+H+C<->I)"
x <- "(C+E<->G)*(G+F<->I)"
is_compatible(x,y)
is.inus(x, selectCases(y))

y <- "(A + B <-> C)*(C + Z <-> F)*(C + D <-> E)"
x <- "(A + B <-> F)*(A + D <-> E)"
is_compatible(x,y)


y <- "(A + B*D <->C)*(C+D<->G)"
x <- "(A + B*D <-> C)*(C <->G)"

is_compatible(x,y)

is_compatible(x,y)
substitute_all(y)
c <- getCond(selectCases(substitute_all(y)$lhss))
rreduce(c, x=selectCases(noblanks(y)), full = FALSE)
is_compatible(x,y)

td <- selectCases(target)

x <- "(A+B<->C)*(C+D<->E)*(E+F<->G)*(G+H+C<->I)"
r <- "(E+F<->G)*(A+B<->C)*(C+D<->E)*(G+H+C<->I)"

y <- "(A+B<->C)*(C+D<->E)*(X+F<->G)*(G+H+c<->I)"
x <- "(G+c<->I)*(C+D<->E)"

y <- "(A+B<->C)*(C+D<->E)*(E+F<->G)*(G+H<->I)"
x <- "(A+C<->C)*(C+D<->E)" #malformed model, infinite loop. fix
x <- "(A+B<->C)*(C+D<->E)"
x <- "A+C+H<->I"
is_compatible(x,y)
is_compatible(x,y)
yt2 <- substitute_all(y2)

yt <- substitute_all(y)
is.submodel(yt2, yt)
a <- substitute_all(x)
b <- substitute_all(r)
substitute_all(y)
all.equal(a,b)

a <- substitute_all(x)
b <- substitute_all(y)
is.submodel(y,a)

y <- "(B*D*F*G+B*F*d*g+b*f*g<->A)*(A*D+G*b*d+G*f<->E)"
x <- "(b*f*g+D*F*E<->A)*(G<->E)" #should be false
is.submodel(x,y)
is_compatible(x,y)

GT <- "(B*D*F*G+B*F*d*g+b*f*g<->A)*(A*D+G*b*d+G*f<->E)"
x <- "(b*f*g+D*F*G<->A)*(G<->E)"
is_compatible(x,GT)

x <- "(f*E<->A)*(C<->E)" # drop rreduce for ultimate lhs for A in check_ccomp_asf
y <- "(C*D+C*F+C*b<->E)*(B*D*E+C*b*f<->A)" #because nothing in y can be substituted 
is_compatible(x,y)

x <- "(A*c*g*E<->B)*(A*c*g+A*g*e<->D)" #candidate, should be false?
b <- "(D*E<->B)*(A*c*g+A*e*g<->D)" # target
is_compatible(x,b)

x <- "(g*b<->A)*(G*B<->E)*(b+D*e<->F)" 
y <- "(C*d*g+b*g<->A)*(B*G<->E)*(C*E*d+D*e+b*g<->F)"

is_compatible(x,y)

x <- "(F*c<->A)*(E*f+c*d<->B)*(c*d*a<->G)" # should be false bc F*c<->A
y <- "(E*f+F*c*d<->B)*(B*F+D*b*c<->A)*(a*c*d<->G)"
is_compatible(x,y)


x <- "(G+F*d<->A)*(F*D*G<->B)*(f*a<->C)*(g*A<->E)"
y <- "(F*d+D*G<->A)*(a*f<->C)*(D*F*G<->B)*(g*A<->E)"
is_compatible(x,y)

y <- "(A+B<->C)*(X+Y<->T)"
x <- "(A+B<->C)*(T<->Y)"
is_compatible(x,y)

#mv


y <- "(A=1+B=2<->C=3)*(C=3+D=5<->E=2)"
dat <- mvdatgen(y)
x <- "(A=1+B=2<->E=2)"
is_compatible(x,y, dat = dat)

x <- "(T+R<->Y)*(A+Y*K<->C)*(C*H+C*I<->E)"

y <- "(B=2*D=0+D=1*B=1<->A=0)*(B=1*D=2+A=0*D=0<->C=1)"
x <- "(B=2*D=0+D=1*B=1<->C=1)"
dat <- mvdatgen(y)
is_compatible(x,y, dat) #what?

y <- ("b*d+D")

dat <- full.ct(list(A=1:3, B=1:3, C=0:2, D=1:2, E=0:2, F=1:3))

randomCsf(dat, maxVarNum = 6)

