
y <-  "(T+R<->Y)*(A+Y*K<->C)*(C*H+C*I<->E)"
is.inus(y)
x <- "H*T+A*I<->E"
x <- "C*H+I*T<->E"
check_ccomp(x,y)
y <- "(C*A+B<->R)*(R+B+X<->Y)*(Y*N+B*n<->L)*(L*a*b+U<->E)"
x <- "A+B<->E"
check_ccomp(x,y)
y <- "(A+B<->C)*(C+X<->E)*(E*R*T*x+X*a*b+Y*a*b<->Z)"
x <- "A+X<->Z"
check_ccomp(x,y)

y <- "(T+R<->Y)*(A+B<->C)*(t*C+a*Y<->E)"
x <- "A+T<->E"
check_ccomp(x,y)
y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(T*C+A<->E)"
x <- "L+T<->E"
check_ccomp(x,y)
y <- "(A+B<->C)*(T+R<->Y)*(C*Y+c*y<->E)"
x <-  "t*r+C<->E"
check_ccomp(x,y)
y <- "(A*X+B<->C)*(C+A*R<->Y)*(C*a*U+Z*y<->E)"
is.inus(y)
x <- "R<->E"
check_ccomp(x,y)
y <- "(A*b+B*a+A*C<->D)*(D+E<->F)"
x <- "A*b+B*a+B*C<->F"
check_ccomp(x,y)
y <- "(A+B*F<->C)*(D+B*f<->E)*(C+E<->G)"
x <- "(A+B*F<->C)*(C+E<->G)"
x <- "A+B*F+E<->G"
check_ccomp(x,y)

is.inus(x, selectCases(y))
y <- "(D*F+a*b<->C)*(F*c<->G)*(B*f+D*c+a*c<->E)"
x <-"D*f+d*B<->E"
check_ccomp(x,y)

y <- "(C*e*f+E*F*c+F*c*g<->B)*(B*e+E*G*c<->D)*(F*G+d*e<->A)"

x <- "e*b+F*G<->A"
check_comp_asf(x,y)

y <- "(E*c*d+a*c+a*d*g<->F)*(A*F+D*E*g+a*d*f<->B)"
x <- "A*F+a*d*G+E*D*g<->B"
check_comp_asf(x,y)


### check_ccomp()
y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(C+U<->E)"
x <- "(L+B<->Y)*(X+Y<->E)"
check_ccomp(x,y)

y <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(T*C+A<->E)"
x <- "(L+T<->A)*(X+Y<->E)"
check_ccomp(x,y)

x <- "L+T<->Y"
x <- "R+C<->E"
check_comp_asf(x, y)

x <- "(C+D<->E)*(A+B<->C)*(E+F<->G)*(G+H+C<->I)"
x <- "(C+E<->G)*(G+F<->I)"
check_ccomp(x,y)
is.inus(x, selectCases(y))

y <- "(A + B <-> C)*(C + Z <-> F)*(C + D <-> E)"
x <- "(A + B <-> F)*(A + D <-> E)"
check_ccomp(x,y)


target <- "(A + B*D <->C)*(C+D <->G)"
candidate <- "(A + B*D <-> C)*(C <->G)"

td <- selectCases(target)

x <- "(A+B<->C)*(C+D<->E)*(E+F<->G)*(G+H+C<->I)"
r <- "(E+F<->G)*(A+B<->C)*(C+D<->E)*(G+H+C<->I)"

y <- "(A+B<->C)*(C+D<->E)*(X+F<->G)*(G+H+c<->I)"
x <- "(G+c<->I)*(C+D<->E)"

y2 <- "(A+B<->C)*(C+D<->E)*(X+F<->G)*(G+c<->I)"

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



