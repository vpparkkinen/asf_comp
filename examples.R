
x <-  "(T+R<->Y)*(A+Y*K<->C)*(C*H+C*I<->E)"
is.inus(x)
b <- "H*T+A*I<->E"
b <- "C*H+I*T<->E"

x <- "(C*A+B<->R)*(R+B+X<->Y)*(Y*N+B*n<->L)*(L*a*b+U<->E)"
b <- "A+B<->E"
check_comp_asf(x,b)
x <- "(A+B<->C)*(C+X<->E)*(E*R*T*x+X*a*b+Y*a*b<->Z)"
b <- "A+X<->Z"


x <- "(T+R<->Y)*(A+B<->C)*(t*C+a*Y<->E)"
b <- "A+T<->E"
check_comp_asf(x,b)
x <- "(L+B<->A)*(T+A+R<->Y)*(X+Y<->C)*(t*C+A<->E)"
b <- "L+T<->E"
check_comp_asf(x,b)
x <- "(A+B<->C)*(T+R<->Y)*(C*Y+c*y<->E)"
b <-  "t*r+C<->E"
check_comp_asf(x,b)
x <- "(A*X+B<->C)*(C+A*R<->Y)*(C*a*U+Z*y<->E)"
is.inus(x)
b <- "R<->E"
check_comp_asf(x,b)
x <- "(A*b+B*a+A*C<->D)*(D+E<->F)"
b <- "A*b+B*a+B*C<->F"
check_comp_asf(x,b)
x <- "(A+B*F<->C)*(D+B*f<->E)*(C+E<->G)"
b <- "A+B*F<->G"

check_comp_asf(x, b)

is.inus(b, selectCases(x))
x <- "(D*F+a*b<->C)*(F*c<->G)*(B*f+D*c+a*c<->E)"
b <-"D*f+d*B<->E"
check_comp_asf(x, b)

x <- "(C*e*f+E*F*c+F*c*g<->B)*(B*e+E*G*c<->D)*(F*G+d*e<->A)"

b <- "e*b+F*G<->A"
check_comp_asf(x,b)



