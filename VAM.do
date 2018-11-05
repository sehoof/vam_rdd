rdplot vote margin,  binselect(esmv)
rdplot vote margin,  binselect(es)

rdplot vote margin,  binselect(qsmv)
rdplot vote margin,  binselect(qs)


gen treated=margin>=0


reg vote c.margin##c.margin##treated
reg vote c.margin##c.margin##c.margin##treated
reg vote c.margin##c.margin##c.margin##c.margin##treated


reg vote margin if abs(margin)<=15 & treated==0
scalar define b1 = _b[_cons]
reg vote margin if abs(margin)<=15 & treated==1
scalar define b2 = _b[_cons]
di b2-b1
reg vote c.margin##treated if abs(margin)<=15

gen weights=max(0, (1-abs(margin)/15))

reg vote c.margin##treated [pw=weights]

rdrobust vote margin, h(15) kernel(tri) p(1)
ereturn list


rdrobust vote margin, rho(1) bwselect(mserd) p(1)

rdrobust vote margin, rho(1) bwselect(mserd) p(2)


rdrobust state_population margin, rho(1) bwselect(mserd) p(1)

rdrobust vote margin, rho(1) bwselect(mserd) p(1) c(-12)

rdrobust vote margin, rho(1) bwselect(mserd) p(1) c(22)


rddensity margin

