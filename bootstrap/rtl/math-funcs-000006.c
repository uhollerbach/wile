#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <complex.h>
#include <math.h>
#include <time.h>
#include <float.h>
#include <sys/types.h>
#include <unistd.h>

#include "wile-rtl1.h"


// Many thanks to the anonymous editor of the wikipedia page
//
//	http://en.wikipedia.org/wiki/Trigonometric_integral
//
// who posted these functions!

static double aux_f(double x)
{
    double y, v;

    y = 1.0/(x*x);
    v = (1.0 +
	 y*(7.44437068161936700618e2 +
	    y*(1.96396372895146869801e5 +
	       y*(2.37750310125431834034e7 +
		  y*(1.43073403821274636888e9 +
		     y*(4.33736238870432522765e10 +
			y*(6.40533830574022022911e11 +
			   y*(4.20968180571076940208e12 +
			      y*(1.00795182980368574617e13 +
				 y*(4.94816688199951963482e12 +
				    y*(-4.94701168645415959931e11)))))))))))
        / (x*(1.0 +
              y*(7.46437068161927678031e2 +
                 y*(1.97865247031583951450e5 +
                    y*(2.41535670165126845144e7 +
                       y*(1.47478952192985464958e9 +
                          y*(4.58595115847765779830e10 +
                             y*(7.08501308149515401563e11 +
                                y*(5.06084464593475076774e12 +
                                   y*(1.43468549171581016479e13 +
                                      y*(1.11535493509914254097e13)))))))))));
    return v;
}

static double aux_g(double x)
{
    double y, v;

    y = 1.0/(x*x);
    v = y*(1.0 +
	   y*(8.1359520115168615e2 +
	      y*(2.35239181626478200e5 +
		 y*(3.12557570795778731e7 +
		    y*(2.06297595146763354e9 +
		       y*(6.83052205423625007e10 +
			  y*(1.09049528450362786e12 +
			     y*(7.57664583257834349e12 +
				y*(1.81004487464664575e13 +
				   y*(6.43291613143049485e12 +
				      y*(-1.36517137670871689e12)))))))))))
	/ (1.0 +
	   y*(8.19595201151451564e2 +
	      y*(2.40036752835578777e5 +
		 y*(3.26026661647090822e7 +
		    y*(2.23355543278099360e9 +
		       y*(7.87465017341829930e10 +
			  y*(1.39866710696414565e12 +
			     y*(1.17164723371736605e13 +
				y*(4.01839087307656620e13 +
				   y*(3.99653257887490811e13))))))))));
    return v;
}

double sine_integral(double x)
{
    double x2, v;

    if (x <= 4.0) {
	x2 = x*x;
	v = x*(1.0 +
	       x2*(-4.54393409816329991e-2 +
		   x2*(1.15457225751016682e-3 +
		       x2*(-1.41018536821330254e-5 +
			   x2*(9.43280809438713025e-8 +
			       x2*(-3.53201978997168357e-10 +
				   x2*(7.08240282274875911e-13 +
				       x2*(-6.05338212010422477e-16))))))))
	    / (1.0 +
	       x2*(1.01162145739225565e-2 +
		   x2*(4.99175116169755106e-5 +
		       x2*(1.55654986308745614e-7 +
			   x2*(3.28067571055789734e-10 +
			       x2*(4.5049097575386581e-13 +
				   x2*(3.21107051193712168e-16)))))));
    } else {
	v = PI_L/2.0 - aux_f(x)*cos(x) - aux_g(x)*sin(x);
    }
    return v;
}

double cosine_integral(double x)
{
    double x2, v;

    if (x <= 4.0) {
	x2 = x*x;
	v = 0.577215664901532860606512 + log(x) +
	    x2*(-0.25 +
		x2*(7.51851524438898291e-3 +
		    x2*(-1.27528342240267686e-4 +
			x2*(1.05297363846239184e-6 +
			    x2*(-4.68889508144848019e-9 +
				x2*(1.06480802891189243e-11 +
				    x2*(-9.93728488857585407e-15)))))))
	    / (1.0 +
	       x2*(1.1592605689110735e-2 +
		   x2*(6.72126800814254432e-5 +
		       x2*(2.55533277086129636e-7 +
			   x2*(6.97071295760958946e-10 +
			       x2*(1.38536352772778619e-12 +
				   x2*(1.89106054713059759e-15 +
				       x2*(1.39759616731376855e-18))))))));
    } else {
	v = aux_f(x)*sin(x) - aux_g(x)*cos(x);
    }
    return v;
}

