/*
 * mccullochquantile.c
 * 
 * An S-PLUS/C-based implementation of McCulloch's quantile method of 
 * stable parameters estimation, as described in the paper
 * "Simple Consistent Estimators of Stable Distribution Parameters",
 * J. Huston McCulloch, Communications in Statistics, Simulation,
 * 1986 (15:4), 1109-1136.
 *  
 * Based on McCulloch's 
 * GAUSS implementation, which can be found on McCulloch's
 * homepage: http://www.econ.ohio-state.edu/jhm/jhm.html
 *
 * Christopher G. Green
 * Department of Statistics
 * University of Washington
 *
 * April 17, 2005
 *
*/
/* July 7, 2007: converted to R */

#include <math.h>
#include <stdlib.h>

#include "cggRutils.h"
/* #include "util.h" */
#include "R_ext/RS.h"

/*
 * The input variables are as follows:
 *
 * quant	vector of the .05, .25, .50, .75, and .95 quantiles of the data
 * len		the length of quant (must be 5)
 * symm		1 to assume the data come from a symmetric distribution, 0 to allow
 *          asymmetry
 * symmlen 	the length of symm (must be 1)
 *
 * The output variable is quant (in accordance with S-PLUS conventions), with the 
 * following components:
 *
 * quant[0] = alpha		the index of stability
 * quant[1] = beta		the skewness parameter
 * quant[2] = c 		the scale parameter
 * quant[3] = delta		the location parameter
 * quant[4] = zeta		the shifted location parameter (of Zolotarev)
 *
*/

void mccullochquantile(double * quant, long * len, long * symm, long * symmlen)
{
#ifdef _SPLUS_
	S_EVALUATOR
#endif

    /* Table I of McCulloch */
    const double ena[5][16] = { 
		 {2.4388,2.5120,2.6080,2.7369,2.9115,3.1480,3.4635,3.8824,4.4468,5.2172,6.3140,7.9098,10.4480,14.8378,23.4831,44.2813},
		 {2.4388,2.5117,2.6093,2.7376,2.9090,3.1363,3.4361,3.8337,4.3651,5.0840,6.0978,7.5900, 9.9336,13.9540,21.7682,40.1367},
		 {2.4388,2.5125,2.6101,2.7387,2.9037,3.1119,3.3778,3.7199,4.1713,4.7778,5.6241,6.8606, 8.7790,12.0419,18.3320,33.0018},
		 {2.4388,2.5129,2.6131,2.7420,2.8998,3.0919,3.3306,3.6257,4.0052,4.5122,5.2195,6.2598, 7.9005,10.7219,16.2163,29.1399},
		 {2.4388,2.5148,2.6174,2.7464,2.9016,3.0888,3.3161,3.5997,3.9635,4.4506,5.1256,6.1239, 7.6874,10.3704,15.5841,27.7822}
    };
    
    /* Table II of McCulloch */
    const double enb[5][16] = {
		 {0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000,0.0000},
		 {0.0000,0.0179,0.0389,0.0626,0.0895,0.1183,0.1478,0.1769,0.2062,0.2362,0.2681,0.3026,0.3415,0.3865,0.4408,0.5095},
		 {0.0000,0.0357,0.0765,0.1226,0.1736,0.2282,0.2849,0.3422,0.3993,0.4561,0.5134,0.5726,0.6343,0.6994,0.7678,0.8381},
		 {0.0000,0.0533,0.1133,0.1784,0.2478,0.3199,0.3942,0.4703,0.5473,0.6240,0.6993,0.7700,0.8339,0.8900,0.9362,0.9700},
		 {0.0000,0.0710,0.1480,0.2281,0.3090,0.3895,0.4686,0.5458,0.6210,0.6934,0.7616,0.8248,0.8805,0.9269,0.9620,0.9847}
    };

    /* Table V of McCulloch */
    const double enc[5][16] = {
		{1.9078,1.9140,1.9210,1.9270,1.9330,1.9390,1.9460,1.9550,1.9650,1.9800,2.0000,2.0400,2.0980,2.1890,2.3370,2.5880},
		{1.9078,1.9150,1.9220,1.9305,1.9405,1.9520,1.9665,1.9845,2.0075,2.0405,2.0850,2.1490,2.2445,2.3920,2.6355,3.0735},
		{1.9078,1.9160,1.9275,1.9425,1.9620,1.9885,2.0220,2.0670,2.1255,2.2050,2.3115,2.4610,2.6765,3.0040,3.5425,4.5340},
		{1.9078,1.9185,1.9360,1.9610,1.9970,2.0450,2.1065,2.1880,2.2945,2.4345,2.6240,2.8865,3.2650,3.8440,4.8085,6.6365},
		{1.9078,1.9210,1.9470,1.9870,2.0430,2.1160,2.2110,2.3330,2.4910,2.6965,2.9735,3.3565,3.9125,4.7755,6.2465,9.1440}
    };

    /* Table VII of McCulloch */
    const double za[5][16] = {
		{0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000, 0.0000},
		{0.0000,-0.0166,-0.0302,-0.0434,-0.0556,-0.0660,-0.0751,-0.0837,-0.0904,-0.0955,-0.0980,-0.0986,-0.0956,-0.0894,-0.0779,-0.0610},
		{0.0000,-0.0322,-0.0615,-0.0878,-0.1113,-0.1340,-0.1542,-0.1733,-0.1919,-0.2080,-0.2230,-0.2372,-0.2502,-0.2617,-0.2718,-0.2790},
		{0.0000,-0.0488,-0.0917,-0.1321,-0.1699,-0.2060,-0.2413,-0.2760,-0.3103,-0.3465,-0.3830,-0.4239,-0.4688,-0.5201,-0.5807,-0.6590},
		{0.0000,-0.0644,-0.1229,-0.1785,-0.2315,-0.2830,-0.3354,-0.3896,-0.4467,-0.5080,-0.5760,-0.6525,-0.7424,-0.8534,-0.9966,-1.1980}
    };

	const double pi = PI; 

	double d,cn,an,t,alpha,cnu,c,bn,sign,beta,delta,zeta,aa,s2,b,bb,t1,t2,s1,dt,ds,s;
	double ah[5] = {0,0,0,0,0};
	double bh[16] = {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0};
	int i,ii,j,jj;


	if ( quant == NULL || len == NULL || symm == NULL || symmlen == NULL )
		PROBLEM "Invalid inputs---one or more inputs is/are NULL." ERROR;
	if ( *len != 5 )
		PROBLEM "len input must be equal to 5." ERROR;
	if ( *symmlen != 1 ) 
		PROBLEM "symmlen input must be equal to 1." ERROR;

	d  = quant[4] - quant[0];
	cn = quant[3] - quant[1];

	if ( cn == 0 )
		PROBLEM "Interquartile range is zero." ERROR;

	an = d/cn;
	if ( an < 2.4388 ) {
		quant[0] = 2; 
		quant[1] = 0; 
		quant[3] = quant[2]; 
		quant[4] = quant[2];
		quant[2] = cn/1.9078; 
	}
	else if ( *symm == 1 ) {
		/* symmetric case */

		/* interpolation in the ena table */
		for ( i = 1; i < 16; i++ ) {
			ii = i;
			if ( an <= ena[0][i] ) break;
		}

		i = ii;
		t = (an - ena[0][i-1])/(ena[0][i]-ena[0][i-1]);
		alpha = (22 - (i+1) - t)/10;
		if ( alpha < .5 ) alpha = .5;
		cnu = enc[0][i-1] * (1 - t) + enc[0][i] * t;
		c = cn/cnu;

		quant[0] = alpha;
		quant[1] = 0;
		quant[3] = quant[4] = quant[2];
		quant[2] = c;
	}
	else {
		/* asymmetric case */

		bn = (quant[0] + quant[4] - 2 * quant[2])/d;

		sign = 1.0;
		if (bn < 0) sign = -1.0;
		if (bn == 0) sign = 0.0;

		bn = fabs(bn);

		for ( j = 1; j <= 5; j++ ) {
			for ( i = 2; i < 16; i++ ) 
				if ( an <= ena[j-1][i-1] ) break;
			t = (an - ena[j-1][i-2])/(ena[j-1][i-1] - ena[j-1][i-2]);
			ah[j-1] = 2 - 0.1*(i-2+t);
		}

		for ( i = 2; i <= 16; i++ ) {
			for ( j = 2; j < 5;  j++ )
				if ( bn <= enb[j-1][i-1] ) break;
			t = (bn - enb[j-2][i-1])/(enb[j-1][i-1] - enb[j-2][i-1]);
			bh[i-1] = 0.25*(j-2+t);
		}
		bh[0] = 2*bh[1] - bh[2];

		for ( j = 2; j <= 5; j++ ) {
			jj = j;
			i = (int)floor(10*(2 - ah[j-1]) + 2);
			if (i < 2) i = 2;
			if (i > 16) i = 16;
			aa = 2 - 0.1*(i-2);
			s2 = -10*(ah[j-1]-aa);
			b = (1-s2)*bh[i-2] + s2*bh[i-1];
			if ( b < 0.25*(j-1) ) break;
		}
		j = jj;

		bb = 0.25*(j-2);
		t1 = 4*(bh[i-2] - bb);
		t2 = 4*(bh[i-1] - bb);
		s1 = -10*(ah[j-2] - aa);
		dt = t2 - t1;
		ds = s2 - s1;
		s  = (s1 + t1*ds)/(1 - ds*dt);
		t  = t1 + s*dt;
		alpha = aa - 0.1*s;
		if ( alpha < 0.5 ) alpha = 0.5;
		beta = bb + 0.25*t;
		if ( beta > 1 ) beta = 1;
		beta = beta*sign;
		c = (1-s) * (1-t) * enc[j-2][i-2] + s * (1-t) * enc[j-2][i-1] + 
				t * (1-s) * enc[j-1][i-2] + s *     t * enc[j-1][i-1];
		c = cn/c;
		zeta = (1-s) * (1-t) * za[j-2][i-2] + s * (1-t) * za[j-2][i-1] + 
				   t * (1-s) * za[j-1][i-2] + s *     t * za[j-1][i-1];
		zeta = quant[2] + c*sign*zeta;
		if (alpha==1)
			delta = zeta;
		else
			delta = zeta - beta*c*tan(pi*alpha/2);

		quant[0] = alpha;
		quant[1] = beta;
		quant[2] = c;
		quant[3] = delta;
		quant[4] = zeta;
	}

	/* quant is implicitly returned */
	return;
}
