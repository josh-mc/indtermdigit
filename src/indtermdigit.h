//header file

//From utils.


#ifndef BUNCH_PROB
#define BUNCH_PROB

#include <Rcpp.h>
using namespace Rcpp;

NumericVector bunch_prob(IntegerVector dec,
                         IntegerVector int_1);

#endif



#ifndef INT_DEC
#define INT_DEC

List int_dec(NumericVector s,
             int decimals);

#endif


#ifndef FULL_VEC
#define FULL_VEC

#include <Rcpp.h>
using namespace Rcpp;

IntegerVector full_vec(IntegerVector int_1,
                       IntegerVector dec_1);


#endif


#ifndef ACTUAL_FRAC
#define ACTUAL_FRAC

#include <Rcpp.h>
using namespace Rcpp;

NumericVector actual_frac(IntegerVector int_full,
                          IntegerVector dec,
                          int new_n);

#endif

#ifndef ACTUAL_FRAC_N
#define ACTUAL_FRAC_N

#include <Rcpp.h>
using namespace Rcpp;

NumericVector actual_frac_n(IntegerVector int_full,
                          IntegerVector dec,
                          int new_n);

#endif





#ifndef TABULATE_FRAC
#define TABULATE_FRAC

#include <Rcpp.h>
using namespace Rcpp;

NumericVector tabulate_frac(IntegerVector x,
                          int n_bins,
                          int new_n);

#endif


#ifndef TAB_IT
#define TAB_IT

#include <Rcpp.h>
using namespace Rcpp;

IntegerVector tab_it(NumericVector x,
                     int bins,
                     int a,
                     int b);

#endif


#ifndef PERM_VECTOR
#define PERM_VECTOR

#include <Rcpp.h>
using namespace Rcpp;

IntegerVector perm_vector(IntegerVector v,
                          IntegerVector r_sum,
                          IntegerVector c_sum);

#endif


#ifndef EXPECTED_CELLS
#define EXPECTED_CELLS

#include <Rcpp.h>
using namespace Rcpp;

NumericVector expected_cells(NumericVector r_frac,
                             NumericVector c_frac);

#endif


#ifndef OUT_VECTOR
#define OUT_VECTOR

#include <Rcpp.h>
using namespace Rcpp;

IntegerVector out_vector_cpp(IntegerVector c_sums);

#endif


//From stats.

#ifndef AVERAGE_FRE
#define AVERAGE_FRE

#include <Rcpp.h>
using namespace Rcpp;

double average_fre(IntegerVector x);

#endif


#ifndef AVERAGE_FRE2
#define AVERAGE_FRE2

#include <Rcpp.h>
using namespace Rcpp;

double average_fre2(IntegerVector x,
                    int n);

#endif



#ifndef FT_STAT
#define FT_STAT

#include <Rcpp.h>
using namespace Rcpp;

double ft_stat(int new_n,
               NumericVector vec_1_frac,
               NumericVector vec_2_frac);

#endif


#ifndef RMS_STAT
#define RMS_STAT

#include <Rcpp.h>
using namespace Rcpp;

double rms_stat(int new_n,
                NumericVector vec_1_frac,
                NumericVector vec_2_frac);

#endif


#ifndef CHISQ_STAT
#define CHISQ_STAT

#include <Rcpp.h>
using namespace Rcpp;

double chisq_stat(int draws,
                  NumericVector vec_1_frac,
                  NumericVector vec_2_frac);

#endif


#ifndef G2_STAT
#define G2_STAT

#include <Rcpp.h>
using namespace Rcpp;

double g2_stat(int draws,
               NumericVector vec_1_frac,
               NumericVector vec_2_frac);

#endif


//From distributions

#ifndef DIST_IN
#define DIST_IN

#include <Rcpp.h>
using namespace Rcpp;

NumericVector dist_in(int distribution,
                      int set_n,
                      double set_mean,
                      double set_sd);

#endif


#ifndef VIOLATION
#define VIOLATION

#include <Rcpp.h>

using namespace Rcpp;

NumericVector violation(NumericVector sample_0,
                        int duplicates,
                        int type);

#endif


#ifndef DATA_IN
#define DATA_IN

#include <Rcpp.h>

using namespace Rcpp;

NumericVector data_in(int distribution,
                      int set_n,
                      double set_mean,
                      double set_sd,
                      int duplicates);

#endif





