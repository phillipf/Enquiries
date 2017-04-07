#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector Event2(NumericVector Consumer, NumericVector Timediff) {

  NumericVector id(Consumer.size());

  NumericVector::iterator it;
  NumericVector::iterator time;
  NumericVector::iterator out_id;

  int loc = 1;
  double prev = Consumer[0];
  Consumer.push_back(prev);

  for(it = Consumer.begin(), time = Timediff.begin(), out_id = id.begin(); it != Consumer.end();
      ++it, ++time, ++out_id) {

      if (*time > 10 || *it != prev) {

        loc += 1;
        *out_id = loc;

      }

      if (*time < 10 && *it == prev) {

        *out_id = loc;

      }

    }

  return(id);

}

// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R

#require(Rcpp)
#sourceCpp("file:///C:/Users/farrelp1/Documents/Enquiries/R/Event.cpp")

#test <- Event2(Events$MEMO_CONSUMER, as.numeric(Events$timediff))


*/
