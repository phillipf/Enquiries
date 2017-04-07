#include <Rcpp.h>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//

// [[Rcpp::export]]
NumericVector Event(NumericVector Consumer, NumericVector Timediff) {

  NumericVector id(Consumer.size());
  double length = Consumer.size();
  int loc = 0;


  for(int i=0; i < length; i++) {

      if (Timediff[i] > 10 || Consumer[i] != Consumer[i - 1]) {

        loc += 1;
        id[i] = loc;

      }

      if (Timediff[i] < 10 && Consumer[i] == Consumer[i - 1]) {

        id[i] = loc;

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

#Event(CallQual3$MEMO_CONSUMER[1:1000], as.numeric(CallQual3$timediff[1:1000]), length(Events$MEMO_CONSUMER[1:1000]))


*/
