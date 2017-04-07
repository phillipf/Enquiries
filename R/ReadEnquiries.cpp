// reading a text file
#include <fstream>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
int main () {
  std::string Borrower [30];
  NumericVector Amt(30);

  int X1 = 0;

  std::ifstream inFile ("N:/ABR/GENTRACK DATA/data AGAIN  for phil 22 nov.txt");

  while ( !inFile.eof () ) {
    inFile >> Borrower [X1];
    inFile >> Amt [X1++];
  }

  X1--;

  for ( int Pntr = 0; Pntr < X1; Pntr++ )
    std::cout << "  " << Pntr+1 << "  " << Borrower [Pntr] << "   $" << Amt [Pntr] << std::endl;

  return 0;
}


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
main()
*/
