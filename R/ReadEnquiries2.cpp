#include <Rcpp.h>
#include <fstream>
#include <sstream>
#include <string>
using namespace Rcpp;


//Function is taking a path to a numeric file and return the same data in a NumericMatrix object

  // [[Rcpp::export]]
  CharacterMatrix readfilecpp(std::string path)
  {

    CharacterMatrix output(979096,34);// output matrix (specifying the size is critical otherwise R crashes)

    std::ifstream myfile(path.c_str()); //Opens the file. c_str is mandatory here so that ifstream accepts the string path

    std::string line;
    std::getline(myfile,line,'\n'); //skip the first line (col names in our case). Remove those lines if note necessary


      for (int row=0; row<979096; ++row) // basic idea: getline() will read lines row=0:19 and for each line will put the value separated by ',' into 46749 columns
          {
            std::string line;
            std::getline(myfile,line,'\n'); //Starts at the second line because the first one was ditched previously

              if(!myfile.good() ) //If end of rows then break
                break;

              std::stringstream iss(line); // take the line into a stringstream
              std::string val;
              std::getline(iss,val,'|'); ///skips the first column (row names)

              for (int col=0; col<34; ++col )
              {
                std::string val;
                std::getline(iss,val,'|'); //reads the stringstream line and separate it into 49749 values (that were delimited by a ',' in the stringstream)


                std::basic_stringstream convertor(val); //get the results into another stringstream 'convertor'
                convertor >> output(row,col); //put the result into our output matrix at for the actual row and col
              }
          }
        return(output);
  }


// You can include R code blocks in C++ files processed with sourceCpp
// (useful for testing and development). The R code will be automatically
// run after the compilation.
//

/*** R
data <- readfilecpp("N:/ABR/GENTRACK DATA/data AGAIN  for phil 22 nov.txt")
*/
