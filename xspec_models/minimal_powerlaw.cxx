// Code for power-law model.
//
// Parameters :
//      alpha          power-law index (E^-alpha)
//

#include "xsTypes.h"

double calcPowerLaw(const valarray& energyArray, const double& index, valarray& fluxArray);

//**************************************************************************

double calcPowerLaw(const valarray& energyArray, const double& index,
		  valarray& fluxArray)
{
  using namespace std;
  const double alpha ( 1 - index );

  size_t N(energyArray.size());
  //fluxArray.resize(N-1);

  // note tolerance to avoid numerical problems

  if ( abs(alpha) < 1e-10 ) {
    valarray logE(std::log(energyArray));
    double first (logE[0]);
    for (size_t i = 1; i < N; ++i) {
      double second(logE[i]);
      fluxArray[i - 1] = second - first;
      first = second;
    }
  } else {
    const double alphani (1./alpha);
    double first (alphani*pow(energyArray[0],alpha));
    for (size_t i = 1; i < N; ++i) {
      double second(alphani*pow(energyArray[i],alpha));
      fluxArray[i - 1] = second - first;
      first =  second;
    }
  }

    return(1.0);

}
