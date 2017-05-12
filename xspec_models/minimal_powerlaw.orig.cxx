// Code for power-law model.
//
// Parameters :
//      alpha          power-law index (E^-alpha)
//

#include "xsTypes.h"

Real calcPowerLaw(const RealArray& energyArray, const Real& index, RealArray& fluxArray);
RealArray make_RealArray(vector& x)
//**************************************************************************

// Try also http://stackoverflow.com/questions/13634504/assign-a-stdvector-to-a-stdvalarray
RealArray make_RealArray(vector& x)
{
    RealArray result;
    size_t N(x.size());
    result = RealArray(N);
    for (size_t i =1; i < N; i++) {
        result[i] = x[i];
    }
    return (result);
}

Real calcPowerLaw(const RealArray& energyArray, const Real& index,
		  RealArray& fluxArray)
{
  using namespace std;
  const Real alpha ( 1 - index );

  size_t N(energyArray.size());
  //fluxArray.resize(N-1);

  // note tolerance to avoid numerical problems

  if ( abs(alpha) < 1e-10 ) {
    RealArray logE(std::log(energyArray));
    Real first (logE[0]);
    for (size_t i = 1; i < N; ++i) {
      Real second(logE[i]);
      fluxArray[i - 1] = second - first;
      first = second;
    }
  } else {
    const Real alphani (1./alpha);
    Real first (alphani*pow(energyArray[0],alpha));
    for (size_t i = 1; i < N; ++i) {
      Real second(alphani*pow(energyArray[i],alpha));
      fluxArray[i - 1] = second - first;
      first =  second;
    }
  }

    return(1.0);

}
