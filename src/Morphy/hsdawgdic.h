#ifndef __HS_DAWGDICT__
#define __HS_DAWGDICT__

#include <dictionary.h>
#include "stdio.h"

#ifdef _cplusplus_
extern "C" {
#endif

  dawgdic::Dictionary * newDictionary();
  void freeDictionary(dawgdic::Dictionary * dict);

#ifdef _cplusplus_
}
#endif

#endif // __HS_DAWGDICT__
