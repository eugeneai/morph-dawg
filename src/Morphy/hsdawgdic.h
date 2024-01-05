#ifndef __HS_DAWGDICT__
#define __HS_DAWGDICT__

namespace dawgdic {
  class Dictionary;
}

#include "dawgdic/dictionary.h"
#include "stdio.h"

namespace dawgdic {

  extern "C" Dictionary * newDictionary();
  extern "C" void freeDictionary(Dictionary * dict);

}

#endif // __HS_DAWGDICT__
