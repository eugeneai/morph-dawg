#include "hsdawgdic.h"
#include <dictionary.h>

namespace dawgdic {

  Dictionary * newDictionary() { printf("Creating Dict\n") ; return new Dictionary(); }
  void freeDictionary(Dictionary * dict) { printf("Releasing Dict\n") ; delete dict; }

}
#ifndef __cplusplus
#error Must be compiled with g++
#endif
