#ifndef __HS_DAWGDICT__
#define __HS_DAWGDICT__

#include "dawgdic/dictionary.h"

using namespace dawgdic;

extern "C" Dictionary * newDictionary();
extern "C" void freeDictionary(Dictionary * dict);
extern "C" bool readDictionaryFromFile(Dictionary * dict, char * fileName);
extern "C" bool followDictionary(Dictionary * dict, char * s, BaseType * index);

#endif // __HS_DAWGDICT__
