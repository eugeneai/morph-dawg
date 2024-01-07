#ifndef __HS_DAWGDICT__
#define __HS_DAWGDICT__

#include "dawgdic/dictionary.h"
#include "dawgdic/guide.h"
#include "dawgdic/completer.h"

using namespace dawgdic;

struct Dict {
  Dictionary * dict;
  Guide * guide;
  Completer * comp;
}

extern "C" Dict * newDictionary();
extern "C" void freeDictionary(Dict * dict);
extern "C" bool readDictionaryFromFile(Dict * dict, char * fileName);
extern "C" bool followDictionary(Dict * dict, char * s, BaseType * index);
extern "C" bool valueDictionary(Dict * dict, BaseType index, char * buf, BaseType len);

extern "C" void startCompleter(Dict * dict, BaseType index);
extern "C" bool nextCompleter(Dict * dict);
extern "C" const char * keyCompleter(Dict * dict);
extern "C" SizeType lengthCompleter(Dict * dict);
extern "C" ValueType valueCompleter(Dict * dict);

#endif // __HS_DAWGDICT__
