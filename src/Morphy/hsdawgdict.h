#ifndef __HS_DAWGDICT__
#define __HS_DAWGDICT__

#include <dictionary.h>
#include "stdio.h"

extern "C" {

Dictionary * newDictionary() { printf("Creating Dict\n") ; return new Dictionary() }
void freeDictionary(Dictionary * dict) { printf("Releasing Dict\n") ; delete dict; }

}

#endif // __HS_DAWGDICT__
