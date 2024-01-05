#ifndef __HS_DAWGDICT__
#define __HS_DAWGDICT__

#include <dictionary.h>
#include "stdio.h"

#ifdef _cplusplus_
extern "C" {
#endif

Dictionary * newDictionary() { printf("Creating Dict\n") ; return new Dictionary() }
void freeDictionary(Dictionary * dict) { printf("Releasing Dict\n") ; delete dict; }

#ifdef _cplusplus_
}
#edif

#endif // __HS_DAWGDICT__
