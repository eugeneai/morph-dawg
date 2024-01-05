#include "hsdawgdic.h"

dawgdic::Dictionary * newDictionary() { printf("Creating Dict\n") ; return new dawgdic::Dictionary(); }
void freeDictionary(dawgdic::Dictionary * dict) { printf("Releasing Dict\n") ; delete dict; }
