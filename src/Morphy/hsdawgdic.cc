#include "hsdawgdic.h"
#include <dictionary.h>

#include <iostream>     // std::ios, std::istream, std::cout
#include <fstream>      // std::filebuf

using namespace dawgdic;

Dictionary * newDictionary() {
  std::cout<<"Creating Dict\n";
  return new Dictionary();
}

void freeDictionary(Dictionary * dict) {
  std::cout<<"Releasing Dict\n";
  delete dict;
}

bool readDictionaryFromFile(Dictionary * dic, char * fileName) {
  std::filebuf fb;
  if (fb.open(fileName, std::ios::in)) {
    std::istream input(&fb);
    std::cerr << "Info: Opened '" << fileName << "' file.\n";
    bool rc = dic->Read(&input);
    fb.close();
    if (rc) std::cerr << "Info: Read successfully '" << fileName << "' file.\n";
    return rc;
  }
  std::cerr << "FATAL: Cannot open '" << fileName << "' file.\n";
  return false;
}

#ifndef __cplusplus
#error Must be compiled with g++
#endif
