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
  dict->Clear();
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

bool followDictionary(Dictionary * dict, char * s, BaseType * index) {
  std::cout<<"Following Dict - '" << s << "' index: " << (*index) << std::endl;
  bool rc = dict->Follow(s, index);
  std::cout<<"Following Dict + " << rc << " new index: " << (*index) << std::endl;
  return rc;
}

#ifndef __cplusplus
#error Must be compiled with g++
#endif
