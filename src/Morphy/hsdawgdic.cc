#include "hsdawgdic.h"
#include <dictionary.h>

#include <iostream>     // std::ios, std::istream, std::cout
#include <fstream>      // std::filebuf
#include <string.h>

using namespace dawgdic;


Dict * newDictionary() {
  std::cout<<"Creating Dict\n";
  Dict * dict = new Dict();
  dict->dict = new Dictionary();
  dict->guide = new Guide();
  dict->comp = new Completer();
  dict->comp->set_dic(*(dict->dict));
  dict->comp->set_guide(*(dict->guide));
  return dict;
}

void freeDictionary(Dict * dict) {
  std::cout<<"Releasing Dict\n";
  dict->guide->Clear();
  dict->dict->Clear();
  delete dict->comp;
  delete dict->guide;
  delete dict->dict;
  delete dict;
}

bool readDictionaryFromFile(Dict * dict, char * fileName) {
  std::filebuf fb;
  if (fb.open(fileName, std::ios::in)) {
    std::istream input(&fb);
    std::cerr << "Info: Opened '" << fileName << "' file.\n";
    bool rc = dict->dict->Read(&input);
    fb.close();
    if (rc) std::cerr << "Info: Read successfully '" << fileName << "' file.\n";
    return rc;
  }
  std::cerr << "FATAL: Cannot open '" << fileName << "' file.\n";
  return false;
}

bool followDictionary(Dict * dict, char * s, BaseType * index) {
  std::cout<<"Following Dict - '" << s << "' index: " << (*index) << std::endl;
  bool rc = dict->dict->Follow(s, index);
  std::cout<<"Following Dict + " << rc << " new index: " << (*index) << std::endl;
  return rc;
}

void startCompleter(Dict * dict, BaseType index) {
  dict->comp->Start(index);
}

bool nextCompleter(Dict * dict) {
  return dict->comp->Next();
}

void keyCompleter(Dict * dict, char * s, BaseType maxSize) {
  const char * k = dict->comp->key();
  strncpy(s, k, maxSize);
}

SizeType lengthCompleter(Dict * dict) {
  return dict->comp->length();
}

ValueType valueCompleter(Dict * dict) {
  return dict->comp->value();
}

#ifndef __cplusplus
#error Must be compiled with g++
#endif
