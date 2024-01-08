#include "hsdawgdic.h"
#include <dictionary.h>

#include <iostream>     // std::ios, std::istream, std::cout
#include <fstream>      // std::filebuf
#include <string.h>
#include <assert.h>

using namespace dawgdic;

#define DEBUG false

Dict * newDictionary() {
  if (DEBUG) std::cout<<"Creating Dict\n";
  Dict * dict = new Dict();
  dict->dict = new Dictionary();
  dict->guide = new Guide();
  dict->comp = new Completer();
  return dict;
}

void freeDictionary(Dict * dict) {
  if (DEBUG) std::cout<<"Releasing Dict\n";
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
    if (DEBUG) std::cerr << "Info: Opened '" << fileName << "' file.\n";
    bool rc1 = dict->dict->Read(&input);
    bool rc = rc1;
    if (!rc1) {
      dict->dict->Clear();
    } else {
      bool rc2 = dict->guide->Read(&input);
      rc = rc && rc2;
      if(!rc2) {
        dict->guide->Clear();
        dict->dict->Clear();
      }
    }
    fb.close();
    if (DEBUG) if (rc) std::cerr << "Info: Read successfully '" << fileName << "' file.\n";
    return rc;
  }
  std::cerr << "FATAL: Cannot open '" << fileName << "' file.\n";
  return false;
}

bool followDictionary(Dict * dict, char * s, BaseType * index) {
  if (DEBUG) std::cout<<"Following Dict - '" << s << "' index: " << (*index) << std::endl;
  bool rc = dict->dict->Follow(s, index);
  if (DEBUG) std::cout<<"Following Dict + " << rc << " new index: " << (*index) << std::endl;
  return rc;
}

void startCompleter(Dict * dict, BaseType index) {

  if (DEBUG) std::cout<<"Start Comp - index: " << index << std::endl;

  if (! dict->setUp) {
    dict->comp->set_dic(*(dict->dict));
    dict->comp->set_guide(*(dict->guide));
    dict->setUp = true;
  };

  dict->comp->Start(index);
}

bool nextCompleter(Dict * dict) {
  auto rc = dict->comp->Next();
  if (DEBUG) std::cout<<"Next Comp + bool: " << rc << std::endl;
  return rc;
}

void keyCompleter(Dict * dict, char * s, BaseType maxSize) {
  const char * k = dict->comp->key();
  if (DEBUG) std::cout<<"Key Comp + key:|" << k << "|" << std::endl;
  strncpy(s, k, maxSize);
}

void keyValueCompleter(Dict * dict, char * k, char * v, BaseType maxSize) {
  const char * s = dict->comp->key();
  char * c = (char *) s;
  BaseType m = maxSize;
  BaseType p = 0;
  v[0] = '\x0';
  k[0] = '\x0';
  assert(maxSize>0);
  while (true) {
    char cc = *c;
    if (cc == '\x0') {
      k[p] = '\x0';
      return;
    } else if (cc == '\x1') {
      k[p] = '\x0';
      p++;
      c++;
      break;
    } else if (p==maxSize) {
      k[p-1] = '\0';
      return;
    }
    k[p] = cc;
    p++;
    c++;
  }

  p = 0;
  while (true) {
    char cc = *c;
    if (cc == '\x0') {
      v[p] = '\x0';
      return;
    } else if (p==maxSize) {
      v[p-1]='\x0';
      return;
    }
    v[p] = cc;
    p++;
    c++;
  }
  if (DEBUG) std::cout<<"Key Comp + key:|" << k << "| val:|" << v << "|" << std::endl;
}

SizeType lengthCompleter(Dict * dict) {
  auto l = dict->comp->length();
  if (DEBUG) std::cout<<"Length Comp + length: " << l << std::endl;
  return l;
}

ValueType valueCompleter(Dict * dict) {
  ValueType v = dict->comp->value();
  if (DEBUG) std::cout<<"Value Comp + value:|" << v << "|" << std::endl;
  return v;
}

#ifndef __cplusplus
#error Must be compiled with g++
#endif
