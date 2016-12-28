///
/// \file Util.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#include <string>

#ifdef _WIN32
#else
#include <cxxabi.h>
#endif

#ifndef UTILITIES_H_
#define UTILITIES_H_

namespace executor {

namespace util {

template<typename T>
std::string typeToString() {
#ifdef _WIN32
  std::string name(typeid(T).name());
#else
  char* cName = abi::__cxa_demangle(typeid(T).name(), NULL, NULL, NULL);
  std::string name(cName);
#endif
  // remove namespaces ...
  auto namesp = name.rfind(":");
  if (namesp != std::string::npos) {
    name.erase(0, namesp+1);
  }
#ifndef _WIN32
  free(cName);
#endif
  return name;
}

} // namespace util

} // namespace executor

#endif // UTILITIES_H_
