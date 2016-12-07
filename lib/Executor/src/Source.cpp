///
/// \file Source.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#include <iostream>

#include <istream>
#include <string>

#include "util/Assert.h"

#include "Source.h"

namespace executor {

Source::Source()
  : _source()
{
}

Source::Source(const char* source)
  : _source(source)
{
  ASSERT_MESSAGE(!_source.empty(),
    "Tried to create source object with empty user source.");
}

Source::Source(const std::string& source)
  : _source(source)
{
  ASSERT_MESSAGE(!_source.empty(),
    "Tried to create source object with empty user source.");
}

Source::Source(std::istream& is)
  : _source( (std::istreambuf_iterator<char>(is)),
              std::istreambuf_iterator<char>()     )
{
  ASSERT_MESSAGE(!_source.empty(),
    "Tried to create source object with empty user source.");
}

Source::Source(std::istream&& is)
  : _source( (std::istreambuf_iterator<char>(is)),
              std::istreambuf_iterator<char>()     )
{
  ASSERT_MESSAGE(!_source.empty(),
    "Tried to create source object with empty user source.");
}

Source::~Source()
{
}

Source::operator std::string() const
{
  return _source;
}

void Source::append(const std::string& source)
{
  _source.append("\n" + source);
}

CommonDefinitions::CommonDefinitions()
  : _sources(Level::SIZE)
{
}

CommonDefinitions& CommonDefinitions::instance()
{
  static CommonDefinitions instance;
  return instance;
}

void CommonDefinitions::append(const std::string& source, Level level)
{
  CommonDefinitions::instance()._sources[level].append(source);
}

Source CommonDefinitions::getSource()
{
  auto& instance = CommonDefinitions::instance();
  auto s = instance._sources[0];
  for (unsigned int i = 1; i < Level::SIZE; ++i) {
    s.append(instance._sources[i]);
  }
  return s;
}

RegisterCommonDefinition::RegisterCommonDefinition(const char* definition,
                                                   CommonDefinitions::Level l)
{
  // if definition contains the string "double" enable double
  if (std::string(definition).find("double") != std::string::npos) {
    CommonDefinitions::append(
        "#if defined(cl_khr_fp64)\n"
        "#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n"
        "#elif defined(cl_amd_fp64)\n"
        "#pragma OPENCL EXTENSION cl_amd_fp64 : enable\n"
        "#endif\n",
        CommonDefinitions::PRAGMA);
  }
  CommonDefinitions::append(definition, l);
}

} // namespace executor

