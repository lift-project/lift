///
/// \file LoggerDef.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef LOGGER_DEF_H_
#define LOGGER_DEF_H_

#include <iomanip>
#include <iostream>
#include <ostream>
#include <sstream>
#include <string>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS

#include "Assert.h"

namespace executor {

namespace logger_impl {

std::string getErrorString(cl_int err);

std::string formatHeader(const Logger& logger,
                         Logger::Severity::Type severity,
                         const char*    file,
                         const int      line);

} // namespace logger_impl

template <typename... Args>
void Logger::log(Severity::Type severity, const char* file, int line,
                 Args&&... args)
{
  if (severity <= _severity) {
    *_output << logger_impl::formatHeader(*this, severity, file, line);
    logArgs(*_output, std::forward<Args>(args)...);
  }
}

template <typename... Args>
void Logger::logArgs(std::ostream& output, const cl::Error& err,
                     Args&&... args)
{
  output << "OpenCL error: " << logger_impl::getErrorString(err.err())
         << " " << err.what();
  logArgs(output, std::forward<Args>(args)...);
}

template <typename T, typename... Args>
void Logger::logArgs(std::ostream& output, T value, Args&&... args)
{
  output << value;
  logArgs(output, std::forward<Args>(args)...);
}

} // namespace executor

#endif // LOGGER_DEF_H_

