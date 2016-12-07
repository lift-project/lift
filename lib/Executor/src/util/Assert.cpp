///
/// \file Assert.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#include <cstdarg>
#include <cstdio>
#include <memory>
#include <stdexcept>

#include "util/Assert.h"

#include "util/Logger.h"

namespace executor {

namespace assert_impl {

void ASSERT_IMPL(const char* file,
                 const int   line,
                 const bool  expression,
                 const char* expressionString)
{
  if (!expression) {
    defaultLogger.log(Logger::Severity::Error, file, line,
                      "Assertion `", expressionString,
                      "' failed.");
    throw new std::runtime_error("Fatal error");
  }
}

#ifdef _MSC_VER
#pragma warning(disable:4996)
#else
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wformat-nonliteral"
#endif

void ASSERT_IMPL(const char* file,
                 const int   line,
                 const bool  expression,
                 const char* expressionString,
                 const char* formatString, ...)
{
  if (!expression) {
    va_list args;
    va_start(args, formatString);
    auto needed = vsnprintf(NULL, 0, formatString, args) + 1;
    ASSERT(needed > 0);
    {
      std::unique_ptr<char[]> buffer(new char[needed]);

      vsnprintf(&buffer[0], static_cast<size_t>(needed), formatString, args);

      defaultLogger.log(Logger::Severity::Error, file, line,
                        "Assertion `", expressionString, "' failed. ",
                        &buffer[0]);
    }
    va_end(args);
    throw new std::runtime_error("Fatal error");
  }
}

#ifdef _MSC_VER
#pragma warning(default:4996)
#else
#pragma GCC diagnostic pop
#endif

} // namespace assert_impl

} // namespace executor

