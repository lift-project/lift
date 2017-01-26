///
/// \file Logger.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef LOGGER_H_
#define LOGGER_H_

#include <chrono>
#include <ostream>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS
#include <stdexcept>

namespace executor {

class Logger {
public:
  struct Severity {
    enum Type {
      LogAlways = 0,
      Error,
      Warning,
      Info,
      Debug,
      DebugInfo
    };
  };

  Logger();

  Logger(std::ostream& output, Severity::Type severity);

  void setOutput(std::ostream& output);

  std::ostream& output() const;

  void setLoggingLevel(Severity::Type severity);

  template <typename... Args>
  void log(Severity::Type severity, const char* file, int line,
           Args&&... args);

  const std::chrono::high_resolution_clock::time_point& startTimePoint() const;

private:
  void logArgs(std::ostream& output);

  template <typename... Args>
  void logArgs(std::ostream& output, const cl::Error& err, Args&&... args);

  template <typename T, typename... Args>
  void logArgs(std::ostream& output, T value, Args&&... args);

  std::chrono::high_resolution_clock::time_point  _startTime;
  Severity::Type                                  _severity;
  std::ostream*                                   _output;
};

#define LOG(severity, ...)\
  executor::defaultLogger.log(severity, __FILE__, __LINE__,\
                                    __VA_ARGS__)

#define LOG_ERROR(...)\
  LOG(executor::Logger::Severity::Error, __VA_ARGS__)

# define ABORT_WITH_ERROR(err)\
   LOG_ERROR(err); throw new cl::Error(err);

#define LOG_WARNING(...)\
  LOG(executor::Logger::Severity::Warning, __VA_ARGS__)

#define LOG_INFO(...)\
  LOG(executor::Logger::Severity::Info, __VA_ARGS__)

#ifdef NDEBUG

#define LOG_DEBUG(...)      (void(0))
#define LOG_DEBUG_INFO(...) (void(0))

#else  // DEBUG

#define LOG_DEBUG(...)\
  LOG(executor::Logger::Severity::Debug, __VA_ARGS__)

#define LOG_DEBUG_INFO(...)\
  LOG(executor::Logger::Severity::DebugInfo, __VA_ARGS__)

#endif // NDEBUG

// Default logger connected per default to std::clog
extern Logger defaultLogger;

} // namespace executor

#include "LoggerDef.h"

#endif // LOGGER_H_

