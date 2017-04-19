///
/// \file Logger.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#include <chrono>
#include <functional>
#include <ostream>

#include "util/Logger.h"

namespace executor {

Logger defaultLogger;

Logger::Logger()
  : _startTime(std::chrono::high_resolution_clock::now()),
#ifdef NDEBUG
   _severity(Severity::Info)  // set default level to Info in release
#else
   _severity(Severity::DebugInfo) // ... and DebugInfo in debug builds
#endif
  , _output(&std::clog)
{
}

Logger::Logger(std::ostream& output, Severity::Type severity)
  : _startTime(std::chrono::high_resolution_clock::now()),
    _severity(severity), _output(&output)
{
}

void Logger::setOutput(std::ostream& output)
{
  _output = &output;
}

std::ostream& Logger::output() const
{
  return *_output;
}

void Logger::setLoggingLevel(Severity::Type severity)
{
  _severity = severity;
}

const std::chrono::high_resolution_clock::time_point&
  Logger::startTimePoint() const
{
  return _startTime;
}

void Logger::logArgs(std::ostream& output)
{
  output << std::endl;
}

namespace logger_impl {

std::string severityToString(Logger::Severity::Type severity)
{
  switch (severity) {
    case Logger::Severity::LogAlways  : return "     ";
    case Logger::Severity::Error      : return "ERROR";
    case Logger::Severity::Warning    : return " WARN";
    case Logger::Severity::Info       : return " INFO";
    case Logger::Severity::Debug      : return "DEBUG";
    case Logger::Severity::DebugInfo  : return "DINFO";
  }
  ASSERT_MESSAGE(false, "This statement should never be reached");
  return "";
}

std::string getFileName(const char* file)
{
  std::string filename(file);
  auto found = filename.rfind("/");
  if (found != std::string::npos) {
    filename = filename.erase(0, found+1);
  }
  return filename;
}

std::string getErrorString(cl_int err)
{
  std::ostringstream ostr;
  switch (err) {
  case CL_SUCCESS:
    ostr << "CL_SUCCESS"; break;
  case CL_DEVICE_NOT_FOUND:
    ostr << "CL_DEVICE_NOT_FOUND"; break;
  case CL_DEVICE_NOT_AVAILABLE:
    ostr << "CL_DEVICE_NOT_AVAILABLE"; break;
  case CL_COMPILER_NOT_AVAILABLE:
    ostr << "CL_COMPILER_NOT_AVAILABLE"; break;
  case CL_MEM_OBJECT_ALLOCATION_FAILURE:
    ostr << "CL_MEM_OBJECT_ALLOCATION_FAILURE"; break;
  case CL_OUT_OF_RESOURCES:
    ostr << "CL_OUT_OF_RESOURCES"; break;
  case CL_OUT_OF_HOST_MEMORY:
    ostr << "CL_OUT_OF_HOST_MEMORY"; break;
  case CL_PROFILING_INFO_NOT_AVAILABLE:
    ostr << "CL_PROFILING_INFO_NOT_AVAILABLE"; break;
  case CL_MEM_COPY_OVERLAP:
    ostr << "CL_MEM_COPY_OVERLAP"; break;
  case CL_IMAGE_FORMAT_MISMATCH:
    ostr << "CL_IMAGE_FORMAT_MISMATCH"; break;
  case CL_IMAGE_FORMAT_NOT_SUPPORTED:
    ostr << "CL_IMAGE_FORMAT_NOT_SUPPORTED"; break;
  case CL_BUILD_PROGRAM_FAILURE:
    ostr << "CL_BUILD_PROGRAM_FAILURE"; break;
  case CL_MAP_FAILURE:
    ostr << "CL_MAP_FAILURE"; break;
  case CL_INVALID_VALUE:
    ostr << "CL_INVALID_VALUE"; break;
  case CL_INVALID_DEVICE_TYPE:
    ostr << "CL_INVALID_DEVICE_TYPE"; break;
  case CL_INVALID_PLATFORM:
    ostr << "CL_INVALID_PLATFORM"; break;
  case CL_INVALID_DEVICE:
    ostr << "CL_INVALID_DEVICE"; break;
  case CL_INVALID_CONTEXT:
    ostr << "CL_INVALID_CONTEXT"; break;
  case CL_INVALID_QUEUE_PROPERTIES:
    ostr << "CL_INVALID_QUEUE_PROPERTIES"; break;
  case CL_INVALID_COMMAND_QUEUE:
    ostr << "CL_INVALID_COMMAND_QUEUE"; break;
  case CL_INVALID_HOST_PTR:
    ostr << "CL_INVALID_HOST_PTR"; break;
  case CL_INVALID_MEM_OBJECT:
    ostr << "CL_INVALID_MEM_OBJECT"; break;
  case CL_INVALID_IMAGE_FORMAT_DESCRIPTOR:
    ostr << "CL_INVALID_IMAGE_FORMAT_DESCRIPTOR"; break;
  case CL_INVALID_IMAGE_SIZE:
    ostr << "CL_INVALID_IMAGE_SIZE"; break;
  case CL_INVALID_SAMPLER:
    ostr << "CL_INVALID_SAMPLER"; break;
  case CL_INVALID_BINARY:
    ostr << "CL_INVALID_BINARY"; break;
  case CL_INVALID_BUILD_OPTIONS:
    ostr << "CL_INVALID_BUILD_OPTIONS"; break;
  case CL_INVALID_PROGRAM:
    ostr << "CL_INVALID_PROGRAM"; break;
  case CL_INVALID_PROGRAM_EXECUTABLE:
    ostr << "CL_INVALID_PROGRAM_EXECUTABLE"; break;
  case CL_INVALID_KERNEL_NAME:
    ostr << "CL_INVALID_KERNEL_NAME"; break;
  case CL_INVALID_KERNEL_DEFINITION:
    ostr << "CL_INVALID_KERNEL_DEFINITION"; break;
  case CL_INVALID_KERNEL:
    ostr << "CL_INVALID_KERNEL"; break;
  case CL_INVALID_ARG_INDEX:
    ostr << "CL_INVALID_ARG_INDEX"; break;
  case CL_INVALID_ARG_VALUE:
    ostr << "CL_INVALID_ARG_VALUE"; break;
  case CL_INVALID_ARG_SIZE:
    ostr << "CL_INVALID_ARG_SIZE"; break;
  case CL_INVALID_KERNEL_ARGS:
    ostr << "CL_INVALID_KERNEL_ARGS"; break;
  case CL_INVALID_WORK_DIMENSION:
    ostr << "CL_INVALID_WORK_DIMENSION"; break;
  case CL_INVALID_WORK_GROUP_SIZE:
    ostr << "CL_INVALID_WORK_GROUP_SIZE"; break;
  case CL_INVALID_WORK_ITEM_SIZE:
    ostr << "CL_INVALID_WORK_ITEM_SIZE"; break;
  case CL_INVALID_GLOBAL_OFFSET:
    ostr << "CL_INVALID_GLOBAL_OFFSET"; break;
  case CL_INVALID_EVENT_WAIT_LIST:
    ostr << "CL_INVALID_EVENT_WAIT_LIST"; break;
  case CL_INVALID_EVENT:
    ostr << "CL_INVALID_EVENT"; break;
  case CL_INVALID_OPERATION:
    ostr << "CL_INVALID_OPERATION"; break;
  case CL_INVALID_GL_OBJECT:
    ostr << "CL_INVALID_GL_OBJECT"; break;
  case CL_INVALID_BUFFER_SIZE:
    ostr << "CL_INVALID_BUFFER_SIZE"; break;
  case CL_INVALID_MIP_LEVEL:
    ostr << "CL_INVALID_MIP_LEVEL"; break;
  default:
    ostr << "unknown error";
  }
  ostr << " (code: " << err << ")";
  return ostr.str();
}

std::string formatHeader(const Logger& logger,
                         Logger::Severity::Type severity,
                         const char*    file,
                         const int      line)
{
  std::ostringstream stream;

  stream << "["
         << std::setw(16) << std::setfill('=')
         << getFileName(file) << ":"
         << std::setw( 4) << std::setfill(' ') << std::left
         << line << " ";

#ifndef NPROFILING
  auto sinceStart =   std::chrono::high_resolution_clock::now()
                    - logger.startTimePoint();
  using std::chrono::milliseconds; // avoid too long next line
  auto ms = std::chrono::duration_cast<milliseconds>(sinceStart).count();
  char prevFill = stream.fill('0'); // save fill char
  stream << std::right
         // extract just the 3 last digits of the seconds
         << std::setw(3) << (ms / 1000) % 1000 << "."
         // extract just the milliseconds
         << std::setw(3) << ms % 1000          << "s";
  stream.fill(prevFill); // reset fill char
  stream << " ";
#endif

  stream << std::setw( 5) << std::left
         << severityToString(severity)
         << "] ";

  return stream.str();
}

} // namespace logger_impl

} // namespacce executor

