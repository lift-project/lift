///
/// \file Event.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef EVENT_H_
#define EVENT_H_

#include <initializer_list>
#include <vector>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS

namespace executor {

class Event {
public:
  Event();

  Event(const std::vector<cl::Event>& events);

  Event(std::initializer_list<cl::Event> events);

  //Event(const Event& rhs) = default;

  Event(Event&& rhs);

  //Event& operator=(const Event& rhs) = default;

  Event& operator=(Event&& rhs);

  //~Event() = default;

  void insert(const cl::Event& event);

  void wait();
private:
  std::vector<cl::Event> _events;
};

} // namespace executor

#endif // EVENT_H_

