///
/// \file Event.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.ued.ac.uk>
///

#include <algorithm>
#include <functional>
#include <initializer_list>
#include <vector>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS

#include "util/Logger.h"

#include "Event.h"

namespace executor {


Event::Event()
  : _events()
{
}

Event::Event(const std::vector<cl::Event>& events)
  : _events(events)
{
}

Event::Event(std::initializer_list<cl::Event> events)
  : _events(events.begin(), events.end())
{
}

Event::Event(Event&& rhs)
  : _events(std::move(rhs._events))
{
}

Event& Event::operator=(Event&& rhs)
{
  _events = std::move(rhs._events);
  return *this;
}

void Event::insert(const cl::Event& event)
{
  _events.push_back(event);
}

void Event::wait()
{
  try {
    // Wait for every single device, because of different context objects
    std::for_each( _events.begin(), _events.end(),
                   std::mem_fn(&cl::Event::wait) );
  } catch (cl::Error& err) {
    ABORT_WITH_ERROR(err);
  }
}

} // namespace executor
