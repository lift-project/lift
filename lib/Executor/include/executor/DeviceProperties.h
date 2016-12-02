///
/// \file DeviceProperties.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef DEVICE_PROPERTIES_H_
#define DEVICE_PROPERTIES_H_

#include <string>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS

#include "Device.h"

namespace executor {

class DeviceProperties {
public:
  static DeviceProperties nDevices(size_t n);

  static DeviceProperties allDevices();

  virtual ~DeviceProperties();

  bool match(const cl::Device& device) const;

  bool matchAndTake(const cl::Device& device);

  DeviceProperties& deviceType(Device::Type value);

private:
  DeviceProperties();

  Device::Type    _deviceType;
  bool            _takeAll;
  size_t          _count;
};

} // namespace executor

#endif // DEVICE_PROPERTIES_H_

