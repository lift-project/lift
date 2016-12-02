///
/// \file DeviceProperties.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.ued.ac.uk>
///

#include <string>
#include <limits>

#include "DeviceProperties.h"

namespace executor {

DeviceProperties DeviceProperties::nDevices(size_t n)
{
  DeviceProperties dp;
  dp._count = n;
  return dp;
}

DeviceProperties DeviceProperties::allDevices()
{
  DeviceProperties dp;
  dp._takeAll = true;
  return dp;
}

DeviceProperties::DeviceProperties()
  : _deviceType(Device::Type::ALL),
    _takeAll(false),
    _count(0)//,
#if 0
    _id(std::numeric_limits<Device::id_type>::max()),
    _name(),
    _vendorName(),
    _maxClockFrequency(),
    _minComputeUnits(0),
    _maxWorkGroupSize(),
    _minWorkGroupSize(0),
    _maxWorkGroups(),
    _minWorkGroups(0),
    _globalMemSize(),
    _localMemSize(),
    _minGlobalMemSize(0),
    _minLocalMemSize(0)
#endif
{
}

DeviceProperties::~DeviceProperties()
{
}

bool DeviceProperties::match(const cl::Device& device) const
{
  if (    _deviceType != Device::Type::ALL
      &&  _deviceType != device.getInfo<CL_DEVICE_TYPE>()) return false;
  return true;
}

bool DeviceProperties::matchAndTake(const cl::Device& device)
{
  if (match(device)) {
    if (_takeAll) return true;
    // _takeAll == false
    if (_count > 0) {
      --_count; // take device out of set
      return true;
    }
  }
  return false;
}

DeviceProperties& DeviceProperties::deviceType(Device::Type value)
{
  _deviceType = value;
  return *this;
}

} // namespace executor
