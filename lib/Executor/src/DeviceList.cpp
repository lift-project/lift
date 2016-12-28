///
/// \file DeviceList.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#include <algorithm>
#include <stdexcept>
#include <functional>
#include <string>
#include <vector>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS

#include "util/Assert.h"
#include "util/Logger.h"

#include "DeviceList.h"
#include "Device.h"
#include "DeviceID.h"
#include "DeviceProperties.h"
#include "PlatformID.h"

namespace executor {

DeviceList globalDeviceList;

DeviceList::DeviceList()
  : _devices()
{
}

DeviceList::DeviceList(std::initializer_list<std::shared_ptr<Device>> list)
  : _devices(list.begin(), list.end())
{
}

bool DeviceList::operator==(const DeviceList& rhs) const
{
  return _devices == rhs._devices;
}

void DeviceList::init(DeviceProperties properties)
{
  ASSERT(_devices.empty()); // call only once
  try {
    std::vector<cl::Platform> platforms;
    cl::Platform::get(&platforms);
    ASSERT(platforms.size() > 0); // TODO: Throw Exception

    LOG_INFO(platforms.size(), " OpenCL platform(s) found");

    size_t deviceId = 0;

    // for each platform ...
    for (auto& platform : platforms) {
      // .. get all devices ..
      std::vector<cl::Device> devices;
      platform.getDevices(CL_DEVICE_TYPE_ALL, &devices);

      LOG_INFO(devices.size(), " device(s) for OpenCL platform `",
               platform.getInfo<CL_PLATFORM_NAME>(), "' found");

      for (auto& device : devices) {
        // ... if device not matches properties ..
        if (!properties.matchAndTake(device)) {
          LOG_INFO("Skip device `", device.getInfo<CL_DEVICE_NAME>(),
                   "' not machting given criteria for device selection.");
          continue; // skip device
        }
        // ... create Device instance and push into _devices
        _devices.push_back( std::make_shared<Device>(device,
                                                     platform,
                                                     deviceId)
                          );
        ++deviceId;
      }
    }
  } catch (cl::Error& err) {
    ABORT_WITH_ERROR(err);
  }
  ASSERT_MESSAGE(!_devices.empty(), "None OpenCL device was selected.");
  LOG_INFO("Using ", _devices.size(), " OpenCL device(s) in total");
}

void DeviceList::init(PlatformID pID, DeviceID dID)
{
  ASSERT(_devices.empty()); // call only once
  try {
    std::vector<cl::Platform> platforms;
    cl::Platform::get(&platforms);
    if (platforms.size() <= pID.id()) {
      throw std::invalid_argument("Given PlatformID is invalid");
    }

    auto& platform = platforms[pID.id()];
    
    std::vector<cl::Device> devices;
    platform.getDevices(CL_DEVICE_TYPE_ALL, &devices);
    if (devices.size() <= dID.id()) {
      throw std::invalid_argument("Given DeviceID is invalid");
    }

    auto& device = devices[dID.id()];

    _devices.push_back( std::make_shared<Device>(device,
                                                 platform,
                                                 0)
                      );

  } catch (cl::Error& err) {
    ABORT_WITH_ERROR(err);
  } catch (std::invalid_argument& ia) {
    LOG_ERROR("Invalid arguments provided: ", ia.what());
    abort();
  }
  LOG_INFO("Using ", _devices.size(), " OpenCL device(s) in total");
}

void DeviceList::clear()
{
  _devices.clear();
}

void DeviceList::barrier() const
{
  try {
    std::for_each( _devices.begin(), _devices.end(),
                   std::mem_fn(&Device::wait) );
  } catch (cl::Error& err) {
    ABORT_WITH_ERROR(err);
  }
  LOG_DEBUG_INFO("Finished waiting for ", _devices.size(), " devices");
}

DeviceList::const_iterator DeviceList::begin() const
{
  return _devices.begin();
}

DeviceList::const_iterator DeviceList::end() const
{
  return _devices.end();
}

DeviceList::const_reverse_iterator DeviceList::rbegin() const
{
  return _devices.rbegin();
}

DeviceList::const_reverse_iterator DeviceList::rend() const
{
  return _devices.rend();
}

DeviceList::size_type DeviceList::size() const
{
  return _devices.size();
}

bool DeviceList::empty() const
{
  return _devices.empty();
}

DeviceList::const_reference DeviceList::operator[](size_type n) const
{
  return _devices.operator[](n);
}

DeviceList::const_reference DeviceList::at(size_type n) const
{
  return _devices.at(n);
}

DeviceList::const_reference DeviceList::front() const
{
  return _devices.front();
}

DeviceList::const_reference DeviceList::back() const
{
  return _devices.back();
}

} // namespace executor

