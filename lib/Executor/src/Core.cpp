///
/// \file Core.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#include "util/Logger.h"

#include "Core.h"

#include "DeviceList.h"
#include "DeviceProperties.h"
#include "PlatformID.h"
#include "DeviceID.h"

namespace executor {

void init(DeviceProperties properites)
{
  globalDeviceList.init(std::move(properites));
}

void init(PlatformID pID, DeviceID dID)
{
  globalDeviceList.init(pID, dID);
}

DeviceProperties allDevices()
{
  return DeviceProperties::allDevices();
}

DeviceProperties nDevices(size_t n)
{
  return DeviceProperties::nDevices(n);
}

PlatformID platform(size_t pID)
{
  return PlatformID(pID);
}

DeviceID device(size_t dID)
{
  return DeviceID(dID);
}

void terminate()
{
  globalDeviceList.clear();
  LOG_INFO("Executor terminating. Freeing all resources.");
}

} // namespace executor

