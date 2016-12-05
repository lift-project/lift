///
/// \file Core.h
///
///	\author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef CORE_H_
#define CORE_H_

#include "Device.h"
#include "DeviceID.h"
#include "DeviceProperties.h"
#include "PlatformID.h"

namespace executor {

///
/// \brief Public name for a type representing different types of device.
///        E.g. CPU or GPU.
///
typedef Device::Type device_type;

///
/// \brief Creates a DeviceProperties object representing all devices in
///        the system. This object should be used as parameter of the
///        init(DeviceProperties) function.
///
DeviceProperties allDevices();

///
/// \brief Creates a DeviceProperties object representing n devices.
///        This object should be used as parameter of the
///        init(DeviceProperties) function.
///
/// \param n Number of OpenCL devices.
///
DeviceProperties nDevices(size_t n);

///
/// \brief Creates an OpenCL platform ID to be used as parameter of the
///        init(PlatformID, DeviceID) function.
///
/// \param pID The ID of the OpenCL platform.
///
PlatformID platform(size_t pID);

///
/// \brief Creates an OpenCL device ID to be used as parameter of the
///        init(PlatformID, DeviceID) function.
///
/// \param dID The ID of the OpenCL device.
///
DeviceID device(size_t dID);

///
/// \brief Initializes the executor library. This function (or another init
///        function) has to be called prior to every other function in the
///        library.
///
/// \param properties Specifies properties about the devices to use by default
///        all devices are selected by calling the allDevices() function. To
///        select a specific number of devices call nDevices(size_t). Additional
///        properties can be specified using member functions of the
///        DeviceProperties class.
///
/// \sa DeviceSelectionTests.cpp
///
void init(DeviceProperties properties = allDevices());

///
/// \brief Initializes the executor library. This function (or another init
///        function) has to be called prior to every other function in the
///        library.
///
/// This function should only be used if one specific OpenCL device should be
/// used by executor.
///
/// \param pID The ID of the OpenCL platform to be used.
/// \param dID The ID of the OpenCL device to be used.
///
void init(PlatformID pID, DeviceID dID);

///
/// \brief Frees all resources allocated internally by the executor.
///
/// This function normally does not have to be called explicitly.
/// If for some reason the number of devices used by the executor should be changed
/// after a first initialization, this function has to be called prior to
/// calling one of the init() functions again.
///
void terminate();

} // namespace executor

#endif // CORE_H_
