///
/// \file DeviceBuffer.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef DEVICE_BUFFER_H_
#define DEVICE_BUFFER_H_

#include <memory>
#include <string>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS

#include "Device.h"

namespace executor {

class DeviceBuffer {
public:
  typedef size_t size_type;

  DeviceBuffer();

  DeviceBuffer(const std::shared_ptr<Device>& devicePtr,
               const size_t size,
               const size_t elemSize,
               cl_mem_flags flags = CL_MEM_READ_WRITE);

  DeviceBuffer(const DeviceBuffer& rhs);

  DeviceBuffer(DeviceBuffer&& rhs);

  DeviceBuffer& operator=(const DeviceBuffer&);

  DeviceBuffer& operator=(DeviceBuffer&& rhs);

  ~DeviceBuffer();

  std::shared_ptr<Device> devicePtr() const;

  size_type size() const;

  size_type elemSize() const;

  size_type sizeInBytes() const;

  const cl::Buffer& clBuffer() const;

  bool isValid() const;

private:
  std::string getInfo() const;

  std::shared_ptr<Device>         _device;
  size_type                       _size;
  size_type                       _elemSize;
  cl_mem_flags                    _flags; // TODO: Needed?
  cl::Buffer                      _buffer;
};

} // namespace executor

#endif // DEVICE_BUFFER_H_

