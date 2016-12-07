///
///  VectorDef.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef VECTOR_DEF_H_
#define VECTOR_DEF_H_

#include <algorithm>
#include <ios>
#include <iterator>
#include <memory>
#include <string>
#include <sstream>
#include <utility>
#include <vector>

#include "util/Assert.h"
#include "util/Logger.h"

#include "Source.h"

#include "Device.h"
#include "DeviceBuffer.h"
#include "DeviceList.h"
#include "Event.h"
#include "Util.h"

namespace executor {

template <typename T>
RegisterVectorDeviceFunctions<T>::RegisterVectorDeviceFunctions()
{
  CommonDefinitions::append(Vector<T>::deviceFunctions(),
      CommonDefinitions::Level::GENERATED_DEFINITION);
}

template <typename T>
Vector<T>::Vector()
  : _size(0),
    _hostBufferUpToDate(true),
    _deviceBuffersUpToDate(true),
    _hostBuffer(),
    _deviceBuffers()
{
  (void)registerVectorDeviceFunctions;
  LOG_DEBUG_INFO("Created new Vector object (", this, ") with ",
                 getDebugInfo());
}

template <typename T>
Vector<T>::Vector(const size_type size, const value_type& value)
  : _size(size),
    _hostBufferUpToDate(true),
    _deviceBuffersUpToDate(false),
    _hostBuffer(size, value),
    _deviceBuffers()
{
  (void)registerVectorDeviceFunctions;
  LOG_DEBUG_INFO("Created new Vector object (", this, ") with ",
                 getDebugInfo());
}

template <typename T>
template <typename InputIterator>
Vector<T>::Vector(InputIterator first, InputIterator last)
  : _size(std::distance(first, last)),
    _hostBufferUpToDate(true),
    _deviceBuffersUpToDate(false),
    _hostBuffer(first, last),
    _deviceBuffers()
{
  (void)registerVectorDeviceFunctions;
  LOG_DEBUG_INFO("Created new Vector object (", this, ") with ",
                 getDebugInfo());
}

template <typename T>
Vector<T>::Vector(const Vector<T>& rhs)
  : _size(rhs._size),
    _hostBufferUpToDate(rhs._hostBufferUpToDate),
    _deviceBuffersUpToDate(rhs._deviceBuffersUpToDate),
    _hostBuffer(rhs._hostBuffer),
    _deviceBuffers(rhs._deviceBuffers)
{
  (void)registerVectorDeviceFunctions;
  LOG_DEBUG_INFO("Created new Vector object (", this, ") by copying (", &rhs,
                 ") with ", getDebugInfo());
}

template <typename T>
Vector<T>::Vector(Vector<T>&& rhs)
  : _size(std::move(rhs._size)),
    _hostBufferUpToDate(std::move(rhs._hostBufferUpToDate)),
    _deviceBuffersUpToDate(std::move(rhs._deviceBuffersUpToDate)),
    _hostBuffer(std::move(rhs._hostBuffer)),
    _deviceBuffers(std::move(rhs._deviceBuffers))
{
  (void)registerVectorDeviceFunctions;
  rhs._size = 0;
  rhs._hostBufferUpToDate = false;
  rhs._deviceBuffersUpToDate = false;
  LOG_DEBUG_INFO("Created new Vector object (", this, ") by moving from (",
                 &rhs,") with ", getDebugInfo());
}

template <typename T>
Vector<T>& Vector<T>::operator=(const Vector<T>& rhs)
{
  if (this == &rhs) return *this; // handle self assignment
  _size                   = rhs._size;
  _hostBufferUpToDate     = rhs._hostBufferUpToDate;
  _deviceBuffersUpToDate  = rhs._deviceBuffersUpToDate;
  _hostBuffer             = rhs._hostBuffer;
  _deviceBuffers          = rhs._deviceBuffers;
  LOG_DEBUG_INFO("Assignment to Vector object (", this, ") now with ",
                 getDebugInfo());
  return *this;
}

template <typename T>
Vector<T>& Vector<T>::operator=(Vector<T>&& rhs)
{
  _size                   = std::move(rhs._size);
  _hostBufferUpToDate     = std::move(rhs._hostBufferUpToDate);
  _deviceBuffersUpToDate  = std::move(rhs._deviceBuffersUpToDate);
  _hostBuffer             = std::move(rhs._hostBuffer);
  _deviceBuffers          = std::move(rhs._deviceBuffers);
  rhs._size = 0;
  rhs._hostBufferUpToDate = false;
  rhs._deviceBuffersUpToDate = false;
  LOG_DEBUG_INFO("Move assignment to Vector object (", this, ") from (",
                 &rhs,") now with ", getDebugInfo());
  return *this;
}

template <typename T>
Vector<T>::~Vector()
{
  //LOG_DEBUG_INFO("Vector object (", this, ") with ", getDebugInfo(),
  //               " destroyed");
}

template <typename T>
typename Vector<T>::iterator Vector<T>::begin()
{
  copyDataToHost();
  return _hostBuffer.begin();
}

template <typename T>
typename Vector<T>::const_iterator Vector<T>::begin() const
{
  copyDataToHost();
  return _hostBuffer.begin();
}

template <typename T>
typename Vector<T>::iterator Vector<T>::end()
{
  copyDataToHost();
  return _hostBuffer.end();
}

template <typename T>
typename Vector<T>::const_iterator Vector<T>::end() const
{
  copyDataToHost();
  return _hostBuffer.end();
}

#if 0
template <typename T>
typename Vector<T>::reverse_iterator Vector<T>::rbegin()
{
  return _hostBuffer.rbegin();
}

template <typename T>
typename Vector<T>::const_reverse_iterator Vector<T>::rbegin() const
{
  return _hostBuffer.rbegin();
}

template <typename T>
typename Vector<T>::reverse_iterator Vector<T>::rend()
{
  return _hostBuffer.rend();
}

template <typename T>
typename Vector<T>::const_reverse_iterator Vector<T>::rend() const
{
  return _hostBuffer.rend();
}
#endif

template <typename T>
typename Vector<T>::size_type Vector<T>::size() const
{
  return _size;
}

template <typename T>
Sizes Vector<T>::sizes() const
{
  Sizes s;
#if 0
  ASSERT(_distribution != nullptr);

  for (auto& devicePtr : _distribution->devices()) {
    s.push_back(this->_distribution->sizeForDevice(*this,
                                                  devicePtr->id()));
  }
#endif
  return s;
}

template <typename T>
typename Vector<T>::size_type Vector<T>::max_size() const
{
  // TODO: take device sizes into account
  return _hostBuffer.max_size();
}

template <typename T>
void Vector<T>::resize( typename Vector<T>::size_type sz, T c )
{
  _size = sz;
  if (_hostBufferUpToDate) {
    _hostBuffer.resize(sz, c);
    _deviceBuffersUpToDate = false;
    _deviceBuffers.clear();
  }
  LOG_DEBUG_INFO("Vector object (", this, ") resized, now with ",
                 getDebugInfo());
}

template <typename T>
typename Vector<T>::size_type Vector<T>::capacity() const
{
  return std::max(_hostBuffer.capacity(), _size);
}

template <typename T>
bool Vector<T>::empty() const
{
  return (_size == 0);
}

template <typename T>
void Vector<T>::reserve( typename Vector<T>::size_type n )
{
  return _hostBuffer.reserve(n);
}

template <typename T>
typename Vector<T>::reference Vector<T>::operator[]( typename Vector<T>::size_type n )
{
  copyDataToHost();
  return _hostBuffer.operator[](n);
}

template <typename T>
typename Vector<T>::const_reference
  Vector<T>::operator[]( typename Vector<T>::size_type n ) const
{
  copyDataToHost();
  return _hostBuffer.operator[](n);
}

template <typename T>
typename Vector<T>::reference Vector<T>::at( typename Vector<T>::size_type n )
{
  copyDataToHost();
  return _hostBuffer.at(n);
}

template <typename T>
typename Vector<T>::const_reference
  Vector<T>::at( typename Vector<T>::size_type n ) const
{
  copyDataToHost();
  return _hostBuffer.at(n);
}

template <typename T>
typename Vector<T>::reference Vector<T>::front()
{
  copyDataToHost();
  return _hostBuffer.front();
}

template <typename T>
typename Vector<T>::const_reference Vector<T>::front() const
{
  copyDataToHost();
  return _hostBuffer.front();
}

template <typename T>
typename Vector<T>::reference Vector<T>::back()
{
  copyDataToHost();
  return _hostBuffer.back();
}

template <typename T>
typename Vector<T>::const_reference Vector<T>::back() const
{
  copyDataToHost();
  return _hostBuffer.back();
}

template <typename T>
template <class InputIterator>
void Vector<T>::assign( InputIterator first, InputIterator last )
{
  _hostBuffer.assign(first, last);
}

template <typename T>
void Vector<T>::assign( typename Vector<T>::size_type n, const T& u )
{
  _hostBuffer.assign(n, u);
}

template <typename T>
void Vector<T>::push_back( const T& x )
{
  _hostBuffer.push_back(x);
  ++_size;
}

template <typename T>
void Vector<T>::pop_back()
{
  _hostBuffer.pop_back();
  --_size;
}

template <typename T>
typename Vector<T>::iterator
    Vector<T>::insert(typename Vector<T>::iterator position, const T& x)
{
  ++_size;
  return _hostBuffer.insert(position, x);
}

template <typename T>
typename Vector<T>::iterator
    Vector<T>::insert(typename Vector<T>::iterator position,
                      typename Vector<T>::size_type n, const T& x)
{
  _size += n;
  return _hostBuffer.insert(position, n, x);
}

template <typename T>
template <class InputIterator>
void Vector<T>::insert(typename Vector<T>::iterator position,
                       InputIterator first, InputIterator last)
{
  _hostBuffer.insert(position, first, last);
  _size = _hostBuffer.size();
// TODO This is NOT Compiling !?!?: _size += std::distance(first, last);
}

template <typename T>
typename Vector<T>::iterator
    Vector<T>::erase(typename Vector<T>::iterator position)
{
  --_size;
  return _hostBuffer.erase(position);
}

template <typename T>
typename Vector<T>::iterator Vector<T>::erase( typename Vector<T>::iterator first,
                                               typename Vector<T>::iterator last )
{
  _size -= std::distance(first, last);
  return _hostBuffer.erase(first, last);
}

template <typename T>
void Vector<T>::swap( Vector<T>& rhs )
{
  // TODO: swap device buffers and the rest
  _hostBuffer.swap(rhs._hostBuffer);
  // swap sizes:
  std::swap(_size, rhs._size);
}

template <typename T>
void Vector<T>::clear()
{
  _hostBuffer.clear();
  _size = 0;
}

template <typename T>
void Vector<T>::createDeviceBuffers() const
{
  // create device buffers only if none have been created so far
  if (_deviceBuffers.empty()) {
    forceCreateDeviceBuffers();
  }
}

template <typename T>
void Vector<T>::forceCreateDeviceBuffers() const
{
  ASSERT(_size > 0);

  _deviceBuffers.clear();

  auto& devicePtr = globalDeviceList.front();
  _deviceBuffers.insert(
      { devicePtr->id(), 
        DeviceBuffer(devicePtr, _size, sizeof(T)) });
}

template <typename T>
Event Vector<T>::startUpload() const
{
  ASSERT(_size > 0);
  ASSERT(!_deviceBuffers.empty());

  Event events;

  if (_deviceBuffersUpToDate) return events;

  auto& devicePtr = globalDeviceList.front();
  auto event = devicePtr->enqueueWrite(this->deviceBuffer(*devicePtr),
                                       _hostBuffer.begin());
  events.insert(event);

  _deviceBuffersUpToDate = true;

  LOG_DEBUG_INFO("Started data upload to 1 device (", getInfo(), ")");

  return events;
}

template <typename T>
void Vector<T>::copyDataToDevices() const
{
  if (_hostBufferUpToDate && !_deviceBuffersUpToDate) {
    startUpload().wait();
  }
}

template <typename T>
Event Vector<T>::startDownload() const
{
  ASSERT(_size > 0);
  ASSERT(!_deviceBuffers.empty());

  Event events;

  if (_hostBufferUpToDate) return events;

  _hostBuffer.resize(_size); // make enough room to store data

  auto& devicePtr = globalDeviceList.front();
  auto event = devicePtr->enqueueRead(this->deviceBuffer(*devicePtr),
                                      _hostBuffer.begin());
  events.insert(event);

  _hostBufferUpToDate = true;

  LOG_DEBUG_INFO("Started data download from 1 device (", getInfo() ,")");

  return events;
}

template <typename T>
void Vector<T>::copyDataToHost() const
{
  if (_deviceBuffersUpToDate && !_hostBufferUpToDate) {
    startDownload().wait();
  }
}

template <typename T>
void Vector<T>::dataOnDeviceModified() const
{
  _hostBufferUpToDate     = false;
  _deviceBuffersUpToDate  = true;
  LOG_DEBUG_INFO("Data on devices marked as modified");
}

template <typename T>
void Vector<T>::dataOnHostModified() const
{
  _hostBufferUpToDate     = true;
  _deviceBuffersUpToDate  = false;
  LOG_DEBUG_INFO("Data on host marked as modified");
}

template <typename T>
const DeviceBuffer&
  Vector<T>::deviceBuffer(const Device& device) const
{
  return _deviceBuffers[device.id()];
}

template <typename T>
DeviceBuffer& Vector<T>::deviceBuffer(const Device& device)
{
  return _deviceBuffers[device.id()];
}

template <typename T>
typename Vector<T>::host_buffer_type& Vector<T>::hostBuffer() const
{
  return _hostBuffer;
}

template <typename T>
std::string Vector<T>::deviceFunctions()
{
  std::string type = util::typeToString<T>();
  std::stringstream s;

  // found "double" => enable double
  if (type.find("double") != std::string::npos) {
    s << "#if defined(cl_khr_fp64)\n"
         "#pragma OPENCL EXTENSION cl_khr_fp64 : enable\n"
         "#elif defined(cl_amd_fp64)\n"
         "#pragma OPENCL EXTENSION cl_amd_fp64 : enable\n"
         "#endif\n";
  }

  return s.str();
}

template <typename T>
std::string Vector<T>::getInfo() const
{
  std::stringstream s;
  s << "size: "                   << _size;
  return s.str();
}

template <typename T>
std::string Vector<T>::getDebugInfo() const
{
  std::stringstream s;
  s << getInfo()
    << std::boolalpha
    << ", deviceBuffersCreated: "  << (!_deviceBuffers.empty())
    << ", hostBufferUpToDate: "    << _hostBufferUpToDate
    << ", deviceBuffersUpToDate: " << _deviceBuffersUpToDate
    << ", hostBuffer: "            << _hostBuffer.data();
  return s.str();
}

} // namespace executor

#endif // VECTOR_DEF_H_
