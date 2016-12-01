///
/// \file DeviceList.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef DEVICE_LIST_H_
#define DEVICE_LIST_H_

#include <initializer_list>
#include <memory>
#include <vector>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>
#undef  __CL_ENABLE_EXCEPTIONS

#include "Device.h"

namespace executor {

class DeviceID;
class DeviceProperties;
class PlatformID;

class DeviceList {
  typedef std::shared_ptr<Device> device_ptr;
  typedef std::vector<device_ptr> vector_type;
public:
  typedef vector_type::value_type value_type;
  typedef vector_type::reference reference;
  typedef vector_type::const_reference const_reference;
  typedef vector_type::iterator iterator;
  typedef vector_type::const_iterator const_iterator;
  typedef vector_type::reverse_iterator reverse_iterator;
  typedef vector_type::const_reverse_iterator const_reverse_iterator;
  typedef vector_type::size_type size_type;

  DeviceList();
  // DeviceList(); = default;
  // throws _devices should be initialized in the member
  // initialization list warning

  DeviceList(std::initializer_list<std::shared_ptr<Device>> list);

  //DeviceList(const DeviceList&) = default;

  //DeviceList& operator=(const DeviceList&) = default;

  //~DeviceList() = default;

  bool operator==(const DeviceList& rhs) const;

  void init(DeviceProperties properties);

  void init(PlatformID pID, DeviceID dID);

  void clear();

  void barrier() const;

  const_iterator begin() const;

  const_iterator end() const;

  const_reverse_iterator rbegin() const;

  const_reverse_iterator rend() const;

  size_type size() const;

  bool empty() const;

  const_reference operator[](size_type n) const;

  const_reference at(size_type n) const;

  const_reference front() const;

  const_reference back() const;

private:
  vector_type _devices;
};

extern DeviceList globalDeviceList;


} // namespace executor

#endif // DEVICE_LIST_H_

