///
/// DeviceID.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#include "DeviceID.h"

namespace executor {

DeviceID::DeviceID(DeviceID::id_type pid)
  : _id(pid)
{
}

DeviceID::id_type DeviceID::id() const
{
  return _id;
}

} // namespace executor

