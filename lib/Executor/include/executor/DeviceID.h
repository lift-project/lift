///
/// \file DeviceID.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef DEVICE_ID_H
#define DEVICE_ID_H

#include <cstring>

namespace executor {

class DeviceID {
public:
  typedef size_t id_type;

  explicit DeviceID(id_type pid);

  id_type id() const;

private:
  const id_type _id;
};

} // namespace executor

#endif // DEVICE_ID_H

