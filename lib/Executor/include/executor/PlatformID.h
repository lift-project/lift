///
/// \file PlatformID.h
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef PLATFORM_ID_H_
#define PLATFORM_ID_H_

#include <cstring>

namespace executor {

class PlatformID {
public:
  typedef size_t id_type;

  explicit PlatformID(id_type pid);

  id_type id() const;

private:
  const id_type _id;
};

} // namespace executor

#endif // PLATFORM_ID_H_

