///
/// PlatformID.cpp
///
/// \author Michel Steuwer <michel.steuwer@ed.ac.ued.ac.uk>
///

#include "PlatformID.h"

namespace executor {

PlatformID::PlatformID(PlatformID::id_type pid)
  : _id(pid)
{
}

PlatformID::id_type PlatformID::id() const
{
  return _id;
}

} // namespace executor

