package cbackends

package object common {
  object WindowsOSInUse {
    def apply(): Boolean = System.getenv("LIFT_OS_WIN") != null
  }

  object OpenCLVersion {
    var is200: Boolean = !WindowsOSInUse()
  }
}
