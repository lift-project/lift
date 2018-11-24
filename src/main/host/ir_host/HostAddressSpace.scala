package host.ir_host


import opencl.ir.OpenCLAddressSpace

abstract class HostAddressSpace extends OpenCLAddressSpace

object CPUMainMemoryAddressSpace extends HostAddressSpace {
  override def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace): Boolean = false
}

