package host.ir_host


import opencl.ir.OpenCLAddressSpace

abstract class HostAddressSpace extends OpenCLAddressSpace

object CPUMainMemory extends HostAddressSpace {
  override def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace): Boolean = false
}
