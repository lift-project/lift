package cbackends.common.common_ir

import opencl.ir.OpenCLAddressSpace

abstract class HostAddressSpace extends OpenCLAddressSpace

object CPUMainMemoryAddressSpace extends HostAddressSpace {
  override def containsAddressSpace(openCLAddressSpace: OpenCLAddressSpace): Boolean = false
}

