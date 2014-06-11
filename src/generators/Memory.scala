package generators

import ir._

abstract class OpenCLAddressSpace
object LocalMemory extends OpenCLAddressSpace
object GlobalMemory extends OpenCLAddressSpace

case class OpenCLMemory(variable: Var, size: Cst, val addressSpace: OpenCLAddressSpace) extends Memory