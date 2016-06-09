package opencl.generator

class IllegalKernel(msg: String) extends Exception(msg)

class NotPrintableExpression(msg: String) extends Exception(msg)

class OpenCLGeneratorException(msg: String) extends Exception(msg)

class VariableNotDeclaredError(msg: String) extends OpenCLGeneratorException(msg)
