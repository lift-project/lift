package backends.spatial.accel

class IllegalAccelBlock(msg: String) extends Exception(msg)

class NotPrintableExpression(msg: String) extends Exception(msg)

class AccelGeneratorException(msg: String) extends Exception(msg)

class VariableNotDeclaredError(msg: String) extends AccelGeneratorException(msg)
