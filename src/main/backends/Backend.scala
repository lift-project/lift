package backends

trait Backend

case object OpenCLBackend extends Backend
case object SpatialBackend extends Backend

case object Backend {
  var _backend: Backend = OpenCLBackend

  def apply(): Backend = _backend
}