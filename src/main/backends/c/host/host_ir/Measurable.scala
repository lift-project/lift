package backends.c.host.host_ir

trait CPUMeasurable {

  val cpu_timer: Boolean

}

trait GPUMeasurable {

  val gpu_timer: Boolean

}

trait Measurable extends CPUMeasurable with GPUMeasurable
