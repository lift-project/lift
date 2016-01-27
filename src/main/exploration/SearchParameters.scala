package exploration

object SearchParameters {
  // matrix size
  val matrix_size = 1024

  // Minimum number of work item per workgroup
  val min_work_items = 128

  // Minimal global grid size
  val min_grid_size = 4

  // Max amount of private memory allocated (this is not necessarily the number of registers)
  val max_amount_private_memory = 1024

  // Max static amount of local memory
  val max_amount_local_memory = 49152

  // Minimum number of workgroups
  val min_num_workgroups = 8

  // Maximum number of workgroups
  val max_num_workgroups = 10000

  // Don't bother cross validating if the timing is not better than the current best solution
  val only_crossvalidate_better_solutions = true

  // print the execution stats after running the kernel
  val print_stats = false

}
