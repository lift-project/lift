// [standard includes]
#include <vector>
#include <set>
#include <queue>
#include <algorithm>
#include <fstream>
#include <sstream>
#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>
#include <atomic>
#include <memory>

// [external includes]
#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>

// [local includes]
#include "csv_utils.h"
#include "file_utils.h"
#include "options.h"
#include "run.h"

struct MMRun: public Run {
  // input matrix size
  std::size_t size;

  // list of additional buffers to allocate
  std::vector<int> extra_buffer_size;

  std::vector<cl::Buffer> extra_args;

  MMRun(const std::vector<std::string>& values) {
    assert(values.size() > 8 && "Bad CSV format");

    size = Csv::readInt(values[0]);
    glob1 = Csv::readInt(values[1]);
    glob2 = Csv::readInt(values[2]);
    glob3 = Csv::readInt(values[3]);
    loc1 = Csv::readInt(values[4]);
    loc2 = Csv::readInt(values[5]);
    loc3 = Csv::readInt(values[6]);

    hash = values[7];
    hash.erase(std::remove_if(std::begin(hash), std::end(hash), isspace), std::end(hash));

    auto num_buf = Csv::readInt(values[8]);
    for(unsigned i = 9+3; i < 9+3 + num_buf; ++i) {
      extra_buffer_size.push_back((int)Csv::readInt(values[i]));
    }
  }

  void setup(cl::Context context) override {
    // Allocate extra buffers
    std::vector<cl::Buffer> extra_args;
    for(auto &size: extra_buffer_size)
      extra_args.push_back({context, CL_MEM_READ_WRITE, size*sizeof(float)});

    // Skip the first 3 to compensate for the csv (forgot a drop(3) in scala)
    for(unsigned i = 3; i < extra_args.size(); ++i) {
      kernel.setArg(3+i,extra_args[i]);
    }
    kernel.setArg((unsigned)extra_args.size()+3, (int)size);
    kernel.setArg((unsigned)extra_args.size()+4, (int)size);
    kernel.setArg((unsigned)extra_args.size()+5, (int)size);
  }

  void cleanup() override {
    extra_buffer_size.clear();
    kernel = cl::Kernel();
  }
};

// Execute the kernel and cross validate
void executeRun(Run& run,
                cl::Context context,
                cl::CommandQueue queue,
                cl::Buffer output,
                std::size_t output_size,
                std::function<bool(const std::vector<float>&)> validation)
{
  using namespace std;
  static int counter = 0; counter++;
  static double best_time = 100.0f;
  try {
    // prepare the kernel for execution
    run.setup(context);

    // executing
    cl::Event evt;
    for(int i = 0; i < 5; ++i) {
      queue.enqueueNDRangeKernel(run.kernel, cl::NullRange,
                                 {run.glob1, run.glob2, run.glob3},
                                 {run.loc1, run.loc2, run.loc3}, nullptr, &evt);
      evt.wait();
      auto time = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() - evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
      double ms = ((double)time)/1000.0/1000.0;
      if(ms > 1.2*best_time) break;
    }
    // read back the result
    std::vector<float> result(output_size);
    queue.enqueueReadBuffer(output, CL_TRUE, 0, result.size()*sizeof(float), result.data());
    auto time = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() - evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
    double ms = ((double)time)/1000.0/1000.0;

    if(!validation(result)) {
      // Save result to file
      File::add_invalid(run.hash);
      std::cerr << "[" << counter << "] Cross validation failed for " << run.hash << endl;
    }
    else {
      // Save result to file
      File::add_time(run.hash, ms);
      best_time = min(best_time, ms);
      std::cout << "[" << counter << "] best time: " << best_time << std::endl;
    }
  } catch (const cl::Error& err) {
    File::add_blacklist(run.hash);
    cerr << "execution failed: " << run.hash << endl;
    cerr << err.what() << " (" << err.err() << ")" << std::endl;
    exit(err.err());
  }
}

int main(int argc, char *argv[]) {
  OptParser op("Harness for simple matrix-matrix multiply.");

  auto opt_platform = op.addOption<unsigned>({'p', "platform", "OpenCL platform index (default 0).", 0});
  auto opt_device = op.addOption<unsigned>({'d', "device", "OpenCL device index (default 0).", 0});

  auto opt_size = op.addOption<std::size_t>({'s', "size", "Matrix size (default 1024).", 1024});
  auto opt_transposeA = op.addOption<bool>({0, "tranpose-A", "Transpose the first matrix before computation.", false});
  auto opt_transposeB = op.addOption<bool>({0, "tranpose-B", "Transpose the second matrix before computation.", false});
  auto opt_transposeRes = op.addOption<bool>({0, "tranpose-res", "Transpose the output before cross validation.", false});
  auto opt_force = op.addOption<bool>({'b', "binary", "Load programs as binaries instead of compiling OpenCL-C source.", false});
  auto opt_binary = op.addOption<bool>({'f', "force", "Override cached cross validation files.", false});
  auto opt_timeout = op.addOption<float>({'t', "timeout", "Timeout to avoid multiple executions (default 100ms).", 100.0f});
  auto opt_double = op.addOption<bool>({'d', "double", "Use double precision.", false});
  op.parse(argc, argv);

  using namespace std;

  // Option handling
  const size_t N = opt_size->get();
  File::setSize(N);

  // === Loading CSV file ===
  vector<std::shared_ptr<Run>> all_run = File::load_run([&](const std::vector<std::string>& values){
    return std::shared_ptr<MMRun>(new MMRun(values));
  });
  cout << "Loaded " << all_run.size() << " runs" << std::endl;

  // filtering the runs we already did
  auto blacklist = File::load_blacklist();
  std::cout << "Loaded " << blacklist.size() << " blacklisted values" << std::endl;

  if(blacklist.size() == all_run.size()) return 0;

  std::cout << "Filtering.... ";
  all_run.erase(remove_if(begin(all_run), end(all_run), [&](const shared_ptr<Run>& run) {
    return blacklist.find(run->hash) != end(blacklist);
  }), end(all_run));
  std::cout << "done" << std::endl;

  // Compute input and output
  vector<float> matA(N*N);
  vector<float> matB(N*N);
  vector<float> gold(N*N);

  std::string gold_file = "/tmp/apart_matrix_gold_" + std::to_string(N);
  std::string matA_file = "/tmp/apart_matrix_A_" + std::to_string(N);
  std::string matB_file = "/tmp/apart_matrix_B_" + std::to_string(N);

  if(File::is_file_exist(gold_file) && File::is_file_exist(matA_file) && File::is_file_exist(matB_file) && !opt_force->get() ) {
    File::load_input(gold, gold_file);
    File::load_input(matA, matA_file);
    File::load_input(matB, matB_file);
  } else {
    for(unsigned y = 0; y < N; ++y)
      for(unsigned x = 0; x < N; ++x)
      {
        matA[y*N+x] = (((y * 3 + x * 2) % 10) + 1) * 1.0f;
        matB[y*N+x] = (((y * 7 + x * 3) % 10) + 1) * 1.0f;
      }

    // compute gold
    std::vector < std::thread > threads;
    auto mmult = [&](unsigned from, unsigned to) {
      float kk[N];
      for (unsigned i=from; i<to; i++) {
        for (unsigned j=0; j<N; j++) kk[j] = 0;

        for (unsigned k=0; k<N; k++)
          for (unsigned j=0; j<N; j++)
            kk[j] += matA[i*N+k] * matB[k*N+j];

        for (unsigned j=0; j<N; j++) gold[i*N+j] = kk[j];
      }
    };
    unsigned nthreads = std::thread::hardware_concurrency();
    if(N % nthreads != 0)
      nthreads = 16;
    assert(N % nthreads == 0);
    const unsigned chunk = N / nthreads;
    for (unsigned tid = 0; tid < nthreads; tid++)
      threads.push_back(std::thread([=]{mmult(tid*chunk, (tid+1)*chunk);}));
    for (auto & t : threads) t.join();

    if (opt_transposeA->get()) {
      std::vector<float> TmatA(N*N);
      for(unsigned y = 0; y < N; ++y)
        for(unsigned x = 0; x < N; ++x)
          TmatA[y*N+x] = matA[x*N+y];
      std::swap(TmatA, matA);
    }

    if (opt_transposeB->get()) {
      std::vector<float> TmatB(N*N);
      for(unsigned y = 0; y < N; ++y)
        for(unsigned x = 0; x < N; ++x)
          TmatB[y*N+x] = matB[x*N+y];
      std::swap(TmatB, matB);
    }

    if (opt_transposeRes->get()) {
      std::vector<float> Tgold(N*N);
      for(unsigned y = 0; y < N; ++y)
        for(unsigned x = 0; x < N; ++x)
          Tgold[y*N+x] = gold[x*N+y];
      std::swap(Tgold, gold);
    }

    File::save_input(gold, gold_file);
    File::save_input(matA, matA_file);
    File::save_input(matB, matB_file);
  }

  // validation function
  auto validate = [&](const std::vector<float> &output) {
    if(gold.size() != output.size()) return false;
    for(unsigned i = 0; i < gold.size(); ++i) {
      auto x = gold[i];
      auto y = output[i];

      if(abs(x - y) > 0.0001f * max(abs(x), abs(y)))
        return false;
    }
    return true;
  };

  // === OpenCL init ===
  vector<cl::Platform> platform;
  cl::Platform::get(&platform);

  if (platform.empty()) {
    cerr << "OpenCL platforms not found." << endl;
    return -1;
  }

  vector<cl::Device> devices;
  platform[0].getDevices(CL_DEVICE_TYPE_ALL, &devices);
  assert(devices.size() > 1);
  cl::Device device = devices[1];
  devices.clear(); devices.push_back(device);
  cl::Context context(devices);
  cl::CommandQueue queue(context, device, CL_QUEUE_PROFILING_ENABLE);

  const cl_ulong max_mem = device.getInfo<CL_DEVICE_LOCAL_MEM_SIZE>();
  const size_t max_workgroup = device.getInfo<CL_DEVICE_MAX_WORK_GROUP_SIZE>();

  cout << "Executing on " << device.getInfo<CL_DEVICE_NAME>() << endl;

  // Allocating buffers
  const size_t buf_size = matA.size() * sizeof(float);
  cl::Buffer matA_dev(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                      buf_size, static_cast<void*>(matA.data()));
  cl::Buffer matB_dev(context, CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                      buf_size, static_cast<void*>(matB.data()));

  cl::Buffer output_dev(context, CL_MEM_READ_WRITE, buf_size);

  // reset buffer
  const std::string code = R"CODE(
    kernel void ZERO(global float *matrix, int N) {
      matrix[get_global_id(0)*N+get_global_id(1)] = 0;
    }
  )CODE";

  /*
  // This doesn't seem to give any speedup 
  int nthreads = std::thread::hardware_concurrency();
  std::cout << "Using " << nthreads << " compilation threads" << std::endl;
  int compile_step = 10;

  auto compiler_thread = [&](int start) {
    int max = std::min<int>(start + compile_step, all_run.size());
    for(int i = start; i < max; ++i)
      all_run[i].compile(context, devices);
  };


  for(int i = 0; i < all_run.size(); i += compile_step * nthreads) {
    std::cout << "Compiling " << compile_step * nthreads << " programs..." << std::endl;

    std::vector < std::thread > threads;
    for (unsigned int tid = 0; tid < nthreads; tid++) {
      //threads.push_back(std::thread([=]{
        compiler_thread(i + tid * compile_step);
      //}));
    }

    //for (auto & t : threads) t.join();

    std::cout << "done" << std::endl;
  }*/

  std::mutex m;
  std::condition_variable cv;

  bool done = false;
  bool ready = false;
  std::queue<Run*> ready_queue;

  // function testing that the kernel is compatible with the device
  auto compatibility_checks = [&](cl::Kernel &kernel, size_t l1, size_t l2, size_t l3) {
    size_t wg_size = -1;
    size_t local_size = -1;

    kernel.getWorkGroupInfo(device, CL_KERNEL_WORK_GROUP_SIZE ,&wg_size);
    kernel.getWorkGroupInfo(device, CL_KERNEL_LOCAL_MEM_SIZE ,&local_size);

    if(wg_size == 0 || l1*l2*l3 > max_workgroup) return false;
    if(local_size > max_mem) return false;

    return true;
  };

  // compilation thread
  auto compilation_thread = std::thread([&]{
    for (auto& r: all_run) {
      if(r->compile(context, devices, compatibility_checks, opt_binary->get()))
      {
        std::unique_lock<std::mutex> locker(m);
        ready_queue.push(&*r);
        ready = true;
        cv.notify_one();
      }
    }
  });

  auto execute_thread = std::thread([&]{
    Run *r = nullptr;
    while(!done) {
      {
        std::unique_lock<std::mutex> locker(m);
        while(!ready) cv.wait(locker);
      }

      while(! ready_queue.empty()) {
        {
          std::unique_lock<std::mutex> locker(m);
          r = ready_queue.front();
          ready_queue.pop();
        }
        r->getKernel().setArg(0,matA_dev);
        r->getKernel().setArg(1,matB_dev);
        r->getKernel().setArg(2,output_dev);
        executeRun(*r, context, queue, output_dev, N*N, validate);
      }
    }
  });

  compilation_thread.join();
  done = true;
  execute_thread.join();
}

