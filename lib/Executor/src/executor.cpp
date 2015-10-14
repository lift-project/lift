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
#include <opencl_utils.h>
#include <cstdio>
#include <typeinfo>

// [local includes]
#include "csv_utils.h"
#include "file_utils.h"
#include "options.h"
#include "run.h"

template<typename T>
using Matrix = std::vector<T>;

template<typename T>
struct MMRun: public Run {
  // input matrix size
  std::size_t size;

  // list of additional buffers to allocate
  std::vector<int> extra_buffer_size;

  std::vector<cl::Buffer> extra_args;

  /**
   * Deserialize a line from the CSV
   */
  MMRun(const std::vector<std::string>& values) {
    assert(values.size() > 8 && "Bad CSV format");

    // input size
    size = Csv::readInt(values[0]);

    // global NDRange
    glob1 = Csv::readInt(values[1]);
    glob2 = Csv::readInt(values[2]);
    glob3 = Csv::readInt(values[3]);

    // local NDRange
    loc1 = Csv::readInt(values[4]);
    loc2 = Csv::readInt(values[5]);
    loc3 = Csv::readInt(values[6]);

    // source hash
    hash = values[7];
    hash.erase(std::remove_if(std::begin(hash), std::end(hash), isspace), std::end(hash));

    // number of temporary buffers to allocate and their sizes
    auto num_buf = Csv::readInt(values[8]);
    for(unsigned i = 9+3; i < 9+3 + num_buf; ++i) {
      extra_buffer_size.push_back((int)Csv::readInt(values[i]));
    }
  }

  void setup(cl::Context context) override {
    // Allocate extra buffers
    std::vector<cl::Buffer> extra_args;
    for(auto &size: extra_buffer_size)
      extra_args.push_back({context, CL_MEM_READ_WRITE, size*sizeof(T)});

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

/**
 * FIXME: This is a lazy copy paste of the old main with a template switch for single and double precision
 */
template<typename Float>
void run_harness(
    std::vector<std::shared_ptr<Run>>& all_run,
    const unsigned N,
    const std::string& matA_file,
    const std::string& matB_file,
    const std::string& gold_file,
    const bool force,
    const bool transposeA,
    const bool transposeB,
    const bool transposeOut,
    const bool threaded,
    const bool binary
)
{
  using namespace std;

  // Compute input and output
  Matrix<Float> matA(N*N);
  Matrix<Float> matB(N*N);
  Matrix<Float> gold(N*N);

  if(File::is_file_exist(gold_file) && File::is_file_exist(matA_file) && File::is_file_exist(matB_file) && !force ) {
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
      Float kk[N];
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

    if (transposeA) {
      std::vector<Float> TmatA(N*N);
      for(unsigned y = 0; y < N; ++y)
        for(unsigned x = 0; x < N; ++x)
          TmatA[y*N+x] = matA[x*N+y];
      std::swap(TmatA, matA);
    }

    if (transposeB) {
      std::vector<Float> TmatB(N*N);
      for(unsigned y = 0; y < N; ++y)
        for(unsigned x = 0; x < N; ++x)
          TmatB[y*N+x] = matB[x*N+y];
      std::swap(TmatB, matB);
    }

    if (transposeOut) {
      std::vector<Float> Tgold(N*N);
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
  auto validate = [&](const std::vector<Float> &output) {
    if(gold.size() != output.size()) return false;
    for(unsigned i = 0; i < gold.size(); ++i) {
      auto x = gold[i];
      auto y = output[i];

      if(abs(x - y) > 0.0001f * max(abs(x), abs(y)))
        return false;
    }
    return true;
  };

  // Allocating buffers
  const size_t buf_size = matA.size() * sizeof(Float);
  cl::Buffer matA_dev = OpenCL::alloc( CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                                       buf_size, static_cast<void*>(matA.data()) );
  cl::Buffer matB_dev = OpenCL::alloc( CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                                       buf_size, static_cast<void*>(matB.data()) );
  cl::Buffer output_dev = OpenCL::alloc( CL_MEM_READ_WRITE, buf_size );

  // multi-threaded exec
  if(threaded) {
    std::mutex m;
    std::condition_variable cv;

    bool done = false;
    bool ready = false;
    std::queue<Run*> ready_queue;

    // compilation thread
    auto compilation_thread = std::thread([&] {
      for (auto &r: all_run) {
        if (r->compile(binary)) {
          std::unique_lock<std::mutex> locker(m);
          ready_queue.push(&*r);
          ready = true;
          cv.notify_one();
        }
      }
    });

    auto execute_thread = std::thread([&] {
      Run *r = nullptr;
      while (!done) {
        {
          std::unique_lock<std::mutex> locker(m);
          while (!ready) cv.wait(locker);
        }

        while (!ready_queue.empty()) {
          {
            std::unique_lock<std::mutex> locker(m);
            r = ready_queue.front();
            ready_queue.pop();
          }
          r->getKernel().setArg(0, matA_dev);
          r->getKernel().setArg(1, matB_dev);
          r->getKernel().setArg(2, output_dev);
          OpenCL::executeRun<Float>(*r, output_dev, N * N, validate);
        }
      }
    });

    compilation_thread.join();
    done = true;
    cv.notify_one();
    execute_thread.join();
  }
    // single threaded exec
  else {
    for (auto &r: all_run) {
      if (r->compile(binary)) {
        r->getKernel().setArg(0, matA_dev);
        r->getKernel().setArg(1, matB_dev);
        r->getKernel().setArg(2, output_dev);
        OpenCL::executeRun<Float>(*r, output_dev, N * N, validate);
      }
    }
  }
};

int main(int argc, char *argv[]) {
  OptParser op("Harness for simple matrix-matrix multiply.");

  auto opt_platform = op.addOption<unsigned>({'p', "platform", "OpenCL platform index (default 0).", 0});
  auto opt_device = op.addOption<unsigned>({'d', "device", "OpenCL device index (default 0).", 0});

  auto opt_size = op.addOption<std::size_t>({'s', "size", "Matrix size (default 1024).", 1024});
  auto opt_transposeA = op.addOption<bool>({0, "transpose-A", "Transpose the first matrix before computation.", false});
  auto opt_transposeB = op.addOption<bool>({0, "transpose-B", "Transpose the second matrix before computation.", false});
  auto opt_transposeRes = op.addOption<bool>({0, "transpose-res", "Transpose the output before cross validation.", false});
  auto opt_force = op.addOption<bool>({'b', "binary", "Load programs as binaries instead of compiling OpenCL-C source.", false});
  auto opt_timeout = op.addOption<float>({'t', "timeout", "Timeout to avoid multiple executions (default 100ms).", 100.0f});
  auto opt_double = op.addOption<bool>({0, "double", "Use double precision.", false});
  auto opt_threaded = op.addOption<bool>({'t', "threaded", "Use a separate thread for compilation and execution (default true).", true});
  auto opt_binary = op.addOption<bool>({'f', "force", "Override cached cross validation files.", false});
  auto opt_clean = op.addOption<bool>({'c', "clean", "Clean temporary files and exit.", false});
  op.parse(argc, argv);

  using namespace std;

  // Option handling
  const size_t N = opt_size->get();
  File::setSize(opt_size->get());
  OpenCL::timeout = opt_timeout->get();

  // temporary files
  std::string gold_file = "/tmp/apart_matrix_gold_" + std::to_string(N);
  std::string matA_file = "/tmp/apart_matrix_A_" + std::to_string(N);
  std::string matB_file = "/tmp/apart_matrix_B_" + std::to_string(N);

  if(*opt_clean) {
    std::cout << "Cleaning..." << std::endl;
    for(const auto& file: {gold_file, matA_file, matB_file})
      std::remove(file.data());
    return 0;
  }

  // === Loading CSV file ===
  auto all_run = Csv::init(
      [&](const std::vector<std::string>& values) -> std::shared_ptr<Run> {
        return (opt_double->get() ?
               std::shared_ptr<Run>(new MMRun<double>(values)) :
               std::shared_ptr<Run>(new MMRun<float>(values)));
      });
  if (all_run.size() == 0) return 0;

  // === OpenCL init ===
  OpenCL::init(opt_platform->get(), opt_device->get());

  // run the harness
  if (opt_double->get())
    run_harness<double>(
        all_run, N,
        matA_file, matB_file, gold_file,
        opt_force->get(),
        opt_transposeA->get(), opt_transposeB->get(), opt_transposeRes->get(),
        opt_threaded->get(), opt_binary->get()
    );
  else
    run_harness<float>(
        all_run, N,
        matA_file, matB_file, gold_file,
        opt_force->get(),
        opt_transposeA->get(), opt_transposeB->get(), opt_transposeRes->get(),
        opt_threaded->get(), opt_binary->get()
    );
}

