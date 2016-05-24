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
#include <cstdio>
#include <cmath>
#include <typeinfo>

// [external includes]
#include <opencl_utils.h>

// [local includes]
#include "options.h"
#include "run.h"

template<typename T>
using Matrix = std::vector<T>;

template<typename T>
struct MVRun : public Run {
  // input matrix size
  std::size_t size;

  // list of additional buffers to allocate
  std::vector<int> extra_buffer_size;
  std::vector<cl::Buffer> extra_args;

  // list of additional local buffers to allocate
  std::vector<int> extra_local_buffer_size;
  std::vector<cl::LocalSpaceArg> extra_local_args;

  /**
   * Deserialize a line from the CSV
   */
  MVRun(const std::vector<std::string>& values) {
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
    for(unsigned i = 9; i < 9 + num_buf; ++i) {
      extra_buffer_size.push_back((int)Csv::readInt(values[i]));
    }

    // number of local buffers to allocate and their sizes
    auto num_local = Csv::readInt(values[9+num_buf]);
    for (unsigned i = 10 + (unsigned) num_buf; i < 10 + num_buf + num_local; ++i) {
      extra_local_buffer_size.push_back((int)Csv::readInt(values[i]));
    }
  }

  void setup(cl::Context context) override {
    // Allocate extra buffers
    for(auto &size: extra_buffer_size)
      extra_args.push_back({context, CL_MEM_READ_WRITE, (size_t) size});

    for (auto &size: extra_local_buffer_size)
      extra_local_args.push_back({(size_t) size});

    // Skip the first 3 to compensate for the csv (forgot a drop(3) in scala)
    for(unsigned i = 0; i < extra_args.size(); ++i)
      kernel.setArg(3+i, extra_args[i]);

    for (unsigned i = 0; i < extra_local_args.size(); ++i)
      kernel.setArg((unsigned) extra_args.size() + 3 + i, extra_local_args[i]);

    kernel.setArg((unsigned)extra_local_args.size()+(unsigned)extra_args.size()+3, (int)size);
    kernel.setArg((unsigned)extra_local_args.size()+(unsigned)extra_args.size()+4, (int)size);
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
    const std::string& mat_file,
    const std::string& vec_file,
    const std::string& gold_file,
    const bool force,
    const bool transposeIn,
    const bool threaded,
    const bool binary
)
{
  using namespace std;

  if(binary)
    std::cout << "Using precompiled binaries" << std::endl;
  // Compute input and output
  Matrix<Float> mat(N*N);
  std::vector<Float> vec(N);
  std::vector<Float> gold(N);

  if(File::is_file_exist(gold_file) && File::is_file_exist(mat_file) && File::is_file_exist(vec_file) && !force ) {
    File::load_input(gold, gold_file);
    File::load_input(mat, mat_file);
    File::load_input(vec, vec_file);
  } else {
    for(unsigned y = 0; y < N; ++y) {
      for (unsigned x = 0; x < N; ++x) {
        mat[y * N + x] = (((y * 3 + x * 2) % 10) + 1) * 1.0f;
      }
      vec[y] = (y%10)*0.5f;
    }

    // compute gold
    for (unsigned i=0; i<N; i++) {
      Float Result=0.0;
      for (unsigned j=0; j<N; j++)
        Result+=mat[i*N+j]*vec[j];

      gold[i]=Result;
    }

    if (transposeIn) {
      std::vector<Float> Tmat(N*N);
      for(unsigned y = 0; y < N; ++y)
        for(unsigned x = 0; x < N; ++x)
          Tmat[y*N+x] = mat[x*N+y];
      std::swap(Tmat, mat);
    }

    File::save_input(gold, gold_file);
    File::save_input(mat, mat_file);
    File::save_input(vec, vec_file);
  }

  // validation function
  auto validate = [&](const std::vector<Float> &output) {
    if(gold.size() != output.size()) return false;
    for(unsigned i = 0; i < gold.size(); ++i) {
      auto x = gold[i];
      auto y = output[i];

      if(abs(x - y) > 0.001f * max(abs(x), abs(y))) {
        cout << "at " << i << ": " << x << "=/=" << y <<std::endl;
        return false;
      }
    }
    return true;
  };

  // Allocating buffers
  const size_t buf_size = mat.size() * sizeof(Float);
  cl::Buffer mat_dev = OpenCL::alloc( CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                                      buf_size, static_cast<void*>(mat.data()) );
  cl::Buffer vec_dev = OpenCL::alloc( CL_MEM_READ_ONLY | CL_MEM_COPY_HOST_PTR,
                                      N*sizeof(Float), static_cast<void*>(vec.data()) );
  cl::Buffer output_dev = OpenCL::alloc( CL_MEM_READ_WRITE, N*sizeof(Float) );

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
          while (!ready && !done) cv.wait(locker);
        }

        while (!ready_queue.empty()) {
          {
            std::unique_lock<std::mutex> locker(m);
            r = ready_queue.front();
            ready_queue.pop();
          }
          r->getKernel().setArg(0, mat_dev);
          r->getKernel().setArg(1, vec_dev);
          r->getKernel().setArg(2, output_dev);
          OpenCL::executeRun<Float>(*r, output_dev, N, validate);
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
        r->getKernel().setArg(0, mat_dev);
        r->getKernel().setArg(1, vec_dev);
        r->getKernel().setArg(2, output_dev);
        OpenCL::executeRun<Float>(*r, output_dev, N, validate);
      }
    }
  }
};

int main(int argc, char *argv[]) {
  OptParser op("Harness for simple matrix-vector multiply.");

  auto opt_platform = op.addOption<unsigned>({'p', "platform", "OpenCL platform index (default 0).", 0});
  auto opt_device = op.addOption<unsigned>({'d', "device", "OpenCL device index (default 0).", 0});

  auto opt_size = op.addOption<std::size_t>({'s', "size", "Matrix size (default 1024).", 1024});
  auto opt_transpose = op.addOption<bool>({0, "transpose-in", "Transpose the input matrix before computation.", false});
  auto opt_binary = op.addOption<bool>({'b', "binary", "Load programs as binaries instead of compiling OpenCL-C source.", false});
  auto opt_timeout = op.addOption<float>({'t', "timeout", "Timeout to avoid multiple executions (default 100ms).", 100.0f});
  auto opt_double = op.addOption<bool>({0, "double", "Use double precision.", false});
  auto opt_threaded = op.addOption<bool>({'t', "threaded", "Use a separate thread for compilation and execution (default true).", true});
  auto opt_force = op.addOption<bool>({'f', "force", "Override cached cross validation files.", false});
  auto opt_clean = op.addOption<bool>({'c', "clean", "Clean temporary files and exit.", false});
  op.parse(argc, argv);

  using namespace std;

  // Option handling
  const size_t N = opt_size->get();
  File::set_size(opt_size->get());
  OpenCL::timeout = opt_timeout->get();

  // temporary files
  std::string gold_file = "/tmp/apart_mv_gold_" + std::to_string(N);
  std::string mat_file = "/tmp/apart_mv_mat_" + std::to_string(N);
  std::string vec_file = "/tmp/apart_mv_vec_" + std::to_string(N);

  if(opt_clean->get()) {
    std::cout << "Cleaning..." << std::endl;
    for(const auto& file: {gold_file, mat_file, vec_file})
      std::remove(file.data());
    return 0;
  }

  // === Loading CSV file ===
  auto all_run = Csv::init(
      [&](const std::vector<std::string>& values) -> std::shared_ptr<Run> {
        return (opt_double->get() ?
               std::shared_ptr<Run>(new MVRun<double>(values)) :
               std::shared_ptr<Run>(new MVRun<float>(values)));
      });
  if (all_run.size() == 0) return 0;

  // === OpenCL init ===
  OpenCL::init(opt_platform->get(), opt_device->get());

  // run the harness
  if (opt_double->get())
    run_harness<double>(
        all_run, N,
        mat_file, vec_file, gold_file,
        opt_force->get(),
        opt_transpose->get(),
        opt_threaded->get(), opt_binary->get()
    );
  else
    run_harness<float>(
        all_run, N,
        mat_file, vec_file, gold_file,
        opt_force->get(),
        opt_transpose->get(),
        opt_threaded->get(), opt_binary->get()
    );
}

