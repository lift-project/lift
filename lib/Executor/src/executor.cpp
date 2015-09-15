#include <vector>
#include <set>
#include <queue>
#include <algorithm>
#include <cmath>
#include <fstream>
#include <sstream>
#include <iostream>
#include <cassert>
#include <thread>
#include <cctype>
#include <mutex>
#include <condition_variable>
#include <atomic>
#include <functional>

#define __CL_ENABLE_EXCEPTIONS
#include <CL/cl.hpp>

// split CSV values in array of strings
std::vector<std::string> getNextLineAndSplitIntoTokens(std::istream& str)
{
  std::vector<std::string> result;
  std::string line;
  std::getline(str,line);

  std::stringstream lineStream(line);
  std::string cell;

  while(std::getline(lineStream,cell,','))
    result.push_back(cell);
  return result;
}


// String to int function
std::size_t readInt(const std::string& str) {
  std::stringstream buffer(str);
  std::size_t value = 0;
  buffer >> value;
  assert(value != 0 && "Bad value");
  return value;
}

// Object representation of the run
struct Run {
  // input matrix size
  std::size_t size;

  // global range
  std::size_t glob1;
  std::size_t glob2;
  std::size_t glob3;

  // local range
  std::size_t loc1;
  std::size_t loc2;
  std::size_t loc3;
  
  // hash file
  std::string hash;

  // list of additional buffers to allocate
  std::vector<int> extra_buffer_size;

  // compiled kernel
  cl::Kernel kernel;

  // read values from CSV and initialize fields
  Run(const std::vector<std::string>& values) {
    assert(values.size() > 9 && "Bad CSV format");
    
    size = readInt(values[0]);
    assert(size == 1024 && "Can only deal with 1024 for now");
    glob1 = readInt(values[1]);
    glob2 = readInt(values[2]);
    glob3 = readInt(values[3]);
    loc1 = readInt(values[4]);
    loc2 = readInt(values[5]);
    loc3 = readInt(values[6]);

    hash = values[7];
    hash.erase(std::remove_if(std::begin(hash), std::end(hash), isspace), std::end(hash));

    auto num_buf = readInt(values[8]);
    for(int i = 9; i < 9 + num_buf; ++i) {
      extra_buffer_size.push_back(readInt(values[i]));
    }
  }

  // Load the file and compile the program
  void compile(cl::Context context, std::vector<cl::Device> dev) 
  {
    auto code = loadCode();
    cl::Program program(context, cl::Program::Sources(1, std::make_pair(code.data(), code.size())));
    try {
      program.build(dev);
      kernel = cl::Kernel(program, "KERNEL");
    } catch (const cl::Error&) {
      std::cerr << "Compilation failed" << std::endl
                << program.getBuildInfo<CL_PROGRAM_BUILD_LOG>(dev[0]) << std::endl;

      std::ofstream outfile;
      outfile.open("blacklist.csv", std::ios_base::app);
      outfile << hash << std::endl;
    }
  }

private:
  // Get a file into a string
  std::string loadCode() 
  {
    std::string path = "cl/";
    std::ifstream t(path + hash + ".cl");
    std::string cl_code = std::string((std::istreambuf_iterator<char>(t)),
                          std::istreambuf_iterator<char>());
    if(cl_code.size() == 0) {
      std::cerr << "\nNo source for " << path << hash << ".cl\n" << std::endl;
    }
    assert(cl_code.size() != 0 && "Cannot read source file");
    return cl_code;
  }
};

// Execute the kernel and cross validate
void executeRun(Run& run, 
                cl::Context context,
                cl::CommandQueue queue, 
                cl::Buffer matA, cl::Buffer matB, cl::Buffer output,
                std::function<bool(const std::vector<float>&)> validation) 
{
  using namespace std;
  static int counter = 0; counter++;
  static double best_time = 10000.0f;
  try {
    // Allocate extra buffers
    //cout << "Allocating " << run.extra_buffer_size.size() << " extra buffers" << std::endl;
    std::vector<cl::Buffer> extra_args;
    for(auto &size: run.extra_buffer_size) 
      extra_args.push_back({context, CL_MEM_READ_WRITE, size*sizeof(float)});

    // set all arguments
    auto &kernel = run.kernel;
    kernel.setArg(0,matA);
    kernel.setArg(1,matB);
    kernel.setArg(2,output);
    for(int i = 0; i < extra_args.size(); ++i) kernel.setArg(3+i,extra_args[i]);

    // executing
    cl::Event evt;
    for(int i = 0; i < 5; ++i) {
      queue.enqueueNDRangeKernel(kernel, cl::NullRange, 
          {run.glob1, run.glob2, run.glob3}, 
          {run.loc1, run.loc2, run.loc3}, nullptr, &evt);
      evt.wait();
      auto time = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() - evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
      double ms = ((double)time)/1000.0/1000.0;
      if(ms > 1.2*best_time) break;
    }
    // read back the result
    std::vector<float> result(run.size * run.size);
    queue.enqueueReadBuffer(output, CL_TRUE, 0, result.size()*sizeof(float), result.data());
    auto time = evt.getProfilingInfo<CL_PROFILING_COMMAND_END>() - evt.getProfilingInfo<CL_PROFILING_COMMAND_START>();
    double ms = ((double)time)/1000.0/1000.0;

    if(!validation(result)) {
      // Save result to file
      std::ofstream outfile;
      outfile.open("invalid.csv", std::ios_base::app);
      outfile << run.hash << std::endl;

      std::cerr << "[" << counter << "Cross validation failed for " << run.hash << endl;
    }
    else {
      // Save result to file
      std::ofstream outfile;
      outfile.open("time.csv", std::ios_base::app);
      outfile << run.hash << "," << ms << std::endl;
      
      best_time = min(best_time, ms);
      std::cout << "[" << counter << "] best time: " << best_time << std::endl;
    }
  } catch (const cl::Error& err) {
    std::ofstream outfile;
    outfile.open("blacklist.csv", std::ios_base::app);
    outfile << run.hash << std::endl;

    cerr << "execution failed: " << run.hash << endl;
    cerr << err.what() << " (" << err.err() << ")" << std::endl;
    exit(err.err());
  }
}

std::vector<std::vector<std::string>> loadCsv(const std::string& filename)
{
  std::vector<std::string> line;
  std::vector<std::vector<std::string>> all_values;

  std::ifstream t(filename);

  do {
    line = getNextLineAndSplitIntoTokens(t);
    if(line.size() > 0) all_values.push_back(line);
  } while(line.size() != 0);
  
  return all_values;
}

// Load the list of hashes we already ran or we want to skip
std::set<std::string> load_blacklist() 
{
  std::set<std::string> blacklist;
  for(auto& filename: {"time.csv", "blacklist.csv", "invalid.csv"}) {
    for(auto& values: loadCsv(filename))
      blacklist.insert(values.front());
  }
  return blacklist;
}

int main() {
  using namespace std; 

  const size_t N = 1024;

  // Compute input and output
  vector<float> matA(N*N);
  vector<float> matB(N*N);
  vector<float> gold(N*N);
  for(int y = 0; y < N; ++y)
    for(int x = 0; x < N; ++x) 
    {
      matA[y*N+x] = (((y * 3 + x * 2) % 10) + 1) * 1.0f;
      matB[y*N+x] = (((y * 7 + x * 3) % 10) + 1) * 1.0f;
    }

  { // compute gold
    std::vector < std::thread > threads;
    auto mmult = [&](int from, int to) {
      float kk[N];
      for (int i=from; i<to; i++) {
        for (int j=0; j<N; j++) kk[j] = 0;

        for (int k=0; k<N; k++)
          for (int j=0; j<N; j++)
            kk[j] += matA[i*N+k] * matB[k*N+j];

        for (int j=0; j<N; j++) gold[i*N+j] = kk[j];
      }
    };
    int nthreads = std::thread::hardware_concurrency();
    assert(N % nthreads == 0);
    int chunk = N / nthreads;
    for (unsigned int tid = 0; tid < nthreads; tid++) 
      threads.push_back(std::thread([=]{mmult(tid*chunk, (tid+1)*chunk);}));
    for (auto & t : threads) t.join();
  }

  { // transpose A
    std::vector<float> TmatA(N*N);
    for(int y = 0; y < N; ++y)
      for(int x = 0; x < N; ++x)
        TmatA[y*N+x] = matA[x*N+y];
    std::swap(TmatA, matA);
  }

  // validation function
  auto validate = [&](const std::vector<float> &output) {
    if(gold.size() != output.size()) return false;
    for(int i = 0; i < gold.size(); ++i) {
      auto x = gold[i];
      auto y = output[i];

      if(abs(x - y) > 0.0001f * max(abs(x), abs(y))) 
        return false;
    }
    return true;
  };

  // === Loading CSV file ===
  ifstream file ( "cl/exec.csv" );  

  vector<string> value;
  vector<Run> all_run;

  bool stop = false;
  do {
    auto str = getNextLineAndSplitIntoTokens(file);
    if(str.size() != 0) {
      all_run.push_back({str});
    } else stop = true;
  } while(!stop);

  cout << "Loaded " << all_run.size() << " runs" << std::endl;

  // filtering the runs we already did
  auto blacklist = load_blacklist();
  std::cout << "Loaded " << blacklist.size() << " blacklisted values" << std::endl;

  std::cout << "Filtering.... ";
  all_run.erase(remove_if(begin(all_run), end(all_run), [&](const Run& run) {
    return blacklist.find(run.hash) != end(blacklist);
  }), end(all_run));
  std::cout << "done" << std::endl;

  // === OpenCL init ===
  vector<cl::Platform> platform;
  cl::Platform::get(&platform);

  if (platform.empty()) {
    cerr << "OpenCL platforms not found." << endl;
    return -1;
  }

  vector<cl::Device> devices;
  platform[1].getDevices(CL_DEVICE_TYPE_GPU, &devices);
  cl::Device device = devices[0];
  cl::Context context(devices);
  cl::CommandQueue queue(context, device, CL_QUEUE_PROFILING_ENABLE);

  cout << "Executing on " << device.getInfo<CL_DEVICE_NAME>() << endl;

  // Allocating buffers
  const int buf_size = matA.size() * sizeof(float);
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

  // compilation thread
  auto compilation_thread = std::thread([&]{
    for (auto& r: all_run) {
      r.compile(context, devices);
      {
        std::unique_lock<std::mutex> locker(m);
        ready_queue.push(&r);
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
        executeRun(*r, context, queue, matA_dev, matB_dev, output_dev, validate);
      }
    }
  });

  compilation_thread.join();
  done = true;
  execute_thread.join();
}

