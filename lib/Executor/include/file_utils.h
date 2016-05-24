#pragma once

// [Standard includes]
#include <string>
#include <set>
#include <iostream>
#include <fstream>
#include <cassert>
#include <vector>
#include <memory>

// [local includes]
#include "csv_utils.h"
#include "run.h"

class File {
  static std::string blacklist_filename;
  static std::string incompatible_filename;
  static std::string invalid_filename;
  static std::string timing_filename;
  static std::string exec_filename;
  static std::string compileerror_filename;

  static std::string & replace(std::string & subj, std::string old, std::string neu)
  {
    size_t c = subj.find(old);
    if (c != std::string::npos) {
      subj.erase(c, old.size());
      subj.insert(c, neu);
    }
    return subj;
  }

public:
  static void set_size(std::size_t size)
  {
    const auto size_str = std::to_string(size);
    set_size(size_str);
  }

  static void set_size(const std::string size_str) {
    const std::string pattern = "%SIZE%";
    replace(blacklist_filename, pattern, size_str);
    replace(incompatible_filename, pattern, size_str);
    replace(invalid_filename, pattern, size_str);
    replace(timing_filename, pattern, size_str);
    replace(exec_filename, pattern, size_str);
    replace(compileerror_filename, pattern, size_str);
  }

  static void file_append(const std::string &filename, const std::string &content)
  {
    std::ofstream outfile;
    outfile.open(filename, std::ios_base::app);
    outfile << content << std::endl;
  }

  static void add_blacklist(const std::string &hash)
  { file_append(blacklist_filename, hash); }

  static void add_invalid(const std::string &hash)
  { file_append(invalid_filename, hash); }

  static void add_incompatible(const std::string &hash)
  { file_append(incompatible_filename, hash); }

  static void add_time(const std::string &hash, double time, cl::NDRange local_size)
  {
    if (local_size.dimensions() != 0) {
      auto sizes = (const size_t*) local_size;
      file_append(timing_filename, hash + "," + std::to_string(time) + "," +
          std::to_string(sizes[0]) + "," + std::to_string(sizes[1]) + "," +
          std::to_string(sizes[2]));
    } else {
      file_append(timing_filename, hash + "," + std::to_string(time));
    }
  }

  static void add_compileerror(const std::string &hash)
  { file_append(compileerror_filename, hash); }

  template<typename T>
  static void load_input(std::vector<T>& data, const std::string &filename)
  {
    using namespace std;
    ifstream fin(filename);
    assert(fin.is_open());
    ifstream in(filename, ios::in | ios::binary);
    in.read((char*)data.data(), sizeof(T) * data.size());
  }

  template<typename T>
  static void save_input(const std::vector<T>& data, const std::string &filename)
  {
    using namespace std;
    ofstream out(filename, ios::out | ios::binary);
    assert(out);
    out.write((char*)data.data(), sizeof(T) * data.size());
  }

  static bool is_file_exist(const std::string &filename)
  {
    std::ifstream infile(filename);
    return infile.good();
  }

  static std::vector<std::shared_ptr<Run>> load_run(std::function<std::shared_ptr<Run>(const std::vector<std::string>&)> factory) {
    using namespace std;
    ifstream file ( exec_filename );
    vector<std::shared_ptr<Run>> all_run;

    bool stop = false;
    do {
      auto str = Csv::getNextLineAndSplitIntoTokens(file);
      if (str.size() != 0) {
        all_run.push_back(factory(str));
      } else stop = true;
    } while (!stop);

    return all_run;
  }

  static std::set<std::string> load_blacklist()
  {
    std::set<std::string> blacklist;
    for(auto& filename: {
        blacklist_filename, 
        incompatible_filename, 
        invalid_filename, 
        timing_filename,
        compileerror_filename
      }) 
    {
      for(auto& values: Csv::loadCsv(filename))
        blacklist.insert(values.front());
    }
    return blacklist;
  }
};
