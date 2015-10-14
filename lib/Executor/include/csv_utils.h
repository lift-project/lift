#pragma once

#include <vector>
#include <sstream>
#include <memory>
#include <fstream>
#include <functional>

class Run;

namespace Csv {

// split CSV values in array of strings
inline std::vector<std::string> getNextLineAndSplitIntoTokens(std::istream &str) {
  std::vector<std::string> result;
  std::string line;
  std::getline(str, line);

  std::stringstream lineStream(line);
  std::string cell;

  while (std::getline(lineStream, cell, ','))
    result.push_back(cell);
  return result;
}

// String to int function
inline std::size_t readInt(const std::string &str) {
  std::stringstream buffer(str);
  std::size_t value = 0;
  buffer >> value;
  return value;
}

// Load a csv and return a list of values for each line
inline std::vector<std::vector<std::string>> loadCsv(const std::string &filename) {
  std::vector<std::string> line;
  std::vector<std::vector<std::string>> all_values;

  std::ifstream t(filename);

  do {
    line = getNextLineAndSplitIntoTokens(t);
    if (line.size() > 0) all_values.push_back(line);
  } while (line.size() != 0);

  return all_values;
}

std::vector<std::shared_ptr<Run>> init(std::function<std::shared_ptr<Run>(const std::vector<std::string> &)> factory);

}