#include <algorithm>

#include "csv_utils.h"
#include "file_utils.h"

#include <iostream>

namespace Csv {
std::vector<std::shared_ptr<Run>> init(
    std::function<std::shared_ptr<Run>(const std::vector<std::string> &)> factory)
{
  using namespace std;
  vector<std::shared_ptr<Run>> all_run = File::load_run(factory);
  cout << "Loaded " << all_run.size() << " runs" << std::endl;

  // filtering the runs we already did
  auto blacklist = File::load_blacklist();
  std::cout << "Loaded " << blacklist.size() << " blacklisted values" << std::endl;

  if (blacklist.size() == all_run.size()) return {};

  std::cout << "Filtering.... ";
  all_run.erase(remove_if(begin(all_run), end(all_run), [&](const shared_ptr<Run> &run) {
    return blacklist.find(run->hash) != end(blacklist);
  }), end(all_run));
  std::cout << "done" << std::endl;

  std::random_shuffle(std::begin(all_run), std::end(all_run));

  return all_run;
}

} // namespace Csv