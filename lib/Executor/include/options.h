#pragma once

// [standard includes]
#include <vector>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <algorithm>

/**
 * Base class for a command line option. Contains the basic info but no data.
 */
class OptionBase {
protected:
  /// @brief Short option
  char _shortopt;

  /// @brief long option
  std::string _longopt;

  /// @brief Description
  std::string _desc;

  /// @brief Flag to know whether a default value was profided.
  bool _has_default = false;

  /***
   * @brief Constructor, fill in the argument info.
   * @param s Short option.
   * @param l Long option.
   * @param d Description.
   */
  OptionBase(const char s, const std::string l, const std::string &d, bool has_default = false)
      : _shortopt{s}, _longopt{l}, _desc{d}, _has_default{has_default} {}

public:
  virtual ~OptionBase(){}

  char getShort() const
  {
    return _shortopt;
  }

  const std::string & getLong() const
  {
    return _longopt;
  }

  const std::string & getDesc() const
  {
    return _desc;
  }

  /// @brief Print a readable description of the argument for debug.
  virtual void print(std::ostream & out = std::cout) const = 0;

  /**
   * Parse the argument. This is called whenever the option is present
   * in the command line options. The command can then parse as many
   * additional argument as needed.
   * @param argc Total count of arguments.
   * @param current Current index (index of the command)
   * @param argv Option list.
   */
  virtual bool parseArgs(const int argc, int &current, char **argv) = 0;

  bool has_default() const { return _has_default; }
};

/// @brief Define an operator to dump command line arguments to stream.
std::ostream & operator<<(std::ostream &os, const OptionBase * a) {
  a->print(os);
  os << std::endl;
  return os;
}

/**
 * A typed argument, contains the data.
 */
template<typename T>
class Option : public OptionBase {
protected:
  /// @brief Command value.
  T _value;
public:
  Option(const char s, const std::string & l, const std::string &d)
      : OptionBase{s, l, d, false}{}

  Option(const char s, const std::string & l, const std::string &d, const T &v)
      : OptionBase{s, l, d, true}, _value{v} {}

  Option(const Option&& other)
      : OptionBase{other._shortopt, other._longopt, other._desc, other._has_default},
        _value{other._value} { }

  T get() const { return _value; }

  operator T() const { return _value; }

  void setValue(const T &v)
  {
    _value = v;
  }

  /// @brief Generic parse function. A typical command line argument expects
  // one argument. The argument following the command is dumped into a string stream
  // and read back into the object. This can de-serialize any type defining
  // the stream operator. Other types can also specialize this function.
  // @return True if the command was well formatted, false otherwise
  bool parseArgs(const int argc, int &current, char **argv)
  {
    // Move to the next string
    current++;

    // It should be a valid string
    if(current >= argc)
      return false;

    std::istringstream ss(argv[current]);
    ss >> _value;

    return true;
  }

  void print(std::ostream & out) const
  {
    out << _longopt << ": " << _value;
  }
};

/// @brief Specialization for bool; a boolean argument is just a switch, it
//requires no argument.
template<>
bool Option<bool>::parseArgs(const int, int &, char **)
{
  _value = !_value;
  return true;
}


/** 
 * The class in charge of doing the actual parsing.
 */
class OptParser {
protected:
  std::string desc;

  /// @brief All registered commands.
  std::vector<std::shared_ptr<OptionBase>> args;

  std::shared_ptr<Option<bool>> help;
public:
  explicit OptParser(const std::string desc, const std::vector<std::shared_ptr<OptionBase>> as = std::vector<std::shared_ptr<OptionBase>>())
      : desc{desc}
  {
    help = addOption<bool>({'h', "help", "Print help and exit.", false});
    args.insert(std::end(args), std::begin(as), std::end(as));
  }

  /// @brief Add one argument to the list.
  void add(std::shared_ptr<OptionBase> arg)
  {
    args.push_back(arg);
  }

  template<typename T>
  std::shared_ptr<Option<T>> addOption(Option<T> &&opt){
    auto opt_ptr = std::make_shared<Option<T>>(std::move(opt));
    args.push_back(std::dynamic_pointer_cast<OptionBase>(opt_ptr));
    return opt_ptr;
  }

  /// @brief Parse a list of C strings. We expect this to have the same format as the 
  // main function: the first string in the list is the program name and then a list
  // of strings containing program options and their arguments.
  void parse(int argc, char **argv)
  {
    using namespace std;

    int c = 1;
    while(c < argc){
      OptionBase *command = nullptr;

      // if option start
      if(argv[c][0] == '-') {
        // if long option
        if(argv[c][1] == '-') {
          for(auto o : args)
            if(o->getLong() == argv[c]+2) {
              command = o.get();
              break;
            }
        }
          // otherwise short option
        else {
          for(auto o : args) {
            if (o->getShort() == argv[c][1]) {
              command = o.get();
              break;
            }
          }
        }
      }

      // Oops, we didn't find a valid command for that...
      if(command == nullptr){
        cout << "Error: Invalid argument '" << argv[c] << "'." << std::endl;
        help->setValue(true);
        break;
      }
        // Otherwise try to parse the arguments, and abort if it fails
      else if(!command->parseArgs(argc, c, argv)){
        cout << "Error: invalid argument for option " << command->getLong() << std::endl;
        help->setValue(true);
        break;
      }

      c++;
    }

    // If help is enabled, print help and exit.
    if(help->get()){
      size_t max = (*max_element(begin(args), end(args),
                                 [](std::shared_ptr<OptionBase> x, std::shared_ptr<OptionBase>y){
                                   return x->getLong().length() < y->getLong().length();
                                 }))->getLong().length();


      cout << "Usage:\n  " << argv[0] << " [OPTIONS]...\nDescription:\n  " << desc << "\nOptions:\n";
      for(auto o : args) {
        if(o->getShort())
          cout << "  -" << o->getShort();
        else
          cout << "    ";
        cout << "  --" << left << setw(max) << o->getLong()
             << "  " << o->getDesc() << endl;
      }
      exit(0);
    }
  }

  void print(std::ostream & out = std::cout)
  {
    for(auto o : args) {
      out << o << std::endl;
    }
  }
};

