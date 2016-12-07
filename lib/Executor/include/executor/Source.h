///
/// \file Source.h
///
///	\author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef SOURCE_H_
#define SOURCE_H_

#include <istream>
#include <string>
#include <sstream>
#include <vector>

namespace executor {

class Source {
public:
  Source();

  ///
  /// \brief Constructor taking the source code as a string
  ///
  Source(const char* source);

  ///
  /// \brief Constructor taking the source code as a string
  ///
  Source(const std::string& source);

  ///
  /// \brief Constructor taking a istream object. The content the istream is
  ///        pointing to is read using the stream operator and interpreted to
  ///        be the source code.
  ///        This variant can be used to read the source code from a file.
  ///
  Source(std::istream& is);

  ///
  /// \brief Constructor taking a istream object. The content the istream is
  ///        pointing to is read using the stream operator and interpreted to
  ///        be the source code.
  ///        This variant can be used to read the source code from a file.
  ///
  Source(std::istream&& is);

  ///
  /// \brief Default destructor
  ///
  ~Source();

  ///
  /// \brief Conversion operator providing access to the source code as a 
  ///        string
  ///
  /// Thanks to this operator an instance of this class is implicitly 
  /// convertible to a string.
  ///
  operator std::string() const;

  ///
  /// \brief Append the given string to the source code
  ///
  /// \param source The string to be appended to the source code.
  ///
  void append(const std::string& source);

private:
  /// string used to store the source code
  std::string _source;
};

class CommonDefinitions {
public:
  enum Level : unsigned int {
    PRAGMA,
    USER_DEFINITION,
    GENERATED_DEFINITION,
    SIZE
  };

  static CommonDefinitions& instance();

  static void append(const std::string& source, Level level);

  static Source getSource();


private:
  CommonDefinitions();// = delete;


  CommonDefinitions(const CommonDefinitions&);// = delete;
  CommonDefinitions& operator=(const CommonDefinitions&) ;// = delete;

  std::vector<Source> _sources;
};

class RegisterCommonDefinition {
public:
  RegisterCommonDefinition(const char* definition,
                           CommonDefinitions::Level level
                              = CommonDefinitions::Level::USER_DEFINITION);
};

class RegisterCommonMacroDefinition {
public:
  template <typename T>
  RegisterCommonMacroDefinition(const char* name,
                                T&& value)
  {
    std::stringstream ss;
    ss << "#define " << name << " " << value;
    CommonDefinitions::append(ss.str(),
                              CommonDefinitions::Level::USER_DEFINITION);
  }
};


} // namespace executor

#endif // SOURCE_H_
