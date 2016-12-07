///
/// \file Assert.h
///
///	\author Michel Steuwer <michel.steuwer@ed.ac.uk>
///

#ifndef ASSERT_H_
#define ASSERT_H_

//#ifdef NDEBUG
//
//#define ASSERT(e) (void(0))
//#define ASSERT_MESSAGE(e, ...) (void(0))
//#define ONLY_IN_DEBUG(e) (void(0))
//
//#else  // DEBUG

#define ASSERT(e)\
  executor::assert_impl::ASSERT_IMPL(__FILE__, __LINE__, e, #e)

#define ASSERT_MESSAGE(e, ...)\
  executor::assert_impl::ASSERT_IMPL(__FILE__, __LINE__, e, #e, __VA_ARGS__)

#define ONLY_IN_DEBUG(e) e

//#endif // NDEBUG

namespace executor {

namespace assert_impl {

///
/// \brief If expression evaluates to false an error message is printed and the
///        execution is aborted. If expression evaluates to true the function
///        call has no effect.
///
/// \param file A string naming the file the functions was called in. Used as
///             part of the printed the error message.
///
///        line An integer naming the line the functions was called in. Used as
///             part of the printed the error message.
///
///        expression The expression to be evaluated.
///
///        expressionString The expression as a string to be printed as part of
///                         the error message
///
void ASSERT_IMPL(const char* file,
                 const int   line,
                 const bool  expression,
                 const char* expressionString);

///
/// \brief If expression evaluates to false an error message is printed and the
///        execution is aborted. If expression evaluates to true the function
///        call has no effect.
///
/// \param file A string naming the file the functions was called in. Used as
///             part of the printed the error message.
///
///        line An integer naming the line the functions was called in. Used as
///             part of the printed the error message.
///
///        expression The expression to be evaluated.
///
///        expressionString The expression as a string to be printed as part of
///                         the error message
///
///        formatString A formated String to be printed as part of the error
///                     message. The String is evaluated by vsnprintf.
///
void ASSERT_IMPL(const char* file,
                 const int   line,
                 const bool  expression,
                 const char* expressionString,
                 const char* formatString, ...);

} // namespace assert_impl

} // namespace executor

#endif // ASSERT_H_

