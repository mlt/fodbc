cmake_minimum_required(VERSION 2.8)
enable_language(Fortran)

include_directories(${CMAKE_BINARY_DIR}/src)

add_executable(sqlite_test sqlite.f90)
target_link_libraries(sqlite_test fodbc)

# Win32 tests

add_executable(access_test access_win32.f90)
target_link_libraries(access_test fodbc)

find_package(KRACKEN QUIET)

if(KRACKEN_DIR)
  include_directories(${KRACKEN_DIR})
  add_executable(excel_test new_excel_win32.f90)
  target_link_libraries(excel_test fodbc kracken)
else()
  message("Download and build KRACKEN for Excel example, then reconfigure this project.")
endif()
