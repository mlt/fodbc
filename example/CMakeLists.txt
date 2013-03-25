cmake_minimum_required(VERSION 2.8)
enable_language(Fortran)

include_directories(${CMAKE_BINARY_DIR}/src)

add_executable(sqlite_test sqlite.f03)
target_link_libraries(sqlite_test fodbc)
set_source_files_properties(sqlite.f03 PROPERTIES LANGUAGE Fortran)

# Win32 tests

add_executable(access_test access_win32.f03)
target_link_libraries(access_test fodbc)
set_source_files_properties(access_win32.f03 PROPERTIES LANGUAGE Fortran)

find_package(KRACKEN QUIET)

if(KRACKEN_DIR)
  include_directories(${KRACKEN_DIR})
  add_executable(excel_test new_excel_win32.f03)
  target_link_libraries(excel_test fodbc kracken)
  set_source_files_properties(new_excel_win32.f03 PROPERTIES LANGUAGE Fortran)
else()
  message("Download and build KRACKEN for Excel example, then reconfigure this project.")
endif()
