cmake_minimum_required(VERSION 2.8)
enable_language(Fortran)

set (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

add_executable(sqlite_test sqlite.f03)
target_link_libraries(sqlite_test fodbc)
set_source_files_properties(sqlite.f03 PROPERTIES LANGUAGE Fortran)

# Win32 tests

add_executable(access_test access_win32.f03)
target_link_libraries(access_test fodbc)
set_source_files_properties(access_win32.f03 PROPERTIES LANGUAGE Fortran)

add_executable(excel_test new_excel_win32.f03)
target_link_libraries(excel_test fodbc)
set_source_files_properties(new_excel_win32.f03 PROPERTIES LANGUAGE Fortran)
