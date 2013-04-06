cmake_minimum_required(VERSION 2.8)
enable_language(Fortran)

if(WIN32)
  add_library(fodbc fodbc_types.f90 fodbc_win32.f90)
  target_link_libraries(fodbc -lodbc32)
else()
  add_library(fodbc fodbc_types.f90 fodbc.f90)
  target_link_libraries(fodbc -lodbc)
endif()

find_program(GCCXML gccxml)
find_program(XSLTPROC xsltproc)
find_file(HEADER sqlext.h /usr/include)

if(XSLTPROC)
  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/fodbc_ext.f90
    COMMAND xsltproc extensions.xslt types.xml > fodbc_ext.f90
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS types.xml extensions.xslt
    )
endif()

if(GCCXML AND XSLTPROC AND HEADER)
  add_custom_command(OUTPUT sql.xml
    COMMAND gccxml /usr/include/sqlext.h -fxml=sql.xml
    )

  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/fodbc.f90 ${CMAKE_CURRENT_SOURCE_DIR}/fodbc_win32.f90
    COMMAND xsltproc sql.xslt ${CMAKE_CURRENT_BINARY_DIR}/sql.xml > fodbc.f90
    COMMAND xsltproc -param win32 1 sql.xslt ${CMAKE_CURRENT_BINARY_DIR}/sql.xml > fodbc_win32.f90
    DEPENDS sql.xslt extensions.xslt sql.xml types.xml
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif()

export(TARGETS fodbc FILE fodbc-config.cmake)
export(PACKAGE FODBC)
