cmake_minimum_required(VERSION 2.8)
enable_language(Fortran)

set (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

set (FODBC_SRCS
  fodbc_types.f03
  fodbc_ext.f03
)
set_source_files_properties(${FODBC_SRCS} fodbc.f03 fodbc_win32.f03 PROPERTIES LANGUAGE Fortran)

if(WIN32)
  add_library(fodbc ${FODBC_SRCS} fodbc_win32.f03)
  target_link_libraries(fodbc -lodbc32)
else()
  add_library(fodbc ${FODBC_SRCS} fodbc.f03)
  target_link_libraries(fodbc -lodbc)
endif()

find_program(GCCXML gccxml)
find_program(XSLTPROC xsltproc)
find_file(HEADER sqlext.h /usr/include)

if(XSLTPROC)
  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/fodbc_ext.f03
    COMMAND xsltproc extensions.xslt types.xml > fodbc_ext.f03
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    DEPENDS types.xml extensions.xslt
    )
endif()

if(GCCXML AND XSLTPROC AND HEADER)
  add_custom_command(OUTPUT sql.xml
    COMMAND gccxml /usr/include/sqlext.h -fxml=sql.xml
    )

  add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/fodbc.f03 ${CMAKE_CURRENT_SOURCE_DIR}/fodbc_win32.f03
    COMMAND xsltproc sql.xslt ${CMAKE_CURRENT_BINARY_DIR}/sql.xml > fodbc.f03
    COMMAND xsltproc -param win32 1 sql.xslt ${CMAKE_CURRENT_BINARY_DIR}/sql.xml > fodbc_win32.f03
    DEPENDS sql.xslt sql.xml
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
endif()
