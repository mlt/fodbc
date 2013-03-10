cmake_minimum_required(VERSION 2.8)
enable_language(Fortran)

set (CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

set (FODBC_SRCS
  fodbc_types.f03
  fodbc.f03
  fodbc_ext.f03
)
set_source_files_properties(${FODBC_SRCS} PROPERTIES LANGUAGE Fortran)

add_library(fodbc ${FODBC_SRCS})
target_link_libraries(fodbc -lodbc)

add_custom_command(OUTPUT sql.xml
  COMMAND gccxml /usr/include/sqlext.h -fxml=sql.xml
)

add_custom_command(OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/fodbc.f03
  COMMAND xsltproc sql.xslt ${CMAKE_CURRENT_BINARY_DIR}/sql.xml > fodbc.f03
  DEPENDS  sql.xslt sql.xml
  WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)
