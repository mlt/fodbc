! This file is a part of fodbc, Fortran 2003 ODBC bindings
! Copyright (C) 2013 Mikhail Titov
!
! fodbc is free software: you can redistribute it and/or modify
! it under the terms of the GNU Affero General Public License as
! published by the Free Software Foundation, either version 3 of the
! License, or (at your option) any later version.
!
! fodbc is distributed in the hope that it will be useful,
! but WITHOUT ANY WARRANTY; without even the implied warranty of
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
! GNU Affero General Public License for more details.
!
! You should have received a copy of the GNU Affero General Public License
! along with fodbc.  If not, see <http://www.gnu.org/licenses/>.

module fodbc_types
  use iso_c_binding
  implicit none

  character(kind=c_char),pointer :: C_STR_NULL_PTR => NULL()
  integer(kind=c_short),pointer :: C_SHORT_NULL_PTR => NULL()

  integer(kind=c_short),parameter :: SQL_HANDLE_ENV = 1
  integer(kind=c_short),parameter :: SQL_HANDLE_DBC = 2
  integer(kind=c_short),parameter :: SQL_HANDLE_STMT = 3
  integer(kind=c_short),parameter :: SQL_HANDLE_DESC = 4

  type(c_ptr),parameter :: SQL_NULL_HENV = C_NULL_PTR
  type(c_ptr),parameter :: SQL_NULL_HDBC = C_NULL_PTR
  type(c_ptr),parameter :: SQL_NULL_HSTMT = C_NULL_PTR
  type(c_ptr),parameter :: SQL_NULL_HANDLE = C_NULL_PTR
  integer(c_short),parameter :: SQL_MAX_MESSAGE_LENGTH = 512

  ! /* identifiers of fields in the diagnostics area */
  ! #if (ODBCVER >= 0x0300)
  integer(c_short), parameter :: SQL_DIAG_RETURNCODE = 1
  integer(c_short), parameter :: SQL_DIAG_NUMBER = 2
  integer(c_short), parameter :: SQL_DIAG_ROW_COUNT = 3
  integer(c_short), parameter :: SQL_DIAG_SQLSTATE = 4
  integer(c_short), parameter :: SQL_DIAG_NATIVE = 5
  integer(c_short), parameter :: SQL_DIAG_MESSAGE_TEXT = 6
  integer(c_short), parameter :: SQL_DIAG_DYNAMIC_FUNCTION = 7
  integer(c_short), parameter :: SQL_DIAG_CLASS_ORIGIN = 8
  integer(c_short), parameter :: SQL_DIAG_SUBCLASS_ORIGIN = 9
  integer(c_short), parameter :: SQL_DIAG_CONNECTION_NAME = 10
  integer(c_short), parameter :: SQL_DIAG_SERVER_NAME = 11
  integer(c_short), parameter :: SQL_DIAG_DYNAMIC_FUNCTION_CODE = 12
  ! #endif

  ! /* SQL data type codes */
  integer(kind=c_short),parameter :: SQL_UNKNOWN_TYPE = 0
  integer(kind=c_short),parameter :: SQL_CHAR = 1
  integer(kind=c_short),parameter :: SQL_NUMERIC = 2
  integer(kind=c_short),parameter :: SQL_DECIMAL = 3
  integer(kind=c_short),parameter :: SQL_INTEGER = 4
  integer(kind=c_short),parameter :: SQL_SMALLINT = 5
  integer(kind=c_short),parameter :: SQL_FLOAT = 6
  integer(kind=c_short),parameter :: SQL_REAL = 7
  integer(kind=c_short),parameter :: SQL_DOUBLE = 8
  ! #if (ODBCVER >= 0x0300)
  integer(kind=c_short),parameter :: SQL_DATETIME = 9
  ! #endif
  integer(kind=c_short),parameter :: SQL_VARCHAR = 12

  ! /* One-parameter shortcuts for date/time data types */
  ! #if (ODBCVER >= 0x0300)
  integer(kind=c_short),parameter :: SQL_TYPE_DATE = 91
  integer(kind=c_short),parameter :: SQL_TYPE_TIME = 92
  integer(kind=c_short),parameter :: SQL_TYPE_TIMESTAMP = 93
  ! #endif

  ! /* Statement attribute values for cursor sensitivity */
  ! #if (ODBCVER >= 0x0300)
  integer(kind=c_short),parameter :: SQL_UNSPECIFIED = 0
  integer(kind=c_short),parameter :: SQL_INSENSITIVE = 1
  integer(kind=c_short),parameter :: SQL_SENSITIVE = 2
  ! #endif

  ! /* GetTypeInfo() request for all data types */
  integer(kind=c_short),parameter :: SQL_ALL_TYPES = 0

! /****************************
!  * some ret values
!  ***************************/
  integer(kind=c_short),parameter :: SQL_NULL_DATA = (-1)
  integer(kind=c_short),parameter :: SQL_DATA_AT_EXEC = (-2)
  integer(kind=c_short),parameter :: SQL_SUCCESS = 0
  integer(kind=c_short),parameter :: SQL_SUCCESS_WITH_INFO = 1
! #if (ODBCVER >= 0x0300)
  integer(kind=c_short),parameter :: SQL_NO_DATA = 100
! #endif
  integer(kind=c_short),parameter :: SQL_ERROR = (-1)
  integer(kind=c_short),parameter :: SQL_INVALID_HANDLE = (-2)
  integer(kind=c_short),parameter :: SQL_STILL_EXECUTING = 2
  integer(kind=c_short),parameter :: SQL_NEED_DATA = 99


  integer(kind=c_int),parameter :: SQL_NTS = -3
  integer(kind=c_short),parameter :: SQL_NTS2 = -3
  integer(kind=c_short),parameter :: SQL_DRIVER_COMPLETE = 1_2

  integer(kind=c_int),parameter :: SQL_ATTR_ODBC_VERSION = 200

  integer(kind=c_int),parameter :: SQL_OV_ODBC3 = 3
  integer(kind=c_int),parameter :: SQL_OV_ODBC3_80 = 380

  type, bind(C) :: SQL_TIMESTAMP_STRUCT
     integer(kind=c_short) :: year
     integer(kind=c_short) :: month
     integer(kind=c_short) :: day
     integer(kind=c_short) :: hour
     integer(kind=c_short) :: minute
     integer(kind=c_short) :: second
     integer(kind=c_int) :: fraction
  end type SQL_TIMESTAMP_STRUCT

  integer(kind=c_short),parameter :: SQL_PARAM_INPUT = 1
  integer(kind=c_short),parameter :: SQL_PARAM_INPUT_OUTPUT = 2
  integer(kind=c_short),parameter :: SQL_PARAM_OUTPUT = 4

end module fodbc_types
