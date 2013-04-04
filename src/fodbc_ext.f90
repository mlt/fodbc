! DO NOT EDIT! Generated file.
!
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

module fodbc_ext
  use fodbc, SQLBindCol0 => SQLBindCol, SQLGetData0 => SQLGetData
  use fodbc, SQLBindParameter0 => SQLBindParameter
  implicit none

  interface SQLBindCol
     module procedure SQLBindCol_SQL_INTEGER
     module procedure SQLBindCol_SQL_REAL
     module procedure SQLBindCol_SQL_DOUBLE
     module procedure SQLBindCol_SQL_TYPE_TIMESTAMP
     procedure SQLBindCol0
  end interface SQLBindCol

  interface SQLGetData
     module procedure SQLGetData_SQL_INTEGER
     module procedure SQLGetData_SQL_REAL
     module procedure SQLGetData_SQL_DOUBLE
     module procedure SQLGetData_SQL_TYPE_TIMESTAMP
     procedure SQLGetData0
  end interface SQLGetData

  interface SQLBindParameter
     module procedure SQLBindParameter_SQL_INTEGER
     module procedure SQLBindParameter_SQL_INTEGER_
     module procedure SQLBindParameter_SQL_INTEGER__
     module procedure SQLBindParameter_SQL_REAL
     module procedure SQLBindParameter_SQL_REAL_
     module procedure SQLBindParameter_SQL_REAL__
     module procedure SQLBindParameter_SQL_DOUBLE
     module procedure SQLBindParameter_SQL_DOUBLE_
     module procedure SQLBindParameter_SQL_DOUBLE__
     module procedure SQLBindParameter_SQL_TYPE_TIMESTAMP
     module procedure SQLBindParameter_SQL_TYPE_TIMESTAMP_
     module procedure SQLBindParameter_SQL_TYPE_TIMESTAMP__
     procedure SQLBindParameter0
  end interface SQLBindParameter

contains

  function SQLBindCol_SQL_INTEGER &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    integer(kind=c_int),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_INTEGER, &
        c_loc(TargetValue),sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindCol_SQL_INTEGER

  function SQLGetData_SQL_INTEGER &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    integer(kind=c_int),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_INTEGER, &
        c_loc(TargetValue),sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetData_SQL_INTEGER

  function SQLBindParameter_SQL_INTEGER &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    integer(kind=c_int),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_INTEGER,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_INTEGER

  function SQLBindParameter_SQL_INTEGER_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_int),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_INTEGER, &
    SQL_INTEGER, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_INTEGER_

  function SQLBindParameter_SQL_INTEGER__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_int),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_INTEGER, &
    SQL_INTEGER, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_INTEGER__
  
  function SQLBindCol_SQL_REAL &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    real(kind=c_float),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_REAL, &
        c_loc(TargetValue),sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindCol_SQL_REAL

  function SQLGetData_SQL_REAL &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    real(kind=c_float),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_REAL, &
        c_loc(TargetValue),sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetData_SQL_REAL

  function SQLBindParameter_SQL_REAL &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    real(kind=c_float),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_REAL,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_REAL

  function SQLBindParameter_SQL_REAL_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    real(kind=c_float),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_REAL, &
    SQL_REAL, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_REAL_

  function SQLBindParameter_SQL_REAL__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    real(kind=c_float),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_REAL, &
    SQL_REAL, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_REAL__
  
  function SQLBindCol_SQL_DOUBLE &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    real(kind=c_double),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_DOUBLE, &
        c_loc(TargetValue),sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindCol_SQL_DOUBLE

  function SQLGetData_SQL_DOUBLE &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    real(kind=c_double),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_DOUBLE, &
        c_loc(TargetValue),sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetData_SQL_DOUBLE

  function SQLBindParameter_SQL_DOUBLE &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    real(kind=c_double),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_DOUBLE,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_DOUBLE

  function SQLBindParameter_SQL_DOUBLE_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    real(kind=c_double),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_DOUBLE, &
    SQL_DOUBLE, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_DOUBLE_

  function SQLBindParameter_SQL_DOUBLE__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    real(kind=c_double),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_DOUBLE, &
    SQL_DOUBLE, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_DOUBLE__
  
  function SQLBindCol_SQL_TYPE_TIMESTAMP &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    type(SQL_TIMESTAMP_STRUCT),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLBindCol0(StatementHandle,ColumnNumber,SQL_TYPE_TIMESTAMP, &
        c_loc(TargetValue),sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindCol_SQL_TYPE_TIMESTAMP

  function SQLGetData_SQL_TYPE_TIMESTAMP &
        (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    type(SQL_TIMESTAMP_STRUCT),target :: TargetValue
    integer(kind=c_long),intent(out),optional :: StrLen_or_Ind
    ret = SQLGetData0(StatementHandle,ColumnNumber,SQL_TYPE_TIMESTAMP, &
        c_loc(TargetValue),sizeof(TargetValue),StrLen_or_Ind)
  end function SQLGetData_SQL_TYPE_TIMESTAMP

  function SQLBindParameter_SQL_TYPE_TIMESTAMP &
    (hstmt,ipar,fParamType,fSqlType, &
    cbColDef,ibScale,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    integer(kind=c_short),intent(in),value :: fSqlType
    integer(kind=c_long),intent(in),value :: cbColDef
    integer(kind=c_short),intent(in),value :: ibScale
    type(SQL_TIMESTAMP_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_TYPE_TIMESTAMP,fSqlType, &
    cbColDef,ibScale,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_TYPE_TIMESTAMP

  function SQLBindParameter_SQL_TYPE_TIMESTAMP_ &
    (hstmt,ipar,fParamType,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    integer(kind=c_short),intent(in),value :: fParamType
    type(SQL_TIMESTAMP_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,fParamType,SQL_TYPE_TIMESTAMP, &
    SQL_TYPE_TIMESTAMP, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_TYPE_TIMESTAMP_

  function SQLBindParameter_SQL_TYPE_TIMESTAMP__ &
    (hstmt,ipar,rgbValue,pcbValue) result(ret)
    integer(kind=c_short) :: ret
    type(c_ptr),intent(in),value :: hstmt
    integer(kind=c_short),intent(in),value :: ipar
    type(SQL_TIMESTAMP_STRUCT),target :: rgbValue
    integer(kind=c_long),intent(out),optional :: pcbValue
    ret = SQLBindParameter0(hstmt,ipar,SQL_PARAM_INPUT,SQL_TYPE_TIMESTAMP, &
    SQL_TYPE_TIMESTAMP, 0, 0_2,c_loc(rgbValue),sizeof(rgbValue),pcbValue)
  end function SQLBindParameter_SQL_TYPE_TIMESTAMP__
  

end module fodbc_ext
  