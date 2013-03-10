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
  use fodbc, SQLBindCol0 => SQLBindCol
  implicit none

  interface SQLBindCol
     module procedure SQLBindColInteger
     module procedure SQLBindColTimestamp
  end interface SQLBindCol

contains

  integer(kind=c_short) function SQLBindColInteger &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind)
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    integer(kind=c_int),intent(inout),target :: TargetValue
    integer(kind=c_long),intent(out) :: StrLen_or_Ind
    SQLBindColInteger = SQLBindCol0(StatementHandle,ColumnNumber,SQL_INTEGER, &
         c_loc(TargetValue),sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindColInteger

  integer(kind=c_short) function SQLBindColTimestamp &
       (StatementHandle,ColumnNumber,TargetValue,StrLen_or_Ind)
    type(c_ptr),intent(in),value :: StatementHandle
    integer(kind=c_short),intent(in),value :: ColumnNumber
    type(SQL_TIMESTAMP_STRUCT),intent(inout),target :: TargetValue
    integer(kind=c_long),intent(out) :: StrLen_or_Ind
    SQLBindColTimestamp = SQLBindCol0(StatementHandle,ColumnNumber,SQL_TYPE_TIMESTAMP, &
         c_loc(TargetValue),sizeof(TargetValue), StrLen_or_Ind)
  end function SQLBindColTimestamp

end module fodbc_ext
